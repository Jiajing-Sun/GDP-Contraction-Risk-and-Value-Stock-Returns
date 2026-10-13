# Publication quantities computed in R, before rounding for display.
source('inference.R')
u<-read.csv('data/us_panel.csv');l<-read.csv('data/four_portfolio_returns.csv')
a<-l[l$group=='all',];a<-a[match(u$quarter,a$quarter),]
v<-list(probability=u$matched_x*10,mean=u$matched_mu,width=u$matched_width,disagreement=u$matched_disagree,aggregate=u$GrowthMinusValue_lead1,low=a$low_growth-a$low_value,high=a$high_growth-a$high_value,C=u$all_VW)
s<-t(vapply(v,function(x)c(mean=mean(x),sd=sd(x),q25=unname(quantile(x,.25)),median=median(x),q75=unname(quantile(x,.75))),numeric(5)))
write.csv(data.frame(variable=names(v),s),'results/publication_descriptives.csv',row.names=FALSE)
f<-lm(all_VW~matched_x+matched_mu,u);x<-resid(lm(matched_x~matched_mu,u))*10
write.csv(data.frame(quarter=u$quarter,original=u$matched_x*10,residual=x,share=100*x^2/sum(x^2),leverage=hatvalues(f)),'results/publication_influence.csv',row.names=FALSE)
ex<-cbind(u[,c('quarter','n_pair','matched_x','matched_mu')],a[,c('low_growth','low_value','high_growth','high_value')],C=u$all_VW,fitted=fitted(f));ex$probability<-ex$matched_x*10
write.csv(ex,'results/publication_examples.csv',row.names=FALSE)
read_section<-function(file,heading){
 lines<-readLines(unz(file,unzip(file,list=TRUE)$Name[1]),warn=FALSE);k<-grep(heading,lines,fixed=TRUE)[1];stopifnot(is.finite(k))
 start<-k+1;while(!grepl('^\\s*[0-9]{6}\\s*,',lines[start]))start<-start+1
 end<-start;while(end<length(lines)&&grepl('^\\s*[0-9]{6}\\s*,',lines[end+1]))end<-end+1
 d<-read.csv(text=paste(lines[(start-1):end],collapse='\n'),check.names=FALSE);names(d)<-c('time',paste0('cell',seq_len(ncol(d)-1)));d$time<-trimws(as.character(d$time));d
}
chars<-list();month<-paste0(substr(u$quarter,1,4),sprintf('%02d',as.integer(substr(u$quarter,6,6))*3))
for(reg in c('all','US'))for(metric in c('Number of Firms in Portfolios','Value Weight Average of BE/ME Calculated','Value Weight Average of OP Calculated')){
 file<-if(reg=='all')'25_Portfolios_BEME_OP_5x5_CSV.zip'else'32_Portfolios_ME_BEME_OP_2x4x4_CSV.zip';d<-read_section(file.path('raw_data',file),metric)
 for(size in if(reg=='all')0 else 1:2){idx<-if(size==0)c(1,21,5,25)else(size-1)*16+c(1,13,4,16)
 for(j in 1:4){z<-d[[idx[j]+1]][match(month,d$time)];chars[[length(chars)+1]]<-data.frame(group=if(size==0)'all'else paste0('US_',size),leg=c('low_growth','low_value','high_growth','high_value')[j],metric,median=median(z,na.rm=TRUE))}}
}
write.csv(do.call(rbind,chars),'results/publication_characteristics.csv',row.names=FALSE)
slopes<-read.csv('results/four_portfolio_slopes.csv');slopes$portfolio<-rep(c('low_growth','low_value','high_growth','high_value'),length.out=nrow(slopes));write.csv(slopes,'results/four_portfolio_slopes.csv',row.names=FALSE)
for(n in c('model_estimates','key_findings','four_portfolio_slopes','publication_descriptives','publication_examples','publication_characteristics','publication_influence')){
 d<-read.csv(paste0('results/',n,'.csv'),check.names=FALSE);for(j in names(d))if(is.numeric(d[[j]]))d[[j]]<-sprintf('%.4f',d[[j]]);write.csv(d,paste0('results/',n,'_four_decimals.csv'),row.names=FALSE)
}
cat('Publication quantities computed in R; four-decimal display copies saved.\n')
