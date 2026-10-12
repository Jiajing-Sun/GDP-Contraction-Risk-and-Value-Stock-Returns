source('inference.R');source('portfolio_helpers.R')
U<-read.csv('data/us_panel.csv');E<-read.csv('data/europe_panel.csv');checks<-list()
check<-function(actual,expected,label){d<-max(abs(actual-expected));stopifnot(d<1e-10);checks[[label]]<<-data.frame(series=label,max_difference=d)}
legs<-list()
for(w in c('Value','Equal')){
 m<-read_block('raw_data/25_Portfolios_BEME_OP_5x5_CSV.zip',w);names(m)[-1]<-paste0('B',rep(1:5,each=5),'P',rep(1:5,5));q<-quarterly(m,names(m)[-1]);idx<-match(qkey(U$quarter)+1,qkey(q$quarter));s<-if(w=='Value')'VW'else'EW'
 C<-q$B1P1-q$B5P1-q$B1P5+q$B5P5;check(C[idx],U[[paste0('all_',s)]],paste0('US_all_',s))
 if(w=='Value')legs$all<-data.frame(quarter=U$quarter,low_growth=q$B1P1[idx],low_value=q$B5P1[idx],high_growth=q$B1P5[idx],high_value=q$B5P5[idx])
 for(region in c('US','EU')){
  f<-if(region=='US')'32_Portfolios_ME_BEME_OP_2x4x4_CSV.zip'else'Europe_32_Portfolios_ME_BE-ME_OP_2x4x4_CSV.zip'
  q<-get32(file.path('raw_data',f),w);D<-if(region=='US')U else E;idx<-match(qkey(D$quarter)+1,qkey(q$quarter))
  for(size in 1:2){
   if(region=='US')check(q[[paste0('S',size,'_contrast')]][idx],U[[paste0(if(size==1)'small_'else'big_',s)]],paste(region,size,s))
   if(region=='EU'&&w=='Value')check(q[[paste0('S',size,'_contrast')]][idx],E[[paste0('S',size,'_contrast')]],paste(region,size,s))
   if(w=='Value')legs[[paste(region,size,sep='_')]]<-data.frame(quarter=D$quarter,low_growth=q[[paste0('S',size,'B1P1')]][idx],low_value=q[[paste0('S',size,'B4P1')]][idx],high_growth=q[[paste0('S',size,'B1P4')]][idx],high_value=q[[paste0('S',size,'B4P4')]][idx])
  }
 }
}
write.csv(do.call(rbind,checks),'results/portfolio_reconstruction_comparison.csv',row.names=FALSE)
all<-do.call(rbind,lapply(names(legs),function(n)cbind(group=n,legs[[n]])));write.csv(all,'data/four_portfolio_returns.csv',row.names=FALSE)
r<-list();for(n in names(legs)){
 d<-merge(if(grepl('EU',n))E else U,legs[[n]],by='quarter')
 r[[n]]<-data.frame(group=n,t(sapply(c('low_growth','low_value','high_growth','high_value'),function(y)c(slope=coef(lm(reformulate(c('matched_x','matched_mu'),y),d))['matched_x'],mean=mean(d[[y]])))))
}
write.csv(do.call(rbind,r),'results/four_portfolio_slopes.csv',row.names=FALSE)
cat('All main US/EU portfolio returns reconstructed from monthly source archives.\n')
