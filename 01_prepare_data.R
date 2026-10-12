source('inference.R')
# Individual responses are matched by reporting ID and target calendar year.
I<-as.data.frame(read_excel('raw_data/Individual_PRGDP.xlsx',na=c('NA','#N/A'),col_types='numeric'))
I$quarter<-quarter_string(I$YEAR,I$QUARTER);I<-I[I$quarter>='1992Q1'&I$quarter<='2025Q4',]
point<-as.data.frame(read_excel('raw_data/Individual_RGDP.xlsx',sheet='RGDP',na=c('NA','#N/A'),col_types='numeric'))
point$quarter<-quarter_string(point$YEAR,point$QUARTER);point$point<-100*(point$RGDPB/point$RGDPA-1)
us<-list()
for(q in unique(I$quarter)){
 a<-I[I$quarter==q,];lay<-layout_for(q);v<-as.matrix(a[,lay$cols]);s<-rowSums(v)
 ok<-rowSums(!is.finite(v))==0 & rowSums(v<0,na.rm=TRUE)==0 & abs(s-100)<=.5;ok[is.na(ok)]<-FALSE
 pr<-v[ok,,drop=FALSE]/s[ok]
 u<-data.frame(quarter=q,ID=a$ID[ok],D=100*rowSums(pr[,lay$bounds[,2]<=0,drop=FALSE]))
 for(k in c('edge','mid','wide'))u[[paste0('mu_',k)]]<-drop(pr%*%representatives(lay$bounds,c(edge=0,mid=.5,wide=1)[k]))
 u$width<-sqrt(pmax(drop(pr%*%(representatives(lay$bounds,.5)^2))-u$mu_mid^2,0));us[[q]]<-u
}
us<-merge(do.call(rbind,us),point[,c('quarter','ID','point')],by=c('quarter','ID'),all.x=TRUE)
# ECB blank bins become zero only for responses that satisfy the probability-sum rule.
eu<-list();signed<-function(x)as.numeric(gsub('_','.',sub('^N','-',x)))
z<-'raw_data/ECB_SPF_individual_forecasts.zip';files<-sort(unzip(z,list=TRUE)$Name);files<-files[grepl('^[0-9]{4}Q[1-4]\\.csv$',files)&substr(files,1,6)<='2025Q4']
for(file in files){
 q<-substr(file,1,6);lines<-readLines(unz(z,file),warn=FALSE,encoding='UTF-8');i<-grep('^GROWTH EXPECTATIONS',lines)[1]
 h<-as.character(read.csv(text=lines[i+1],header=FALSE,check.names=FALSE,fill=TRUE)[1,]);cols<-which(seq_along(h)>=4 & !is.na(h) & h!="NA" & nzchar(trimws(h)));bins<-h[cols]
 starts<-vapply(bins[-1],function(b)signed(sub('^F(N?[0-9]+_[0-9]+).*','\\1',b)),1.0)
 lo<-c(-Inf,starts);hi<-c(starts,Inf);stopifnot(all(diff(starts)>0),0%in%starts)
 mids<-(lo+hi)/2;mids[1]<-hi[1]-.5*(hi[2]-lo[2]);mids[length(mids)]<-tail(lo,1)+.5*(hi[length(hi)-1]-lo[length(lo)-1])
 j<-i+2;end<-j
 while(end<=length(lines)&&nzchar(trimws(strsplit(lines[end],',',fixed=TRUE)[[1]][1])))end<-end+1
 a<-read.csv(text=paste(lines[j:(end-1)],collapse='\n'),header=FALSE,fill=TRUE,colClasses='character',na.strings='',check.names=FALSE)
 a<-a[!is.na(a[[1]])&a[[1]]==as.character(as.integer(substr(q,1,4))+1),,drop=FALSE]
 if(!nrow(a))next
 while(ncol(a)<length(h))a[[ncol(a)+1]]<-NA_character_
 v<-as.matrix(data.frame(lapply(a[,cols,drop=FALSE],as.numeric)));v[is.na(v)]<-0;s<-rowSums(v)
 ok<-abs(s-100)<=.5 & rowSums(v<0)==0 & !is.na(a[[3]]);pr<-v[ok,,drop=FALSE]/s[ok]
 eu[[q]]<-data.frame(quarter=q,ID=as.integer(a[[2]][ok]),D=100*rowSums(pr[,hi<=0,drop=FALSE]),mu_mid=drop(pr%*%mids),point=as.numeric(a[[3]][ok]))
}
eu<-do.call(rbind,eu)
pair<-function(ind,joint=FALSE){
 out<-list()
 for(q in sort(unique(ind$quarter))){
  if(substr(q,6,6)=='1')next
  prev<-paste0(substr(q,1,5),as.integer(substr(q,6,6))-1)
  a<-ind[ind$quarter==q,];b<-ind[ind$quarter==prev,]
  if(joint){a<-a[is.finite(a$point),];b<-b[is.finite(b$point),]}
  d<-merge(a,b,by='ID',suffixes=c('_n','_p'));if(nrow(d)<5)next
  r<-data.frame(quarter=q,n_pair=nrow(d),x=mean(d$D_n-d$D_p)/10)
  for(k in c(grep('^mu_',names(ind),value=TRUE),if(joint)'point'))r[[k]]<-mean(d[[paste0(k,'_n')]]-d[[paste0(k,'_p')]])
  if('width'%in%names(ind)){r$width<-mean(d$width_n-d$width_p);r$disagreement<-sd(d$mu_mid_n)-sd(d$mu_mid_p)}
  out[[q]]<-r
 };do.call(rbind,out)
}
checks<-list()
for(region in c('US','EU'))for(kind in c('density','joint')){
 d<-pair(if(region=='US')us else eu,kind=='joint');fn<-paste0(region,'_',kind,'_revisions.csv')
 write.csv(d,file.path('results',fn),row.names=FALSE);ref<-read.csv(file.path('reference_results',fn));m<-match(ref$quarter,d$quarter);stopifnot(!anyNA(m),nrow(d)==nrow(ref))
 for(c in setdiff(names(ref),'quarter')){
  dif<-max(abs(d[[c]][m]-ref[[c]]));checks[[paste(region,kind,c)]]<-data.frame(region,kind,variable=c,max_difference=dif);if(dif>=1e-10)stop(paste(region,kind,c,dif))
 }
 write.csv(d,file.path('data',fn),row.names=FALSE)
}
write.csv(do.call(rbind,checks),'results/data_conversion_comparison.csv',row.names=FALSE)
# Rebuilt core predictors replace their snapshots only after numerical equivalence is verified.
U<-read.csv('data/us_panel.csv');d<-pair(us);m<-match(U$quarter,d$quarter)
stopifnot(max(abs(U$matched_x-d$x[m]))<1e-10,max(abs(U$matched_mu-d$mu_mid[m]))<1e-10,max(abs(U$matched_width-d$width[m]))<1e-10,max(abs(U$matched_disagree-d$disagreement[m]))<1e-10)
cat('R reconstruction matches all US and ECB density and joint-point revisions.\n')
