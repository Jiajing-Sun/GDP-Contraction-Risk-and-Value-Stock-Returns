source('inference.R')
suppressPackageStartupMessages(library(data.table))
files<-list.files('data',pattern='\\.zip$',full.names=TRUE);files<-files[grepl('\\[monthly\\]',files)]
x<-rbindlist(lapply(files,function(f)read.csv(unz(f,unzip(f,list=TRUE)$Name[1]))))
stopifnot(!anyDuplicated(x[,.(location,name,weighting,date,pf)]))
x[,return_quarter:=paste0(substr(date,1,4),'Q',(as.integer(substr(date,6,7))-1)%/%3+1)]
x[,quarter:=paste0((qkey(return_quarter)-2)%/%4,'Q',(qkey(return_quarter)-2)%%4+1)]
E<-read.csv('data/europe_panel.csv');origins<-E$quarter[E$quarter>='2001Q2'&E$quarter<='2025Q3']
x<-x[quarter%in%origins & pf%in%c(1,3)]
q<-x[,.(outcome=if(.N==6 && all(is.finite(ret)) && all(n>=5) && all(table(pf)==3))100*(sum(ret[pf==1])-sum(ret[pf==3]))else NA_real_),by=.(location,name,weighting,quarter)]
q[,col:=paste(location,name,weighting,sep='_')];wide<-dcast(q,quarter~col,value.var='outcome')
wide<-wide[match(origins,quarter)];stopifnot(ncol(wide)==97)
cc<-strsplit('aut bel dnk fin fra deu grc irl ita nld nor prt esp swe che gbr',' ')[[1]]
ea<-strsplit('aut bel fin fra deu grc irl ita nld prt esp',' ')[[1]];ne<-setdiff(cc,ea)
make_panel<-function(cs,allweights=TRUE){
 cols<-c('quarter',unlist(lapply(cs,function(c)grep(paste0('^',c,'_'),names(wide),value=TRUE))))
 w<-as.data.frame(wide)[,cols];w<-w[complete.cases(w),];p<-merge(E,w,by='quarter')
 for(f in c('be_me','ope_be'))for(weight in if(allweights)c('vw','vw_cap','ew')else'vw'){
  groups<-list(all=cs,ea=intersect(cs,ea),ne=ne)
  for(g in names(groups))p[[paste(f,weight,g,sep='_')]]<-rowMeans(p[,paste(groups[[g]],f,weight,sep='_'),drop=FALSE])
  p[[paste(f,weight,'diff',sep='_')]]<-p[[paste(f,weight,'ea',sep='_')]]-p[[paste(f,weight,'ne',sep='_')]]
 };p
}
checks<-list()
for(kind in c('country','country_coverage')){
 p<-if(kind=='country')make_panel(cc)else make_panel(setdiff(cc,'irl'),FALSE)
 ref<-read.csv(paste0('data/',kind,'_panel.csv'));stopifnot(identical(p$quarter,ref$quarter))
 cols<-intersect(names(p)[vapply(p,is.numeric,TRUE)],names(ref));dif<-max(abs(as.matrix(p[,cols])-as.matrix(ref[,cols])),na.rm=TRUE);stopifnot(dif<1e-10)
 write.csv(p,paste0('data/',kind,'_panel.csv'),row.names=FALSE)
 checks[[kind]]<-data.frame(panel=kind,n=nrow(p),maximum_difference=dif)
}
write.csv(do.call(rbind,checks),'results/country_data_comparison.csv',row.names=FALSE)
cat('Country panels reconstructed in R: 63 and 71 common survey dates.\n')
