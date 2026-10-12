suppressPackageStartupMessages({library(readxl);library(jsonlite);library(digest)})
ROOT <- normalizePath(".")
put <- function(d,n,folder="results") write.csv(d,file.path(folder,paste0(n,".csv")),row.names=FALSE)
source("survey_helpers.R")
# General French archive parser: first numeric block, with explicit frequency.
read_ff<-function(file,daily=FALSE){
 lines<-readLines(unz(file,unzip(file,list=TRUE)$Name[1]),warn=FALSE)
 regex<-if(daily)'^\\s*[0-9]{8}\\s*,' else '^\\s*[0-9]{6}\\s*,'
 start<-which(grepl(regex,lines))[1];stopifnot(is.finite(start))
 end<-start;while(end<length(lines)&&grepl(regex,lines[end+1]))end<-end+1
 d<-read.csv(text=paste(lines[(start-1):end],collapse='\n'),check.names=FALSE)
 names(d)[1]<-'time';d$time<-trimws(as.character(d$time))
 for(j in 2:ncol(d)){d[[j]]<-as.numeric(d[[j]]);d[[j]][d[[j]]%in%c(-99.99,-999)]<-NA_real_}
 d
}
periodq<-function(m)paste0(substr(m,1,4),'Q',(as.integer(substr(m,5,6))-1)%/%3+1)
compound<-function(v)if(any(!is.finite(v))||!length(v))NA_real_ else 100*(prod(1+v/100)-1)
quarterly<-function(m,cols){
 m$quarter<-periodq(m$time)
 out<-do.call(rbind,lapply(split(m,m$quarter),function(z){
  ans<-sapply(cols,function(c)if(nrow(z)==3)compound(z[[c]])else NA_real_)
  data.frame(quarter=z$quarter[1],as.list(ans),check.names=FALSE)
 }));out[order(qkey(out$quarter)),]
}
RNGkind('Mersenne-Twister','Inversion','Rejection')
B<-9999L; key0<-qkey('1992Q1'); key1<-qkey('2026Q4')
WEIGHTS<-list()
getW<-function(keys,bw=4){
 k<-as.character(bw)
 if(is.null(WEIGHTS[[k]])){
  set.seed(20261011+bw)
  WEIGHTS[[k]]<<-dependent_wild_multipliers(key1-key0+1,B,bw)
 }
 WEIGHTS[[k]][keys-key0+1,,drop=FALSE]
}
hac_cal<-function(u,X,bread,keys,lags=4){
 z<-X*u; meat<-crossprod(z)
 for(l in seq_len(lags)){
  j<-match(keys-l,keys);i<-which(!is.na(j))
  if(length(i)){g<-crossprod(z[i,,drop=FALSE],z[j[i],,drop=FALSE]);meat<-meat+(1-l/(lags+1))*(g+t(g))}
 }
 bread%*%meat%*%bread*nrow(X)/(nrow(X)-ncol(X))
}
fit_ev<-function(d,outcome,regs,focal=regs[1],label='',family='',lags=4,bw=4){
 d<-d[complete.cases(d[,c('quarter',outcome,regs)]),];d<-d[order(qkey(d$quarter)),]
 X<-cbind(Intercept=1,as.matrix(d[,regs,drop=FALSE]));y<-d[[outcome]];keys<-qkey(d$quarter)
 stopifnot(!anyDuplicated(keys),all(is.finite(X)),all(is.finite(y)),qr(X)$rank==ncol(X),nrow(X)>ncol(X)+8)
 j<-match(focal,colnames(X));n<-length(y);bread<-solve(crossprod(X));P<-bread%*%t(X)
 beta<-drop(P%*%y);fitted<-drop(X%*%beta);u<-y-fitted;V<-hac_cal(u,X,bread,keys,lags);se<-sqrt(V[j,j])
 W<-getW(keys,bw);Z<-X[,-j,drop=FALSE];P0<-solve(crossprod(Z),t(Z));f0<-drop(Z%*%(P0%*%y))
 u0<-(y-f0)/sqrt(pmax(1-rowSums(Z*t(P0)),1e-8))
 student<-function(ys,center=0){
  bs<-P%*%ys;us<-ys-X%*%bs;score<-us*drop(X%*%bread[,j]);v<-colSums(score^2)
  for(l in seq_len(lags)){jj<-match(keys-l,keys);ii<-which(!is.na(jj));if(length(ii))v<-v+2*(1-l/(lags+1))*colSums(score[ii,,drop=FALSE]*score[jj[ii],,drop=FALSE])}
  (bs[j,]-center)/sqrt(pmax(v*n/(n-ncol(X)),1e-15))
 }
 ts<-student(f0+u0*W)
 # Invert the same null-imposed test used for p_dwb; unrestricted bootstrap
 # intervals can disagree materially in this high-leverage sample.
 h0<-sqrt(pmax(1-rowSums(Z*t(P0)),1e-8))
 r1<-drop(X[,j]-Z%*%(P0%*%X[,j]))/h0
 U0<-u0*W;U1<-r1*W
 A0<-drop(P[j,,drop=FALSE]%*%U0);A1<-drop(P[j,,drop=FALSE]%*%U1)
 direction<-drop(X%*%bread[,j])
 Q0<-(U0-X%*%(P%*%U0))*direction;Q1<-(U1-X%*%(P%*%U1))*direction
 cv<-function(a,b){
  v<-colSums(a*b)
  for(l in seq_len(lags)){jj<-match(keys-l,keys);ii<-which(!is.na(jj));if(length(ii))v<-v+(1-l/(lags+1))*colSums(a[ii,,drop=FALSE]*b[jj[ii],,drop=FALSE]+b[ii,,drop=FALSE]*a[jj[ii],,drop=FALSE])}
  v*n/(n-ncol(X))
 }
 v00<-cv(Q0,Q0);v01<-cv(Q0,Q1);v11<-cv(Q1,Q1)
 pc<-function(c){tt<-(A0-c*A1)/sqrt(pmax(v00-2*c*v01+c*c*v11,1e-15));(1+sum(abs(tt)>=abs((beta[j]-c)/se)))/(B+1)}
 stopifnot(abs(pc(0)-(1+sum(abs(ts)>=abs(beta[j]/se)))/(B+1))<1e-8)
 span<-max(10,8*se)
 for(zz in 1:8){grid<-sort(unique(c(seq(beta[j]-span,beta[j]+span,length.out=161),0,beta[j])));acc<-vapply(grid,function(c)pc(c)>=.05,TRUE);if(!acc[1]&&!tail(acc,1))break;span<-span*3}
 accepted<-which(acc);stopifnot(length(accepted)>0,!acc[1],!tail(acc,1))
 transitions<-which(diff(acc)!=0)
 bounds<-sapply(transitions,function(k){lo<-grid[k];hi<-grid[k+1];val<-acc[k];for(z in 1:18){mid<-(lo+hi)/2;if((pc(mid)>=.05)==val)lo<-mid else hi<-mid};(lo+hi)/2})
 low<-min(bounds);high<-max(bounds);nc<-length(bounds)/2
 ci_set<-paste(vapply(seq(1,length(bounds),by=2),function(k)sprintf('[%.6f, %.6f]',bounds[k],bounds[k+1]),''),collapse=' U ')
 row<-data.frame(family,label,outcome,focal,n,start=d$quarter[1],end=tail(d$quarter,1),coef=beta[j],se_hac=se,
  ci95_low=low,ci95_high=high,ci_components=nc,ci95_set=ci_set,p_dwb=(1+sum(abs(ts)>=abs(beta[j]/se)))/(B+1),
  p_hac=2*pnorm(-abs(beta[j]/se)),r2=1-sum(u^2)/sum((y-mean(y))^2),max_leverage=max(rowSums(X*t(P))),lags,bw,
  controls=paste(setdiff(regs,focal),collapse=';'),row.names=NULL)
 list(row=row,stars=ts,t=beta[j]/se,frame=d,X=X,y=y,beta=beta,V=V)
}
adjust_family<-function(fs){
 # Distinct multiplier bandwidths currently use distinct RNG streams. Do not
 # treat their columns as a valid joint bootstrap. Control each same-process
 # subgroup by RW, then use a Bonferroni union bound across bandwidth groups.
 bws<-vapply(fs,function(f)f$row$bw,1.0)
 if(length(unique(bws))>1){
  p<-numeric(length(fs));G<-length(unique(bws))
  for(b in unique(bws)){ix<-which(bws==b);p[ix]<-pmin(1,G*adjust_family(fs[ix])$p_rw_family)}
  tab<-do.call(rbind,lapply(fs,`[[`,'row'));tab$p_rw_family<-p;return(tab)
 }
 ts<-vapply(fs,function(f)f$t,1.0);stars<-sapply(fs,function(f)f$stars);o<-order(abs(ts),decreasing=TRUE);p<-rep(NA,length(fs));last<-0
 for(k in seq_along(o)){rem<-o[k:length(o)];p[o[k]]<-last<-max(last,(1+sum(apply(abs(stars[,rem,drop=FALSE]),1,max)>=abs(ts[o[k]])))/(B+1))}
 tab<-do.call(rbind,lapply(fs,`[[`,'row'));tab$p_rw_family<-p;tab
}
