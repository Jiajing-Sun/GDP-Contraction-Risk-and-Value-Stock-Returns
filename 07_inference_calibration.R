source('inference.R')
outdir<-'results';U<-read.csv('data/us_panel.csv')
X<-cbind(1,U$matched_x,U$matched_mu);Z<-X[,-2];n<-nrow(X);keys<-qkey(U$quarter)
P<-solve(crossprod(X),t(X));bread<-solve(crossprod(X));P0<-solve(crossprod(Z),t(Z));h0<-rowSums(Z*t(P0));dir<-drop(X%*%bread[,2])
M<-1000L;BB<-1999L;set.seed(20261022)
# Fixed common random multipliers across Monte Carlo designs; full calendar grid.
W<-dependent_wild_multipliers(max(keys)-min(keys)+1,BB,4)[keys-min(keys)+1,,drop=FALSE]
lagpairs<-lapply(1:4,function(l){j<-match(keys-l,keys);i<-which(!is.na(j));list(i=i,j=j[i],weight=1-l/5)})
t_batch<-function(Y){
 b<-P%*%Y;u<-Y-X%*%b;s<-u*dir;v<-colSums(s*s)
 for(p in lagpairs)v<-v+2*p$weight*colSums(s[p$i,,drop=FALSE]*s[p$j,,drop=FALSE])
 b[2,]/sqrt(pmax(v*n/(n-ncol(X)),1e-15))
}
crisis<-function(k)(k>=qkey('2007Q3')&k<=qkey('2009Q4'))|(k>=qkey('2020Q1')&k<=qkey('2021Q4'))
specs<-expand.grid(ar=c(0,.3),crisis_sd=c(1,3));results<-list();draws<-list()
for(s in 1:nrow(specs)){
 ar<-specs$ar[s];sigma<-ifelse(crisis(keys),specs$crisis_sd[s],1);pvals<-numeric(M);ha<-numeric(M)
 set.seed(20261023) # same innovations isolate effects of variance/dependence choices
 T<-max(keys)-min(keys)+1
 innov<-matrix(rnorm((T+200)*M),T+200,M);e<-innov
 if(ar>0)for(t in 2:nrow(e))e[t,]<-ar*e[t-1,]+sqrt(1-ar^2)*innov[t,]
 Y<-e[200+keys-min(keys)+1,,drop=FALSE]*sigma
 for(m in 1:M){
  y<-Y[,m];obs<-t_batch(matrix(y,ncol=1));f0<-drop(Z%*%(P0%*%y));u0<-(y-f0)/sqrt(1-h0)
  tb<-t_batch(f0+u0*W);pvals[m]<-(1+sum(abs(tb)>=abs(obs)))/(BB+1);ha[m]<-2*pnorm(-abs(obs))
 }
 reject<-sum(pvals<=.05);ci<-binom.test(reject,M)$conf.int
 results[[s]]<-data.frame(ar,crisis_sd=specs$crisis_sd[s],M,bootstrap_B=BB,rejection=reject/M,ci_low=ci[1],ci_high=ci[2],hac_rejection=mean(ha<=.05))
 draws[[s]]<-data.frame(ar,crisis_sd=specs$crisis_sd[s],iteration=1:M,p_boot=pvals,p_hac=ha)
 cat('Calibration',s,'of',nrow(specs),'rejection',reject/M,'\n');flush.console()
 put(do.call(rbind,results),'size_calibration',outdir)
}
put(do.call(rbind,draws),'size_calibration_draws',outdir)
# Numerical equivalence to production null-imposed p value for the same multipliers.
oldB<-B;B<-BB;oldWeights<-WEIGHTS;WEIGHTS<-list();WEIGHTS[['4']]<-matrix(0,key1-key0+1,BB);WEIGHTS[['4']][keys-key0+1,]<-W
prod<-fit_ev(U,'all_VW',c('matched_x','matched_mu'))$row$p_dwb
y<-U$all_VW;f0<-drop(Z%*%(P0%*%y));u0<-(y-f0)/sqrt(1-h0)
direct<-(1+sum(abs(t_batch(f0+u0*W))>=abs(t_batch(matrix(y,ncol=1)))))/(BB+1)
stopifnot(abs(prod-direct)<1e-12)
write_json(list(M=M,bootstrap_B=BB,production_bootstrap_B=9999,method_matches=TRUE,observed_p_for_common_B=prod,
 interpretation='Fixed-design sensitivity exercise under specified Gaussian error laws, not a proof of validity for unknown errors; inner draws fewer than production.'),file.path(outdir,'size_calibration_validation.json'),pretty=TRUE,auto_unbox=TRUE)
