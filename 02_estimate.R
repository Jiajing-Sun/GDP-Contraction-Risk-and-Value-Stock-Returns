source('inference.R')
designs<-fromJSON('data/model_designs.json',simplifyVector=FALSE)
fits<-list();comparisons<-list()
for(i in seq_along(designs)){
 z<-designs[[i]];r<-z$row[[1]]
 X<-do.call(rbind,lapply(z$X,unlist));colnames(X)<-unlist(z$columns)
 d<-data.frame(quarter=unlist(z$quarter),X[,-1,drop=FALSE],check.names=FALSE);d[[r$outcome]]<-unlist(z$y)
 f<-fit_ev(d,r$outcome,colnames(X)[-1],r$focal,r$label,r$family,lags=r$lags,bw=r$bw)
 fits[[i]]<-f
 comparisons[[i]]<-data.frame(family=r$family,label=r$label,coefficient_before=r$coef,coefficient_R=f$row$coef,p_before=r$p_dwb,p_R=f$row$p_dwb,ci_low_difference=f$row$ci95_low-r$ci95_low,ci_high_difference=f$row$ci95_high-r$ci95_high)
 if(i%%10==0){cat('Estimated',i,'of',length(designs),'models\n');flush.console()}
}
check<-do.call(rbind,comparisons);write.csv(check,'results/numerical_comparison.csv',row.names=FALSE)
stopifnot(max(abs(check$coefficient_R-check$coefficient_before))<1e-8,max(abs(check$p_R-check$p_before))<1e-8)
fam<-vapply(fits,function(x)x$row$family,'');out<-do.call(rbind,lapply(split(fits,fam),adjust_family))
write.csv(out,'results/model_estimates.csv',row.names=FALSE)
writeLines(capture.output(sessionInfo()),'results/session_info.txt')
cat('All',nrow(check),'coefficient and marginal p-value comparisons passed.\n')
