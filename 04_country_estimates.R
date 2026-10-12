source('inference.R')
put<-function(d,n,folder='results')write.csv(d,file.path(folder,paste0('country_',n,'.csv')),row.names=FALSE)
O<-'results';D<-read.csv('data/country_panel.csv')
cc<-strsplit('aut bel dnk fin fra deu grc irl ita nld nor prt esp swe che gbr',' ')[[1]]
ea<-strsplit('aut bel fin fra deu grc irl ita nld prt esp',' ')[[1]];ne<-setdiff(cc,ea)
fit<-function(y,lab,fam,regs=c('matched_x','matched_mu'))fit_ev(D,y,regs,label=lab,family=fam)
fs<-list();for(f in c('be_me','ope_be'))for(g in c('all','ea','ne','diff')){
 y<-paste(f,'vw',g,sep='_');fs[[y]]<-fit(y,y,'country_primary8')
}
put(adjust_family(fs),'primary8',O);cat('Primary eight complete\n');flush.console()
fs2<-list();for(f in c('be_me','ope_be'))for(c in cc){
 y<-paste(c,f,'vw',sep='_');fs2[[y]]<-fit(y,y,'country_exploratory32')
}
put(adjust_family(fs2),'country32',O);cat('Country estimates complete\n');flush.console()
fs3<-list();for(w in c('vw_cap','ew'))for(f in c('be_me','ope_be'))for(g in c('all','ea','ne','diff')){
 y<-paste(f,w,g,sep='_');fs3[[y]]<-fit(y,y,'weight_sensitivity16')
}
put(adjust_family(fs3),'weight_sensitivity16',O)
loo<-list()
for(f in c('be_me','ope_be'))for(w in c('vw','vw_cap','ew'))for(g in c('all','ea','ne','diff'))for(c in if(g=='ea')ea else if(g=='ne')ne else cc){
 cs<-setdiff(cc,c);ea1<-intersect(ea,cs);ne1<-intersect(ne,cs)
 avg<-function(v)rowMeans(D[,paste(v,f,w,sep='_'),drop=FALSE])
 D$loo_y<-switch(g,all=avg(cs),ea=avg(ea1),ne=avg(ne1),diff=avg(ea1)-avg(ne1))
 m<-lm(loo_y~matched_x+matched_mu,D)
 loo[[length(loo)+1]]<-data.frame(name=f,weighting=w,group=g,excluded=c,n=nobs(m),coef=unname(coef(m)['matched_x']))
}
put(do.call(rbind,loo),'leave_country_out',O)
# Ex post covariation attribution, with the original French outcome unchanged.
fs4<-list();for(y in c('S1_contrast','S2_contrast'))for(s in c('baseline','country_style_controls')){
 regs<-c('matched_x','matched_mu',if(s!='baseline')c('be_me_vw_diff','ope_be_vw_diff'))
 fs4[[paste(y,s)]]<-fit(y,paste(y,s),'french_attribution4',regs)
}
put(adjust_family(fs4),'french_attribution4',O)
# Direct joint test: reject only if country slope dispersion exceeds a common-slope null.
# Keep cross-country dependence by applying identical calendar multipliers to all countries.
ht<-list();X<-cbind(1,D$matched_x,D$matched_mu);P<-solve(crossprod(X),t(X));keys<-qkey(D$quarter)
W<-getW(keys,4);Z<-X[,c(1,3),drop=FALSE];Pz<-solve(crossprod(Z),t(Z));h<-sqrt(1-rowSums(Z*t(Pz)))
for(f in c('be_me','ope_be')){
 Y<-as.matrix(D[,paste(cc,f,'vw',sep='_')]);Zy<-Y-rowMeans(Y)
 co<-P%*%Zy;U<-Zy-X%*%co;se<-sapply(seq_along(cc),function(j)sqrt(hac_cal(U[,j],X,solve(crossprod(X)),keys,4)[2,2]))
 obs<-max(abs(co[2,]/se));bootmax<-rep(0,B)
 for(j in seq_along(cc)){
  fit0<-Z%*%(Pz%*%Zy[,j]);u0<-drop(Zy[,j]-fit0)/h;ys<-drop(fit0)+u0*W;bs<-P%*%ys;us<-ys-X%*%bs
  score<-us*drop(X%*%solve(crossprod(X))[,2]);v<-colSums(score^2)
  for(l in 1:4){jj<-match(keys-l,keys);ii<-which(!is.na(jj));if(length(ii))v<-v+2*(1-l/5)*colSums(score[ii,,drop=FALSE]*score[jj[ii],,drop=FALSE])}
  v<-v*nrow(X)/(nrow(X)-ncol(X));tt<-bs[2,]/sqrt(pmax(v,1e-15));bootmax<-pmax(bootmax,abs(tt))
 }
 ht[[f]]<-data.frame(name=f,n=nrow(D),stat_max_t=obs,p_dwb=(1+sum(bootmax>=obs))/(B+1))
}
H<-do.call(rbind,ht);H$p_holm_2<-p.adjust(H$p_dwb,'holm');put(H,'heterogeneity_joint2',O)
stopifnot(all.equal(unname(sapply(fs[1:4],function(f)f$beta[2]))[1],mean(sapply(fs2[1:16],function(f)f$beta[2])),tolerance=1e-10))
writeLines(capture.output(sessionInfo()),file.path(O,'R_session.txt'))
cat('DONE\n')
