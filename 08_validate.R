source('inference.R')
checks<-list()
add<-function(name,maximum_difference,tolerance=1e-8){checks[[name]]<<-data.frame(check=name,maximum_difference,tolerance,passed=is.finite(maximum_difference)&&maximum_difference<tolerance)}
d<-read.csv('results/numerical_comparison.csv');add('251 model coefficients',max(abs(d$coefficient_R-d$coefficient_before)));add('251 marginal bootstrap p-values',max(abs(d$p_R-d$p_before)));add('251 confidence interval lower limits',max(abs(d$ci_low_difference)),1e-5);add('251 confidence interval upper limits',max(abs(d$ci_high_difference)),1e-5)
for(n in c('primary8','country32','weight_sensitivity16','french_attribution4','heterogeneity_joint2','coverage15_8','leave_country_out')){
 fn<-paste0('country_',n,'.csv');a<-read.csv(file.path('results',fn));b<-read.csv(file.path('reference_results',fn));stopifnot(nrow(a)==nrow(b));cols<-intersect(names(a)[vapply(a,is.numeric,TRUE)],names(b));add(paste('Country',n),max(abs(as.matrix(a[,cols])-as.matrix(b[,cols])),na.rm=TRUE))
}
a<-read.csv('results/size_calibration.csv');b<-read.csv('reference_results/size_calibration.csv');add('Monte Carlo calibration',max(abs(as.matrix(a)-as.matrix(b))))
models<-read.csv('results/model_estimates.csv');p<-models[models$family=='characteristic_OP_paired_family'&models$label=='GV_OP_low_minus_high',];stopifnot(nrow(p)==1)
add('Primary family adjusted p-value',abs(p$p_rw_family-.1591))
u<-read.csv('data/us_panel.csv');size<-sd(u$matched_x*10);impact<-p$coef*size/10
key<-data.frame(quantity=c('US observations','US contrast slope','Primary marginal p-value','Primary adjusted p-value','Revision standard deviation in probability points','One-standard-deviation return effect in percentage points'),value=c(nrow(u),p$coef,p$p_dwb,p$p_rw_family,size,impact))
write.csv(key,'results/key_findings.csv',row.names=FALSE)
checks<-do.call(rbind,checks);write.csv(checks,'results/replication_checks.csv',row.names=FALSE);stopifnot(all(checks$passed))
write_json(list(status='passed',models=nrow(d),largest_coefficient_difference=max(abs(d$coefficient_R-d$coefficient_before)),largest_marginal_p_difference=max(abs(d$p_R-d$p_before)),main_adjusted_p=p$p_rw_family,manuscript_numerical_update_required=FALSE),'results/replication_summary.json',pretty=TRUE,auto_unbox=TRUE)
cat('All checks passed. No reported numerical finding requires a manuscript update.\n')
