# Run from the repository root: Rscript run_all.R
required<-c('readxl','jsonlite','digest','MASS','data.table','ggplot2','ragg')
missing<-required[!vapply(required,requireNamespace,TRUE,quietly=TRUE)]
if(length(missing))stop('Install missing packages first: ',paste(missing,collapse=', '))
for(d in c('data','results','figures','raw_data'))dir.create(d,showWarnings=FALSE)
steps<-c('00_download.R','01_prepare_data.R','01b_prepare_portfolios.R','02_estimate.R','03_country_data.R','04_country_estimates.R','05_country_coverage.R','06_figures.R','07_inference_calibration.R','08_validate.R')
for(step in steps){cat('\nRunning ',step,'\n',sep='');status<-system2(file.path(R.home('bin'),'Rscript'),step);if(status!=0)stop('Stopped at ',step)}
