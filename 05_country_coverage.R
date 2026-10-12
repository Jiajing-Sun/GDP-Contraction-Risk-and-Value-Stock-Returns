source('inference.R');put<-function(d,n,folder='results')write.csv(d,file.path(folder,paste0('country_',n,'.csv')),row.names=FALSE);O<-'results'
D<-read.csv('data/country_coverage_panel.csv')
fs<-list();for(f in c('be_me','ope_be'))for(g in c('all','ea','ne','diff')){
 y<-paste(f,'vw',g,sep='_');fs[[y]]<-fit_ev(D,y,c('matched_x','matched_mu'),label=y,family='coverage15_8')
}
put(adjust_family(fs),'coverage15_8',O)
cat('Coverage sensitivity N =',nrow(D),'\n')
