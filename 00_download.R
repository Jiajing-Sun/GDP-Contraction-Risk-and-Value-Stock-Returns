suppressPackageStartupMessages(library(digest))
dir.create('raw_data',showWarnings=FALSE)
m<-read.csv('data/source_manifest.csv',stringsAsFactors=FALSE)
log<-list()
for(i in seq_len(nrow(m))){
 dest<-file.path('raw_data',m$file[i])
 if(!file.exists(dest)){
  bundled<-file.path('data',m$file[i])
  if(file.exists(bundled))file.copy(bundled,dest) else download.file(m$url[i],dest,mode='wb',quiet=FALSE)
 }
 sha<-digest(dest,algo='sha256',file=TRUE)
 log[[i]]<-data.frame(file=m$file[i],expected_sha256=m$sha256[i],observed_sha256=sha,identical_vintage=sha==m$sha256[i])
}
write.csv(do.call(rbind,log),'results/download_checks.csv',row.names=FALSE)
if(!all(vapply(log,function(z)z$identical_vintage,TRUE)))stop('A provider has revised a download. The new file is saved, but do not silently substitute it for the manuscript vintage. See results/download_checks.csv.')
cat(nrow(m),'raw source files verified against the saved vintage.\n')
