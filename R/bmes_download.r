bmes_download=function(url,destfile){
  if(!file.exists(destfile)){
    tempdestfile=paste0(destfile,'.crdownload');
    if(download.file(url,tempdestfile,method="auto",mode="wb")!=0){
      stop(paste0('Failed to download url [',url,'] to file [',destfile,']'));
    }
    file.rename(tempdestfile,destfile);
  }
  return(destfile)
}
