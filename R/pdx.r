######################################################
#Generating files for PDX Consortium data 

pdx_files=function(disease,...){
  file=io_name(config('datasetdir'), 'pdx_cons_data', paste0(disease,'PDX.RNA.xlsx'))
  if(file.exists(file)){
    dbg_warnf('The file already exists, returning the file path')
    return(file)
  }
  m=readxl::read_xlsx("~/Library/CloudStorage/Box-Box/Rawan/RMS PhD Project/RMS_DIA/Code/datasets/pdx_cons_data/NIHMS1542708-supplement-2.xlsx", sheet = 'sample.clin')
  d=readRDS("~/Library/CloudStorage/Box-Box/Rawan/RMS PhD Project/RMS_DIA/Code/datasets/pdx_cons_data/celline-pdx-gene-expression-rsem-tpm-collapsed.rds")
  disease='Neuroblastoma'
  ids=m$Model[which(m['Histology'] == disease)]
  d=d[,colnames(d) %in% ids]
  
  openxlsx::write.xlsx(d, file, rowNames=TRUE)
  return(file)
}


pdx_project=function(disease, file=NULL,... ){
  if(is.null(file)){
    file=pdx_files(disease, ...)
  }
  pancancer_project(disease, datafile=file)
  
}
