#################################################################
#Downloading supplemental files for Ewing sarcoma and multiple myeloma surfaceome data 

ewingmm_downloadfiles=function(disease,...){  
  filename=io_name(biodb_dir(), io_sanitizefilename(disease), paste0("processed_", io_sanitizefilename(disease), ".xlsx"))
  if(io_isfileandnotempty(filename)){ return(filename); }
  
  ds=list("Multiple Myeloma"=list(url='https://static-content.springer.com/esm/art%3A10.1038%2Fs41467-022-31810-6/MediaObjects/41467_2022_31810_MOESM3_ESM.xlsx'
                               ,samples=c('AMO1', 'RPMI', 'KMS12', 'L363'))
          ,"Ewing's Sarcoma"=list(url="https://aacr.silverchair-cdn.com/aacr/content_public/journal/clincancerres/proof/10.1158_1078-0432.ccr-23-2187/1/ccr-23-2187_supplementary_data_s4_suppds4.xlsx"
                             ,samples=c("PDX3","ES1","CHLA10","ES6","EW9","EW8","ES8","CHLA258","EW5","ES4","TC71","NCHEWS1","SKNEP","TC32",
                                        "EW12","EW13","ES3","ES7","ES2"))
  )
  url=ds[[disease]]$url 
  downloaddir=io_mkdirif(io_name(biodb_dir(),io_sanitizefilename(disease)))
  file=downloadurl(url,file=paste0(downloaddir,'/'));
  
  if(disease == "Ewing's Sarcoma"){
    sheetname='RelAbundance_Surface'
  }else{sheetname=NULL}
  
  d=readxl::read_xlsx(file, sheet = sheetname)
  
  if(disease=="Ewing's Sarcoma"){
    column.name='Symbol'
  }else{ column.name='Protein'}
  
  freq=table(d[, column.name])
  d=d[-which(d[,column.name] == names(freq)[freq >= 2]),]
  
  df=data.frame(symb=d[,column.name])
  
  for(i in ds[[disease]]$samples){
    sample.d=d[,c(c(which(colnames(d) == column.name), which(grepl(i, colnames(d)))))]
    sample.d[,paste0('avg_', i)]=rowMeans(sample.d[,2:ncol(sample.d)], na.rm = T)
    sample.d=unique(sample.d[,c(column.name, paste0('avg_',i))])
    df=merge(df, sample.d, by=column.name)
    
  }
  
  if(disease == "Multiple Myeloma"){
    sym.map=d[,c('Gene', 'Protein')]
    df=merge(df, sym.map, by= column.name, all.x = T)
    df=unique(df)
    freq=table(df$Gene)
    df=df[-which(df$Gene %in% names(freq)[freq >= 2]),]
    df$Protein=NULL
    df=df[,c(5, 1:4)]
    
  }
  
  if(disease == "Ewing's Sarcoma"){
    df[,2:ncol(df)]=2^df[,2:ncol(df)]
  }

  writexl::write_xlsx(df, filename)
  return(filename)
}

##############################################################################
#Create Ewing Surfaceome or MultipleMyeloma Project 
ewingmm_project=function(diseasename,...){
  o=opt_set(...);
  datafile=ewingmm_downloadfiles(diseasename);
  ocache=cache_opts(depends=c(datafile,time_numeric('2024-02-16 11:30:00')),...);
  if(!ocache$recache){
    p=cache_load(ocache);
    p=project_cache_updateifneeded(p,ocache,o);
    return(p);
  }
  p=pancancer_project(cancername=diseasename, datafile=datafile,o)
  p=project_run(p);
  cache_save(p,ocache);
  return(p);
}

ewing_project=function(...){
  p=ewingmm_project("Ewing's Sarcoma",...);
  p[['curated']]=csv('IL1RAP,ROR1,STEAP1,STEAP2,LINGO1,CD99,ATP11C,ADGRG2,ENG,SLCO5A1,CDH11,ENPP1');
  p[['validatedpositives']]=csv('CDH11,ENPP1')
  p[['opentarget']]=rownames(p$data)[which(p$data$opentargetsurface_isopentargetsurface!=0)]
  return(p);
}

mm_project=function(...){
  p=ewingmm_project("Multiple Myeloma",...);
  p[['curated']]=csv('BCMA,TNFRSF13B,ITGB7,SLAMF7,LAX1,BTLA,CCR1,TXNDC11,SLC38A5,SEMA4A,LILRB4,KCNA3,DERL3,LY9,CD48,CCR10,CD28')
  p[['validatedpositives']]=csv('CCR10')
  p[['validatednegatives']]=csv('TXNDC11')
  p[['opentarget']]=rownames(p$data)[which(p$data$opentargetsurface_isopentargetsurface!=0)]
  return(p);
}

ewingmm_ssgsea=function(ps=NULL,...){
  o=opt_set(ps=NULL
            ,doplot=F, ...)
  if(is.null(ps)){
    e=ewing_project();
    m=mm_project();
    ps=list(e, m)
  }
  
  rows=csv('validatedpositives,knownpositives__,opentarget,curated')
  res=data.frame(matrix(nrow = length(rows)))
  for(p in ps){
      p.score=data.frame()
      pathways=p[rows]
      ranks=as.matrix(p$datawithscore[,'score', drop=F])
      names(ranks)=rownames(p$datawithscore)
      g.s=fgsea::fgsea(pathways, ranks, minSize=1)
      p.score=rbind(p.score, g.s[,c('ES','pval')])
      colnames(p.score)=c(paste0(p$disease, '_Enrichment'),paste0(p$disease, '_p.val'))
      rownames(p.score)=g.s$pathway
      res=cbind(res,p.score)
      rownames(res)=rownames(p.score)
      if(o$doplot){
        for(r in rows){
          print(fgsea::plotEnrichment(p[[r]], ranks) + labs(title=paste0(p$disease,'_',r)))
        }
      }
  }
  res[,1]=NULL
  
  return(as.data.frame(res))
  
}

