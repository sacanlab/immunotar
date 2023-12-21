## Function to types columns from proteomics dset 

#Proteins in supplied d frame must be rownames 
#Can have columns other than protoemics expression but must define columns with sample proteomics expression 

#Options to types d 
# 0. types - if left empty will calculate all fields 
#     1. numhighexpressed - types how many samples have average or above average expression of protein
#     2. numexpressed - types how many samples have expression of protein 
#     3. mean - calculate mean expression of every protein across all samples 
#     4. median - calculate median expression of every protein across all samples 


data_summarize=function(d, o=list()){
#  source('util.r')  
  alltypes=unlist(strsplit('mean,median,numlowexpressed,numhighexpressed,numexpressed',','));
  o = list_merge(list(
    summarytypes=alltypes
    ,summaryprefix='expr_' #%prefix to be added to the summary column names being created. If a prefix is given, ending it with underscore '_' is recommended.
    ,lowthreshold='20%'
    ,highthreshold='80%'
    ,cols='__NUMERIC__' #%if non-empty, we only apply to these columns. Can bee a list of column names or a logical/numerical index for columns. or '__NUMERIC__' to only use numeric columns.
  ), o);
  
  
  types=ensurecsvlist(o$summarytypes,alltypes)
  if(length(types)==0) return(d); #no summarization done.

  ##calculating threshold values
  out=data.frame(matrix(nrow= nrow(d), ncol = length(types)), row.names = rownames(d))
  colnames(out)=types

#  source('util.r');
  Icols=data_Icols(d,o$cols);
  if(!ncol(d)){ return(d); }
  
  m=d[,Icols];

  ##Go through summary types entered
  if('mean' %in% types){
      out$mean=rowMeans(m, na.rm = T)
  }
  
  if('median' %in% types){
      out$median = apply(m, 1, median, na.rm=T)
  }
  
  if('numexpressed' %in% types){
    if(hasnegative(m)){ warning('The threshold of expression is set to zero, but your data has negative values. numexpressed summary is invalid/unreliable.'); }
    out$numexpressed=rowSums(m > 0 & !is.na(m))
  }
  
  
  needlowhigh=intersect(types, c('numlowexpressed','numhighexpressed'))
  if(length(needlowhigh) > 0){
#    source('vec_rescale.r')
      m2=matrix(vec_rescale(unlist(m),'percentile'), nrow=nrow(m), byrow = F)
      for(type in needlowhigh){
        if(type=='numlowexpressed'){ threshold=o$lowthreshold; }
        else if(type=='numhighexpressed'){ threshold=o$highthreshold; }
        
        if(is.character(threshold)){ 
          if(grepl("%",threshold,fixed=T)){
            threshold=as.numeric(gsub("%", "", threshold))
          }
        }
        else{ threshold=as.numeric(threshold) }
        
        if(type == "numlowexpressed" ){ out$numlowexpressed=rowSums(m2 < threshold,  na.rm = T); }
        else if(type == "numhighexpressed" ){out$numhighexpressed=rowSums(m2 > threshold, na.rm = T); }
        

      }
    }
    
  if(length(o$summaryprefix)>0){
    colnames(out)=lapply(colnames(out), function(f) paste0(o$summaryprefix,f));
  }
  
  return(out)
}

  
