source_disabled__=function(...){invisible(NULL)}
## Function to types columns from proteomics dset 

#Proteins in supplied d frame must be rownames 
#Can have columns other than protoemics expression but must define columns with sample proteomics expression 

#Options to types d 
# 0. types - if left empty will calculate all fields 
#     1. numhighexpressed - types how many samples have average or above average expression of protein
#     2. numexpressed - types how many samples have expression of protein 
#     3. mean - calculate mean expression of every protein across all samples 
#     4. median - calculate median expression of every protein across all samples 


data_summarize=function(d, ...){
  source_disabled__('util.r')  
  alltypes=unlist(strsplit('mean,median,numlowexpressed,numhighexpressed,numexpressed',','));
  o = opt_set(
    summarytypes=alltypes
    ,summaryprefix='expr_' #%prefix to be added to the summary column names being created. If a prefix is given, ending it with underscore '_' is recommended.
    ,lowthreshold='20%'
    ,highthreshold='80%'
    ,expressedthreshold=NULL #what expression value is considered expressed? used for calculating numexpressed. Use NA if you only want to consider NA's as non-expressed (only useful if handlenan is not done yet). When NULL, defaults to NA if data has any NA's; otherwise defults to '0ormin'.
    ,cols='__NUMERIC__' #%if non-empty, we only apply to these columns. Can bee a list of column names or a logical/numerical index for columns. or '__NUMERIC__' to only use numeric columns.
  , ...);
  
  types=ensurecsvlist(o$summarytypes,alltypes)
  if(length(types)==0) return(d); #no summarization done.

  ##calculating threshold values
  #print(list(nrow= nrow(d), ncol = length(types)))
  out=data.frame(matrix(nrow= nrow(d), ncol = length(types)), row.names = rownames(d))
  colnames(out)=types

  Icols=data_Icols(d,o$cols);
  if(!ncol(d)){ return(d); }
  if(length(Icols)==0||!any(as.logical(Icols))){
    warnfif(is.character(o$cols)&&o$cols=='__NUMERIC__', 'No numeric columns were identified. No summarization is done. Make sure the data is numeric; you may need to replace any text with blanks so the data is loaded as numerical matrix.');
    return(d);
  }
  
  m=d[,Icols];

  ##Go through summary types entered
  if('mean' %in% types){
      out$mean=rowMeans(m, na.rm = T)
  }
  
  if('median' %in% types){
      out$median = apply(m, 1, median, na.rm=T)
  }
  
  if('numexpressed' %in% types){
    if(is.null(o$expressedthreshold)){
      if(any(is.na(m))){
        o$expressedthreshold=NA;
        msgf('data_summarize(): expressedthreshold defaulted to NA, since data contains some NAs. You can explicitly set expressedthreshold to avoid this warning.');
      }
      else{
        o$expressedthreshold='0ormin';
        msgf('data_summarize(): expressedthreshold defaulted to "0ormin". We will use 0 if all data is positive, or the smallest value if it is not. You can explicitly set expressedthreshold to avoid this warning.');
      }
    }
    if(is.na(o$expressedthreshold)){ out$numexpressed=rowSums(!is.na(m)); }
    else{
      if(o$expressedthreshold=='0ormin'){ o$expressedthreshold=var_pick(hasnegative(m), min(m), 0); }
      if(o$expressedthreshold&&hasnegative(m)){ warnf('data_summarize(): The threshold of expression (for counting "numexpressed") is zero, but your data has negative values. numexpressed summary is invalid/unreliable.'); }
      out$numexpressed=rowSums(m > o$expressedthreshold & !is.na(m));
    }
  }
  
  
  needlowhigh=intersect(types, c('numlowexpressed','numhighexpressed'))
  if(length(needlowhigh) > 0){
    source_disabled__('vec_rescale.r')
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

  
