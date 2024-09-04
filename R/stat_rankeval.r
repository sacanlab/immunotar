source_disabled__=function(...){invisible(NULL)}
#% Copyright (C) 2022 by Ahmet Sacan, Rawan Shraim
#evaluate a ranked list of items (e.g., items=genes) against known positive set.
#for methods (in Information Retrieval domain), see: https://en.wikipedia.org/wiki/Evaluation_measures_(information_retrieval)
# also see: https://www.cl.cam.ac.uk/teaching/1516/InfoRtrv/lecture6-evaluation-2x2.pdf


###############################################################
#return gsea score
#https://bmcbioinformatics.biomedcentral.com/articles/10.1186/s12859-017-1674-0
#Check/use/test available implementations. I found these:
#https://bioconductor.org/packages/devel/bioc/manuals/fgsea/man/fgsea.pdf
#https://rdrr.io/bioc/fgsea/man/calcGseaStat.html
#use the more popular library.
stat_rankeval_gsea=function(d, positives, o=list()){
  stopfif(!('score' %in% colnames(d)),'Data frame must have a score column.'); 
  rankeditems = rownames(d);
  
  #rankeditems: genes you find in your experiment (top DEGs).
  #positives: genes known to be involved in a pathway.
  #TODO: can be implemented later.
}



###############################################################
#return mean average precision.
#order: 'ascend' evaluates knownpositives having low-scores; 'descend' evaluates knownpositives having high-scores.
stat_rankeval_map=function(d, positiveinds, order='descend', ...){
  if(!exists('stopfif')){ source_disabled__('util.r'); }
  stopfif(!('score' %in% colnames(d)),'Data frame should have a score column.');
  stopfif(!is.numeric(positiveinds),'positiveinds should be the list of row-indices (not rownames).')
	scores=d$score; 
	
  if(order=='descend'){ scores=-scores; }

  ranks=rank(scores);
  ranksofpositives = sort(ranks[positiveinds]);
  precisions=(1:length(positiveinds)) / ranksofpositives;

  #20240227: identical values will have identical ranks, but that may result in some precisions to be >1. fix that:
  I=precisions>1; precisions[I]=1;

  return(mean(precisions));
}

###############################################################
#return weigthed mean average precision.
#order: 'ascend' evaluates knownpositives having low-scores; 'descend' evaluates knownpositives having high-scores.
#It's your responsibility to ensure positiveinds and positiveweights are aligned. If you rely on stat_rankeval to convert positives==>positiveinds, that would make them misaligned.
stat_rankeval_wmap=function(d, positiveinds,positiveweights,order='descend', ...){
  if(!exists('stopfif')){ source_disabled__('util.r'); }
  stopfif(!('score' %in% colnames(d)),'Data frame should have a score column.');
  stopfif(!is.numeric(positiveinds),'positiveinds should be the list of row-indices (not rownames).')
	scores=d$score; 
	
  if(order=='descend'){ scores=-scores; }

  ranks=rank(scores);
  ranksofpositives = ranks[positiveinds];
  Iorder = order(ranksofpositives);
  precisions=(1:length(positiveinds)) / ranksofpositives[Iorder];
  #20240227: identical values will have identical ranks, but that may result in some precisions to be >1. fix that:
  I=precisions>1; precisions[I]=1;

  #20240227: fixed mean(precisions...) with sum(precisions...)
  return(sum(precisions * positiveweights[Iorder])/sum(positiveweights));
}

###############################################################
#rankeditems can be a data frame (e.g., from data_score()) or just a list of genenames.
#if rankeditems is a dataframe, it should be a single column data, or should have a "score" column.
#rankeditems: ALK1: 99, MEGF10: 50, .....
#positives: ALK1, ...; or indices of the positives.
#order defaults to 'ascend' if d is a list of names, otherwise 'descend' (we take higher scores to be ranked at the top.)
stat_rankeval=function(d, positives, method='map', order=NULL, ...){
  if(!exists('opt_set')){ source_disabled__('util.r'); } 
  if(istext(positives)){ positives=csv(positives); }
  if(!is.data.frame(d)){
    if(istext(d)){ #if a list of names, we assume they are listed in best-to-worst order.
       names=csv(d);
       #Making it that the top name has the highest score
       d=data.frame(score=length(names):1);
       rownames(d)=names;
       if(is.null(order)){ order='ascend'; }
    }
    else{
      stopfif(!is.numeric(d),'Input must be a list of names, a data matrix (containing a score column), or a numeric vector of scores.');
      d=data.frame(score=d);
    }
  }
  if(is.null(order)){ order='descend'; }
  if(is.numeric(positives)){ positiveinds=positives; }
  else{ positiveinds=which(rownames(d) %in% positives); }


  if(method=='map'){ func=stat_rankeval_map; }
  else if(method=='wmap'){ func=stat_rankeval_wmap; }
  else if(method=='gsea'){ func=stat_rankeval_gsea; }
  else{
    func = paste0('stat_rankeval_',method);
    if(!exists(func)){
      stop(sprintf('Rank evaluation method [%s] does not exist.',func))
    }
    func=get(func);
  }
  return(func(d,positiveinds,order=order,...));

  #dummy function calls appearing after return() to help with func_depends()
  stat_rankeval_map()
  stat_rankeval_wmap()
  stat_rankeval_gsea()  
}

#' @export
genereport=function(rankeditems, gene){
  perc_score=(length(which(rankeditems$score < rankeditems$score[which(rownames(rankeditems) == gene)]))/(nrow(rankeditems) - 1 ))*100
  return(perc_score)
}


#stk__=dbg_nicestack(1); message(sprintf('stat_rankeval.r sourced from: %s',stk__));
