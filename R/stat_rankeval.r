#% Copyright (C) 2022 by Ahmet Sacan, Rawan Shraim
#evaluate a ranked list of items (e.g., items=genes) against known positive set.
#for methods (in Information Retrieval domain), see: https://en.wikipedia.org/wiki/Evaluation_measures_(information_retrieval)
# also see: https://www.cl.cam.ac.uk/teaching/1516/InfoRtrv/lecture6-evaluation-2x2.pdf


###############################################################
#return mean average precision.
stat_rankeval_map=function(rankeditems, knownpositives, o=list()){
	#locate the ranks of the positive items.
  knownpos_rank=which(rankeditems %in% knownpositives)
  precisions=rep(0, length(knownpos_rank))
  
	#at each such position, calculate precision (number of positive items at this and 
	#previous positions; divided by this position.).
  for(i in 1:length(precisions)){
    precisions[i]=i/knownpos_rank[i];
  }
  
  return(mean(precisions));
}




###############################################################
#return gsea score
#https://bmcbioinformatics.biomedcentral.com/articles/10.1186/s12859-017-1674-0
#Check/use/test available implementations. I found these:
#https://bioconductor.org/packages/devel/bioc/manuals/fgsea/man/fgsea.pdf
#https://rdrr.io/bioc/fgsea/man/calcGseaStat.html
#use the more popular library.
stat_rankeval_gsea=function(rankeditems, knownpositives, o=list()){
	#rankeditems: genes you find in your experiment (top DEGs).
	#knownpositives: genes known to be involved in a pathway.
	#TODO: can be implemented later.

}


###############################################################
#rankeditems: e.g., the score column resulting from data_score().
#rankeditems: ALK1: 99, MEGF10: 50, .....
#knownpositives: ALK1, ...?

stat_rankeval=function(rankeditems, knownpositives, method='map', o=list()){
#  source('util.r');
  if(istext(knownpositives)){ knownpositives=ensurecsvlist(knownpositives); }
  if(is.data.frame(rankeditems)){ rankeditems = rownames(rankeditems); }

  func = paste0('stat_rankeval_',method);
  if(!exists(func)){
    stop(sprintf('Rank evaluation method [%s] does not exist.',func))
  }
  func=get(func);
  return(func(rankeditems,knownpositives,o));
}

#' @export
genereport=function(rankeditems, gene){
  perc_score=(length(which(rankeditems$score < rankeditems$score[which(rownames(rankeditems) == gene)]))/(nrow(rankeditems) - 1 ))*100
  return(perc_score)
}
