##ssgsea score 

####Return list that gives the ssgsea scores datamatrix to feed into plotEnrichment

# ssgsea_plot = function(X, gene_sets, alpha = 0.25, scale = T, norm = F, single = T) {
#   row_names = rownames(X)
#   num_genes = nrow(X)
#   gene_sets = lapply(gene_sets, function(genes) {which(row_names %in% genes)})
#   
#   # Ranks for genes
#   R = matrixStats::colRanks(X, preserveShape = T, ties.method = 'average')
#   # Calculate enrichment score (es) for each sample (column)
#   es = apply(R, 2, function(R_col) {
#     gene_ranks = order(R_col, decreasing = TRUE)
#     # Calc es for each gene set
#     es_sample = sapply(gene_sets, function(gene_set_idx) {
#       # pos: match (within the gene set)
#       # neg: non-match (outside the gene set)
#       indicator_pos = gene_ranks %in% gene_set_idx
#       indicator_neg = !indicator_pos
#       
#       rank_alpha  = (R_col[gene_ranks] * indicator_pos) ^ alpha
#       
#       step_cdf_pos = cumsum(rank_alpha)    / sum(rank_alpha)
#       step_cdf_neg = cumsum(indicator_neg) / sum(indicator_neg)
#       
#       step_cdf_diff = step_cdf_pos - step_cdf_neg
#       
#       # Normalize by gene number
#       if (scale) step_cdf_diff = step_cdf_diff / num_genes
#       # Use ssGSEA or not
#       if (single) {
#         sum(step_cdf_diff)
#       } else {
#         step_cdf_diff[which.max(abs(step_cdf_diff))]
#       }
#       es_sample=list(step_cdf_diff)
#     })
#     es=es_sample
#   })
#   
#   return(es)
# }

# #####Returns the ssgsea score only 
# ssgsea_score = function(X, gene_sets, alpha = 0.25, scale = T, norm = F, single = T) {
#   row_names = rownames(X)
#   num_genes = nrow(X)
#   gene_sets = lapply(gene_sets, function(genes) {which(row_names %in% genes)})
#   
#   # Ranks for genes
#   R = matrixStats::colRanks(X, preserveShape = T, ties.method = 'average')
#   
#   # Calculate enrichment score (es) for each sample (column)
#   es = apply(R, 2, function(R_col) {
#     gene_ranks = order(R_col, decreasing = T)
#     
#     # Calc es for each gene set
#     es_sample = sapply(gene_sets, function(gene_set_idx) {
# 
#       # pos: match (within the gene set)
#       # neg: non-match (outside the gene set)
#       
#       indicator_pos = gene_ranks %in% gene_set_idx
#       indicator_neg = !indicator_pos
#       
#       rank_alpha  = (R_col[gene_ranks] * indicator_pos) ^ alpha
#       
#       step_cdf_pos = cumsum(rank_alpha)    / sum(rank_alpha)
#       step_cdf_neg = cumsum(indicator_neg) / sum(indicator_neg)
#       
#       step_cdf_diff = step_cdf_pos - step_cdf_neg
#       
#       # Normalize by gene number
#       if (scale) step_cdf_diff = step_cdf_diff / num_genes
#       
#       # Use ssGSEA or not
#       if (single) {
#         sum(step_cdf_diff)
#       } else {
#         step_cdf_diff[which.max(abs(step_cdf_diff))]
#       }
#     })
#     unlist(es_sample)
#   })
#   
#   if (length(gene_sets) == 1) es = matrix(es, nrow = 1)
#   
#   # Normalize by absolute diff between max and min
#   if (norm) es = es / diff(range(es))
#   
#   # Prepare output
#   rownames(es) = names(gene_sets)
#   colnames(es) = colnames(X)
#   return(es)
# }

gsea_score_old=function(pathways, pathwayname, stats, gseaParam = 1){
  stats.v=as.vector(as.matrix(stats))
  names(stats.v)=rownames(stats)
  rnk <- rank(-stats.v)
  ord <- order(rnk)
  statsAdj <- stats.v[ord]
  statsAdj <- sign(statsAdj) * (abs(statsAdj)^gseaParam)
  statsAdj <- statsAdj/max(abs(statsAdj))
  pathway <- unname(as.vector(na.omit(match(pathway, names(statsAdj)))))
  pathway <- sort(pathway)
  #To get p-value and enrichment score - need to change the form of the stats for this function 
  
  gseaRes2 <- fgsea::fgsea(pathways, stats=ranks, minSize=1)
  #Needed for plotting in plotEnrichment
  #gseaRes <- fgsea::calcGseaStat(statsAdj, selectedStats = pathway, returnAllExtremes = TRUE)
  #print(plotEnrichment(gseaRes, pathwayname, statsAdj, pathway))
  return(gseaRes2)
}



