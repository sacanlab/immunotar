#% Copyright (C) 2022 by Rawan Shraim, Ahmet Sacan
#functions for optimizing weights and curve parameters.
#"sophisticated" learn-to-rank methods available at: https://en.wikipedia.org/wiki/Learning_to_rank


###############################################################
# Use linear regression to identify weights for different columns
# NOTE: linear regression did not result in sensible weights, likely due to having very few knowntargets for linearregression to be applicable.
data_score_optimize_linearregression=function(d, knownpositives, o=list()){
  #d=fusion_pos_cal$data
  #d$score=NULL
  #knownpositives=unlist(strsplit(fusion_pos_cal$knowntargets, ", "))
  score_vec=rep(0,nrow(d))
  score_vec[which(rownames(d) %in% knownpositives)]=1

#  source('util.r')
  installpackageifmissing_bioc('tidyverse'); #library(tidyverse)
  mdl= tidyverse::lm(score_vec ~ ., data=d)
  summary(mdl)
  
  #TODO: (Rawan) implement this function.
  #call data_score(), obtain the score column.. --> evaluate how good the score list is compared to knownpositives.
}
