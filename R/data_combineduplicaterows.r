#% Copyright (C) 2022 by Ahmet Sacan

###############################################################
data_combineduplicaterows =function(df, func=mean, idcolumn='genesymbol'){
  #Solution selected from: https://stackoverflow.com/questions/10180132/consolidate-duplicate-rows
  #library(plyr)
  return( plyr::ddply(df, idcolumn, plyr::numcolwise(func)) );
}
