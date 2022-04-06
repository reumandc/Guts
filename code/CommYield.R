# CommYield
# 
# CommYield takes the original S3 object with all gut abundances and no NAs and the linear model outputs from ThreshYL to estimate the community overyielding coeffiecient chi per Thibaut & Connolly 2012 (with error terms).
# 
# Args: 
# dat: the list of gut data as described above
# TYLout: the output of ThreshYL
# clevel: confidence level for error terms for mean abundance of a given species in a monoculture
# 
# Outputs:
# An nx3 data frame which contains which gut it is and the upper and lower bounds for community chi.

CommYield<-function(dat,TYLout,clevel){
  
}