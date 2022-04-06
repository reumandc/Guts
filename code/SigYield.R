# SigYield
# 
# SigYield takes the output of ThreshYL and returns a new data frame with the first column being taxon name, the second column being r^2 of the linear model from ThreshYL, and the last two being the upper and lower bounds for the coefficient chi in overyielding as per Thibaut & Connolly 2012, as indicated by the coefficient in the linear model.
# 
# Args: 
# TYLOut: the output of ThreshYL
# clevel: confidence level for error terms for species-level chi
# 
# Outputs:
# OYMat: an nx4 data frame as described above.

SigYield<-function(TYLOut,clevel){
  
}