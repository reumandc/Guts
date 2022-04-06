# ThreshYL
# 
# ThreshYL takes the output of OverTax and some threshold number of guts and runs a linear model on the log-scaled terms for each column of each object in the list. It outputs the outputs of lm() from stats in BaseR.
# 
# Args:
# OTout: the output of OverTax
# Thresh: the minimum number of guts in which a taxon must be observed before running the linear model.
# 
# Output: an S3 object containing the outputs of lm for each taxon that meets the threshold.

ThreshYL<-function(OTout, Thresh){
  
}