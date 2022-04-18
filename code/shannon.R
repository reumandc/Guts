#Shannon's diversity index
#
#Args
#abund          A vector of abundances of species, no NAs. If there are any NAs it throws the
#                 error "Error in shannon: NAs present"
#
#Output - the index, a single numbers
#
shannon<-function(abund)
{
  #error checking
  if (any(is.na(abund)))
  {
    stop("Error in shannon: NAs present")
  }
  
  #calculations
  p<-abund/sum(abund)
  return(-sum(p*log(p)))
}