#Simpson's index
#
#Args
#abund          A vector of abundances of species, no NAs. If there are any NAs it throws the
#                 error "Error in simpson: NAs present"
#
#Output - the index, a single number
#
simpson<-function(abund)
{
  #error checking
  if (any(is.na(abund)))
  {
    stop("Error in simpson: NAs present")
  }
  
  #calculations
  p<-abund/sum(abund)
  p<-p[p!=0]
  return(sum(p^2))
}