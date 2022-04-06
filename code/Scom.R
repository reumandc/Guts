#Computes Scom as defined in Lei's manuscript, see Methods section at the end.
#
#Args
#dat        A matrix, time by species, of numbers. Throws an error if there are any NAs present,
#             "Error in Scom: NAs present".
#
#Output
#A single number which is Scom.
#
Scom<-function(dat)
{
  #error checking
  if (any(is.na(dat)))
  {
    stop("Error in Scom: NAs present")
  }
  
  xtot<-apply(FUN=sum,X=dat,MARGIN=1)
  return(mean(xtot)/sd(xtot))
}