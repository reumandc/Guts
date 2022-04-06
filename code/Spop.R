#Computes Spop as defined in Lei's manuscript, see Methods section at the end.
#
#Args
#dat        A matrix, time by species, of numbers. Throws an error if there are any NAs present,
#             "Error in Spop: NAs present".
#
#Output
#A single number which is Spop.
#
Spop<-function(dat)
{
  #error checking
  if (any(is.na(dat)))
  {
    stop("Error in Spop: NAs present")
  }
  
  #calculations
  xtot<-apply(FUN=sum,X=dat,MARGIN=1)
  sigi<-apply(FUN=sd,X=dat,MARGIN=2)
  return(mean(xtot)/(sum(sigi)))
}