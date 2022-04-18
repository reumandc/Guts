#Computes the classic variance ratio.
#
#Args
#dat        A matrix, time by species, of numbers. Throws an error if there are any NAs present,
#             "Error in varrat: NAs present".
#
#Output
#A single number which is variance ratio.
#
varrat<-function(dat)
{
  #error checking
  if (any(is.na(dat)))
  {
    stop("Error in varrat: NAs present")
  }  
  
  #calculations
  xtot<-apply(FUN=sum,X=dat,MARGIN=1)
  vari<-apply(FUN=var,X=dat,MARGIN=2)
  return((var(xtot))/(sum(vari)))
}