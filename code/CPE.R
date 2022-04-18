#Computes CPE as defined in Lei's manuscript, see Methods section at the end.
#
#Args
#dat        A matrix, time by species, of numbers. Throws an error if there are any NAs present,
#             "Error in CPE: NAs present".
#
#Output
#A single number which is CPE.
#
CPE<-function(dat)
{
  #error checking
  if (any(is.na(dat)))
  {
    stop("Error in CPE: NAs present")
  }
  
  #calculations
  xtot<-apply(FUN=sum,X=dat,MARGIN=1)
  vari<-apply(FUN=var,X=dat,MARGIN=2)
  return((sqrt(sum(vari)))/(sd(xtot)))
}