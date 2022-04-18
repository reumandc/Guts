#Computes SAE as defined in Lei's manuscript, see Methods section at the end.
#
#Args
#dat        A matrix, time by species, of numbers. Throws an error if there are any NAs present,
#             "Error in SAE: NAs present".
#
#Output
#A single number which is SAE.
#
SAE<-function(dat)
{
  #error checking
  if (any(is.na(dat)))
  {
    stop("Error in SAE: NAs present")
  }
  
  #calculations
  sigi<-apply(FUN=sd,X=dat,MARGIN=2)
  return(sum(sigi)/(sqrt(sum(sigi^2))))
}