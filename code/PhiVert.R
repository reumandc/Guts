#Computes the index phi used in Lei's paper. This is actually the square root of the inverse
#of the Loreau-de Mazancourt synchrony metric. Lei calls it a metric of asynchrony, and it is.
#
#Args
#dat        A matrix, time by species, of numbers. Throws an error if there are any NAs present,
#             "Error in PhiVert: NAs present".
#
#Output
#A single number which is Lie's phi.
#
PhiVert<-function(dat)
{
  
}