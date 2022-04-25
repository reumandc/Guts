# Rich
# 
# Rich takes matrices of gut microbial abundances through time with NAs removed and returns the mean species richness at all times sampled.
#
# Args: mat - matrix of microbial abundances with NAs removed

Rich <- function(mat){
  if(any(is.na(mat))){
    stop("Error in Rich: NAs included in abundance matrix")
  }
  
  RichtVec <- vector(mode = "numeric", length = dim(mat)[1])
  for(i in 1:length(RichtVec)){
    nZVec <- which(mat[i,] != 0)
    RichtVec[i] <- length(nZVec)
  }
  RichOut <- mean(RichtVec)
  
  return(RichOut)
}