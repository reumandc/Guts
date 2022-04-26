# Simp
# 
# Simp takes matrices of gut microbial abundances through time with NAs removed and returns the mean Simpson's index of diversity index at all times sampled.
#
# Args: mat - matrix of microbial abundances with NAs removed

Simp <- function(mat){
  if(any(is.na(mat))){
    stop("Error in Simp: NAs included in abundance matrix")
  }
  
  SimptVec <- vector(mode = "numeric", length = dim(mat)[1])
  SimpsVec <- vector(mode = "numeric", length = dim(mat)[2])
  for(i in 1:length(SimptVec)){
    AbTot <- sum(mat[i,])
    for(j in 1:length(SimpsVec)){
      SimpsVec[j] <- (mat[i,j]*(mat[i,j] - 1))
    }
    SimptVec[i] <- sum(SimpsVec) / (AbTot*(AbTot - 1))
  }
  SimpOut <- mean(1 - SimptVec)
  
  return(SimpOut)
}