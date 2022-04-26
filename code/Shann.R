# Shann
# 
# Shann takes matrices of gut microbial abundances through time with NAs removed and returns the mean Shannon diversity index at all times sampled.
#
# Args: mat - matrix of microbial abundances with NAs removed

Shann <- function(mat){
  if(any(is.na(mat))){
    stop("Error in Shann: NAs included in abundance matrix")
  }
  
  ShanntVec <- vector(mode = "numeric", length = dim(mat)[1])
  ShannsVec <- vector(mode = "numeric", length = dim(mat)[2])
  for(i in 1:length(ShanntVec)){
    AbTot <- sum(mat[i,])
    
    for(j in 1:length(ShannsVec)){
      pAb <- mat[i,j]/AbTot
      if(pAb != 0){
      ShannsVec[j] <- pAb * log(pAb)
      }
      else(ShannsVec[j] = 0)
    }
    
    ShanntVec[i] <- -1 * sum(ShannsVec)
  }
  ShannOut <- mean(ShanntVec)
  
  return(ShannOut)
}
