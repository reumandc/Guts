# ShannNet
# 
# ShannNet takes matrices of gut microbial abundances through time with NAs removed and returns the Shannon diversity index across all times sampled.
#
# Args: mat - matrix of microbial abundances with NAs removed

ShannNet <- function(mat){
  if(any(is.na(mat))){
    stop("Error in ShannNet: NAs included in abundance matrix")
  }
  
  NetVec <- vector(mode = "numeric", length = dim(mat)[2])
  for(j in 1:length(NetVec)){
    NetVec[j] <- sum(mat[,j])
  }
  
  AbTot <- sum(NetVec)
  Shann <- 0
  
  for(j in 1:length(NetVec)){
    pAb <- NetVec[j]/AbTot
    
    if(pAb != 0){
      Shann <- Shann + pAb*log(pAb)
    }
  }
  
  return(-1*Shann)
}
