# SimpNet
# 
# SimpNet takes matrices of gut microbial abundances through time with NAs removed and returns Simpson's index of diversity index across all times sampled.
#
# Args: mat - matrix of microbial abundances with NAs removed

SimpNet <- function(mat){

  if(any(is.na(mat))){
    stop("Error in SimpNet: NAs included in abundance matrix")
  }
      
  NetVec <- vector(mode = "numeric", length = dim(mat)[2])
  for(j in 1:length(NetVec)){        
    NetVec[j] <- sum(mat[,j])
  }
      
  AbTot <- sum(NetVec)
  Simp
    
  SimpVec<-vector(mode = "numeric", length = length(NetVec))
  for(j in 1:length(NetVec)){
  
    SimpVec[j] <- NetVec[j]^2/AbTot^2
      
  }
  SimpOut <- mean(1-SimpVec)
  return(SimpOut)
}
