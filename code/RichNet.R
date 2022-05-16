# RichNet
# 
# RichNet takes matrices of gut microbial abundances through time with NAs removed and returns the species richness across all times sampled.
#
# Args: mat - matrix of microbial abundances with NAs removed

RichNet <- function(mat){
  if(any(is.na(mat))){
    stop("Error in RichNet: NAs included in abundance matrix")
  }
  
  NetVec <- vector(mode = "numeric", length = dim(mat)[2])
  for(j in 1:length(NetVec)){        
    NetVec[j] <- sum(mat[,j])
  }
  RichOut <- length(which(NetVec != 0))
  
  return(RichOut)
}
