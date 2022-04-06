#Computes diversity metrics: richness, Shannon diversity, and eveness based on mean abundances
#
#ARGS
#m0    vector of mean abundances; may contain zeros
#
#OUT
#named vector of richness, Shannon diversity (H), and evenness
#
divMets <- function(m0){
  
  #present community; remove zeros
  m <- m0[m0>0]
  
  #numbner of species dectected
  richness <- length(m)
  
  #ln of species richness
  ln_r <- log(richness)
  
  #total mean abundance
  tot_abun <- sum(m)
  
  #proportion of each species
  P <- m/tot_abun
  
  #Shannon diversity index
  H <- sum(P*log(P))*-1
  
  #evenness = H/ln(richness)
  evn <- H/ln_r
  
  return(c(rich=richness, H=H, evn=evn))
}










