#Computes diversity metrics: richness, Shannon diversity, and eveness based on mean abundances
#
#ARGS
#m    vector of mean abundances
#
#OUT
#named vector of richness, Shannon diversity (H), and evenness
#
divMets <- function(m){
  
  #numbner of species dectected
  richness <- length(m[m>0])
  
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










