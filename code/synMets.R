#Computes synchrony metrics as described in the introduction of Ghosh, Cottingham & Reuman (2021) 
#
#Args
#V      covariance matrix of species abundances over time; from complete pairs 
#m      vector of species mean abundance over time
#
#Output
#a named vector containing Vcom, Vind, phiV, Vsyn and phiV_LdM
synMets <- function(V, m){
  
  #species-level variance
  v_s <- diag(V)
  
  #community variance 
  v_c <- sum(V)
  
  #mean community abundance squared
  m_c_sq <- sum(m)^2
  
  #Vcom: community CV^2
  Vcom <- v_c/m_c_sq
  
  #Vind: the value Vcom would take if species were independent
  Vind <- sum(v_s)/m_c_sq 
  
  #phiV, "classic variance ratio"
  phiV <- Vcom/Vind
  
  #Vsyn: value of Vcom if all species were synchronous
  Vsyn <- sum(sqrt(v_s))^2/m_c_sq 
  
  #phiV_LdM
  phiV_LdM <- Vsyn/Vcom #also v_c/sum(sqrt(v_s))^2
  
  return(c(Vcom=Vcom, Vind=Vind, phiV=phiV, Vsyn=Vsyn, phiV_LdM=phiV_LdM))
}
  