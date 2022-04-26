
# This function takes a list of 20 individuals of 178 species
# length(indv) = Number individuals 
# length(indv[[1]]) = Number of species
# length(Data) = Number of species

getTaylor <- function(Data, N= 10){
  colnames(Data) <- c("ID", 1:178)
  
  indv <- vector(mode='list', length=21) #empty list
  
  for (i in 1:length(indv)){
    indv[[i]] <- Data[Data$ID >= i & Data$ID<(i+1),]
  }
  indv <- indv[-14]
  names(indv) <- c(1:11, 13, 15:17,12, 18:21)

  #get variance's metrics
  taylor_var <- matrix(NA, nrow = length(indv), ncol = length(indv[[1]])-1)
  for (i in 1:length(indv)){
    indvs <- indv[[i]][,-1]
    covv <- cov(indvs, use = "pairwise.complete.obs")
  
    var_sp <- diag(covv)
    taylor_var[i,] <- var_sp
  }
  #get means's metrics
  taylor_mean <- matrix(NA, nrow = length(indv), ncol =length(indv[[1]])-1)
  for (i in 1:length(indv)){
   indvs <- indv[[i]][,-1]
    mean_sp <- colMeans(indvs, na.rm = TRUE)
    taylor_mean[i,] <-  mean_sp
  }
# indvidual by species
  mat_coef <- matrix(NA, nrow = length(indv[[1]])-1, ncol = 3)
  spe_sum <- colSums(taylor_mean>0)
  mat_coef[,3] <- spe_sum
  for (i in 1:(length(indv[[1]])-1)){
    present <- (taylor_mean[,i]>0)
    if (sum(present)< N) next
    m <- lm(log(taylor_var[present,i])~log(taylor_mean[present,i])) 
    coef <- coef(m)
    mat_coef[i,1:2]  <- c(exp(coef[1]),coef[2])
  }
  return(mat_coef)
}
