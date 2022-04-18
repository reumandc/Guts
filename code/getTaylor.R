
# This function takes a list of 20 individuals of 178 species
# length(indv) = Number individuals 
# length(indv[[1]]) = Number of species

getTaylor <- function(Data, N= 10){
  colnames(Data) <- c("ID", 1:178)
  
  indv <- vector(mode='list', length=21) #empty list
  
  for (i in 1:length(inds)){
    indv[[i]] <- Data[Data$ID >= i & Data$ID<(i+1),]
  }
  indv <- indv[-14]
  names(indv) <- c(1:11, 13, 15:17,12, 18:21)

  #get taylor's metrics
  taylor_var <- matrix(NA, nrow = length(indv), ncol =length(indv[[1]])-1)
  taylor_var <- data.frame(taylor_var)
  for (i in 1:length(indv)){
   indvs <- indv[[i]][,-1]
    covv <- cov(indvs, use = "pairwise.complete.obs")
  
    var_sp <- diag(covv)
    taylor_var[i,] <- var_sp
  }

  taylor_mean <- matrix(NA, nrow = length(indv), ncol =length(indv[[1]])-1)
  taylor_mean <- data.frame(taylor_mean)
  for (i in 1:length(indv)){
   indvs <- indv[[i]][,-1]
    mean_sp <- colMeans(indvs, na.rm = TRUE)
    taylor_mean[i,] <-  mean_sp
  }
# ind by species
  names(taylor) <- c("Var_sp", "Mean_sp")
  mat_coef <- matrix(NA, nrow = length(indv[[1]])-1, ncol = 3)
  dd <- colSums(taylor_mean>0)
  mat_coef[,3] <- dd
  for (i in 1:(length(indv[[1]])-1)){
    present <- (taylor_mean[,i]>0)
    if (sum(present)< N) next
    #m <- lm(log(taylor_var[present,i])) ~ lm(log(taylor_mean[present,i]))
    m <- lm(log(taylor_var[present,i])~log(taylor_mean[present,i])) 
    coef <- coef(m)
    mat_coef[i,1:2]  <- c(coef[1],exp(coef[2]))
  }
  return(mat_coef)
}