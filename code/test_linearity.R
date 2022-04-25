# this function tests linearity of the taylor's law

test_linearity <- function(Data, N=10){
  res <- matrix(NA, nrow = length(indv[[1]])-1, ncol = 7)
  spe_sum  <- colSums(taylor_mean>0)
  res[,3] <- spe_sum 
  for (i in 1:(length(indv[[1]])-1)){
    present <- (taylor_mean[,i]>0)
    if (sum(present)< N) next
    m_linear <- lm(log(taylor_var[present,i])~log(taylor_mean[present,i]))
    coef <- coef(m)
    res[i,1:2]  <- c(exp(coef[1]), coef[2])
    
    m_quad <- lm(log(taylor_var[present,i]) ~ log(taylor_mean[present,i]) + I(log(taylor_mean[present,i])^2))
    #linearity test
    test_linear <- anova(m_linear, m_quad)
    p_value3 <- test_linear[2,6]
    res[i,4] <-p_value3
    
    #test for homoscedasticity
    resid <- abs(resid(m_linear))
    m_resid <- lm(resid ~ log(taylor_mean[present,i]))
    res_homos <- coef(summary(m_resid))
    p_value <- res_homos[2,4]
    res[i,5] <- p_value
    
    #Jarque Bera test(normality of residuals)
    res_jarqbera <- jarque.bera.test(resid(m_linear))
    p_value4 <- res_jarqbera$p.value
    res[i,6] <- p_value4
    
    #Dubin-watson (autocorrelation)
    res_Dubin <- durbinWatsonTest(m_linear)
    p_value_dubin <- res_Dubin$p
    res[i,6] <-  p_value_dubin
    
    #Vadim's test
    m_linear <- lm(log(taylor_var[present,i])~log(taylor_mean[present,i]))
    res_vadim <- coef(summary(m_linear))
    p_value2 <- res_vadim[2,4]
    res[i,7] <- p_value2
  }
  return(res)
}
