source("synMets.R")
source("divMets.R")
#
#Computes stability and diversity metrics from cleaned data and saves

#results location
res_loc <- "../results/"

#load clean data
dat <- readRDS(paste0(res_loc,"dataList.Rds"))

#list of covariances matrices
V <- lapply(dat, cov, use="pairwise.complete.obs")

#matrix of mean abundace
m_n <- sapply(dat, colMeans, na.rm=T) #178 x 20

#initialize storage
stability <- {}; diversity <- {}

for (subj in 1:length(dat)){
	stability <- rbind(stability, synMets(V[[subj]], m_n[,subj]))
	diversity <- rbind(diversity, divMets(m_n[,subj]))
}

#combine
DivStab <- data.frame(stability, diversity)

#save
saveRDS(DivStab, paste0(res_loc,"DSdat.Rds"))


















syn <-readRDS(paste0(res_loc,"SynchronyMetrics.RDS"))

stability[,"Vsyn"] == syn$Vsyn

stab <- data.frame(stability)

plot(syn$phiVldm, col='hotpink', ylim=c(0,1), ylab='phiV_LdM')
points(stab$phiV_LdM, col='blue')

plot(syn$Vcom, col='hotpink', ylim=c(0,0.5), ylab='Vcom')
points(stab$Vcom, col='blue')

plot(syn$phiV, col='hotpink', ylim=c(0,11), ylab='phiV')
points(stab$phiV, col='blue')














