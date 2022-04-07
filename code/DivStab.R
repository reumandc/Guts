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






