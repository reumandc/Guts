#results location
res_loc <- "../results/"

#load data
dat <- readRDS(paste0(res_loc,"dataList.Rds"))
synMets <- readRDS(paste0(res_loc,"DSdat.Rds"))

#figure location
fig_loc <- "../figures/"
if (dir.exists(fig_loc)==F){
	dir.create(fig_loc)
}

source("plotAbun.R")

pdf(paste0(fig_loc,"Abun_wNA.pdf"), height=7, width=12)
par(mfrow=c(4,5), mar=c(1,1,2,0.5), oma=c(3,1.5,1.5,1))
plotAbun(dat, synMets)
dev.off()

pdf(paste0(fig_loc,"Abun.pdf"), height=7, width=12)
par(mfrow=c(4,5), mar=c(1,1,2,0.5), oma=c(3,1.5,1.5,1))
plotAbun(dat, synMets, na=F)
dev.off()

pdf(paste0(fig_loc,"totAbun_wNA.pdf"), height=7, width=12)
par(mfrow=c(4,5), mar=c(1,1,2,0.5), oma=c(3,1.5,1.5,1))
plotAbun(dat, synMets, total=T)
dev.off()

pdf(paste0(fig_loc,"totAbun.pdf"), height=7, width=12)
par(mfrow=c(4,5), mar=c(1,1,2,0.5), oma=c(3,1.5,1.5,1))
plotAbun(dat, synMets, total=T, na=F)
dev.off()


