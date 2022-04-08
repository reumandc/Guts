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

##number of days vs samples
days <- sapply(dat, nrow)
samples <- sapply(dat, function(X){nrow(X[!is.na(rowSums(X)),])})









