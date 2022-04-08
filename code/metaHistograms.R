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

blue <- rgb(0,0,0.75,0.3)
red <- rgb(0.75,0,0,0.3)

breaks <- seq(min(samples),max(days), length.out=13)

hist(days, breaks=breaks, col=blue, main="number of days and samples per subject", border=blue, 
     xlab="number of days or samples", cex.main=1.6)
abline(v=mean(days), col=rgb(0,0,0.5), lty=2, lwd=1.5)
hist(samples, breaks=breaks, add=TRUE, col=red, border=red)
abline(v=mean(samples), col=rgb(0.5,0,0), lty=2, lwd=1.5)
legend("topright", legend=c("days","samples"), fill=c(blue, red), bty='n', border=c(blue,red), cex=1.3)









