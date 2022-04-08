#getting to know the data: make metadata histograms

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

pdf(paste0(fig_loc, "histDaysSamples.pdf"), height=5, width=6)

hist(days, breaks=breaks, col=blue, main="number of days and samples per subject", border=blue, 
     xlab="number of days or samples", cex.main=1.6)
abline(v=mean(days), col=rgb(0,0,0.5), lty=2, lwd=1.5)
hist(samples, breaks=breaks, add=TRUE, col=red, border=red)
abline(v=mean(samples), col=rgb(0.5,0,0), lty=2, lwd=1.5)
legend("topright", legend=c("days","samples"), fill=c(blue, red), bty='n', border=c(blue,red), cex=1.3)

dev.off()

#number of microbes in a gut
microbes <- sapply(dat, function(X){nrow(X[!colSums(X, na.rm=T)==0,])})

green <- rgb(0, 0.75, 0, 0.5)

breaks <- seq(min(microbes), max(microbes), length.out=11)

pdf(paste0(fig_loc, "histMicrobes.pdf"), height=5, width=6)

hist(microbes, breaks=breaks, col=green, border=green, 
     main=paste("total microbe genera=", ncol(dat[[1]]),
                "\nmean=", mean(microbes), "; sd=", round(sd(microbes),2)),
     xlab="number of microbes in a gut", cex.main=1.4)
rug(microbes)

dev.off()


