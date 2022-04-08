#make figures comparing phi_LdM with Vcom and Vsyn, looking at their relationship, 
#and histograms

#results location
res_loc <- "../results/"

#load data
mets <- readRDS(paste0(res_loc,"DSdat.Rds"))
dat <- readRDS(paste0(res_loc,"dataList.Rds"))

#figure location
fig_loc <- "../figures/"
if (dir.exists(fig_loc)==F){
	dir.create(fig_loc)
}

syn <- mets[,c("Vcom","Vsyn","phiV_LdM")]
green <- rgb(0,0.6,0,0.4)
red <- rgb(0.6,0,0,0.4)
blue <- rgb(0,0,0.6,0.6)
fill <- c(green,red,blue)
pt <- c(16,16,17)
txtcol <- c("darkgreen", "darkred","darkblue")

#histograms
breaks <- 8
pdf(paste0(fig_loc, "histPhiLdM.pdf"), height = 5, width=7)
par(mfrow=c(1,3), mar=c(4,1.5,2,1), oma=c(0,3.5,1,0))

for (met in 1:ncol(syn)){
  hist(syn[,met], breaks=breaks, main=names(syn)[met], xlab='',
       col='whitesmoke', border=fill[met], cex.axis=1.8, cex.main=2)
  abline(v=mean(syn[,met]), col='grey', lwd=1.5, lty=2)
  rug(syn[,met])
}
title(ylab="frequency", line=1.7, outer=T, cex.lab=2)
dev.off()

#stacked scatter plot (1D) 
pdf(paste0(fig_loc,"phiLdMfig.pdf"), height=5.5, width=8)

plot(NULL, ylim=c(0,max(syn)), xlim=c(1,nrow(syn)), xaxt='n', xlab='',ylab='')
for (met in 1:ncol(syn)){
	points(syn[,met], col=fill[met], pch=pt[met], cex=1.3)
}
axis(1, at=1:20, labels=names(dat), tick=F, mgp=c(0,0.5,2))
legend("topright", legend=colnames(syn), col=fill, pch=pt, bty="n", pt.cex=1.3, y.intersp=1.3)
title(xlab="gut", line=1.7)
title(main="phiV_LdM = Vcom / Vsyn", line=1)

dev.off()

#scatter plots (2D)
pdf(paste0(fig_loc,"scatterPlotMetrics.pdf"), height=5, width=6)
par(mfrow=c(1,1), mar=c(5,5,2,1), mgp=c(2,0.5,0))

i <- cbind(1:2, 2:3, c(1,3))

plot(NULL, ylim=c(range(syn)), xlim=c(range(syn)), xlab='',ylab='', bty='n')
line <- -6
for (met in 1:ncol(syn)){
  vars <- syn[,i[,met]]
  x <- vars[,1]
  y <- vars[,2]
  points(x, y, col=fill[met], pch=16)
  
  ctest <- cor.test(x,y)
  cdat <- round(c(ctest$estimate, ctest$p.value),3)
  mtext(paste("rho=", cdat[1], " p=", cdat[2]), font=2, 
        side=1, col=txtcol[met], line=line, adj=0, at=0.815)
  line <- line +1
  
  legend("right", col = fill, pch=16, y.intersp = 1.15, inset =c(0.065,0),
         legend=c("Vcom x Vsyn", "Vsyn x phiV_LdM", "Vcom x phiV_LdM"),
         bty='n')
}
dev.off()










