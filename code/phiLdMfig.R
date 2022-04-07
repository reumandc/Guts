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