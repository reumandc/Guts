#results location
res_loc <- "../results/"

#load data
mets <- readRDS(paste0(res_loc,"DSdat.Rds"))

#figure location
fig_loc <- "../figures/"
if (dir.exists(fig_loc)==F){
	dir.create(fig_loc)
}

yvars <- c('phiV_LdM', 'Vcom', 'Vsyn')
xvars <- c('richness', 'H', 'evenness')

pdf(paste0(fig_loc,'DSR.pdf'), height=7, width=8)
par(mfrow=c(3,3), mar=c(2,2,1,1), oma=c(3,2,3,1), mgp=c(2,0.7,0), xpd=NA)

for (y in yvars){
	for (x in xvars){
		stab <- mets[y]; div <- mets[x]
		plot(div[,1], stab[,1], yaxt='n', xaxt='n', pch=16, col='darkblue',
		xlab='', ylab='')
		
		if (x==xvars[1]) {title(ylab=y, cex.lab=1.3, col.lab='darkblue')}
		if (y==yvars[3]) {title(xlab=x, cex.lab=1.3, col.lab='darkblue')}
		if (which(y==yvars)==which(x==xvars)){
			axis(side=1); axis(side=2)
		}
		
		cor <- round(c(cor.test(stab[,1],div[,1])$estimate,
		cor.test(stab[,1],div[,1])$p.value),3)
		
		mtext(paste("rho=",cor[1],"\np=",cor[2]),side=3, line=-2.3, adj=.99, cex=.7)
	}
}
title(main="Diversity-stability relationships", outer=T, cex.main=1.7, line=0)


dev.off()



















