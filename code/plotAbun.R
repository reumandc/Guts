#makes subject amount of figures with each figures containing lines
#for each taxa that are time series of taxa abundance
#option to include a line for total community abundance
#and option to include gaps where NAs are present
#
#ARGS
#datList    list of data.frame where columns are taxa and rows are days
#na         T/F; include gaps in lines where NAs are present? 
#           default T
#total      T/F; plot a line for total community abundance over time? 
#           default F
#synMets    dataframe of synchrony/stability metrics
plotAbun <- function(datList, na=TRUE, total=FALSE, synMets){
  if (total==TRUE){
    dat <- lapply(datList, function(X){cbind(X,rowSums(X))})
  } else {dat <- datList}
  
  ymax <- max(sapply(dat, max, na.rm=T))
  
  blue <- rgb(0,0,0.75,0.3)
  
  for (subj in 1:length(dat)){
    gut <- dat[[subj]]
    if (na==FALSE){
      gut <- gut[!is.na(rowSums(gut)),]
    }
    
    plot(NULL, ylim=c(0,ymax), xlim=c(1,nrow(gut)), yaxt='n')
    
    if (subj==1){axis(side=2, at=c(0, ymas*.8), labels=TRUE)}
    
    title(main=paste('subject', names(dat[subj])), line=-1, font.main=1, adj=0.01)
    
    for (bug in 1:ncol(gut)){
      lines(gut[,bug], col=blue, lwd=0.8)
      
      if (total==TRUE & bug==ncol(gut)){
        lines(gut[,bug], col='red', lwd=2) #xtot
      }
    }
    mets <- round(synMets,2)
    mtext(paste('Vcom', mets$Vcom[subj], 
                'Vind', mets$Vind[subj],
                'Vsyn', mets$Vsyn[subj],
                'phiV', mets$phiV[subj],
                'phiV_LdM', mets$phiV_LdM[subj]),
                side=3, cex=0.5, line=-4, at=nrow(gut), adj=1)
  }
  xlab <- "days"
  if (na==FALSE){xlab <- paste(xlab, "(NA's removed)")}
  title(xlab=xlab, outer=T, line=1.2, cex.lab=1.5)
  title(ylab='microbe abundance', outer=T, line=-.2, cex.lab=1.5)
  title(main="microbiome taxa abundance over time", outer=T, line=-0.7,
        font.main=1, cex.main=1.5)
}

#results location
res_loc <- "../results/"

#load clean data
dat <- readRDS(paste0(res_loc,"dataList.Rds"))












