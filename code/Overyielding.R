# This section runs the necessary analyses to investigate whether or not species in 


# Clearing and reading section taken from LeiAnalysis.R
rm(list=ls())
set.seed(20164)

#where to get the data from
datloc<-"../results/dataList.Rds"

#where to put the results
resloc<-"../results/Leiresults/"
if (!dir.exists(resloc)){
  dir.create(resloc)
}

#load the data - see top of CleanDat.R for description
dat<-readRDS(file=datloc)

# Function sourcing
source("Shann.R")
source("Simp.R")
source("Rich.R")

source("ShannNet.R")
source("SimpNet.R")
source("RichNet.R")

TaxVec <- vector(mode = "character")
for(i in 1:length(dat)){
  # Append any new taxa 
  TaxVec <- unique(c(TaxVec, colnames(dat[[i]])))
}

TaxList <- vector(mode = "list", length = length(TaxVec))
TaxMat <- matrix(data = NA, nrow = 1, ncol = 8)
colnames(TaxMat) <- c("mAb", "Shann", "Simp", "Rich", "NetShann", "NetSimp", "NetRich", "Days")
for(i in 1:length(TaxList)){
  TaxList[[i]] <- TaxMat
}
names(TaxList) <- TaxVec

for(i in 1:length(dat)){
  dSub <- dat[[i]]
  #remove days when no sample was available, relies on the check in CleanDat
  #that either all or none of the data are missing for a day, also removes taxa which are not found in the sample
  dSub<-dSub[-which(is.na(dSub[,1])),]
  IncVec <- apply(FUN = sum, X = dSub, MARGIN = 2)
  dSub <- dSub[,which(IncVec != 0)]
  
  # Calculating each value of interest  
  ShannSub <- Shann(dSub)
  SimpSub <- Simp(dSub)
  RichSub <- Rich(dSub)
  NetShannSub <- ShannNet(dSub)
  NetSimpSub <- SimpNet(dSub)
  NetRichSub <- RichNet(dSub)
  DaysSub <- dim(dSub)[1]
  for(j in colnames(dSub)){
    AppVec <- c(mean(dSub[,j]), ShannSub, SimpSub, RichSub, NetShannSub, NetSimpSub, NetRichSub, DaysSub)
    TaxList[[j]] <- rbind(TaxList[[j]], AppVec)
  }
}

# Removing the NA values we used to construct the matrices initially
for(i in 1:length(TaxList)){
  TaxList[[i]] <- TaxList[[i]][-1,]
}

TaxFrame <- data.frame(Taxa = names(TaxList), n = NA, ShannCorr = NA, ShannSig = NA, SimpCorr = NA, SimpSig = NA, RichCorr = NA, RichSig = NA,
                       NetShannCorr = NA, NetShannSig = NA, NetSimpCorr = NA, NetSimpSig = NA, NetRichCorr = NA, NetRichSig = NA)

m <- "spearman"
for(i in 1:length(TaxList)){
  Check <- dim(TaxList[[i]])[1]
  if(length(Check > 0)){
  TaxFrame[i,"n"] <- Check
  
  Sh <- cor.test(x = TaxList[[i]][,"mAb"], y = TaxList[[i]][,"Shann"], method = m, exact = F)
  TaxFrame[i, "ShannCorr"] <- Sh[["estimate"]]
  TaxFrame[i, "ShannSig"] <- Sh[["p.value"]]
  
  Si <- cor.test(x = TaxList[[i]][,"mAb"], y = TaxList[[i]][,"Simp"], method = m, exact = F)
  TaxFrame[i, "SimpCorr"] <- Si[["estimate"]]
  TaxFrame[i, "SimpSig"] <- Si[["p.value"]]
  
  R <- cor.test(x = TaxList[[i]][,"mAb"], y = TaxList[[i]][,"Rich"], method = m, exact = F)
  TaxFrame[i, "RichCorr"] <- R[["estimate"]]
  TaxFrame[i, "RichSig"] <- R[["p.value"]]
  
  NetSh <- cor.test(x = TaxList[[i]][,"mAb"], y = TaxList[[i]][,"NetShann"], method = m, exact = F)
  TaxFrame[i, "NetShannCorr"] <- NetSh[["estimate"]]
  TaxFrame[i, "NetShannSig"] <- NetSh[["p.value"]]
  
  NetSi <- cor.test(x = TaxList[[i]][,"mAb"], y = TaxList[[i]][,"NetSimp"], method = m, exact = F)
  TaxFrame[i, "NetSimpCorr"] <- NetSi[["estimate"]]
  TaxFrame[i, "NetSimpSig"] <- NetSi[["p.value"]]
  
  NetR <- cor.test(x = TaxList[[i]][,"mAb"], y = TaxList[[i]][,"NetRich"], method = m, exact = F)
  TaxFrame[i, "NetRichCorr"] <- NetR[["estimate"]]
  TaxFrame[i, "NetRichSig"] <- NetR[["p.value"]]
  } else {
    TaxFrame[i,"n"] <- 1
  }
  
}
TaxFrame<-na.omit(TaxFrame)


