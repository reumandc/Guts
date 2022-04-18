#This script analyzes the data from the same viewpoint as Lei's paper

#***prelims

rm(list=ls())
set.seed(101)

#where to get the data from
datloc<-"../results/dataList.Rds"

#where to put the results
resloc<-"../results/Leiresults/"
if (!dir.exists(resloc))
{
  dir.create(resloc)
}

#load the data - see top of CleanDat.R for description
dat<-readRDS(file=datloc)

#source the functions you'll need
source("PhiVert.R")
source("CPE.R")
source("SAE.R")
source("Scom.R")
source("Scomip.R")
source("Spop.R")
source("varrat.R")
source("shannon.R")
source("simpson.R")

#***now calculate all the quantities of Lei for each subject

leires<-data.frame(subject=1:length(dat),Scom=NA,Spop=NA,Scomip=NA,SAE=NA,CPE=NA,varrat=NA,PhiVert=NA,
                   richness1=NA,richness2=NA,shannon1=NA,shannon2=NA,simpson1=NA,simpson2=NA,days=NA)
for (subject in 1:length(dat))
{
  sdat<-dat[[subject]]
  
  #remove days when no sample was available, relies on the check in CleanDat
  #that either all or none of the data are missing for a day
  sdat<-sdat[-which(is.na(sdat[,1])),]
  if (any(is.na(sdat)))
  {
    stop("Error in LeiAnalysis: checkpoint 1")
  }
  
  #remove genera which were never present
  sdat<-sdat[,-which(apply(FUN=max,X=sdat,MARGIN=2)==0)]
  
  #now fill in the values
  leires[subject,"Scom"]<-Scom(sdat)
  leires[subject,"Spop"]<-Spop(sdat)
  leires[subject,"Scomip"]<-Scomip(sdat)
  leires[subject,"SAE"]<-SAE(sdat)
  leires[subject,"CPE"]<-CPE(sdat)
  leires[subject,"varrat"]<-varrat(sdat)
  leires[subject,"PhiVert"]<-PhiVert(sdat)
  leires[subject,"richness1"]<-dim(sdat)[2] #total number of taxa ever seen
  leires[subject,"richness2"]<-mean(apply(FUN=function(x){sum(x>0)},MARGIN=1,X=sdat)) #average across time of numbers detected in each sample
  leires[subject,"shannon1"]<-shannon(apply(FUN=mean,X=sdat,MARGIN=2)) #take the mean abundance of each species and then compute shannon
  leires[subject,"shannon2"]<-mean(apply(FUN=shannon,MARGIN=1,X=sdat)) #compute shannon for each time step and then average across time
  leires[subject,"simpson1"]<-simpson(apply(FUN=mean,X=sdat,MARGIN=2)) #take the mean abundance of each species and then compute simpson
  leires[subject,"simpson2"]<-mean(apply(FUN=simpson,MARGIN=1,X=sdat)) #compute simpson for each time step and then average across time
  leires[subject,"days"]<-dim(sdat)[1]
}

saveRDS(leires,file=paste0(resloc,"NumericResults.Rds"))

#***check a few identities

if (any(abs(leires$Scom-leires$PhiVert*leires$Spop)>1e-12))
{
  stop("Error in LeiAnalysis: checkpoint 2")
}
if (any(abs(leires$PhiVert-leires$SAE*leires$CPE)>1e-12))
{
  stop("Error in LeiAnalysis: checkpoint 3")
}
if (any(abs(leires$Scomip-leires$SAE*leires$Spop)>1e-12))
{
  stop("Error in LeiAnalysis: checkpoint 4")
}
if (any(abs(leires$CPE-1/sqrt(leires$varrat))>1e-12))
{
  stop("Error in LeiAnalysis: checkpoint 5")
}

#***make a few comparisons between measures of diversity

pdf(file=paste0(resloc,"CorrelationBetweenTwoMeasuresOfRichness.pdf"))
plot(leires$richness1,leires$richness2,type="p",xlab="richness 1",ylab="richness 2")
dev.off()
RichnessCor<-cor(leires$richness1,leires$richness2) 
saveRDS(RichnessCor,paste0(resloc,"CorrelationBetweenTwoMeasuresOfRichness.Rds"))

pdf(file=paste0(resloc,"CorrelationBetweenTwoMeasuresOfShannon.pdf"))
plot(leires$shannon1,leires$shannon2,type="p",xlab="shannon 1",ylab="shannon 2")
dev.off()
ShannonCor<-cor(leires$shannon1,leires$shannon2) 
saveRDS(ShannonCor,paste0(resloc,"CorrelationBetweenTwoMeasuresOfShannon.Rds"))

pdf(file=paste0(resloc,"CorrelationBetweenTwoMeasuresOfSimpson.pdf"))
plot(leires$simpson1,leires$simpson2,type="p",xlab="simpson 1",ylab="simpson 2")
dev.off()
SimpsonCor<-cor(leires$simpson1,leires$simpson2) 
saveRDS(SimpsonCor,paste0(resloc,"CorrelationBetweenTwoMeasuresOfSimpson.Rds"))


pdf(file=paste0(resloc,"RichnessVsShannon1.pdf"))
plot(leires$richness1,leires$shannon1,type="p",xlab="richness 1",ylab="shannon 1")
dev.off()
RichnessShannon1Cor<-cor(leires$richness1,leires$shannon1)
saveRDS(RichnessShannon1Cor,paste0(resloc,"CorRichnessShannon1.Rds"))

pdf(file=paste0(resloc,"RichnessVsInvSimpson1.pdf"))
plot(leires$richness1,1/leires$simpson1,type="p",xlab="richness 1",ylab="1/(simpson 1)")
dev.off()
RichnessInvSimpson1Cor<-cor(leires$richness1,1/leires$simpson1)
saveRDS(RichnessInvSimpson1Cor,paste0(resloc,"CorRichnessInvSimpson1.Rds"))

pdf(file=paste0(resloc,"ShannonVsInvSimpson1.pdf"))
plot(leires$shannon1,1/leires$simpson1,type="p",xlab="shannon 1",ylab="1/(simpson 1)")
dev.off()
ShannonInvSimpson1Cor<-cor(leires$shannon1,1/leires$simpson1)
saveRDS(ShannonInvSimpson1Cor,paste0(resloc,"CorShannonInvSimpson1.Rds"))

#The upshot of the above is basically that it should not matter much what index you use

#***now make plots to compare our results to those of Lei





