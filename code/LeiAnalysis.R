#This script analyzes the data from the same viewpoint as Lei's paper

#***prelims

rm(list=ls())
set.seed(101)

#where to get the data from
datloc<-"../results/dataList.Rds"

#where to put the results
resloc<-"../results/Leiresults"
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

#***now calculate all the quantities of Lei for each subject

leires<-data.frame(subject=1:length(dat),Scom=NA,Spop=NA,Scomip=NA,SAE=NA,CPE=NA,varrat=NA,PhiVert=NA)
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
}

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

#***now make plots to compare our results to those of Lei




