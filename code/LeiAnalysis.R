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
leires<-data.frame(subject=1:length(dat),Scom=NA,Spop=NA,Scomip=NA)
for (subject in 1:length(dat))
{
  sdat<-dat[[subject]]
  
  #remove days when no sample was available, relies on the check in CleanDat
  #that either all or none of the data are missing for a day
  sdat<-sdat[-which(is.na(sdat[,1])),]
  
  #remove genera which were never present
  sdat<-sdat[,-which(apply(FUN=max,X=sdat,MARGIN=2)==0)]
  
  #now fill in the values
  #***DAN: still need to do this after writing functions
}
