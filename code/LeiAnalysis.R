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

#load the data
dat<-readRDS(file=datloc)
