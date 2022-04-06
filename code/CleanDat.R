###Data import and cleaning. Data Daved to results as data.RData. 

##description of what the data looks like when saved:
#cleaned data is a list whose length is equal to the 
#number of individuals
#list elements are data.frames for each individual
#columns are microbe genera, rows are days
#NAs are gaps in the data (row-wide; days without samples) 
#and 0s mean no cells detected 


rm(list=ls())

##location to save results
res_loc <- "../results/"
if (dir.exists(res_loc)==FALSE){
	dir.create(res_loc)
}

##import tab deliminated .txt file
dat <- read.delim("../data/QMPs.txt")

#rows are days and 1st column is sample ID followed by 
#columns of genera
#individuals are stacked as rows 

colnames(dat)[1] <- 'sample'

#dat$sample

#individuals have different total samples
#each individual is identified by a unique integer followed by 
#a decimal and sample number, starting at 0 
#example: 1.1 is individual one day two 
#use this to separate individuals into list elements

##individual order in raw data is: 
ind_ID<-unique(floor(dat[[1]])) #no 14

datList <- vector(mode='list', length=21)

for (i in 1:length(datList)){
	datList[[i]] <- dat[dat$sample >= i & dat$sample<(i+1),-1]
}

datList <- datList[-14]
names(datList)<-ind_ID

#check that, for a given subject, whenever the microbial genus entries 
#are either all of no NAs
for (subject in 1:length(datList))
{
  h<-datList[[subject]]
  for (day in 1:(dim(h)[1]))
  {
    hday<-as.numeric(h[day,])
    if (any(is.na(hday)) & !all(is.na(hday)))
    {
      stop("Error in data cleaning: see checkpoint 1 in code")
    }
  }
}

##save
saveRDS(datList, file=paste0(res_loc, "dataList.Rds"))




















