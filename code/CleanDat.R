###Data import and cleaning. Data Daved to results as data.RData. 

##description of what the data looks like when saved:
#cleaned data is a list whose length is equal to the 
#number of individuals
#list elements are data.frames for each individual
#columns are microbe genera, rows are samples

##location to save results
res_loc <- "../results/"
if (dir.exists(res_loc)==FALSE){
	dir.create(res_loc)
}

##import tab deliminated .txt file
dat <- read.delim("../data/QMPs.txt")

#rows are samples and 1st column is sample ID followed by 
#columns of genera
#individuals are stacked as rows 

colnames(dat)[1] <- 'sample'

#dat$sample

#individuals have different total samples
#each individual is identified by a unique integer followed by 
#a decimal and sample number 
#example: 1.1 is individual one sample one 
#use this to separate individuals into list elements

##individual order in raw data is: 
ind_ID <- c(1:11, 13, 15:17, 12, 18:21) #no 14

datList <- vector(mode='list', length=21)

for (i in 1:length(datList)){
	datList[[i]] <- dat[dat$sample >= i & dat$sample<(i+1),-1]
}

datList <- datList[-14]
names(datList) <- c(1:11, 13, 15:17, 12, 18:21)


##save
save(datList, file=paste(res_loc, "dataList.RData"))




















