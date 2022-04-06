###Data import and cleaning. Data Daved to results as data.RData. 

##description of what the data looks like when saved:
#cleaned data is a list whose length is equal to the 
#number of individuals
#list elements are data.frames for each individual
#columns are microbe genera, rows are samples
#***DAN: jasmin, please add statement here about what the rows are, and what the
#difference is between NAs and 0s. Then delete this comment.

rm(list=ls())

##location to save results
res_loc <- "../results/"
if (dir.exists(res_loc)==FALSE){
	dir.create(res_loc)
}

##import tab deliminated .txt file
dat <- read.delim("../data/QMPs.txt")

#rows are samples and 1st column is sample ID followed by 
#***DAN: I am not sure it is true that rows are samples. It looks like, for a
#given subject, you'll have NAs under all genera if you have NAs under any of them.
#That suggests rows are days, with NAs when there was no sample that day. The rows
#cannot be samples, because you cannot have a sample with all NAs for all genera,
#what would that mean? After fixing, pls delete this comment.
#columns of genera
#individuals are stacked as rows 

colnames(dat)[1] <- 'sample'

#dat$sample

#individuals have different total samples
#each individual is identified by a unique integer followed by 
#a decimal and sample number 
#example: 1.1 is individual one sample one 
#***DAN: I do not think this is quite right because there is a 1.00, so I guess
#that is the first sample, not 1.1.
#use this to separate individuals into list elements

##individual order in raw data is: 
#ind_ID <- c(1:11, 13, 15:17, 12, 18:21) #no 14
ind_ID<-unique(floor(dat[[1]]))
#***DAN: Please check this change, which gives the same thing as the commented line.
#If the change looks OK, please delete this comment and the commented line.

datList <- vector(mode='list', length=21)

for (i in 1:length(datList)){
	datList[[i]] <- dat[dat$sample >= i & dat$sample<(i+1),-1]
}

datList <- datList[-14]
#names(datList) <- c(1:11, 13, 15:17, 12, 18:21)
names(datList)<-ind_ID
#***DAN: Jasmin, pls check this change and if it looks OK delete this comment 
#and the commented line

##save
saveRDS(datList, file=paste0(res_loc, "dataList.Rds"))




















