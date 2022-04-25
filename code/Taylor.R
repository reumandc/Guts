#install.packages('tseries')
#install.packages("car")
datloc <- "./../data/"
dat <-  read.delim(paste0(datloc, "QMPs.txt"))

Data <- dat
aa <- getTaylor(Data)
bb <- test_linearity(Data)
