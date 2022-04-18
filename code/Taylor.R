
datloc <- "./../data/"
dat <-  read.delim(paste0(datloc, "QMPs.txt"))

Data <- dat
getTaylor(Data)
