setwd("C:/Users/Jessica/Google Drive/Waldrapp/data")
dat=read.table("TEST_12.08.2018_1.csv", sep=",", dec=".", fill=T, header=F)
head(dat)

dat<- dat[, c(2, 4, 6, 8, 10, 12,14 ,16,18, 19, 20, 22, 23, 24)]

names(dat)<- c("ms", "p", "t", "alt", "ms2", "gx", "gy", "gz" , "x", "y", "z", "mx", "my", "mz")

head(dat)

dat1<- subset(dat, ms>=(500))
dat1<- subset(dat1, ms<=(8000))

plot(dat1$z~dat1$ms, type="l")



setwd("C:/Users/Jessica/Google Drive/Waldrapp/data")

fname=read.delim("TEST_12.08.2018_1.txt")
write.table(fname, file="TEST_12.08.2018_1_(2).csv", sep=",", dec=".")



dat=read.csv("TEST_12.08.2018_1_(2).csv" ,sep=",",header=F, skip=28)
dat=dat[, -1]

dat<- as.data.frame(dat)

dat_new=NA

dat_new$ms<- as.numeric(paste(substr(as.character(dat[1,]),3,5)))



head(dat_new)
names(dat)<- c("ms", "p", "t", "alt", "ms2", "gx", "gy", "gz" , "x", "y", "z", "mx", "my", "mz")





#test logger lying down####
setwd("C:/Users/Jessica/Google Drive/Waldrapp/data")
fname=read.delim("test_ld.txt")
write.table(fname, file="test_ld_(2).csv", sep=",", dec=".")

dat_ld=read.csv("test_ld_(2).csv" ,sep=",",header=F, skip=28)
dat_ld=dat_ld[, -1]
head(dat_ld)

dat_ld<- as.data.frame(dat_ld)

dat_ld$dat_ld<-as.character(dat_ld$dat_ld)

dat_nld=NA

dat_nld$ms<- as.numeric(paste(substr(as.character(dat_ld[2,]),3,5)))
dat_nld$gX<- as.numeric(paste(substr(as.character(dat_ld[2,]),9,10)))
dat_nld$gY<- as.numeric(paste(substr(as.character(dat_ld[2,]),14,14)))
dat_nld$gZ<- as.numeric(paste(substr(as.character(dat_ld[2,]),18, 18)))
dat_nld$x<- as.numeric(paste(substr(as.character(dat_ld[2,]),22,28)))
dat_nld$y<- as.numeric(paste(substr(as.character(dat_ld[2,]),29,35)))
dat_nld$z<- as.numeric(paste(substr(as.character(dat_ld[2,]),36,42)))


dat_sld<-data.frame(unlist(strsplit(dat_ld$dat_ld, " ")))
dat_nsld<-dat_sld[!apply(dat_sld == "", 1, all),]
###funktioniert noch nicht dat_nsld<-dat_nsld[!apply(dat_sld == "A:", 1, all),]


dat_sld
dat_sld$dat_sld<-gsub(" ", "", dat_sld$dat_sld)

dat_sld<-as.data.frame(dat_sld)

dat_sld[1]

head(dat_nsld, 50)
tail(dat_ld)
