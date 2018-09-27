setwd("C:/Users/Jessica/Google Drive/Waldrapp/data")
dat=read.table("TEST_12.08.2018_1.csv", sep=",", dec=".", fill=T, header=F)
head(dat)

dat<- dat[, c(2, 4, 6, 8, 10, 12,14 ,16,18, 19, 20, 22, 23, 24)]

names(dat)<- c("ms", "p", "t", "alt", "ms2", "gx", "gy", "gz" , "x", "y", "z", "mx", "my", "mz")

head(dat)

dat1<- subset(dat, ms>=(500))
dat1<- subset(dat1, ms<=(8000))

plot(dat1$z~dat1$ms, type="l")
