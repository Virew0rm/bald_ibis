#test logger raw data to readable data####
setwd("C:/Users/Jessica/Google Drive/Waldrapp/data")

#converting the text file into a csv
dat_ld=read.table("TEST_12.08.2018_1.txt", skip=28,fill=T, header=F)


#converting the 
dat_ld1 <- dat_ld[seq(1,length(dat_ld[,1]),2),1:4]
dat_ld2 <- dat_ld[seq(2,length(dat_ld[,1]),2),]

head(dat_ld)

dat_ld <- cbind(dat_ld1, dat_ld2)
dat_ld<- dat_ld[c(-9, -13)]
names(dat_ld) <- c("ms1", "p", "temp", "alt", "ms2", "gX", "gY", "gZ", "X", "Y", "Z", "mX", "mY", "mZ")


#deleting the letters out of the columns
#ms1
dat_ld$ms1<- gsub("ms", "", dat_ld$ms1)
#p
dat_ld$p<- gsub("p", "", dat_ld$p)
#temp
dat_ld$temp<-gsub("t", "", dat_ld$temp)
#alt
dat_ld$alt<-gsub("alt","", dat_ld$alt)
#ms2
dat_ld$ms2<- gsub("ms", "", dat_ld$ms2)
#gX
dat_ld$gX<- gsub("gX", "", dat_ld$gX)
#gY
dat_ld$gY<- gsub("gY", "", dat_ld$gY)
#gZ
dat_ld$gZ<- gsub("gZ", "", dat_ld$gZ)

#changing all variables into numeric
dat_ld$ms1<- as.numeric(dat_ld$ms1)
dat_ld$ms2<- as.numeric(dat_ld$ms2)
dat_ld$p<- as.numeric(dat_ld$p)
dat_ld$temp<- as.numeric(dat_ld$temp)
dat_ld$alt<- as.numeric(dat_ld$alt)
dat_ld$gX<- as.numeric(dat_ld$gX)
dat_ld$gY<- as.numeric(dat_ld$gY)
dat_ld$gZ<- as.numeric(dat_ld$gZ)
dat_ld$X<- as.numeric(dat_ld$X)
dat_ld$Y<- as.numeric(dat_ld$Y)
dat_ld$mZ<- as.numeric(dat_ld$Z)
dat_ld$mX<- as.numeric(dat_ld$mX)
dat_ld$mY<- as.numeric(dat_ld$mY)
dat_ld$mZ<- as.numeric(dat_ld$mZ)

dat_ld<-na.omit(dat_ld)

write.csv(dat_ld, file="test_nl_bernhard.csv", row.names=F)
