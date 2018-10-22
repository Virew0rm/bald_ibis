#test logger raw data to readable data####
setwd("C:/Users/Jessica/Google Drive/Waldrapp/data")

#converting the text file into a csv
fname=read.delim("TEST_12.08.2018_1.txt")
write.table(fname, file="test_ld_(2).csv", sep=",", dec=".")
dat_ld=read.csv("test_ld_(2).csv" ,sep=",",header=F, skip=28)
#deleting the first column because it does not contain any data
dat_ld=dat_ld[, -1]
head(dat_ld)

#converting the 
dat_ld<- as.data.frame(dat_ld)

dat_ld$dat_ld<-as.character(dat_ld$dat_ld)

#putting all the values in one column below each other
dat_sld<-data.frame(unlist(strsplit(dat_ld$dat_ld, " ")))

#removing all the empty spaces, datapoints are one long factor now
dat_nsld<-dat_sld[!apply(dat_sld == "", 1, all),]

dat_nsld<- na.omit(dat_nsld)


#creating a new empty vector 
dat_nl=NULL
#length needs to changed depending on individual file length
#check how long your file is by just extracting one variable beforehand
p<- grep("p", dat_nsld)
#and then adding this to the new dataframe
dat_nl=rep(0, 470400)
#and converting it into a dataframe
dat_nl<- as.data.frame(dat_nl)

#grab all variable containing ms 
ms<- grep("ms", dat_nsld)
ms<-dat_nsld[ms]
#splitting of the first ms variable
ms1<- ms[seq(1, length(ms), 2)]
dat_nl$ms1<- ms1

#grab all variables containing p
p<-grep("p", dat_nsld)
dat_nl$p<-dat_nsld[p]

#grab all variables containing t
t<- grep("t", dat_nsld)
t<- dat_nsld[t]
#split the temperature variable off
temp<- t[seq(1, length(t), 2)]
dat_nl$temp<- temp
#split the altitude variable off
alt<-t[seq(2, length(t),2)]
dat_nl$alt<-alt

#splitting off the second ms variable
ms2<- ms[seq(2, length(ms), 2)]
dat_nl$ms2<- ms2

#grab all variables containing gX
gX<- grep("gX", dat_nsld)
dat_nl$gX<- dat_nsld[gX]

#grab all variables containing gY
gY<- grep("gY", dat_nsld)
dat_nl$gY<- dat_nsld[gY]

#grab all variables containing gZ
gZ<- grep("gZ", dat_nsld)
dat_nl$gZ<- dat_nsld[gZ]

#extract the 10th, 11th and 12th element from at sequence of 16 
X<- dat_nsld[seq(10, length(dat_nsld), 16)]
dat_nl$X<- X

Y<- dat_nsld[seq(11, length(dat_nsld), 16)]
dat_nl$Y<-Y

Z<- dat_nsld[seq(12, length(dat_nsld), 16)]
dat_nl$Z<-Z

#extract the 14th, 15th and 16th element from at sequence of 16 
mX<- dat_nsld[seq(14, length(dat_nsld), 16)]
dat_nl$mX<- mX

mY<- dat_nsld[seq(15, length(dat_nsld), 16)]
dat_nl$mY<-mY

mZ<- dat_nsld[seq(16, length(dat_nsld), 16)]
dat_nl$mZ<-mZ

#delete the first column
dat_nl<- dat_nl[-1]

head(dat_nl)

#deleting the letters out of the columns
#ms1
dat_nl$ms1<- gsub("ms", "", dat_nl$ms1)
#p
dat_nl$p<- gsub("p", "", dat_nl$p)
#temp
dat_nl$temp<-gsub("t", "", dat_nl$temp)
#alt
dat_nl$alt<-gsub("alt","", dat_nl$alt)
#ms2
dat_nl$ms2<- gsub("ms", "", dat_nl$ms2)
#gX
dat_nl$gX<- gsub("gX", "", dat_nl$gX)
#gY
dat_nl$gY<- gsub("gY", "", dat_nl$gY)
#gZ
dat_nl$gZ<- gsub("gZ", "", dat_nl$gZ)

write.csv(dat_nl, file="test_nl_bernhard.csv", row.names=F)

