options(digits.secs=3)

#####4075####
setwd("C:/Users/Jessica/Google Drive/Waldrapp/data")
dat=read.table("tag4075.txt", sep=",", dec=".", fill=T, header=F)
head(dat)

#separate ACC from GPS
GPS <- dat[dat$V1=="GPS",1:22]
ACC <- dat[dat$V1=="ACC",]

secs <- as.character(round(seq(0,1,1/12)[1:12],3))
secs <- ifelse(nchar(secs)==1, paste0(secs, ".000"),
               ifelse(nchar(secs)==3, paste0(secs, "00"),
                      ifelse(nchar(secs)==4, paste0(secs, "0"),
                             ifelse(nchar(secs)==5, secs, NA))))
secs <- substr(secs,2,5)

t1<- Sys.time()
#separate every burst into one row and add the 12th second to it
ACC_new_4075 <- NULL
for(i in 1:length(ACC[,1])) {
  temp <- data.frame(ID=rep(NA,12), time=rep(NA,12), x=rep(NA,12), y=rep(NA,12), z=rep(NA,12))
  temp$x <- as.numeric(t(ACC[i,seq(6,39,3)]))
  temp$y <- as.numeric(t(ACC[i,seq(7,40,3)]))
  temp$z <- as.numeric(t(ACC[i,seq(8,41,3)]))
  temp$datetime <- paste(ACC$V3[i], ACC$V5[i], sep=" ")
  temp$time <- strptime(paste0(ACC$V3[i], " ", ACC$V5[i], secs), format="%d.%m.%Y %H:%M:%OS")
  
  ACC_new_4075 <- rbind(ACC_new_4075, temp)
}

ACC_new_4075$ID <- "4075"

Sys.time()-t1


#deleting unnecessary variables and renaming columns
GPS_new_4075<- GPS[,c(-1,-3,-4,-5)]
names(GPS_new_4075)<- c("ID", "north", "east", "asl",  "type", "status", "GPS clog flag","GPS noise level", "time_fix", "date", "weekday", "time", "battery voltage", "temperature", "speed", "heading", "speed inaccuray estimate", "horizontal inaccuracy estimate")
GPS_new_4075$datetime<-as.POSIXct(strptime(paste(GPS_new_4075$date, GPS_new_4075$time), format="%d.%m.%Y %H:%M:%S"))
GPS_new_4075$ID<-"4075"
#removing data where GPS was not working appropriately
GPS_new_4075<- subset(GPS_new_4075, GPS_new_4075$status=="A")

write.csv(GPS_new_4075, file="GPS_tag4075.csv", sep="\t", col.names=T, row.names=F)

#ACC_new_4078<- read.csv("ACC_4078.csv", sep=",", dec=".", fill=T, header=T)
#changing format of datetime and beforehand extracting only date and time from time variable
ACC_new_4075$datetime<-ACC_new_4075$time
ACC_new_4075$datetime<-substr(ACC_new_4075$datetime, 1, 19)
ACC_new_4075$datetime<- as.POSIXct(strptime(ACC_new_4075$datetime, format="%d.%m.%Y %H:%M:%S"))

#add g to the excel file, because excel is too stupid to do it 
head(ACC_new_4075)

ACC_new_4075$x_g <-  (ACC_new_4075$x-2048)*0.0022*9.81
ACC_new_4075$y_g <-  (ACC_new_4075$y-2048)*0.0022*9.81
ACC_new_4075$z_g <-  (ACC_new_4075$z-2048)*0.0022*9.81

write.csv(ACC_new_4075, file="ACC_4075_g.csv", sep="\t", col.names=T, row.names=F)
#ACC_new_4075$datetime<- ACC_new_4075$datetime1
#merging ACC and GPS
ACC_GPS_merge_4075<-merge(ACC_new_4075, GPS_new_4075, by="datetime")
#in case you only want good quality data, delete all occurences of status B in the GPS data
ACC_GPS_merge_4075<- subset(ACC_GPS_merge_4075,ACC_GPS_merge_4075$status=="A")

#tag 4075 has a long break in the data. cutting it at 06:00:00 on the 2014-08-28.
ACC_GPS_merge_4075<- subset(ACC_GPS_merge_4075, datetime>=as.POSIXct("2014-08-28 06:00:00"))

#write.csv(ACC_new_4075, file="ACC_4075.csv", sep="\t", col.names=T, row.names=F)

write.csv(ACC_GPS_merge_4075, file="merge_4075.csv", sep="\t", col.names=T, row.names=F)
####4078####
setwd("C:/Users/Jessica/Google Drive/Waldrapp/data")
dat=read.table("tag4078.txt", sep=",", dec=".", fill=T, header=F)
head(dat)

#separate ACC from GPS
GPS <- dat[dat$V1=="GPS",1:22]
ACC <- dat[dat$V1=="ACC",]

secs <- as.character(round(seq(0,1,1/12)[1:12],3))
secs <- ifelse(nchar(secs)==1, paste0(secs, ".000"),
               ifelse(nchar(secs)==3, paste0(secs, "00"),
                      ifelse(nchar(secs)==4, paste0(secs, "0"),
                             ifelse(nchar(secs)==5, secs, NA))))
secs <- substr(secs,2,5)

t1<- Sys.time()
#separate every burst into one row and add the 12th second to it
ACC_new_4078 <- NULL
for(i in 1:length(ACC[,1])) {
  temp <- data.frame(ID=rep(NA,12), time=rep(NA,12), x=rep(NA,12), y=rep(NA,12), z=rep(NA,12))
  temp$x <- as.numeric(t(ACC[i,seq(6,39,3)]))
  temp$y <- as.numeric(t(ACC[i,seq(7,40,3)]))
  temp$z <- as.numeric(t(ACC[i,seq(8,41,3)]))
  temp$datetime <- paste(ACC$V3[i], ACC$V5[i], sep=" ")
  temp$time <- strptime(paste0(ACC$V3[i], " ", ACC$V5[i], secs), format="%d.%m.%Y %H:%M:%OS")
  
  ACC_new_4078 <- rbind(ACC_new_4078, temp)
}

ACC_new_4078$ID <- "4078"

Sys.time()-t1


#deleting unnecessary variables and renaming columns
GPS_new_4078<- GPS[,c(-1,-3,-4,-5)]
names(GPS_new_4078)<- c("ID", "north", "east", "asl",  "type", "status", "GPS clog flag","GPS noise level", "time_fix", "date", "weekday", "time", "battery voltage", "temperature", "speed", "heading", "speed inaccuray estimate", "horizontal inaccuracy estimate")
GPS_new_4078$datetime<-as.POSIXct(strptime(paste(GPS_new_4078$date, GPS_new_4078$time), format="%d.%m.%Y %H:%M:%S"))
GPS_new_4078$ID<-"4078"
#removing data where GPS was not working appropriately
GPS_new_4078<- subset(GPS_new_4078, GPS_new_4078$status=="A")

write.csv(GPS_new_4078, file="GPS_tag4078.csv", sep="\t", col.names=T, row.names=F)
#ACC_new_4078<- read.csv("ACC_4078.csv", sep=",", dec=".", fill=T, header=T)
#changing format of datetime and beforehand extracting only date and time from time variable
ACC_new_4078$datetime<-ACC_new_4078$time
ACC_new_4078$datetime<-substr(ACC_new_4078$datetime, 1, 19)
ACC_new_4078$datetime<- as.POSIXct(strptime(ACC_new_4078$datetime, format="%Y-%m-%d %H:%M:%S"))
ACC_new_4078$datetime<- as.POSIXct(strptime(ACC_new_4078$datetime, format="%d.%m.%Y %H:%M:%S"))

#add g to the excel file, because excel is too stupid to do it 
head(ACC_new_4078)

ACC_new_4078$x_g <-  (ACC_new_4078$x-2048)*0.0022*9.81
ACC_new_4078$y_g <-  (ACC_new_4078$y-2048)*0.0022*9.81
ACC_new_4078$z_g <-  (ACC_new_4078$z-2048)*0.0022*9.81



write.csv(ACC_new_4078, file="ACC_4078_g.csv", sep="\t", col.names=T, row.names=F)
#ACC_new_4078$datetime<- ACC_new_4078$datetime1
#merging ACC and GPS
ACC_GPS_merge_4078<-merge(ACC_new_4078, GPS_new_4078, by="datetime")
#in case you only want good quality data, delete all occurences of status B in the GPS data
ACC_GPS_merge_4078<- subset(ACC_GPS_merge_4078,ACC_GPS_merge_4078$status=="A")

#tag 4078 has a long break in the data. cutting it at 06:00:00 on the 2014-08-28.
ACC_GPS_merge_4078<- subset(ACC_GPS_merge_4078, datetime>=as.POSIXct("2014-08-28 06:00:00"))

#write.csv(ACC_new_4078, file="ACC_4078.csv", sep="\t", col.names=T, row.names=F)

write.csv(ACC_GPS_merge_4078, file="merge_4078.csv", sep="\t", col.names=T, row.names=F)


####4079####
setwd("C:/Users/Jessica/Google Drive/Waldrapp/data")
dat=read.table("tag4079.txt", sep=",", dec=".", fill=T, header=F)
head(dat)

#separate ACC from GPS
GPS <- dat[dat$V1=="GPS",1:22]
ACC <- dat[dat$V1=="ACC",]

secs <- as.character(round(seq(0,1,1/12)[1:12],3))
secs <- ifelse(nchar(secs)==1, paste0(secs, ".000"),
               ifelse(nchar(secs)==3, paste0(secs, "00"),
                      ifelse(nchar(secs)==4, paste0(secs, "0"),
                             ifelse(nchar(secs)==5, secs, NA))))
secs <- substr(secs,2,5)

t1<- Sys.time()
#separate every burst into one row and add the 12th second to it
ACC_new_4079 <- NULL
for(i in 1:length(ACC[,1])) {
  temp <- data.frame(ID=rep(NA,12), time=rep(NA,12), x=rep(NA,12), y=rep(NA,12), z=rep(NA,12))
  temp$x <- as.numeric(t(ACC[i,seq(6,39,3)]))
  temp$y <- as.numeric(t(ACC[i,seq(7,40,3)]))
  temp$z <- as.numeric(t(ACC[i,seq(8,41,3)]))
  temp$datetime <- paste(ACC$V3[i], ACC$V5[i], sep=" ")
  temp$time <- strptime(paste0(ACC$V3[i], " ", ACC$V5[i], secs), format="%d.%m.%Y %H:%M:%OS")
  
  ACC_new_4079 <- rbind(ACC_new_4079, temp)
}

ACC_new_4079$ID <- "4079"

Sys.time()-t1


#deleting unnecessary variables and renaming columns
GPS_new_4079<- GPS[,c(-1,-3,-4,-5)]
names(GPS_new_4079)<- c("ID", "north", "east", "asl",  "type", "status", "GPS clog flag","GPS noise level", "time_fix", "date", "weekday", "time", "battery voltage", "temperature", "speed", "heading", "speed inaccuray estimate", "horizontal inaccuracy estimate")
GPS_new_4079$datetime<-as.POSIXct(strptime(paste(GPS_new_4079$date, GPS_new_4079$time), format="%d.%m.%Y %H:%M:%S"))
GPS_new_4079$ID<-"4079"
#removing data where GPS was not working appropriately
GPS_new_4079<- subset(GPS_new_4079, GPS_new_4079$status=="A")

write.csv(GPS_new_4079, file="GPS_tag4079.csv", sep="\t", col.names=T, row.names=F)
#ACC_new_4079<- read.csv("ACC_4079.csv", sep=",", dec=".", fill=T, header=T)
#changing format of datetime and beforehand extracting only date and time from time variable
ACC_new_4079$datetime<-ACC_new_4079$time
ACC_new_4079$datetime<-substr(ACC_new_4079$datetime, 1, 19)
ACC_new_4079$datetime<- as.POSIXct(strptime(ACC_new_4079$datetime, format="%d.%m.%Y %H:%M:%S"))


#add g to the excel file, because excel is too stupid to do it 
head(ACC_new_4079)

ACC_new_4079$x_g <-  (ACC_new_4079$x-2048)*0.0022*9.81
ACC_new_4079$y_g <-  (ACC_new_4079$y-2048)*0.0022*9.81
ACC_new_4079$z_g <-  (ACC_new_4079$z-2048)*0.0022*9.81


write.csv(ACC_new_4079, file="ACC_4079_g.csv", sep="\t", col.names=T, row.names=F)
#ACC_new_4079$datetime<- ACC_new_4079$datetime1
#merging ACC and GPS
ACC_GPS_merge_4079<-merge(ACC_new_4079, GPS_new_4079, by="datetime")
#in case you only want good quality data, delete all occurences of status B in the GPS data
ACC_GPS_merge_4079<- subset(ACC_GPS_merge_4079,ACC_GPS_merge_4079$status=="A")


#tag 4079 has a long break in the data. cutting it at 06:00:00 on the 2014-08-28.
ACC_GPS_merge_4079<- subset(ACC_GPS_merge_4079, datetime>=as.POSIXct("2014-08-28 06:00:00"))

#write.csv(ACC_new_4079, file="ACC_4079.csv", sep="\t", col.names=T, row.names=F)

write.csv(ACC_GPS_merge_4079, file="merge_4079.csv", sep="\t", col.names=T, row.names=F)





####4080####

setwd("C:/Users/Jessica/Google Drive/Waldrapp/data")
dat=read.table("tag4080.txt", sep=",", dec=".", fill=T, header=F)
head(dat)

#separate ACC from GPS
GPS <- dat[dat$V1=="GPS",1:22]
ACC <- dat[dat$V1=="ACC",]

secs <- as.character(round(seq(0,1,1/12)[1:12],3))
secs <- ifelse(nchar(secs)==1, paste0(secs, ".000"),
               ifelse(nchar(secs)==3, paste0(secs, "00"),
                      ifelse(nchar(secs)==4, paste0(secs, "0"),
                             ifelse(nchar(secs)==5, secs, NA))))
secs <- substr(secs,2,5)

t1<- Sys.time()
#separate every burst into one row and add the 12th second to it
ACC_new_4080 <- NULL
for(i in 1:length(ACC[,1])) {
  temp <- data.frame(ID=rep(NA,12), time=rep(NA,12), x=rep(NA,12), y=rep(NA,12), z=rep(NA,12))
  temp$x <- as.numeric(t(ACC[i,seq(6,39,3)]))
  temp$y <- as.numeric(t(ACC[i,seq(7,40,3)]))
  temp$z <- as.numeric(t(ACC[i,seq(8,41,3)]))
  temp$datetime <- paste(ACC$V3[i], ACC$V5[i], sep=" ")
  temp$time <- strptime(paste0(ACC$V3[i], " ", ACC$V5[i], secs), format="%d.%m.%Y %H:%M:%OS")
  
  ACC_new_4080 <- rbind(ACC_new_4080, temp)
}

ACC_new_4080$ID <- "4080"

Sys.time()-t1


#deleting unnecessary variables and renaming columns
GPS_new_4080<- GPS[,c(-1,-3,-4,-5)]
names(GPS_new_4080)<- c("ID", "north", "east", "asl",  "type", "status", "GPS clog flag","GPS noise level", "time_fix", "date", "weekday", "time", "battery voltage", "temperature", "speed", "heading", "speed inaccuray estimate", "horizontal inaccuracy estimate")
GPS_new_4080$datetime<-as.POSIXct(strptime(paste(GPS_new_4080$date, GPS_new_4080$time), format="%d.%m.%Y %H:%M:%S"))
GPS_new_4080$ID<-"4080"
#removing data where GPS was not working appropriately
GPS_new_4080<- subset(GPS_new_4080, GPS_new_4080$status=="A")

write.csv(GPS_new_4080, file="GPS_tag4080.csv", sep="\t", col.names=T, row.names=F)
#ACC_new_4080<- read.csv("ACC_4080.csv", sep=",", dec=".", fill=T, header=T)
#changing format of datetime and beforehand extracting only date and time from time variable
ACC_new_4080$datetime<-ACC_new_4080$time
ACC_new_4080$datetime<-substr(ACC_new_4080$datetime, 1, 19)
ACC_new_4080$datetime<- as.POSIXct(strptime(ACC_new_4080$datetime, format="%Y-%m-%d %H:%M:%S"))
ACC_new_4080$datetime<- as.POSIXct(strptime(ACC_new_4080$datetime, format="%d.%m.%Y %H:%M:%S"))

#add g to the excel file, because excel is too stupid to do it 
head(ACC_new_4080)

ACC_new_4080$x_g <-  (ACC_new_4080$x-2048)*0.0022*9.81
ACC_new_4080$y_g <-  (ACC_new_4080$y-2048)*0.0022*9.81
ACC_new_4080$z_g <-  (ACC_new_4080$z-2048)*0.0022*9.81


write.csv(ACC_new_4080, file="ACC_4080_g.csv", sep="\t", col.names=T, row.names=F)
#ACC_new_4080$datetime<- ACC_new_4080$datetime1
#merging ACC and GPS
ACC_GPS_merge_4080<-merge(ACC_new_4080, GPS_new_4080, by="datetime")
#in case you only want good quality data, delete all occurences of status B in the GPS data
ACC_GPS_merge_4080<- subset(ACC_GPS_merge_4080,ACC_GPS_merge_4080$status=="A")

#write.csv(ACC_new_4080, file="ACC_4080.csv", sep="\t", col.names=T, row.names=F)

write.csv(ACC_GPS_merge_4080, file="merge_4080.csv", sep="\t", col.names=T, row.names=F)





####4084####

setwd("C:/Users/Jessica/Google Drive/Waldrapp/data")
dat=read.table("tag4084.txt", sep=",", dec=".", fill=T, header=F)
head(dat)

#separate ACC from GPS
GPS <- dat[dat$V1=="GPS",1:22]
ACC <- dat[dat$V1=="ACC",]

secs <- as.character(round(seq(0,1,1/12)[1:12],3))
secs <- ifelse(nchar(secs)==1, paste0(secs, ".000"),
               ifelse(nchar(secs)==3, paste0(secs, "00"),
                      ifelse(nchar(secs)==4, paste0(secs, "0"),
                             ifelse(nchar(secs)==5, secs, NA))))
secs <- substr(secs,2,5)

t1<- Sys.time()
#separate every burst into one row and add the 12th second to it
ACC_new_4084 <- NULL
for(i in 1:length(ACC[,1])) {
  temp <- data.frame(ID=rep(NA,12), time=rep(NA,12), x=rep(NA,12), y=rep(NA,12), z=rep(NA,12))
  temp$x <- as.numeric(t(ACC[i,seq(6,39,3)]))
  temp$y <- as.numeric(t(ACC[i,seq(7,40,3)]))
  temp$z <- as.numeric(t(ACC[i,seq(8,41,3)]))
  temp$datetime <- paste(ACC$V3[i], ACC$V5[i], sep=" ")
  temp$time <- strptime(paste0(ACC$V3[i], " ", ACC$V5[i], secs), format="%d.%m.%Y %H:%M:%OS")
  
  ACC_new_4084 <- rbind(ACC_new_4084, temp)
}

ACC_new_4084$ID <- "4084"

Sys.time()-t1


#deleting unnecessary variables and renaming columns
GPS_new_4084<- GPS[,c(-1,-3,-4,-5)]
names(GPS_new_4084)<- c("ID", "north", "east", "asl",  "type", "status", "GPS clog flag","GPS noise level", "time_fix", "date", "weekday", "time", "battery voltage", "temperature", "speed", "heading", "speed inaccuray estimate", "horizontal inaccuracy estimate")
GPS_new_4084$datetime<-as.POSIXct(strptime(paste(GPS_new_4084$date, GPS_new_4084$time), format="%d.%m.%Y %H:%M:%S"))
GPS_new_4084$ID<-"4084"
#removing data where GPS was not working appropriately
GPS_new_4084<- subset(GPS_new_4084, GPS_new_4084$status=="A")

write.csv(GPS_new_4084, file="GPS_tag4084.csv", sep="\t", col.names=T, row.names=F)
#ACC_new_4084<- read.csv("ACC_4084.csv", sep=",", dec=".", fill=T, header=T)
#changing format of datetime and beforehand extracting only date and time from time variable
ACC_new_4084$datetime<-ACC_new_4084$time
ACC_new_4084$datetime<-substr(ACC_new_4084$datetime, 1, 19)
ACC_new_4084$datetime<- as.POSIXct(strptime(ACC_new_4084$datetime, format="%Y-%m-%d %H:%M:%S"))
ACC_new_4084$datetime<- as.POSIXct(strptime(ACC_new_4084$datetime, format="%d.%m.%Y %H:%M:%S"))

#add g to the excel file, because excel is too stupid to do it 
head(ACC_new_4084)

ACC_new_4084$x_g <-  (ACC_new_4084$x-2048)*0.0022*9.81
ACC_new_4084$y_g <-  (ACC_new_4084$y-2048)*0.0022*9.81
ACC_new_4084$z_g <-  (ACC_new_4084$z-2048)*0.0022*9.81


write.csv(ACC_new_4084, file="ACC_4084_g.csv", sep="\t", col.names=T, row.names=F)
#ACC_new_4084$datetime<- ACC_new_4084$datetime1
#merging ACC and GPS
ACC_GPS_merge_4084<-merge(ACC_new_4084, GPS_new_4084, by="datetime")
#in case you only want good quality data, delete all occurences of status B in the GPS data
ACC_GPS_merge_4084<- subset(ACC_GPS_merge_4084,ACC_GPS_merge_4084$status=="A")

#write.csv(ACC_new_4084, file="ACC_4084.csv", sep="\t", col.names=T, row.names=F)

write.csv(ACC_GPS_merge_4084, file="merge_4084.csv", sep="\t", col.names=T, row.names=F)








####test graphs####
#trying out some plots
#speed and altitude over time
par(mar=c(4,4,3,4))
plot(speed~datetime, data=ACC_GPS_merge_4075, type="l", ylab="speed", bty="n")
par(new=T)
plot(asl~datetime, data=ACC_GPS_merge_4075, type="l",bty="n", xaxt = "n", yaxt = "n",ylab = " ", xlab = "", col="red")
mtext("altitude", side=4, line=2.5, col="red")
axis(4, col="red")

#only plotting parts of the data
GPS_new_subset <- subset(GPS_new_4078, format(datetime,'%H') %in% '09')
GPS_new_subset2 <- subset(GPS_new_subset, format(datetime,'%M') %in% '01')
GPS_new_subsubset<- subset(GPS_new_subset2, format(datetime, "%S") %in% c("01", "02","03", "04", "05", "06", "07", "08", "09", "10"))

par(mfrow=c(1,1))
par(mar=c(4,4,3,4))
plot(speed~datetime, data=GPS_new_subsubset, type="l", ylab="speed", bty="n")
par(new=T)
plot(asl~datetime, data=GPS_new_subsubset, type="l",bty="n", xaxt = "n", yaxt = "n",ylab = " ", xlab = "", col="red")
mtext("altitude", side=4, line=2.5, col="red")
axis(4, col="red")

#plotting accelerometer data against time of day
ACC_GPS_new_subset <- subset(ACC_GPS_merge_4078, format(datetime,'%H') %in% '07')
ACC_GPS_new_subset2 <- subset(ACC_GPS_new_subset, format(datetime,'%M') %in% '04')
ACC_GPS_new_subsubset<- subset(ACC_GPS_new_subset2, format(datetime, "%S") %in% c("01", "02","03", "04", "05", "06", "07", "08", "09", "10"))
par(mfrow=c(2,2))
plot(x~datetime, data=ACC_GPS_new_subsubset)
plot(y~datetime, data=ACC_GPS_new_subsubset, col="blue")
plot(z~datetime, data=ACC_GPS_new_subsubset, col="red")
#and in the same plot as altitude
par(new=T)
plot(asl~datetime, data=ACC_GPS_merge, col="red")

#
par(mfrow=c(1,1))
plot(x~datetime, data=ACC_GPS_merge, type="l", ylab="accx", bty="n")
par(new=T)
plot(speed~datetime, data=ACC_GPS_merge, type="l",bty="n", xaxt = "n", yaxt = "n",ylab = " ", xlab = "", col="red")
mtext("speed", side=4, line=2.5, col="red")
axis(4, col="red")
