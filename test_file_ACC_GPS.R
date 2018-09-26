options(digits.secs=3)

setwd("C:/Users/Jessica/Google Drive/Waldrapp/data")
dat=read.table("test.txt", sep=",", dec=".", fill=T, header=F)
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
ACC_test <- NULL
for(i in 1:length(ACC[,1])) {
  temp   <- data.frame(ID=rep(NA,12), time=rep(NA,12), x=rep(NA,12), y=rep(NA,12), z=rep(NA,12))
  temp$x <- as.numeric(t(ACC[i,seq(6,39,3)]))
  temp$y <- as.numeric(t(ACC[i,seq(7,40,3)]))
  temp$z <- as.numeric(t(ACC[i,seq(8,41,3)]))
  temp$datetime <- paste(ACC$V3[i], ACC$V5[i], sep=" ")
  temp$time <- strptime(paste0(ACC$V3[i], " ", ACC$V5[i], secs), format="%d.%m.%Y %H:%M:%OS")
  
  ACC_test <- rbind(ACC_test, temp)
}
ACC_test$ID <- "test" 

Sys.time()-t1

#deleting unnecessary variables and renaming columns
GPS_test<- GPS[,c(-1,-3,-4,-5)]
names(GPS_test)<- c("ID", "north", "east", "asl",  "type", "status", "GPS clog flag","GPS noise level", "time_fix", "date", "weekday", "time", "battery voltage", "temperature", "speed", "heading", "speed inaccuray estimate", "horizontal inaccuracy estimate")
GPS_test$datetime<-as.POSIXct(strptime(paste(GPS_test$date, GPS_test$time), format="%d.%m.%Y %H:%M:%S"))
GPS_test$ID<-"test"
#removing data where GPS was not working appropriately
GPS_test<- subset(GPS_test, GPS_test$status=="A")

write.table(GPS_test, file="GPS_test.txt", sep="\t", col.names=T, row.names=F)



#changing format of datetime
ACC_test$datetime<- as.POSIXct(strptime(ACC_test$datetime, format="%d.%m.%Y %H:%M:%S"))
#ACC_new_4078$datetime<- ACC_new_4078$datetime1
#merging ACC and GPS
ACC_GPS_test<-merge(ACC_test, GPS_test, by="datetime")

ACC_test_collins<-ACC_test[,c(-1, -6)] 
ACC_test_collins$time<- as.POSIXct(strptime(ACC_test_collins$time, format="%Y-%m-%d %H:%M:%OS"))

write.csv(ACC_test_collins, file="ACC_test_collins.csv", sep="\t", col.names=T, row.names=F)
write.table(ACC_test_collins, file="ACC_test_collins.txt", sep="\t", col.names=T, row.names=F)

ACC_test_collins_1<- read.table("ACC_test_collins.txt", sep=" ", dec=".", fill=T, header=F)

colnames(ACC_test_collins_1)<- c("datetime","x", "y", "z")
ACC_test_collins_1<- ACC_test_collins_1[-1,]
ACC_test_collins_1$datetime<- as.POSIXct(strptime(ACC_test_collins_1$datetime, format="%Y-%m-%d %H:%M:%OS"))
#ACC_test_collins_1$datetime<- paste(ACC_test_collins_1$date,ACC_test_collins_1$time )
#ACC_test_collins_1<- ACC_test_collins_1[, c(-1, -2)]
#ACC_test_collins_1<-ACC_test_collins_1[c(4,1,2,3)]

write.table(ACC_test_collins_1, file="ACC_test_collins_1.txt", sep="\t", col.names=T, row.names=F)

write.csv(ACC_GPS_merge_test, file="merge_test.csv", sep="\t", col.names=T, row.names=F)
