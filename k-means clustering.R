#try k-means algorithm on our data

#data preparation first only using flight phases for our first analyses, after some of the 
#birds stopped in between we have to copy it together

###detecting flight behavioural differences####
####4075 flight####
#setwd("C:/Users/Jessica/Google Drive/Waldrapp/data")
#ACC_new_4075=read.csv("ACC_new_4075.csv", sep=",", dec=".",header=T)

set.seed(20)
clusters <- kmeans(ACC_GPS_4075_flight[,4:6], 2)
ACC_GPS_4075_flight$behaviour <- as.factor(clusters$cluster) 

ACC_GPS_4075_flight$datetime<- as.POSIXct(strptime(ACC_GPS_4075_flight$datetime, format="%Y-%m-%d %H:%M:%S"))


par(mfrow=c(1,1))
#ACC_test_sub<- subset(ACC_test, datetime>=as.POSIXct("2014-08-27 13:09:40"))
#ACC_test_sub<- subset(ACC_test_sub, datetime<=as.POSIXct("2014-08-27 13:10:00"))

plot(z~datetime, data=ACC_GPS_merge_4084, type="l")

library("ggplot2")
ggplot(data = ACC_GPS_4075_flight, aes(x=datetime, y=z)) + geom_line(aes(colour=ACC_GPS_4075_flight$behaviour))


####4078 flight####
#setwd("C:/Users/Jessica/Google Drive/Waldrapp/data")
#ACC_new_4078=read.csv("ACC_new_4078.csv", sep=",", dec=".",header=T)

set.seed(20)
clusters <- kmeans(ACC_GPS_4078_flight[,4:6], 2)
ACC_GPS_4078_flight$behaviour <- as.factor(clusters$cluster) 

#ACC_GPS_4078_flight$datetime<- as.POSIXct(strptime(ACC_GPS_4078_flight$datetime, format="%Y-%m-%d %H:%M:%S"))


#par(mfrow=c(1,1))
#ACC_test_sub<- subset(ACC_test, datetime>=as.POSIXct("2014-08-27 13:09:40"))
#ACC_test_sub<- subset(ACC_test_sub, datetime<=as.POSIXct("2014-08-27 13:10:00"))

#plot(z~datetime, data=ACC_GPS_merge_4078, type="l")

library("ggplot2")
ggplot(data = ACC_GPS_4078_flight, aes(x=datetime, y=z)) + geom_line(aes(colour=ACC_GPS_4078_flight$behaviour))


####4079 flight####
#setwd("C:/Users/Jessica/Google Drive/Waldrapp/data")
#ACC_new_4079=read.csv("ACC_new_4079.csv", sep=",", dec=".",header=T)

set.seed(20)
clusters <- kmeans(ACC_GPS_4079_flight[,4:6], 2)
ACC_GPS_4079_flight$behaviour <- as.factor(clusters$cluster) 

#ACC_GPS_4079_flight$datetime<- as.POSIXct(strptime(ACC_GPS_4079_flight$datetime, format="%Y-%m-%d %H:%M:%S"))


par(mfrow=c(1,1))
#ACC_test_sub<- subset(ACC_test, datetime>=as.POSIXct("2014-08-27 13:09:40"))
#ACC_test_sub<- subset(ACC_test_sub, datetime<=as.POSIXct("2014-08-27 13:10:00"))

plot(z~datetime, data=ACC_GPS_merge_4079, type="l")

library("ggplot2")
ggplot(data = ACC_GPS_4079_flight, aes(x=datetime, y=z)) + geom_line(aes(colour=ACC_GPS_4079_flight$behaviour))

####4084 flight####

set.seed(20)
clusters <- kmeans(ACC_GPS_4084_flight[,4:6], 2)
ACC_GPS_4084_flight$behaviour <- as.factor(clusters$cluster) 

ACC_GPS_4084_flight$datetime<- as.POSIXct(strptime(ACC_GPS_4084_flight$datetime, format="%Y-%m-%d %H:%M:%S"))


par(mfrow=c(1,1))
ACC_test_sub<- subset(ACC_test, datetime>=as.POSIXct("2014-08-27 13:09:40"))
ACC_test_sub<- subset(ACC_test_sub, datetime<=as.POSIXct("2014-08-27 13:10:00"))

plot(z~datetime, data=ACC_GPS_merge_4084, type="l")

library("ggplot2")
ggplot(data = ACC_GPS_4084_flight, aes(x=datetime, y=x)) + geom_line(aes(colour=ACC_GPS_4084_flight$behaviour))



####4075 clean####

#setwd("C:/Users/Jessica/Google Drive/Waldrapp/data")
#ACC_new_4075=read.csv("ACC_new_4075.csv", sep=",", dec=".",header=T)
ACC_GPS_4075_clean<- na.omit(ACC_GPS_4075_clean)
set.seed(20)
clusters <- kmeans(ACC_GPS_4075_clean[,c(27,29,31)], 3)
ACC_GPS_4075_clean$behaviour <- as.factor(clusters$cluster) 

ACC_GPS_4075_clean$datetime<- as.POSIXct(strptime(ACC_GPS_4075_clean$datetime, format="%Y-%m-%d %H:%M:%S"))
ACC_GPS_4075_clean<- subset(ACC_GPS_4075_clean, ACC_GPS_4075_clean$status=="A")

ACC_GPS_4075_clean_s<- subset(ACC_GPS_4075_clean, datetime>=as.POSIXct("2014-08-28 08:29:50"))
ACC_GPS_4075_clean_s<- subset(ACC_GPS_4075_clean_s, datetime<=as.POSIXct("2014-08-28 08:30:00"))
library("ggplot2")
ggplot(data = ACC_GPS_4075_clean_s, aes(x=datetime, y=z)) + geom_point(aes(colour=ACC_GPS_4075_clean_s$behaviour))


library("zoo")
freq<-12
secs<-1

numrow=freq*secs
ACC_GPS_4075_flight$meanX=rollapply(ACC_GPS_4075_flight$x,numrows,mean,fill=NA)
ACC_GPS_4075_flight$SDX=rollapply(ACC_GPS_4075_flight$x,numrows,sd,fill=NA)
ACC_GPS_4075_flight$meanY=rollapply(ACC_GPS_4075_flight$y,numrows,mean,fill=NA)
ACC_GPS_4075_flight$SDY=rollapply(ACC_GPS_4075_flight$y,numrows,sd,fill=NA)
ACC_GPS_4075_flight$meanZ=rollapply(ACC_GPS_4075_flight$z,numrows,mean,fill=NA)
ACC_GPS_4075_flight$SDZ=rollapply(ACC_GPS_4075_flight$z,numrows,sd,fill=NA)

ACC_GPS_4075_clean$meanX=rollapply(ACC_GPS_4075_clean$x,numrows,mean,fill=NA)
ACC_GPS_4075_clean$SDX=rollapply(ACC_GPS_4075_clean$x,numrows,sd,fill=NA)
ACC_GPS_4075_clean$meanY=rollapply(ACC_GPS_4075_clean$y,numrows,mean,fill=NA)
ACC_GPS_4075_clean$SDY=rollapply(ACC_GPS_4075_clean$y,numrows,sd,fill=NA)
ACC_GPS_4075_clean$meanZ=rollapply(ACC_GPS_4075_clean$z,numrows,mean,fill=NA)
ACC_GPS_4075_clean$SDZ=rollapply(ACC_GPS_4075_clean$z,numrows,sd,fill=NA)
