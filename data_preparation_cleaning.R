
#####only summing up flight behaviour#####

###4075 flight####
#setwd("C:/Users/Jessica/Google Drive/Waldrapp/data")
#ACC_new_4075=read.csv("ACC_new_4075.csv", sep=",", dec=".",header=T)

ACC_GPS_merge_4075$datetime<- as.POSIXct(strptime(ACC_GPS_merge_4075$datetime, format="%Y-%m-%d %H:%M:%S"))

ACC_GPS_4075_flight1<- subset(ACC_GPS_merge_4075, datetime>=as.POSIXct("2014-08-28 07:39:59"))
ACC_GPS_4075_flight1<- subset(ACC_GPS_4075_flight1, datetime<=as.POSIXct("2014-08-28 10:36:11"))
ACC_GPS_4075_flight2<- subset(ACC_GPS_merge_4075, datetime>=as.POSIXct("2014-08-28 11:50:52"))
ACC_GPS_4075_flight2<- subset(ACC_GPS_4075_flight2, datetime<=as.POSIXct("2014-08-28 13:34:45"))

ACC_GPS_4075_flight<- rbind(ACC_GPS_4075_flight1, ACC_GPS_4075_flight2)

####4078 flight####
#setwd("C:/Users/Jessica/Google Drive/Waldrapp/data")
#ACC_new_4078=read.csv("ACC_new_4078.csv", sep=",", dec=".",header=T)

ACC_GPS_merge_4078$datetime<- as.POSIXct(strptime(ACC_GPS_merge_4078$datetime, format="%Y-%m-%d %H:%M:%S"))

ACC_GPS_4078_flight1<- subset(ACC_GPS_merge_4078, datetime>=as.POSIXct("2014-08-28 07:40:06"))
ACC_GPS_4078_flight1<- subset(ACC_GPS_4078_flight1, datetime<=as.POSIXct("2014-08-28 10:36:10"))
ACC_GPS_4078_flight2<- subset(ACC_GPS_merge_4078, datetime>=as.POSIXct("2014-08-28 11:50:52"))
ACC_GPS_4078_flight2<- subset(ACC_GPS_4078_flight2, datetime<=as.POSIXct("2014-08-28 13:34:45"))

ACC_GPS_4078_flight<- rbind(ACC_GPS_4078_flight1, ACC_GPS_4078_flight2)


####4079 flight####
#setwd("C:/Users/Jessica/Google Drive/Waldrapp/data")
#ACC_new_4079=read.csv("ACC_new_4079.csv", sep=",", dec=".",header=T)

ACC_GPS_merge_4079$datetime<- as.POSIXct(strptime(ACC_GPS_merge_4079$datetime, format="%Y-%m-%d %H:%M:%S"))

ACC_GPS_4079_flight1<- subset(ACC_GPS_merge_4079, datetime>=as.POSIXct("2014-08-28 07:40:04"))
ACC_GPS_4079_flight1<- subset(ACC_GPS_4079_flight1, datetime<=as.POSIXct("2014-08-28 10:36:11"))
ACC_GPS_4079_flight2<- subset(ACC_GPS_merge_4079, datetime>=as.POSIXct("2014-08-28 11:50:53"))
ACC_GPS_4079_flight2<- subset(ACC_GPS_4079_flight2, datetime<=as.POSIXct("2014-08-28 13:32:25"))

ACC_GPS_4079_flight<- rbind(ACC_GPS_4079_flight1, ACC_GPS_4079_flight2)


####4084 flight####
#setwd("C:/Users/Jessica/Google Drive/Waldrapp/data")
#ACC_new_4=read.csv("ACC_new_4084.csv", sep=",", dec=".",header=T)

ACC_GPS_4084_flight<- subset(ACC_GPS_merge_4084, datetime>=as.POSIXct("2014-08-30 06:31:51"))
ACC_GPS_4084_flight<- subset(ACC_GPS_merge_4084, datetime<=as.POSIXct("2014-08-30 09:04:04"))



####4075 clean####
ACC_GPS_merge_4075$datetime<- as.POSIXct(strptime(ACC_GPS_merge_4075$datetime, format="%Y-%m-%d %H:%M:%S"))

ACC_GPS_4075_clean<- subset(ACC_GPS_merge_4075, datetime>=as.POSIXct("2014-08-28 06:00:00"))
ACC_GPS_4075_clean<- subset(ACC_GPS_4075_clean, datetime<=as.POSIXct("2014-08-28 15:55:11"))

plot(z~datetime, data=ACC_GPS_4075_clean, type="l")



####4078 clean####
#setwd("C:/Users/Jessica/Google Drive/Waldrapp/data")
#ACC_new_4078=read.csv("ACC_new_4078.csv", sep=",", dec=".",header=T)

ACC_GPS_merge_4078$datetime<- as.POSIXct(strptime(ACC_GPS_merge_4078$datetime, format="%Y-%m-%d %H:%M:%S"))



#no cleaning needed
ACC_GPS_4078_clean<- subset(ACC_GPS_merge_4078, datetime>=as.POSIXct("2014-08-28 06:00:00"))

plot(z~datetime, data=ACC_GPS_4078_clean, type="l")



####4079 clean####
#setwd("C:/Users/Jessica/Google Drive/Waldrapp/data")
#ACC_new_4079=read.csv("ACC_new_4079.csv", sep=",", dec=".",header=T)

ACC_GPS_merge_4079$datetime<- as.POSIXct(strptime(ACC_GPS_merge_4079$datetime, format="%Y-%m-%d %H:%M:%S"))

#no cleaning needed
ACC_GPS_4079_clean<- ACC_GPS_merge_4079

plot(speed~datetime, data=ACC_GPS_merge_4079, type="l")





###4084 clean####
#setwd("C:/Users/Jessica/Google Drive/Waldrapp/data")
#ACC_new_4=read.csv("ACC_new_4084.csv", sep=",", dec=".",header=T)


ACC_GPS_4084_clean<- subset(ACC_GPS_merge_4084, datetime<=as.POSIXct("2014-08-30 10:27:41"))

#no cleaning needed
plot(speed~datetime, data=ACC_GPS_4084_clean, type="l")
