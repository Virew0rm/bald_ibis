library("ggplot2")

####4075####
par(mfrow)
plot(z~datetime, data=ACC_GPS_4075_clean)

summary(ACC_GPS_4075_clean)

ACC_GPS_4075_gr<- subset(ACC_GPS_4075_clean, datetime>=as.POSIXct("2014-08-28 10:00:00"))
ACC_GPS_4075_gr<- subset(ACC_GPS_4075_gr, datetime<=as.POSIXct("2014-08-28 11:50:00"))


plot(z~datetime, data=ACC_GPS_4075_gr, type="l")

setEPS()
postscript("first_start4075_2014_08_28.eps")

ACC_GPS_4075_gr<- subset(ACC_GPS_4075_clean, datetime>=as.POSIXct("2014-08-28 07:39:30"))
ACC_GPS_4075_gr<- subset(ACC_GPS_4075_gr, datetime<=as.POSIXct("2014-08-28 07:40:30"))

ACC_GPS_4075_gr$datetime<- as.factor(ACC_GPS_4075_gr$datetime)

ggplot(data=ACC_GPS_4075_gr, aes(x=datetime, y=z))+
  scale_x_discrete(breaks=c("30","45","00","15","30"), labels=c("07:39:30","07:39:45","07:40:00","07:40:15","07:40:30"))+
  geom_line(aes(color="red"))+
  theme_grey()
  

