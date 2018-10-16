library("ggplot2")

####4075####

#getting an idea of what the plot looks like
plot(z~datetime, data=ACC_GPS_4075_clean)
par(new=T)
plot(asl~datetime, data=ACC_GPS_4075_clean, type="l", col="blue")

head(ACC_GPS_4075_gr)
plot(z~datetime, data=ACC_GPS_4075_gr, type="l")



#plotting start phase ggplot####
setEPS()
postscript("first_start_4075_2014_08_28.eps")

ACC_GPS_4075_gr<- subset(ACC_GPS_4075_clean, datetime>=as.POSIXct("2014-08-28 07:39:30"))
ACC_GPS_4075_gr<- subset(ACC_GPS_4075_gr, datetime<=as.POSIXct("2014-08-28 07:40:30"))

ACC_GPS_4075_gr$time.y<- as.character(ACC_GPS_4075_gr$time.y)

is.character(ACC_GPS_4075_gr$time.y)

ggplot(data=ACC_GPS_4075_gr, aes(x=time.y, y=z))+
  geom_line(data=ACC_GPS_4075_gr,aes(), color="red")+
   scale_x_discrete(name ="time",breaks=c("07:39:30","07:39:45","07:40:00","07:40:15","07:40:30"),labels=c("07:39:30","07:39:45","07:40:00","07:40:15","07:40:30"))+
  scale_y_continuous(name="z-axis")+ 
  theme(plot.margin = margin(10, 30, 20, 20), panel.background =element_rect(fill="white"), axis.line = element_line(color="black", size = 0.5))+
  annotate("segment", x = 0, xend = 27.5, y = 1, yend = 1, colour = "red")  +
  annotate("text", x =15, y =-27,  colour = "red",label = "sitting", size=3)  +
  annotate("segment", x = 28.5, xend = 38, y = 1, yend = 1, colour = "blue")  +
  annotate("text", x = 33, y =-27,  colour = "blue",label = "startphase", size=3)  +
  annotate("segment", x = 38.5, xend = 60, y = 1, yend = 1, colour = "darkgreen")  +
  annotate("text", x = 49, y =-27,  colour = "darkgreen",label = "flying", size=3) 

dev.off()


#plotting start phase with speed ggplot####
setEPS()
postscript("first_start_with_speed_4075_2014_08_28.eps")

ACC_GPS_4075_gr<- subset(ACC_GPS_4075_clean, datetime>=as.POSIXct("2014-08-28 07:39:30"))
ACC_GPS_4075_gr<- subset(ACC_GPS_4075_gr, datetime<=as.POSIXct("2014-08-28 07:40:30"))

ACC_GPS_4075_gr$time.y<- as.character(ACC_GPS_4075_gr$time.y)

is.character(ACC_GPS_4075_gr$time.y)

ggplot(data=ACC_GPS_4075_gr, aes(x=time.y, y=z))+
  geom_line(data=ACC_GPS_4075_gr,aes(), color="red")+
  geom_point(aes(x=ACC_GPS_4075_gr$time.y,y=ACC_GPS_4075_gr$speed*100), color="blue")+
  scale_x_discrete(name ="time",breaks=c("07:39:30","07:39:45","07:40:00","07:40:15","07:40:30"),labels=c("07:39:30","07:39:45","07:40:00","07:40:15","07:40:30"))+
  scale_y_continuous(name="z-axis", sec.axis = sec_axis(~./100, name = "asl"))+ 
  theme(plot.margin = margin(10, 30, 20, 20), panel.background =element_rect(fill="white"), axis.line = element_line(color="black", size = 0.5))+
  annotate("segment", x = 0, xend = 27.5, y = 1, yend = 1, colour = "red")  +
  annotate("text", x =15, y =-27,  colour = "red",label = "sitting", size=3)  +
  annotate("segment", x = 28.5, xend = 38, y = 1, yend = 1, colour = "blue")  +
annotate("text", x = 33, y =-27,  colour = "blue",label = "startphase", size=3)  +
annotate("segment", x = 38.5, xend = 60, y = 1, yend = 1, colour = "darkgreen")  +
annotate("text", x = 49, y =-27,  colour = "darkgreen",label = "flying", size=3) 

dev.off()


#plotting landing phase ggplot with speed####
setEPS()
postscript("landing_with_speed_4075_2014_08_28.eps")


ACC_GPS_4075_gr<- subset(ACC_GPS_4075_clean, datetime>=as.POSIXct("2014-08-28 10:27:00"))
ACC_GPS_4075_gr<- subset(ACC_GPS_4075_gr, datetime<=as.POSIXct("2014-08-28 10:34:00"))

ACC_GPS_4075_gr$time.y<- as.character(ACC_GPS_4075_gr$time.y)

is.character(ACC_GPS_4075_gr$time.y)


ggplot(data=ACC_GPS_4075_gr, aes(x=time.y, y=z))+
  geom_line(aes())+
geom_point(aes(x=ACC_GPS_4075_gr$time.y,y=ACC_GPS_4075_gr$speed*100), color="blue")+
  scale_x_discrete(name ="time",breaks=c("10:27:00","10:28:00","10:29:00","10:30:00","10:31:00","10:32:00","10:33:00","10:34:00"),labels=c("10:27:00","10:28:00","10:29:00","10:30:00","10:31:00","10:32:00","10:33:00","10:34:00"))+
  scale_y_continuous(name="z-axis", sec.axis = sec_axis(~./100, name = "speed", breaks=c(0,10,20), labels=c("0", "10", "20")))+
    theme(plot.margin = margin(10, 30, 20, 20), panel.background =element_rect(fill="white"), axis.line = element_line(color="black", size = 0.5))+
  annotate("segment", x = 0, xend = 120, y = 550, yend =550, colour = "darkgreen")  +
  annotate("text", x =70, y =500,  colour = "darkgreen",label = "flying", size=3)  +
  
  annotate("segment", x = 122, xend = 300, y = 550, yend = 550, colour = "blue")  +
  annotate("text", x = 200, y =500,  colour = "blue",label = "landing", size=3)  +
  
  annotate("segment", x = 302, xend = 350, y = 550, yend = 550, colour = "purple")  +
  annotate("text", x = 330, y =500,  colour = "purple",label = "short flight/jump", size=3) +
  
  annotate("segment", x =352, xend = 400, y = 550, yend = 550, colour = "red") +
  annotate("text", x = 380, y =500,  colour = "red",label = "sitting", size=3) 

dev.off()

#plotting landing phase ggplot with asl####
setEPS()
postscript("landing_with_asl_4075_2014_08_28.eps")


ACC_GPS_4075_gr<- subset(ACC_GPS_4075_clean, datetime>=as.POSIXct("2014-08-28 10:27:00"))
ACC_GPS_4075_gr<- subset(ACC_GPS_4075_gr, datetime<=as.POSIXct("2014-08-28 10:34:00"))

ACC_GPS_4075_gr$time.y<- as.character(ACC_GPS_4075_gr$time.y)

is.character(ACC_GPS_4075_gr$time.y)


ggplot(data=ACC_GPS_4075_gr, aes(x=time.y, y=z))+
  geom_line(aes())+
  geom_point(aes(x=ACC_GPS_4075_gr$time.y,y=ACC_GPS_4075_gr$asl*10), color="blue")+
  scale_x_discrete(name ="time",breaks=c("10:27:00","10:28:00","10:29:00","10:30:00","10:31:00","10:32:00","10:33:00","10:34:00"),labels=c("10:27:00","10:28:00","10:29:00","10:30:00","10:31:00","10:32:00","10:33:00","10:34:00"))+
  scale_y_continuous(name="z-axis", sec.axis = sec_axis(~./10, name = "asl", breaks=c(0,50,100,150,200,250,300), labels=c("0", "50", "100","150", "200", "250", "300")))+
  theme(plot.margin = margin(10, 30, 20, 20), panel.background =element_rect(fill="white"), axis.line = element_line(color="black", size = 0.5))+
  annotate("segment", x = 0, xend = 120, y = 100, yend =100, colour = "darkgreen")  +
  annotate("text", x =70, y =0,  colour = "darkgreen",label = "flying", size=3)  +
  
  annotate("segment", x = 122, xend = 300, y = 100, yend = 100, colour = "blue")  +
  annotate("text", x = 200, y =0,  colour = "blue",label = "landing", size=3)  +
  
  annotate("segment", x = 302, xend = 350, y = 100, yend = 100, colour = "purple")  +
  annotate("text", x = 330, y =0,  colour = "purple",label = "short flight/jump", size=3) +
  
  annotate("segment", x =352, xend = 400, y = 100, yend = 100, colour = "red") +
  annotate("text", x = 380, y =0,  colour = "red",label = "sitting", size=3) 

dev.off()
#4078##### not finished yet######

plot(z~datetime, data=ACC_GPS_4078_clean)

summary(ACC_GPS_4078_clean)

ACC_GPS_4078_gr<- subset(ACC_GPS_4078_clean, datetime>=as.POSIXct("2014-08-28 10:27:00"))
ACC_GPS_4078_gr<- subset(ACC_GPS_4078_gr, datetime<=as.POSIXct("2014-08-28 10:34:00"))


plot(z~datetime, data=ACC_GPS_4078_gr, type="l")


#plotting start phase ggplot####
setEPS()
postscript("first_start4078_2014_08_28.eps")

ACC_GPS_4078_gr<- subset(ACC_GPS_4078_clean, datetime>=as.POSIXct("2014-08-28 07:39:30"))
ACC_GPS_4078_gr<- subset(ACC_GPS_4078_gr, datetime<=as.POSIXct("2014-08-28 07:40:30"))




ACC_GPS_4078_gr$time.y<- as.character(ACC_GPS_4078_gr$time.y)

is.character(ACC_GPS_4078_gr$time.y)

ggplot(data=ACC_GPS_4078_gr, aes(x=time.y, y=z))+
  scale_x_discrete(name ="time",breaks=c("07:39:30","07:39:45","07:40:00","07:40:15","07:40:30"),labels=c("07:39:30","07:39:45","07:40:00","07:40:15","07:40:30"))+
  scale_y_continuous(name="z-axis")+
  geom_line(aes())+
  theme(plot.margin = margin(10, 30, 20, 20), panel.background =element_rect(fill="white"), axis.line = element_line(color="black", size = 0.5))+
  annotate("segment", x = 0, xend = 27.5, y = 1, yend = 1, colour = "red")  +
  annotate("text", x =15, y =-27,  colour = "red",label = "sitting", size=3)  +
  annotate("segment", x = 28.5, xend = 38, y = 1, yend = 1, colour = "blue")  +
  annotate("text", x = 33, y =-27,  colour = "blue",label = "startphase", size=3)  +
  annotate("segment", x = 38.5, xend = 60, y = 1, yend = 1, colour = "darkgreen")  +
  annotate("text", x = 49, y =-27,  colour = "darkgreen",label = "flying", size=3) 

dev.off()

###first want to try a graph on a different animal




#4084####


plot(z~datetime, data=ACC_GPS_4084_clean)

summary(ACC_GPS_4084_clean)

ACC_GPS_4084_gr<- subset(ACC_GPS_4084_clean, datetime>=as.POSIXct("2014-08-28 10:27:00"))
ACC_GPS_4084_gr<- subset(ACC_GPS_4084_gr, datetime<=as.POSIXct("2014-08-28 10:34:00"))


plot(z~datetime, data=ACC_GPS_4084_gr, type="l")

#plotting start phase ggplot####
setEPS()
postscript("first_start4084_2014_08_30.eps")

ACC_GPS_4084_gr<- subset(ACC_GPS_4084_clean, datetime>=as.POSIXct("2014-08-30 06:31:30"))
ACC_GPS_4084_gr<- subset(ACC_GPS_4084_gr, datetime<=as.POSIXct("2014-08-30 06:32:30"))

ACC_GPS_4084_gr$time.y<- as.character(ACC_GPS_4084_gr$time.y)

is.character(ACC_GPS_4084_gr$time.y)

ggplot(data=ACC_GPS_4084_gr, aes(x=time.y, y=z))+
  scale_x_discrete(name ="time",breaks=c("06:31:30","06:31:45","06:32:00","06:32:15","06:32:30"),labels=c("06:31:30","06:31:45","06:32:00","06:32:15","06:32:30"))+
  scale_y_continuous(name="z-axis")+
  geom_line(aes())+
  theme(plot.margin = margin(10, 30, 20, 20), panel.background =element_rect(fill="white"), axis.line = element_line(color="black", size = 0.5))+
  annotate("segment", x = 0, xend = 20.5, y = 1, yend = 1, colour = "red")  +
  annotate("text", x =12, y =-27,  colour = "red",label = "sitting", size=3)  +
  annotate("segment", x = 21.5, xend = 29, y = 1, yend = 1, colour = "blue")  +
  annotate("text", x = 25, y =-27,  colour = "blue",label = "startphase", size=3)  +
  annotate("segment", x = 29.5, xend = 62, y = 1, yend = 1, colour = "darkgreen")  +
  annotate("text", x = 45, y =-27,  colour = "darkgreen",label = "flying", size=3) 

dev.off()


#plotting landing phase ggplot####
setEPS()
postscript("landing_4084_2014_08_30.eps")


ACC_GPS_4084_gr<- subset(ACC_GPS_4084_clean, datetime>=as.POSIXct("2014-08-30 10:28:00"))
ACC_GPS_4084_gr<- subset(ACC_GPS_4084_gr, datetime<=as.POSIXct("2014-08-30 10:34:00"))
plot(z~datetime, data=ACC_GPS_4084_gr, type="l")

ACC_GPS_4084_gr$time.y<- as.character(ACC_GPS_4075_gr$time.y)

is.character(ACC_GPS_4084_gr$time.y)


ggplot(data=ACC_GPS_4075_gr, aes(x=time.y, y=z))+
  scale_x_discrete(name ="time",breaks=c("10:27:00","10:28:00","10:29:00","10:30:00","10:31:00","10:32:00","10:33:00","10:34:00"),labels=c("10:27:00","10:28:00","10:29:00","10:30:00","10:31:00","10:32:00","10:33:00","10:34:00"))+
  scale_y_continuous(name="z-axis")+
  geom_line(aes())+
  theme(plot.margin = margin(10, 30, 20, 20), panel.background =element_rect(fill="white"), axis.line = element_line(color="black", size = 0.5))+
  annotate("segment", x = 0, xend = 120, y = 550, yend =550, colour = "darkgreen")  +
  annotate("text", x =70, y =500,  colour = "darkgreen",label = "flying", size=3)  +
  
  annotate("segment", x = 122, xend = 300, y = 550, yend = 550, colour = "blue")  +
  annotate("text", x = 200, y =500,  colour = "blue",label = "landing", size=3)  +
  
  annotate("segment", x = 302, xend = 350, y = 550, yend = 550, colour = "purple")  +
  annotate("text", x = 330, y =500,  colour = "purple",label = "short flight/jump", size=3) +
  
  annotate("segment", x =352, xend = 400, y = 550, yend = 550, colour = "red") +
  annotate("text", x = 380, y =500,  colour = "red",label = "sitting", size=3) 

dev.off()



