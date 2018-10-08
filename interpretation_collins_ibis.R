##### This file calculates metrics of accelerometry, plots histograms of these metrics
###### and makes behavioural assignments based on these histograms. 

# to reset R removing assigned function
#rm(list=ls())
library("zoo")
options(digits.secs=3)
getwd()


#Input data should consist of one csv file of time, X,Y,Z, where X is assumed to be acceleration
# in the surge channel, Y is acceleration in sway, and Z is acceleration in heave.

Data<-read.csv(file.choose(),stringsAsFactors=FALSE)
head(Data)
Data<-Data[,c(-1,-6)] 

Data<-Data[,c(-1,-3,-4,-5, -6)] 
tail(Data)
str(Data)

names(Data)<- c("time", "x", "y", "z")



freq<-12 #The Frequency of accelerometry data
secs<-1 # the number of seconds over which to calculate the desired metrics.The manuscript says to use 1 second intervals,
#but to capture gliding flight as well I've found that a longer period is needed. I will update this on the ms! 

numrows<-freq*secs # the number of rows required to calculate metrics over the chosen period. 

##Calculate rolling means over a set period.

Data$meanX=rollapply(Data$x,numrows,mean,fill=NA)#25 = number of cells to average across, change to desired time considering sampling frequency.
Data$meancent=rollapply(Data$x,numrows,mean,fill=NA)#25 = number of cells to average across, change to desired time considering sampling frequency.


Data$meanY=rollapply(Data$y,numrows,mean,fill=NA)#25 = number of cells to average across, change to desired time considering sampling frequency. 
Data$meanZ=rollapply(Data$z,numrows,mean,fill=NA)#25 = number of cells to average across, change to desired time considering sampling frequency. 

#Calculate rollingstandard deviation over a set period.

Data$SDX=rollapply(Data$x,numrows,sd,fill=NA)#25 = number of cells to average across, change to desired time considering sampling frequency.
Data$SDY=rollapply(Data$y,numrows,sd,fill=NA)#25 = number of cells to average across, change to desired time considering sampling frequency. 
Data$SDZ=rollapply(Data$z,numrows,sd,fill=NA)#25 = number of cells to average across, change to desired time considering sampling frequency. 



##Calculate pitch

Data$pitch<-atan((Data$x/(sqrt((Data$y*Data$y)+(Data$z*Data$z)))))*(180/pi);


#Calculate roll

Data$roll<-atan((Data$y/(sqrt((Data$x*Data$x)+(Data$z*Data$z)))))*(180/pi);



#####Calculate ODBA and VeDBA####

Data$RunningAx=rollapply(Data$x,numrows,mean,fill=NA) 
Data$RunningAy=rollapply(Data$y,numrows,mean,fill=NA)
Data$RunningAz=rollapply(Data$z,numrows,mean,fill=NA)


####Calculates DBA for each axis. 



Data$StaticX<-Data$x-Data$RunningAx
Data$StaticY<-Data$y-Data$RunningAy
Data$StaticZ<-Data$z-Data$RunningAz


Data$ODBA<-abs(Data$StaticX)+abs(Data$StaticY)+abs(Data$StaticZ)
Data$Vedba<-sqrt((Data$StaticX^2)+(Data$StaticY^2)+(Data$StaticZ^2))



Data<-subset(Data, select=c(time,x,y,z,meanX,meanY,meanZ,SDX,SDY,SDZ,pitch,roll,ODBA,Vedba))


head(Data)
####Subset to 1-second intervals
Data$NewTime2<-as.POSIXct(Data$time,format ="%d.%m.%Y %H:%M:%S")

Data$NewTime2<-as.POSIXct(Data$time,format ="%Y-%m-%d %H:%M:%S")
library("plyr")


Data2<-ddply(Data, .(NewTime2), function(x) x[6,])


Data2<-Data2[-1,] # removes first row which is likely to be NA, depending on interval of metric calculation.This will need to be run a few times until there are no NAs, depending on the time used to average/ calculate SD over.
head(Data2)
Data2<-Data2[-nrow(Data2),]
tail(Data2)

#cut the data so that only flying behaviour is left
Data2<- subset(Data2, NewTime2>=as.POSIXct("2014-08-28 07:39:59"))
Data2<- subset(Data2, NewTime2<=as.POSIXct("2014-08-28 13:34:45"))

#####Plot histograms of the calculated metrics. 
##Change bin sizes and range as shown with the SDZ metric to fully explore the data. 

maxSDZ<-max(Data2$SDZ)+0.1 # have to add a small amount for the breaks to work. 
minSDZ<-min(Data2$SDZ)-0.1
interval<-0.01 # change depending on bin size required. 
hist(Data2$SDZ,breaks=seq(minSDZ,maxSDZ, by=interval),)
str(Data2$SDZ)


print(sum(is.na(Data2$pitch)))
print(sum(is.na(Data2$SDZ)))

maxSDY<-max(Data2$SDY)+0.1 # have to add a small amount for the breaks to work. 
minSDY<-min(Data2$SDY)-0.1
interval<-0.1 # change depending on bin size required. 
hist(Data2$SDY,breaks=seq(minSDY,maxSDY, by=interval),)
str(Data2$SDY)

maxSDX<-max(Data2$SDX)+0.1 # have to add a small amount for the breaks to work. 
minSDX<-min(Data2$SDX)-0.1
interval<-0.1 # change depending on bin size required. 
hist(Data2$SDX,breaks=seq(minSDX,maxSDX, by=interval),)
str(Data2$SDX)

Data2$pitch<- na.locf(Data2$pitch,fromLast=FALSE)
Data2$SDZ<- na.locf(Data2$SDZ,fromLast=FALSE)

head(Data2)


hist(Data2$meanX)
hist(Data2$meanY)
hist(Data2$meanZ)
hist(Data2$SDX)
hist(Data2$SDY)
hist(Data2$SDZ)
hist(Data2$pitch)
hist(Data2$roll)
hist(Data2$ODBA)
hist(Data2$Vedba)

###### Identify peaks in the data and calculate the inter-peak frequency minimum by taking the metric value
###corresponding to the minimum frequency count between peaks. 

SDZhist<-hist(Data2$SDZ,breaks=seq(minSDZ,maxSDZ, by=interval),)
SDZhist

mids<-SDZhist$mids
counts<-SDZhist$counts

SDZhistdata<-data.frame(mids,counts) # make a dataframe from the histogram data
SDZhistdata

#Identify the Inter-peak frequency minimum value(s) In this example the IPFM is calculated between a value above 0.1 and below 0.5
#change this depending on where the peaks fall in your dataset. 
#If this returns numerous values, take the average.
firstpeak<-0.53 # set a value corresponding to the  value with the highest frequency at the first peak. This does not have to be accurate. 
secondpeak<-6.33 # set a value corresponding to the  value with the highest frequency at the second peak. This does not have to be accurate. 

SDZhistdata<-subset(SDZhistdata,mids>firstpeak&mids<secondpeak)

IPFM<-SDZhistdata[which(SDZhistdata$counts==min(SDZhistdata$counts)),1]
IPFM

##Assign behaviours based on the IPFM. Use numbers to indicate discrete behaviours. In this example, 1 indicates flight,
##2 indicates being on water, and 3 indicates being on land. 



for(i in 1:length(Data2))
{
  Data2$behaviour[Data2$SDZ>IPFM]<-1
}

#Assign a number to data not falling within the identified threshold. 
Data2[is.na(Data2)] <- 2

#The next round of behavioural assignment can then take place. e.g.after plotting histograms with 
#data belonging to the first behavioural assignment having been removed and the IPFM for the second
#metric having been identified


# The example below incorporates the above round of assignment as well as another set of arguments which depends on
#the IPFM found from the metric 'pitch'

head(Data2)

######For pitch
minpitch<-min(Data2$pitch)-1
minpitch
maxpitch<-max(Data2$pitch)+1
maxpitch
interval<-1
hist(Data2$pitch[Data$SDZ>IPFM],breaks=seq(minpitch,maxpitch, by=interval))
pitchhist<-hist(Data2$pitch,breaks=seq(minpitch,maxpitch, by=interval))  
pitchhist

mids<-pitchhist$mids
counts<-pitchhist$counts

pitchhistdata<-data.frame(mids,counts) # make a dataframe from the histogram data
pitchhistdata

#If this returns numerous values, take the average.
firstpeak<- -1.9 # set a value corresponding to the  value with the highest frequency at the first peak. This does not have to be accurate. 
secondpeak<- NA # set a value corresponding to the  value with the highest frequency at the second peak. This does not have to be accurate. 


pitchhistdata<-subset(pitchhistdata,mids>firstpeak&mids<secondpeak)

IPFMpitch<-pitchhistdata[which(pitchhistdata$counts==min(pitchhistdata$counts)),1]
IPFMpitch



for(i in 1:length(Data2))
{
  Data2$behaviour[Data2$SDZ>IPFM]<-1 # 1 =flight
  Data2$behaviour[Data2$SDZ<IPFM & Data2$pitch<=IPFMpitch]<-3 # 2 = on water
  Data2$behaviour[Data2$SDZ<IPFM & Data2$pitch>IPFMpitch]<-4 # 3 = on land
}


#Assign a number to data not falling within the identified threshold. 
Data2[is.na(Data2)] <- 5




####The following is the script used for averaging pitch between flight periods in the manuscript. 
head(Data2)
Data2[1,16]<-1

rval<-as.numeric(row.names(Data2))
Data2$b0row<-rval

for(i in 1:length(Data2)){
  
  Data2$b0row[Data2$behaviour==2]<-NA
  Data2$b0row[Data2$behaviour==3]<-NA
  
}

Data2$b0na<- na.locf(Data2$b0row,fromLast=FALSE)

for(i in 1:length(Data2)){
  
  Data2$b0na[Data2$behaviour==1]<-NA
  Data2$b0na[Data2$behaviour==4]<-NA
  
}

library(plyr) 

avg<-ddply(Data2, .(b0na), transform, pitchavg=mean(pitch)) 
avg<-avg[order(avg$NewTime),]
Data2$pitchavg<-avg$pitchavg

tail(Data2)
Data2 <- subset(Data2, select = -c(NewTime2,b0row,b0na)) 

#####Behaviours can then be reassigned using the same process as above, but with assignments now dependent on the average pitch values. 
for(i in 1:length(Data2))
{
  Data2$behaviour[Data2$SDZ>IPFM]<-1
  Data2$behaviour[Data2$SDZ<IPFM & Data2$pitchavg<=IPFMpitch]<-2
  Data2$behaviour[Data2$SDZ<IPFM & Data2$pitchavg>IPFMpitch]<-3 
}


head(Data2)

Data<-subset(Data2, select=c(time, behaviour,pitch,SDZ))
head(Data)
tail(Data)
write.csv(Data,"Accelerometerwithbehaviours_4075.csv",row.names=FALSE)

Data2$datetime<-Data2$NewTime2
Data2_merge<-merge(Data2, ACC_GPS_merge_4075, by="datetime")
Data2_merge<- subset(Data2_merge, NewTime2>=as.POSIXct("2014-08-28 07:39:59"))
Data2_merge<- subset(Data2_merge, NewTime2<=as.POSIXct("2014-08-28 13:34:45"))
Data2_merge$behaviour<-as.factor(Data2_merge$behaviour)
par(mfrow=c(3,1))

plot(z.x~NewTime2, data=Data2_merge,col = Data2_merge$behaviour, type="l")
plot(asl~NewTime2, data=Data2_merge,col = Data2_merge$behaviour, type="l")
plot(speed~NewTime2, data=Data2_merge,col = Data2_merge$behaviour, type="l")
plot(SDZ~NewTime2, data=Data2_merge,col = Data2_merge$behaviour, type="l")
legend("topright", legend= unique(Data2_merge$behaviour), col=unique(Data2_merge$behaviour), pch=21)

#try k-means algorithm on our data
setwd("C:/Users/Jessica/Google Drive/Waldrapp/data")

ACC_test=read.csv("ACC_test.csv", sep=",", dec=".",header=T)
set.seed(20)
clusters <- kmeans(ACC_GPS_merge_4075[,4:6], 2)
ACC_GPS_merge_4075$behaviour <- as.factor(clusters$cluster) 

ACC_test$datetime<- as.POSIXct(strptime(ACC_test$datetime, format="%Y-%m-%d %H:%M:%S"))


par(mfrow=c(1,1))
ACC_test_sub<- subset(ACC_test, datetime>=as.POSIXct("2014-08-27 13:09:40"))
ACC_test_sub<- subset(ACC_test_sub, datetime<=as.POSIXct("2014-08-27 13:10:00"))

plot(asl~datetime, data=ACC_GPS_merge_4075, col=ACC_GPS_merge_4075$behaviour, type="l")

library("ggplot2")
ggplot(data = ACC_GPS_merge_4075, aes(x=datetime, y=y)) + geom_line(aes(colour=ACC_GPS_merge_4075$behaviour))


#####comparing the axes with each other in one graph#####

setEPS()
postscript("4075_axis_comparison.eps")
par(mfrow=c(3,1))
plot(x.x~datetime, data=Data2_merge,col="red", type="l")
plot(y.x~datetime, data=Data2_merge,col="darkblue", type="l")
plot(z.x~datetime, data=Data2_merge, col="darkgreen", type="l")

dev.off()


#####trying to put ODBA in the mix as a second option to look for significant differences#####

Data2$datetime<-Data2$NewTime2
Data2_merge<-merge(Data2, ACC_GPS_merge_4075, by="datetime")
Data2_merge<- subset(Data2_merge, NewTime2>=as.POSIXct("2014-08-28 07:39:59"))
Data2_merge<- subset(Data2_merge, NewTime2<=as.POSIXct("2014-08-28 13:34:45"))

par(mfrow=c(3,1))
plot(ODBA~datetime, data=Data2_merge)
plot(z.x~datetime, data=Data2_merge)
plot(asl~datetime, data=Data2_merge)
