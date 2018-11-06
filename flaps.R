#either load data from file or existing dataframe
setwd("C:/Users/Jessica/Google Drive/Waldrapp/data")
#####get a fake test data set
flaps<- read.csv("flaps_0101_2010_2018.csv")

head(flaps)
summary(flaps)
str(flaps)

flaps<- flaps[-23]

flaps$datetime<- as.POSIXct(strptime(flaps$UTC_datetime, format="%Y-%m-%d %H:%M:%S"))

flaps<- subset(flaps, flaps$datetime>=as.POSIXct("2018-07-01 00:00:00")& flaps$datetime<=as.POSIXct("2018-10-19 12:00:00"))




plot(acc_z~datetime, data=flaps, type="l")
plot(solar_I_mA~datetime, data=flaps, type="l")
