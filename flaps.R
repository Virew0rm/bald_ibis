#either load data from file or existing dataframe
setwd("C:/Users/Jessica/Google Drive/Waldrapp/data")
#####get a fake test data set
flaps<- read.csv("flaps_0110_3110_2018.csv")

head(flaps)
summary(flaps)
str(flaps)

flaps<- flaps[-23]

flaps$datetime<- as.POSIXct(strptime(flaps$UTC_datetime, format="%Y-%m-%d %H:%M:%S"))

plot(acc_z~datetime)