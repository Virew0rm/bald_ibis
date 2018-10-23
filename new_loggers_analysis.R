#either load data from file or existing dataframe
setwd("C:/Users/Jessica/Google Drive/Waldrapp/data")
dat<- read.csv("test_nl.csv")

#checking the test dataset and cutting off all movement
head(dat)

plot(dat$Z~dat$ms2)

dat<- subset(dat, dat$ms2>60000& dat$ms2<1240000)

mean(dat$Z)
sd(dat$Z)

apply(dat, 2, mean)
apply(dat, 2, sd)

#getting a real dataset 

head(dat_ld)

plot(Z~xms2, data=dat_ld)

#main file to determine the sitting phases
dat_ldn<- subset(dat_ld, dat_ld$xms2>=as.POSIXct("2018-10-22 02:12:30")& dat_ld$xms2<=as.POSIXct("2018-10-22 02:16:00"))

dat_ldn1<- subset(dat_ld, dat_ld$xms2>=as.POSIXct("2018-10-22 02:12:30")& dat_ld$xms2<=as.POSIXct("2018-10-22 02:16:00"))



head(dat_ldn)

plot(Z~xms2, data=dat_ldn)

apply(dat_ldn, 2, mean, na.rm=T)
apply(dat_ldn, 2, sd)

mean(dat_ldn$Z)

ACC_GPS_4075_tt<- subset(ACC_GPS_4075_clean, datetime>=as.POSIXct("2014-08-28 06:35:00"))
ACC_GPS_4075_tt<- subset(ACC_GPS_4075_tt, datetime<=as.POSIXct("2014-08-28 06:45:00"))

plot(z~datetime, data=ACC_GPS_4075_tt)


####comparing the two values with a t.test

t.test(dat_ldn$Z, dat$Z)
var.test(dat$Z, dat_ldn$Z)