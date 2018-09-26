setwd("C:/Users/Jessica/Google Drive/Waldrapp/data")
ACC_4075_g=read.csv("ACC_4075_g.csv", sep=",", dec=".", fill=T, header=T)


ACC_4075_g_scale<- ACC_4075_g

ACC_4075_g_scale$x_g<- scale(ACC_4075_g$x_g, center=T, scale=T)
ACC_4075_g_scale$y_g<- scale(ACC_4075_g$y_g, center=T, scale=T)
ACC_4075_g_scale$z_g<- scale(ACC_4075_g$z_g, center=T, scale=T)



ACC_4075_g_scale<-as.data.frame(ACC_4075_g_scale)

head(ACC_4075_g_scale)

summary(ACC_4075_g_scale)

ACC_4075_g_scale$datetime<- ACC_4075_g$datetime

plot(z_g~datetime, data=ACC_4075_g_scale)
