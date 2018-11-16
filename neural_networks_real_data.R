####first dataset####
#either load data from file or existing dataframe
setwd("C:/Users/cornilsj/Dropbox/jessi_und_franz/r/bald ibis")
#####get a fake test data set
dat<- read.csv("test_nl.csv")

#checking the test dataset and cutting off all movement
head(dat)

plot(dat$Z~dat$ms2)

dat<- subset(dat, dat$ms2>60000& dat$ms2<1240000)

dat$xms2<-format(as.POSIXct("2018-10-10")+dat$ms2/1000, "%H:%M:%S")
dat$xms2<-as.POSIXct(strptime(dat$xms2, format="%H:%M:%S"))

head(dat)
dat$behaviour<- "lying"

mean(dat$Z)
sd(dat_ldn4$Z)

apply(dat, 2, mean)
apply(dat, 2, sd)

var.test(dat_ldn5$Z, dat$Z)


#####second dataset: getting real data#### 

head(dat_ld)
plot(Z~xms2, data=dat_ld, type="l")

#convert ms into hours for a more intuitive graph
dat_ld$xms2<-format(as.POSIXct(Sys.Date(), tz="GMT")+dat_ld$ms2/1000, "%H:%M:%S")
dat_ld$xms2<- as.POSIXct(strptime(dat_ld$xms2, format="%H:%M:%S"))
#make sure that there are not NA´s
dat_ld<- na.omit(dat_ld)

#main file to determine active phases
dat_ldn<-na.omit( subset(dat_ld, dat_ld$xms2>=as.POSIXct("2018-11-16 00:00:00")& dat_ld$xms2<=as.POSIXct("2018-11-16 06:50:00")))
plot(Z~xms2, data=dat_ldn)

#"sitting phases" in the datafile
dat_ldn1<- subset(dat_ld, dat_ld$xms2>=as.POSIXct("2018-11-16 01:12:30")& dat_ld$xms2<=as.POSIXct("2018-11-16 01:16:00"))
dat_ldn2<- subset(dat_ld, dat_ld$xms2>=as.POSIXct("2018-11-16 02:08:46")& dat_ld$xms2<=as.POSIXct("2018-11-16 02:10:55"))
dat_ldn3<- subset(dat_ld, dat_ld$xms2>=as.POSIXct("2018-11-16 02:11:20")& dat_ld$xms2<=as.POSIXct("2018-11-16 02:15:25"))
dat_ldn4<- subset(dat_ld, dat_ld$xms2>=as.POSIXct("2018-11-16 02:16:45")& dat_ld$xms2<=as.POSIXct("2018-11-16 02:18:34"))
dat_ldn5<- subset(dat_ld, dat_ld$xms2>=as.POSIXct("2018-11-16 02:21:45")& dat_ld$xms2<=as.POSIXct("2018-11-16 02:22:20"))
#binding them all together
dat_sit<-rbind (dat_ldn1, dat_ldn2,dat_ldn3,dat_ldn4, dat_ldn5)
plot(Z~xms2, data=dat_sit)

#extracting only "flying" phases in the datafile
dat_fly1<- subset(dat_ld, dat_ld$xms2>=as.POSIXct("2018-11-16 01:00:00")& dat_ld$xms2<=as.POSIXct("2018-11-16 01:12:30"))
dat_fly2<- subset(dat_ld, dat_ld$xms2>=as.POSIXct("2018-11-16 01:16:00")& dat_ld$xms2<=as.POSIXct("2018-11-16 02:08:46"))
dat_fly3<- subset(dat_ld, dat_ld$xms2>=as.POSIXct("2018-11-16 02:10:55")& dat_ld$xms2<=as.POSIXct("2018-11-16 02:11:20"))
dat_fly4<- subset(dat_ld, dat_ld$xms2>=as.POSIXct("2018-11-16 02:15:25")& dat_ld$xms2<=as.POSIXct("2018-11-16 02:16:45"))
dat_fly5<- subset(dat_ld, dat_ld$xms2>=as.POSIXct("2018-11-16 02:18:34")& dat_ld$xms2<=as.POSIXct("2018-11-16 02:21:45"))
dat_fly6<- subset(dat_ld, dat_ld$xms2>=as.POSIXct("2018-11-16 02:22:20")& dat_ld$xms2<=as.POSIXct("2018-11-16 05:50:00"))

dat_fly<-rbind (dat_fly1, dat_fly2,dat_fly3,dat_fly4, dat_fly5, dat_fly6)
plot(Z~xms2, data=dat_fly, type="l")


#create a new column with the behaviour
dat_sit$behaviour<- "sitting"
dat_fly$behaviour<- "flying"




####getting data from experiments####
dat_wiese<- dat_ld

dat_wiese$xms2<-format(as.POSIXct(Sys.Date(), tz="GMT")+dat_wiese$ms2/1000, "%H:%M:%S")
dat_wiese$xms2<- as.POSIXct(strptime(dat_wiese$xms2, format="%H:%M:%S"))

#dead
dat_wiese1<-subset(dat_wiese, dat_wiese$xms2>=as.POSIXct("2018-11-15 01:05:42")& dat_wiese$xms2<=as.POSIXct("2018-11-15 01:11:22"))
dat_wiese2<-subset(dat_wiese, dat_wiese$xms2>=as.POSIXct("2018-11-15 01:11:29")& dat_wiese$xms2<=as.POSIXct("2018-11-15 01:12:51"))
dat_wiese3<-subset(dat_wiese, dat_wiese$xms2>=as.POSIXct("2018-11-15 01:12:53")& dat_wiese$xms2<=as.POSIXct("2018-11-15 01:14:21"))
dat_wiese5<-subset(dat_wiese, dat_wiese$xms2>=as.POSIXct("2018-11-15 01:14:30")& dat_wiese$xms2<=as.POSIXct("2018-11-15 01:15:21"))
dat_wiese7<-subset(dat_wiese, dat_wiese$xms2>=as.POSIXct("2018-11-15 01:15:46")& dat_wiese$xms2<=as.POSIXct("2018-11-15 01:20:42"))
dat_dead<- rbind(dat_wiese1, dat_wiese2,dat_wiese3, dat_wiese5,dat_wiese7)

dat_dead$behaviour<- "dead"

#fox
dat_wiese4<-subset(dat_wiese, dat_wiese$xms2>=as.POSIXct("2018-11-15 01:14:22")& dat_wiese$xms2<=as.POSIXct("2018-11-15 01:14:29"))
dat_wiese6<-subset(dat_wiese, dat_wiese$xms2>=as.POSIXct("2018-11-15 01:15:22")& dat_wiese$xms2<=as.POSIXct("2018-11-15 01:15:45"))
dat_fox<- rbind(dat_wiese4,dat_wiese6)

dat_fox$behaviour<- "fox"

#freefall
dat_wiese8<-subset(dat_wiese, dat_wiese$xms2>=as.POSIXct("2018-11-15 01:21:09")& dat_wiese$xms2<=as.POSIXct("2018-11-15 01:21:09"))
dat_wiese9<-subset(dat_wiese, dat_wiese$xms2>=as.POSIXct("2018-11-15 01:26:08")& dat_wiese$xms2<=as.POSIXct("2018-11-15 01:26:08"))
dat_fall<- rbind(dat_wiese8,dat_wiese9)

dat_fall$behaviour<- "fall"

dat_all$Z<-as.numeric(dat_all$Z)

####comparing the two values with a t.test or variance test
t.test(dat_dead$Z, dat_sitting$Z)
var.test(dat_dead$Z, dat_sit$Z)

#####copying the two datasets together

dat_all<-rbind(dat, dat_sit, dat_fly, dat_dead, dat_fox, dat_fall)

plot(Z~xms2, data=dat_all)


dat_all$behaviour<- as.factor(dat_all$behaviour)

dat_all<- cbind(dat_all, dat_all$behaviour=="lying")
dat_all<- cbind(dat_all, dat_all$behaviour=="sitting")
dat_all<- cbind(dat_all, dat_all$behaviour=="flying")
dat_all<- cbind(dat_all, dat_all$behaviour=="dead")
dat_all<- cbind(dat_all, dat_all$behaviour=="fox")
dat_all<- cbind(dat_all, dat_all$behaviour=="fall")
names(dat_all)<- c("ms1","p","temp", "alt","ms2","gX","gY", "gZ", "X", "Y", "Z", "mX", "mY", "mZ", "xms2", "behaviour","lying", "sitting", "flying", "dead", "fox", "fall")




#starting the neural network journey
samplesize=0.6*nrow(dat_all)
set.seed(80)
index=sample(seq_len(nrow(dat_all)), size=samplesize)

dat_train<- dat_all[index, ]
dat_test<- dat_all[-index, ]

library(MASS)
ind <- sapply(dat_all, is.numeric)
#dat_all[ind] <- lapply(dat_all[ind], scale)

max<- apply(dat_all[ind], 2, max)
min<- apply(dat_all[ind], 2, min)
scaled<- as.data.frame(scale(dat_all[ind], center=min, scale=max-min))

scaled2<- cbind(scaled, dat_all$behaviour, dat_all$xms2)
library(neuralnet)

trainNN<- scaled2[index, ]
testNN<- scaled2[-index,]


names(trainNN)<- c("ms1","p","temp", "alt","ms2","gX","gY", "gZ", "X", "Y", "Z", "mX", "mY", "mZ","behaviour", "xms2")
names(testNN)<- c("ms1","p","temp", "alt","ms2","gX","gY", "gZ", "X", "Y", "Z", "mX", "mY", "mZ","behaviour", "xms2")

trainNN<- cbind(trainNN, trainNN$behaviour=="lying")
trainNN<- cbind(trainNN, trainNN$behaviour=="sitting")
trainNN<- cbind(trainNN, trainNN$behaviour=="flying")
trainNN<- cbind(trainNN, trainNN$behaviour=="dead")
trainNN<- cbind(trainNN, trainNN$behaviour=="fox")
trainNN<- cbind(trainNN, trainNN$behaviour=="fall")
#have to change the names again
names(trainNN)<- c("ms1","p","temp", "alt","ms2","gX","gY", "gZ", "X", "Y", "Z", "mX", "mY", "mZ", "xms2", "behaviour","lying", "sitting", "flying", "dead", "fox", "fall")



set.seed(2)
NN=neuralnet(lying+sitting+flying+dead+fox+fall~X+Y+Z, trainNN, linear.output=T, rep=2, hidden=5)

plot(NN)


predict_testNN<- compute (NN, testNN)



predict_testNN<- compute(NN, testNN[, c(9:11)])
predict_testNN = (predict_testNN$net.result * (max(dat_all$behaviour) - min(dat_all$behaviour))) + min(dat_all$behaviour)
pred.weights<- predict_testNN$net.result

idx <- apply(pred.weights, 1, which.max)

pred <- c('lying', 'sitting', 'flying')[idx]
table(pred, dat_test$behaviour)

predict_testNN = (predict_testNN$net.result * (max(dat_all$behaviour) - min(dat_all$behaviour))) + min(dat_all$behaviour)
plot(dat_test$behaviour, predict_testNN, col='blue', pch=16, ylab = "predicted sitting NN", xlab = "real sitting behaviour")
RMSE.NN = (sum((dat_test$behaviour - predict_testNN)^2) / nrow(dat_test)) ^ 0.5



comp <- compute(nn, irisvalid[-5])
pred.weights <- comp$net.result
idx <- apply(pred.weights, 1, which.max)
pred <- c('setosa', 'versicolor', 'virginica')
table(pred, irisvalid$Species)


