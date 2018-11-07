#either load data from file or existing dataframe
setwd("C:/Users/Jessica/Google Drive/Waldrapp/data")
dat<- read.csv("test_nl.csv")

#checking the test dataset and cutting off all movement
head(dat)

plot(dat$Z~dat$ms2)

dat<- subset(dat, dat$ms2>60000& dat$ms2<1240000)

dat$xms2<-format(as.POSIXct(Sys.Date(), tz="GMT")+dat$ms2/1000, "%H:%M:%S")
dat$xms2<- as.POSIXct(strptime(dat$xms2, format="%H:%M:%S"))

dat$behaviour<- "lying"

mean(dat$Z)
sd(dat$Z)

apply(dat, 2, mean)
apply(dat, 2, sd)

#getting a real dataset 

head(dat_ld)

#convert ms into hours for a more intuitive graph
dat_ld$xms2<-format(as.POSIXct(Sys.Date(), tz="GMT")+dat_ld$ms2/1000, "%H:%M:%S")
dat_ld$xms2<- as.POSIXct(strptime(dat_ld$xms2, format="%H:%M:%S"))
dat_ld$Z<- as.numeric(dat_ld$Z)
plot(Z~xms2, data=dat_ld)
#main file to determine the sitting phases
dat_ldn<- subset(dat_ld, dat_ld$xms2>=as.POSIXct("2018-11-02 00:00:00")& dat_ld$xms2<=as.POSIXct("2018-11-02 06:00:00"))
dat_ldn$Z<- as.numeric(dat_ldn$Z)
plot(Z~xms2, data=dat_ldn)

#"sitting phases" in the datafile
dat_ldn1<- subset(dat_ld, dat_ld$xms2>=as.POSIXct("2018-11-02 02:12:30")& dat_ld$xms2<=as.POSIXct("2018-11-02 02:16:00"))
dat_ldn2<- subset(dat_ld, dat_ld$xms2>=as.POSIXct("2018-11-02 03:08:46")& dat_ld$xms2<=as.POSIXct("2018-11-02 03:10:55"))
dat_ldn3<- subset(dat_ld, dat_ld$xms2>=as.POSIXct("2018-11-02 03:11:20")& dat_ld$xms2<=as.POSIXct("2018-11-02 03:15:25"))
dat_ldn4<- subset(dat_ld, dat_ld$xms2>=as.POSIXct("2018-11-02 03:16:45")& dat_ld$xms2<=as.POSIXct("2018-11-02 03:18:34"))
dat_ldn5<- subset(dat_ld, dat_ld$xms2>=as.POSIXct("2018-11-02 03:21:45")& dat_ld$xms2<=as.POSIXct("2018-11-02 03:22:20"))

#turns out the variance is huge
dat_new<-rbind (dat_ldn1, dat_ldn2,dat_ldn3,dat_ldn4, dat_ldn5)
#so perhaps use only the last two?
dat_new45<-rbind(dat_ldn4, dat_ldn5)

#create a new column with the behaviour
dat_new45$behaviour<- "sitting"
dat_new$behaviour<- "sitting"

summary(dat_new)
#checking if it looks ok
head(dat_new45)
dat_new$Z<- as.numeric(dat_new$Z)
plot(Z~xms2, data=dat_new)

mean(dat_new45$Z)
sd(dat_new45$Z)

####comparing the two values with a t.test or variance test

t.test(dat_new45$Z, dat$Z)
var.test(dat_new$Z, dat$Z)


#####copying the two datasets together for a bootstrap experiment

dat_boot<-rbind(dat_new45, dat)
dat_boot_all<-rbind(dat_new, dat)

plot(Z~xms2, data=dat_boot_all)

dat_boot_all$behaviour<- as.factor(dat_boot_all$behaviour)


#trying to create a test data set for later bootstrapping
library("boot")

test<-sample(dat_boot_all, size=500, replace=T)

#deleting the behavioural component
dat_boot_st<- dat_boot_all[c(11, 16)]

dat_new2<- dat_new[11,16]
dat2<- dat[11,16]



m<- boot(dat_boot_st, )

###first version

varcomp <- function (data, indices, formula) {
  d1 <- subset(dat_boot_st[indices,],behaviour=="lying") #sample for boot
  d2 <- subset(dat_boot_st[indices,],behaviour=="sitting")  #sample for boot
  fit1 <- lm(formula, data=d1) #linear model
  fit2 <- lm(formula, data=d2) #linear model
  a = (attr (VarCorr(fit1), "sc")^2) #output variance estimation
  b = (attr (VarCorr(fit2), "sc")^2) #output variance estimation
  drv = a - b #difference between the variance estimations
  return(drv)
}


ip1.boot <- boot (dat_boot_st, statistic=varcomp, R=100, formula=Z~behaviour)

ip1.boot <- boot ( data = list (d1=dataframe1, d2=dataframe2), statistic=varcomp, R=100, formula=CNPC~(1|Cell.line:DNA.extract)+Cell.line)


##second version
SDdiff = function(dataFrame, indexVector) { 
  f1 = sd(subset(dataFrame[indexVector, 1], dataFrame[indexVector, 2] == "lying"))
  f2 = sd(subset(dataFrame[indexVector, 1], dataFrame[indexVector, 2] == "sitting"))
 f=f1-f2
  return(f)
}

totalBoot = boot(dat_boot_st, SDdiff, R = 1000, strata = dat_boot_st[,2])
totalBootCI = boot.ci(totalBoot)

summary(dat_boot_st)
totalBoot


###third simple version 

b_dat<- boot(dat$Z, sum, R=1000)
b_dat_new<-boot(dat_new$Z, sum, R=1000)

z<- (b_dat_new$t0-b_dat$t0)/sqrt(var(b_dat_new$t[,1])+var(b_dat$t[,1]))
pnorm(z)
z

####fourth simple version ;)
###this could acutally work

set.seed(1300)
B<- 1000
t.vect<- vector(length=B)
p.vect<- vector(length=B)
for (i in 1:B){
  boot.c<-sample(dat$Z, size=22710, replace=T)
  boot.p<- sample(dat_new$Z,size=14066, replace=T )
  vartest<- var.test(boot.c, boot.p)
  t.vect[i]<- vartest$statistic
  p.vect[i]<- vartest$p.value
  }

t.vect
p.vect
?var.test
vartest
