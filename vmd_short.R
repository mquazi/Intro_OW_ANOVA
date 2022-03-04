#library(asbio)
library(stargazer)
#library(MASS)
#library(lmtest)
#library(car)


getwd()
setwd("...")
vmddata<-read.csv("VMDdata_short.csv",header=T)
attach(vmddata)
nt<-nrow(vmddata)
nt
## eight obsns per trt 
head(vmddata)
tapply(VMD,Holesize,mean)
tapply(VMD,Holesize,var)
tapply(VMD,Holesize,length)
mu..<-mean(VMD)
mu..
par(mfrow=c(1,1))
boxplot(VMD~Holesize, las=1, main="Boxplots for trts")
abline(mean(VMD),0,lty=2)
text(1.5,mu..+1,labels="Mean VMD 20.46")



fit1<-aov(VMD~factor(Holesize),data=vmddata)
summary(fit1)
print(model.tables(fit1,"means"),digits=3)
anova(fit1)
stargazer(anova(fit1),summary=F)
plot(fit1)

par(mfrow=c(1,1))
myfitTukey<-TukeyHSD(fit1,conf.level=.95)
myfitTukey
plot(myfitTukey,sub="Tukey Honest Sig Differences",las=1)


