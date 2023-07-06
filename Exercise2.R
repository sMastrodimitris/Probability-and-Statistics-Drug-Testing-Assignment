data = read.table(file="data2.txt",header=TRUE)
data

#----------------------------------(a)----------------------------
#Data exploration

hist(data$Y)
hist(data$X1)
hist(data$X2)
hist(data$X3)
hist(data$X4)

library(ggplot2)
ggplot(data, aes(x=W)) +
  geom_bar()

boxplot(data$Y ~ data$W, data)
boxplot(data$X1 ~ data$W, data)
boxplot(data$X2 ~ data$W, data)
boxplot(data$X3 ~ data$W, data)
boxplot(data$X4 ~ data$W, data)
boxplot(data$X4 ~ data$W, data)



#Anova
library(dplyr)
df=data

library("stats") 
df = arrange(df, W)
rownames(df) = 1:nrow(df)


XY=df$Y
XY=append(XY,df$X1)
XY=append(XY,df$X2)
XY=append(XY,df$X3)
XY=append(XY,df$X4)

#O<-factor(rep(rep(1:5,each=3),3))

a = rep(1, times = 76)
b = rep(2, times = 67)
c = rep(3, times = 57)

y=a
y = append(y,b)
y = append(y,c)

cat = append(y,y)
cat = append(cat,cat)
cat = append(cat,y)
cat

f <- factor(c(cat))


#M<-factor(c(df$W))

xy<-factor(rep(1:5,each=200))

fit = aov(XY~f*xy)

summary(fit)

TukeyHSD(fit)


#--------------------------------(b)--------------------------
p = subset(df, select = -c(W))
pairs(p)


ggplot(df, aes(X1, Y, colour = W)) + 
   geom_point()

ggplot(df, aes(X2, Y, colour = W)) + 
   geom_point()

ggplot(df, aes(X3, Y, colour = W)) + 
   geom_point()

ggplot(df, aes(X4, Y, colour = W)) + 
   geom_point()



#install.packages('GGally')
library(GGally)
ggpairs(df, aes(color = W))


#-----------------------------------(c)------------------------
plot(df$X4,df$Y ,main="Y vs X4", xlab="x1", ylab="y", pch=19)

cor(df$Y,df$X4)

cor.test(df$Y,df$X4)

fit<-lm(df$Y ~ df$X4)
summary(fit)

#Y = 1.9347*X4 + 26.1973 

plot(df$X4, df$Y , main="Y vs X along with the OLS line", xlab="X4", ylab="Y",pch=19) +
abline(fit, col="red")


qqnorm(fit$residuals,main="NPP for residuals")
qqline(fit$residuals,col="red",lty=1,lwd=2)

shapiro.test(fit$residuals)


#-----------------------------------(d)--------------------------
W = y
fit<-lm(df$Y ~ df$X1+df$X2+df$X3+df$X4+W)
summary(fit)


#Y= 0.96269*X1 + 1.93024*X2 + 0.23562*X3 + 0.08043*X4 + 0.19876*W + 16.87133


qqnorm(fit$residuals,main="NPP for residuals")
qqline(fit$residuals,col="red",lty=1,lwd=2)

shapiro.test(fit$residuals)

#install.packages("nortest")
library(nortest)
lillie.test(fit$residuals)


par(mfrow=c(2,2)) # provide plots in a 2x2 layout
plot(fit)
mtext("Diagnostic plots for the linear model", outer=TRUE, line=-2, font=2, cex=1.2)


#------------------------------------(e)------------------------------
fit<-lm(df$Y ~ df$X1+df$X2+df$X4)
summary(fit)


fit<-lm(df$Y ~ df$X1+df$X2)
summary(fit)


#Y= 1.00369*X1 + 1.99812*X2 + 19.88520 

qqnorm(fit$residuals,main="NPP for residuals")
qqline(fit$residuals,col="red",lty=1,lwd=2)

shapiro.test(fit$residuals)

#install.packages("nortest")
library(nortest)
lillie.test(fit$residuals)


par(mfrow=c(2,2)) # provide plots in a 2x2 layout
plot(fit)
mtext("Diagnostic plots for the linear model", outer=TRUE, line=-2, font=2, cex=1.2)


#-------------------------------------(f)---------------------------------
dffs subset(df, select = -c(W))
FitStart = lm(Y~1,dffs)
summary(FitStart)

all = lm(Y ~ ., dffs)

forward = step(FitStart, direction='forward', scope=formula(all), trace=0)

forward$anova

summary(forward)


qqnorm(forward$residuals,main="NPP for residuals")
qqline(forward$residuals,col="red",lty=1,lwd=2)

shapiro.test(forward$residuals)

lillie.test(forward$residuals)


par(mfrow=c(2,2)) # provide plots in a 2x2 layout
plot(forward)
mtext("Diagnostic plots for the linear model", outer=TRUE, line=-2, font=2, cex=1.2)


#The forward selection method proposes a model of four dimentions 
#Y = 0.96631*X1 + 1.94436*X2 + 0.23677*X3 + 0.06437*X4 + 17.82622 

#---------------------------------------(g)-----------------------------------
attach(dffs)
prediction.lm = lm(Y ~ X1*X2*X3*X4)

test = data.frame (
  X1 = 120,
  X2 = 30,
  X3 = 10,
  X4 = 90
)

predict(prediction.lm, test, interval="predict") 

#Y~X1*X2*X3*X4*W
prediction.lm = lm(Y ~ X1*X2*X3*X4*W)

test = data.frame (
  X1 = 120,
  X2 = 30,
  X3 = 10,
  X4 = 90,
  W = 2
)

predict(prediction.lm, test, interval="predict") 

#---------------------------------------(h)-------------------------------------
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(df$X4, probs = seq(0, 1, by = 0.25))), 
      labels=c("Q1","Q2","Q3","Q4"), include.lowest=TRUE)
}
df$Z <- sapply(df$X4, ApplyQuintiles)


conTable = table(df$W,df$Z)
print(conTable)

conTable = table(df$Z,df$W)
print(conTable)

df = arrange(df, W)
df$W = y

df = arrange(df, Z)


a = rep(1, times = 50)
b = rep(2, times = 50)
c = rep(3, times = 50)
d = rep(4, times = 50)

z=a
z = append(z,b)
z = append(z,c)
z = append(z,d)
df$Z = z
df


z = factor(c(z))
fit = aov(y~z)
summary(fit)

TukeyHSD(fit)