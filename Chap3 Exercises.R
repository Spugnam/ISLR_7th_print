library(MASS)
install.packages("ISLR")
library(ISLR)

names(Boston)
?Boston
View(Boston)

attach(Boston)
lm.fit=lm(medv~lstat)
lm.fit
summary(lm.fit)
names(lm.fit)
predict(lm.fit,data.frame(lstat=c(5,10,15)), interval =" confidence ")
plot(lstat, medv)
abline(lm.fit)
?abline
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:30,1:30,pch=1:30)
par(mfrow=c(2,2))
plot(lm.fit)
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))
?hatvalues

# Multiple Linear Regression
test_name=lm(medv~lstat+age,data=Boston)
summary(test_name)
lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)
plot(lm.fit)
?summary.lm
summary.lm(lm.fit)$r.squared
library(car)
vif(lm.fit)
lm.fit1=lm(medv~.-age, data=Boston)
summary(lm.fit1)
attach(Boston)
summary(lm(medv~lstat*age))
lm.fit2 = lm(medv~lstat+I(lstat^2))
anova((lm(medv~lstat*age)), (lm(medv~lstat+I(lstat^2))))

par(mfrow=c(2,2))
plot(lm.fit2)

lm.fit5=lm(medv~poly(lstat, 5))
summary(lm.fit5)
anova(lm.fit, lm.fit5)

#Qualitative Predictors
View(Carseats)
attach(Carseats)
contrasts(ShelveLoc)
?contrasts

hello_world=function(){
  print("Hello world!")
}
hello_world()

#ex 8
typeof(Auto)
attach(Auto)
lm.fit=lm(mpg~horsepower)
summary(lm.fit)
predict(lm.fit, data.frame(horsepower=c(98)), interval="confidence")

plot(horsepower, mpg)
abline(lm.fit,lwd=3,col="red")

par(mfrow=c(2,2))
plot(lm.fit)

#Ex9
attach(Auto)
pairs(Auto)
?cor()
View(Auto)
cor(Auto[,1:8]) #or cor(subset(Auto, select=-name))
lm.fit = lm(mpg~.-name, data=Auto)
summary(lm.fit)

par(mfrow=c(2,2))
plot(lm.fit)
plot(predict(lm.fit), rstudent(lm.fit))

lm.fit6 = lm(mpg~weight*horsepower,na.action="na.exclude") #variable lengths differ Error
summary(lm.fit6)
lm.fit7=lm(mpg~log(weight))
lm.fit8 = lm(mpg~log(weight)+sqrt(horsepower)+acceleration+I(acceleration^2))

?confint
confint(lm.fit8, level=0.95)

#Ex11
set.seed (1)
x=rnorm(100)
y=2*x+rnorm(100)
lm.fit = lm(y~x+0)
summary((lm.fit))

lm.fit2 = lm(y~x)
summary((lm.fit2))

?rnorm
?legend

?runif
?abline
