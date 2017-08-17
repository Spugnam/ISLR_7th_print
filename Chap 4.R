library(ISLR)
names(Smarket)
View(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket)
cor(Smarket[,-9]) #cor(Smarket[,1:8])
attach(Smarket)
plot(Year, Volume)

glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family = binomial)
summary(glm.fits)
coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[,4]
?logit
contrasts(Direction)  #see what R has associated with qualitative parameter Direction
glm.probs = predict(glm.fits, type="response")
typeof(glm.probs)
dim(glm.probs)
glm.probs[1:10]
?rep
glm.pred=rep("Down", 1250)
glm.pred[glm.probs>0.5]="Up"
glm.pred

table(glm.pred, Direction)
mean(glm.pred==Direction)

# Create train/ test subsets
train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]
View(Direction.2005)

glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fits,Smarket.2005,type="response")

glm.pred=rep("Down",252)
glm.pred[glm.probs >.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)

#LDA
library(MASS)
lda.fit=lda(Direction~Lag1+Lag2, data=Smarket, subset=train)
?lda
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class ,Direction.2005)

#QDA
qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit
qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)

# K-Nearest Neighbors

library(class)
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)
(83+43)/252
knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)

# An Application to Caravan Insurance Data

dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822
standardized.X=scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])
mean(standardized.X[,3])

#Ex 10
View(Weekly)
pairs(Weekly)
attach(Weekly)
lda.fit=lda(Direction~., data=Weekly)
summary(lda.fit)
lda.fit
plot(lda.fit)

#Logistics Regression
glm.fits=glm(Direction~.-Today-Year,data=Weekly,family=binomial)
summary(glm.fits)$coeff
glm.probs=predict(glm.fits,type="response")
glm.probs[1:5]
contrasts(Direction)
dim(Weekly)
glm.pred=rep("Down",1089)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction)
#with train/ test sets
train=(Year<2009)
train
Weekly.test=Weekly[!train,]
glm.fits=glm(Direction~Lag2,data=Weekly,family=binomial, subset=train)
glm.probs=predict(glm.fits, Weekly.test, type="response")
dim(Weekly.test)
glm.pred=rep("Down", 104)
glm.pred[glm.probs>0.5]="Up"
glm.pred
table(glm.pred, Weekly.test$Direction) # or table(glm.pred, Direction[!train])

#LDA
lda.fit2=lda(Direction~Lag2, data=Weekly, train)
summary(lda.fit2)
lda.pred=predict(lda.fit2, Weekly[!train,])
names(lda.pred)
lda.pred$class
table(lda.pred$class, Direction[!train])
sum(lda.pred$posterior[,1]>.55)

#QDA
qda.fit=qda(Direction~Lag2, data=Weekly, subset=train)
qda.fit
qda.pred=predict(qda.fit, Weekly[!train,])
names(qda.pred)
qda.pred$class #predict always Up ??
table(qda.pred$class, Direction[!train])
sum(qda.pred$posterior[,1]>.55)

#KNN
library(class)
set.seed(1)
knn.pred=knn(data.frame(Lag2[train]), data.frame(Lag2[!train]), Direction[train], k=1)
table(knn.pred,Direction[!train])
knn.pred=knn(data.frame(Lag2[train]), data.frame(Lag2[!train]), Direction[train], k=3)
table(knn.pred,Direction[!train])

#Ex 11
View(Auto)
typeof(Auto)
dim(Auto)
mpg01=rep(0, 316)
mpg01[Auto$mpg>median(Auto$mpg)]=1
mpg01
Auto2=data.frame(cbind(mpg01,Auto))
attach(Auto2)
View(Auto2)
pairs(Auto2)
cor(Auto2[,1:8])
boxplot(mpg01~horsepower,data=Auto2, main="Car Milage Data", xlab="Year", ylab="MPG01")
dim(Auto2)
train=rep("True", 392)
train[300:392]="False"
train

#Ex 12
Power=function(x){
  y=x^3
  print(y)
}
Power(10)
Power2=function(x, a){
  print(x^a)
}
Power2(3, 8)
Power3=function(x, a){
  return(x^a)
}
Power3(3,8)
x=1:10
plot(x, Power3(x, 2), main="X^2", xlab="x", ylab="y=x^2", log="y")

PlotPower=function(x, a){
  plot(x, Power3(x, a), main="PowerPlot", xlab="x", ylab="y", log="y")
}
PlotPower(1:100, 3)




