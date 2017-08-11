#Ex8
# Load the data
college = read.csv('College.csv')
# Visualize the data in Viewer
View(college)
# Assign university name to each row
rownames(college)=college[,1]
View(college)
# Eliminate first column of data
college=college[,-1]
View(college)

summary(college)
pairs(college[,1:10])
attach(college)
typeof(Oustate)
plot(Private, Outstate)

#create new column Elite
Elite=rep("No",nrow(college))
Elite[Top10perc >50]="Yes"
Elite=as.factor(Elite)
college=data.frame(college ,Elite)
summary(college)
plot(college$Elite, college$Outstate)

par(mfrow=c(2,2))
hist(college$Books)
hist(college$perc.alumni,breaks=15)
hist(college$Personal, breaks=10)
hist(college$Enroll)

#Ex9
Auto = read.csv("../data/Auto.csv", header=T, na.strings="?")
Auto = na.omit(Auto)
dim(Auto)
summary(Auto)

sapply(Auto[, 1:7], range)

sapply(Auto[,1:7], mean)
sapply(Auto[,1:7], sd)

Auto = Auto[-(10:85),]
sapply(Auto[,1:7], mean)
sapply(Auto[,1:7], sd)

pairs(Auto[1:9])

#Ex10
library(MASS)
View(Boston)
?Boston
pairs(Boston)
sapply(Boston$chas, sum)

dim(subset(Boston, chas == 1))
help(subset)
  
t(subset(Boston, medv == min(Boston$medv)))

dim(subset(Boston, rm>=8))
