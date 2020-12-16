library(dplyr)
library(MASS)
library(tidyr)
library(tidyverse)
library(stats)
library(e1071)
library(class)
library(randomForest)
########### SCENARIO 1 ######################

data_training<-read.csv(file.choose(), header=TRUE)
data_testing<-read.csv(file.choose(), header=TRUE)

training=data_training
testing=data_testing




############################ Random Forest ###################

###Applying the missing values by median/mode

data3<-data
for ( i in 2:ncol(data)){
  data3[,i]<- na.roughfix(data[,i])
}
#######Scenario 1 #####

bag.Rtree1<-randomForest(as.factor(ClusterID)~Industry.Category+State+Company.Type+Company.Age+Revenue.Band1+Employee.Band1,data=training, mtry=3, importance=TRUE)

print(bag.Rtree1)

summary(bag.Rtree1)
bag.Rtree1$importance
par(mfrow=c(2,1), cex=0.7)
barplot(sort(bag.Rtree1$importance[,1], decreasing = TRUE))
barplot(sort(bag.Rtree1$importance[,2], decreasing = TRUE))

importance(bag.Rtree1)
varImpPlot (bag.Rtree1)

bag.Rtree1_testing<-randomForest(as.factor(ClusterID)~Industry.Category+State+Company.Type+Company.Age+Revenue.Band1+Employee.Band1,data=testing, mtry=3, importance=TRUE)

print(bag.Rtree1_testing)

########### SCENARIO 2 ######################

data_traininghp<-read.csv(file.choose(), header=TRUE)
data_testinghp<-read.csv(file.choose(), header=TRUE)
# Using 3 variables #

bag.Rtree2<-randomForest(as.factor(ClusterID)~Industry.Category+State+Company.Type+Company.Age+Revenue.Band1+Employee.Band1+Equifax.Score.Norm,data=data_traininghp, mtry=3, importance=TRUE)
print(bag.Rtree2)
summary(bag.Rtree2)
bag.Rtree2$importance
par(mfrow=c(2,1), cex=0.7)
barplot(sort(bag.Rtree2$importance[,1], decreasing = TRUE))
barplot(sort(bag.Rtree2$importance[,2], decreasing = TRUE))

importance(bag.Rtree2)
varImpPlot (bag.Rtree2)

bag.Rtree2_testing<-randomForest(as.factor(ClusterID)~Industry.Category+State+Company.Type+Company.Age+Revenue.Band1+Employee.Band1+Equifax.Score.Norm,data=data_testinghp, mtry=3, importance=TRUE)
print(bag.Rtree2_testing)

########### SCENARIO 3 ######################

training=data_training

# Using 3 variables #

bag.Rtree3<-randomForest(as.factor(ClusterID)~Industry.Category+State+Company.Type+Company.Age+Revenue.Band2+Employee.Band2,data=training, mtry=3, importance=TRUE)
print(bag.Rtree3)
summary(bag.Rtree3)
bag.Rtree3$importance
par(mfrow=c(2,1), cex=0.7)
barplot(sort(bag.Rtree3$importance[,1], decreasing = TRUE))
barplot(sort(bag.Rtree3$importance[,2], decreasing = TRUE))

importance(bag.Rtree3)
varImpPlot (bag.Rtree3)

bag.Rtree3_testing<-randomForest(as.factor(ClusterID)~Industry.Category+State+Company.Type+Company.Age+Revenue.Band2+Employee.Band2,data=training, mtry=3, importance=TRUE)
print(bag.Rtree3_testing)

########### SCENARIO 4 ######################

bag.Rtree4<-randomForest(as.factor(ClusterID)~Industry.Category+State+Company.Type+Company.Age+Revenue.Band2+Employee.Band2+Equifax.Score.Norm,data=data_traininghp, mtry=3, importance=TRUE)
print(bag.Rtree4)
summary(bag.Rtree4)
bag.Rtree4$importance
par(mfrow=c(2,1), cex=0.7)
barplot(sort(bag.Rtree4$importance[,1], decreasing = TRUE))
barplot(sort(bag.Rtree4$importance[,2], decreasing = TRUE))

importance(bag.Rtree4)
varImpPlot (bag.Rtree4)

bag.Rtree4_testing<-randomForest(as.factor(ClusterID)~Industry.Category+State+Company.Type+Company.Age+Revenue.Band2+Employee.Band2+Equifax.Score.Norm,data=data_testinghp, mtry=3, importance=TRUE)
print(bag.Rtree4_testing)

########### SCENARIO 5 ######################

bag.Rtree5<-randomForest(as.factor(ClusterID)~Industry.Category+Number.Of.Employees+Revenue+State+Company.Type+Company.Age,data=training, mtry=3, importance=TRUE)
print(bag.Rtree5)
summary(bag.Rtree5)
bag.Rtree5$importance
par(mfrow=c(2,1), cex=0.7)
barplot(sort(bag.Rtree5$importance[,1], decreasing = TRUE))
barplot(sort(bag.Rtree5$importance[,2], decreasing = TRUE))

importance(bag.Rtree5)
varImpPlot (bag.Rtree5)

bag.Rtree5_testing<-randomForest(as.factor(ClusterID)~Industry.Category+Number.Of.Employees+Revenue+State+Company.Type+Company.Age,data=testing, mtry=3, importance=TRUE)
print(bag.Rtree5_testing)

########### SCENARIO 6 ######################

bag.Rtree6<-randomForest(as.factor(ClusterID)~Industry.Category+Number.Of.Employees+Revenue+State+Company.Type+Company.Age+Equifax.Score.Norm,data=data_traininghp, mtry=3, importance=TRUE)
print(bag.Rtree6)
summary(bag.Rtree6)
bag.Rtree6$importance
par(mfrow=c(2,1), cex=0.7)
barplot(sort(bag.Rtree6$importance[,1], decreasing = TRUE))
barplot(sort(bag.Rtree6$importance[,2], decreasing = TRUE))

importance(bag.Rtree6)
varImpPlot (bag.Rtree6)

bag.Rtree6_testing<-randomForest(as.factor(ClusterID)~Industry.Category+Number.Of.Employees+Revenue+State+Company.Type+Company.Age+Equifax.Score.Norm,data=data_testinghp, mtry=3, importance=TRUE)
print(bag.Rtree6_testing)

