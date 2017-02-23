#Author @ Mohammed 23/02/2017

#Loading Libraries
source("Libraries.R")

#Loading data
train<-read.csv("churnTrain.csv",header = T)

#View(head(train))

summary(train$Customer_Left)
#Clearly we see data is biased
#Accoring to null deviance if we make a prediction without making any model
#we would be accurate 85.5 % ofthe times so we need to increase our accuracy

#Since it is a churn analysis we will concentrate on two methods,
# Decision trees and logistic regression

#Since data is biased using Smote to make data balanced

train$Customer_Left<-as.numeric(train$Customer_Left)
summary(as.factor(train$Customer_Left))

train$Customer_Left[train$Customer_Left==2]<-0

train$Customer_Left<-as.factor(train$Customer_Left)

#here false ->1
#     true ->0

# cpy<-train
# train<-cpy
ntrain<-SMOTE(Customer_Left~.,train,perc.over=200,k = 3)
ntrain$Customer_Left<-as.factor(ntrain$Customer_Left)
summary(ntrain$Customer_Left)

Smote_Train<-ntrain

rm(ntrain)

#Preparing Test data
test<-read.csv("churnTest.csv",header = T)
test$Customer_Left<-as.numeric(test$Customer_Left)
summary(as.factor(test$Customer_Left))
test$Customer_Left[test$Customer_Left==2]<-0
test$Customer_Left<-as.factor(test$Customer_Left)
summary(test$Customer_Left)
print("=======================Data Loaded ===============================")