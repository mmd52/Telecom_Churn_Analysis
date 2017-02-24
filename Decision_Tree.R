#Author @ Mohammed 23/02/2017

#Load Libraries
source("Libraries.R")

#Load Data
source("FE.R")

#Running decision tree on normal Data 
set.seed(999)
#To run a decision tree and make a model im going to make use of 
#package RWEKA here

j48<-J48(Customer_Left~.,data=train,control=Weka_control(),options=NULL)

summary(j48)

# eval_j48 <- evaluate_Weka_classifier(j48, numFolds = 100,
#                                      complexity = FALSE, 
#                                      seed = 1, class = TRUE)
# eval_j48

preds<-predict(j48,test[,-21])
caret::confusionMatrix(test[,21], preds, mode = "prec_recall")

auc<-roc(test[,21],as.numeric(predict(j48,newdata=test[,-21],type="class")))
print(auc)
plot(auc,print.auc=T)

#Seems like Decision Tree on Plain data is quite good

#===========================================================================
#Running decision tree on Smote Data 
set.seed(999)
#To run a decision tree and make a model im going to make use of 
#package RWEKA here

j48<-J48(Customer_Left~.,data=Smote_Train,control=Weka_control(),options=NULL)

summary(j48)

# eval_j48 <- evaluate_Weka_classifier(j48, numFolds = 100,
#                                      complexity = FALSE, 
#                                      seed = 1, class = TRUE)
# eval_j48

preds<-predict(j48,test[,-21])
caret::confusionMatrix(test[,21], preds, mode = "prec_recall")

auc<-roc(test[,21],as.numeric(predict(j48,newdata=test[,-21],type="class")))
print(auc)
plot(auc,print.auc=T)

#Seems like Decision Tree on Plain data is quite good

#Normal Decision Tree led to a better accuracy ,
#Considering that 

#=================================================================
fit<-rpart(Customer_Left~.,method = "class",data = train)
#plot tree
plot(fit,uniform = T,main="Classification")
text(fit,use.n=T,all = T,cex=.6,pretty = T)

preds<-predict(fit,test[,-21])
npreds<-preds
for(i in preds){
  npreds[i]<-ifelse(preds[i,1]>preds[i,2],0,1)
}

caret::confusionMatrix(test[,21], npreds, mode = "prec_recall")

auc<-roc(test[,21],as.numeric(npreds))
print(auc)
plot(auc,print.auc=T)
