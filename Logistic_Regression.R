#Author @ Mohammed 23/02/2017

#Load Libraries
source("Libraries.R")

#Load Data
source("FE.R")

#Running logistic on as is data
logit.fit=glm(Customer_Left~.,family=binomial(logit),data=train)

summary(logit.fit)
vif(logit.fit)

preds<-ifelse(predict(logit.fit,newdata=test[,-21],type="response")>=0.45,1,0)
table(test[,21],preds)
caret::confusionMatrix(test[,21], preds, mode = "prec_recall")

auc<-roc(test[,21],ifelse(predict(logit.fit,newdata=test[,-21],type="response")>=0.45,1,0))
print(auc)
plot(auc,print.auc=T)

#Accuracy 87.53%
#Kappa  0.275
#Precision  22.807%
#Recall 0.59091 %
#AUC  0.602
#=============================================================================
#Logistic Regression With Smote
logit.fit=glm(Customer_Left~.,family=binomial(logit),data=Smote_Train)

summary(logit.fit)
vif(logit.fit)

preds<-ifelse(predict(logit.fit,newdata=test[,-21],type="response")>=0.35,1,0)
table(test[,21],preds)
caret::confusionMatrix(test[,21], preds, mode = "prec_recall")

auc<-roc(test[,21],ifelse(predict(logit.fit,newdata=test[,-21],type="response")>=0.35,1,0))
print(auc)
plot(auc,print.auc=T)

#Accuracy 84.71 %
#Kappa 0.3265
#Precision 0.40351
#Recall 0.42593
#AUC 0.660