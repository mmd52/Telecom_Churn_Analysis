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
#Plotting Tree
form <- as.formula(Customer_Left ~ .)
tree.2 <- rpart(form,train)			# A more reasonable tree
prp(tree.2)                                     # A fast plot													
fancyRpartPlot(tree.2)				# A fancy plot from rattle


