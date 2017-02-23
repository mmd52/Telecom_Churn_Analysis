#Author @ Mohammed 23/02/2017

#Load Libraries
source("Libraries.R")

#Load Data
source("FE.R")

#Running Boruta on data as is
set.seed(999)
boruta.train <- Boruta(Customer_Left~., data = train, doTrace = 2)
print(boruta.train)
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)


boruta.df <- attStats(final.boruta)
print(boruta.df)

#According to Boruta for as is data confirmed attributes are
# International_Plan,Voice_Mail_Plan ,No_Vmail_Messages,Total_Day_minutes,
# Total_Day_charge,Total_Eve_Minutes,Total_Eve_Charge,Total_Night_Minutes,
# Total_Night_Charge , Total_Intl_Minutes,Total_Intl_Calls,Total_Intl_Charge,
# No_CS_Calls

#Now running boruta on smote data
set.seed(999)
boruta.train <- Boruta(Customer_Left~., data = Smote_Train, doTrace = 2)
print(boruta.train)
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

boruta.df <- attStats(final.boruta)
print(boruta.df)

rm(boruta.df,boruta.train,final.boruta,Labels,lz)
#Accoring to Smote Data all attributes have been deemed important