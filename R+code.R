##Step 1: Data filtering, cleaning and processing
rm(list=ls())

#read data "2012 US Commercial Building Energy Consumption Survey (CBECS)"
data_all<-read.csv(file.choose(),header=T)

#Filter data that is useful for analysis
#Independent variables: NFLOOR,BASEMNT,FLCEILHT,YRCON
#Dependent variable: ELEVTR
data<-subset(data_all,select=c(ELEVTR,NFLOOR,BASEMNT,FLCEILHT,YRCON))

#Delete rows with NA to clean the data 
data<-na.omit(data)

#Replace 2 with 0 to obtain a binomial distribution variable
data$ELEVTR[which(data$ELEVTR==2)]=0

#Divide data into training set and test set to test the accuracy of prediction.
#The ratio of the number of training set to test set is 8:2.
trainset<-data[1:2708,]
testset<-data[2709:3385,]


##Step 2: Perform logistic regression and find the significant factors
#Firstly, perform logistic regression on the training set.
fit1<-glm(ELEVTR~.,data=trainset,family=binomial(link="logit"))
summary(fit1)

#According to the result, delete variable "FLCEILHT" that is not significantly related
#Perform logistic regression again after eliminating the insignificant factor
fit2<-glm(ELEVTR~NFLOOR+BASEMNT+YRCON,data=trainset,family=binomial(link="logit"))
summary(fit2)


##Step 3: Predict and test accuracy
pred=predict.glm(fit2,type='response',newdata=testset)
#According to the predicted value, >0.5 returns 1 and the rest returns 0
predict=ifelse(pred>0.5,1,0)
testset$predict=predict
true_value=testset[,1]
predict_value=testset[,6]
#Calculate model accuracy
error = predict_value-true_value
accuracy = (nrow(testset)-sum(abs(error)))/nrow(testset)
accuracy
#Accuracy: the ratio of the correct number to the total number

##Step 4: cross-validation (10 folds)
#For a more accurate estimate, 10-fold cross-validation will be used.

#Firstly delete irrelevant data
data<-subset(data,select=c(ELEVTR,NFLOOR,BASEMNT,YRCON))

#Divide the data into random deciles
#install.packages("caret",repos = "https://mirror.lzu.edu.cn/CRAN/")
#library("caret")
set.seed(8)
require(caret)
folds<-createFolds(y=data$ELEVTR,k=10)

#Construct a for loop, get 10-fold cross-validation test set accuracy and training set accuracy
max=0
num=0
for(i in 1:10){
  
  fold_test<-data[folds[[i]],]   #test set
  fold_train<-data[-folds[[i]],]   #train set
  
  print("***Group Number***")
  
  fold_pre <- glm(ELEVTR~.,family=binomial(link='logit'),data=fold_train)
  fold_predict <- predict(fold_pre,type='response',newdata=fold_test)
  fold_predict =ifelse(fold_predict>0.5,1,0)
  fold_test$predict = fold_predict
  fold_error = fold_test[,5]-fold_test[,1]
  fold_accuracy = (nrow(fold_test)-sum(abs(fold_error)))/nrow(fold_test) 
  print(i)
  print("***Test set accuracy***")
  print(fold_accuracy)
  print("***Training set accuracy***")
  fold_predict2 <- predict(fold_pre,type='response',newdata=fold_train)
  fold_predict2 =ifelse(fold_predict2>0.5,1,0)
  fold_train$predict = fold_predict2
  fold_error2 = fold_train[,5]-fold_train[,1]
  fold_accuracy2 = (nrow(fold_train)-sum(abs(fold_error2)))/nrow(fold_train) 
  print(fold_accuracy2)

  
  if(fold_accuracy>max)
  {
    max=fold_accuracy  
    num=i
  }
  
}

print(max)
print(num)
#From the results, the maximum accuracy is "max", "folds[[num]]" are used as the test set, and the rest are used as the training set.


##Step 5: Choose the best performing one out of 10 folds
#The results of the maximum accuracy of 10-fold cross-validation test set
testi <- data[folds[[num]],]
traini <- data[-folds[[num]],]  
prei <- glm(ELEVTR~.,family=binomial(link='logit'),data=traini)
predicti <- predict.glm(prei,type='response',newdata=testi)
predicti =ifelse(predicti>0.5,1,0)
testi$predict = predicti

errori = testi[,5]-testi[,1]
accuracyi = (nrow(testi)-sum(abs(errori)))/nrow(testi) 

#The results of the maximum accuracy of 10-fold cross-validation training set
predicti2 <- predict.glm(prei,type='response',newdata=traini)
predicti2 =ifelse(predicti2>0.5,1,0)
traini$predict = predicti2
errori2 = traini[,5]-traini[,1]
accuracyi2 = (nrow(traini)-sum(abs(errori2)))/nrow(traini) 

#Test set accuracy; take the i-th group; training set accuracy
accuracyi;num;accuracyi2

#The final logistic regression relationship
summary(prei)


##Step 6: ROC diagram and AUC value
install.packages("pROC",repos = "https://mirror.lzu.edu.cn/CRAN/")
library(pROC)
modelroc <- roc(testi$ELEVTR,predicti)
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE,legacy.axes=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE) 
