#Name:Mandeep Rana
#IST 707
#HW 5

rm(list=ls()) 
#install.packages("C50")
#install.packages("caret")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("rattle")
library("C50")
library("caret")
library("rpart")
library("rpart.plot")
library("rattle")

#Section 1: Data preparation

setwd("/Users/manu/Desktop/Fall'18/IST 707/Wk5/")
fed<- read.csv("fedPapers85.csv", header = TRUE, stringsAsFactors = FALSE)

fed$author<-as.factor(fed$author)
str(fed)


train <- fed[(fed$author!="dispt"),]
test <- fed[(fed$author=="dispt"),]

#Section 2: Build and tune decision tree models
##C50 decision tree
##Basic tree based model
str(train)
model_deafult<- C5.0(x=train[,-1:-2],y=train$author)
names(model_deafult)
print(model_deafult)
summary(model_deafult)

##Rule based model

rule_model<- C5.0(author~., data=train[,-2], rules=TRUE)
print(rule_model)
summary(rule_model)


##Boosting based model
model_boost<- C5.0(x=train[,-1:-2],y=train$author, trials = 3)
print(model_boost)
summary(model_boost)


##Decision Trees
levels(train$author)
train$author<-factor(train$author)
DT<-train(train[,-1:-2], train$author ,metric="Accuracy",method="rpart")
print(DT)
print(DT$finalModel)
prp(DT$finalModel)
fancyRpartPlot(DT$finalModel)

#Tuning model

DT_tune_1<-train(train[,-1:-2], train$author ,metric="Accuracy",method="rpart", tuneLength=8)
DT_tune_1$finalModel
prp(DT_tune_1$finalModel)
fancyRpartPlot(DT_tune_1$finalModel)

DT_tune_2<-train(train[,-1:-2], train$author ,method="rpart", tuneGrid = expand.grid(cp=seq(0,0.1,0.01)))
DT_tune_2$finalModel
prp(DT_tune_2$finalModel)
fancyRpartPlot(DT_tune_2$finalModel)

#Section 3: Prediction
model_deafult$predictors
pred_default_model<-predict(model_deafult, newdata = test[,-1])
pred_default_model

rule_model$predictors
pred_rule_model<-predict(rule_model, newdata = test[,-1])
pred_rule_model

model_boost$predictors
pred_boost_model<-predict(model_boost, newdata = test[,-1], type = "prob")
pred_boost_model

DT_pred<-predict(DT, newdata = test[,-1], type = "prob")
DT_pred

DT_pred_r<-predict(DT, newdata = test[,-1], type = "raw")
DT_pred_r

DT_pred_tune_1<- predict(DT_tune_1, newdata=test[,-1],type = "raw")
DT_pred_tune_1

DT_pred_tune_2<- predict(DT_tune_2, newdata=test[,-1],type = "raw")
DT_pred_tune_2


