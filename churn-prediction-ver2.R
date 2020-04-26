library(dplyr)
library(data.table)
library(ggplot2)
library(car)
library(adabag)
library(rpart)
library(aod)
library(caret)
library(foreach)
library(ROCR)
library(pROC)
library(lubridate)
library(scales) 
library(grid) 
library(gridExtra) 
library(RColorBrewer) 
library(corrplot)
library(MASS)
require(caTools)
library(randomForest)
library(neuralnet)


#setwd("/Users/jasneekchugh/Desktop/DataScience/R-Programming/kk-box-churn-prediction")

#reading the required files
trainData <- fread('data/train.csv', sep = ",", header=T, stringsAsFactors = T)
members <- fread('data/members_v3.csv', sep = ",", header=T, stringsAsFactors = T)
trans <- fread('data/transactions_v2.csv', sep = ",", header=T, stringsAsFactors = T)
testData<- fread('data/sample_submission_v2.csv', sep = ",", header=T, stringsAsFactors = T)
#ul <- fread('data/user_logs.csv', sep = ",", header=T, stringsAsFactors = T)

dim(trainData)#992931, 2
dim(trans) #1431009, 9

#Reformating 
#trainData <- trainData %>%mutate(is_churn = factor(is_churn))

#testData <- testData %>% mutate(is_churn = factor(is_churn))

trans <- trans %>%
  mutate(pay_met = factor(payment_method_id),
         auto_renew = factor(is_auto_renew),
         trans_date = ymd(transaction_date),
         exp_date = ymd(membership_expire_date))


members <- members %>%
  mutate(city = factor(city),
         gender = factor(gender),
         reg_via = factor(registered_via),
         reg_init = ymd(registration_init_time))

str(trainData)
str(trans)
table(trainData$is_churn)
#merging data
tr_data<- trainData %>%      
  inner_join(trans,by="msno")

str(tr_data)
dim(tr_data)#1134356

#creating test data from the dataset
tr_dataLength = nrow(tr_data)/2
trainSet = tr_data[(1:tr_dataLength),]
testSet = tr_data[((tr_dataLength+1):nrow(tr_data)),] 

dim(trainSet)#567178 10
dim(testSet)#567178 10
trainSet<-as.data.frame(trainSet)
head(trainSet)
head(testSet)
#summary of data
summary(trainData)
glimpse(trainData)
dim(trainData)

summary(members)
summary(trans)
glimpse(members)
dim(trainSet)

head(trainSet)
head(testSet)
str(trainSet)

sum(is.na(trainSet))
sum(is.na(testSet))
sum(is.na(trans))

# selecting few features only
columns = c("is_churn", "payment_plan_days", "plan_list_price","auto_renew",
            "trans_date", "exp_date", "is_cancel")
kk_train_data = trainSet %>%
  select(columns)

kk_test_data = testSet%>%
  select(columns)
head(kk_train_data)
#Question 1.
rf_model <-randomForest(as.factor(is_churn) ~ .,ntree=75, data=kk_train_data,importance=T)
rf_predict<-predict(rf_model,newdata=kk_train_data[-1], type="response")
table(rf_predict)

#Accuracy
accuracy = mean(kk_train_data$is_churn == rf_predict) 

table(kk_train_data[1])

#AUC, for train data
pred<-prediction(kk_train_data$is_churn,rf_predict)
AUC=performance(pred,"auc")
AUC@y.values[[1]]

#AUC for test data
rf_predict<-predict(rf_model,newdata=kk_test_data[-1], type="response")
pred<-prediction(kk_test_data$is_churn,rf_predict)
AUC=performance(pred,"auc")
AUC@y.values[[1]]

head(kk_test_data)

#Question 2
m <- model.matrix(~ is_churn +payment_plan_days +transaction_date+membership_expire_date+is_auto_renew +is_cancel,data = trainSet)

nn_model<-neuralnet(is_churn~ payment_plan_days +transaction_date+membership_expire_date+is_auto_renew +is_cancel,data=m, hidden=3,act.fct = "logistic",linear.output = FALSE)
plot(nn_model)

prob_nn_train <- neuralnet::compute(nn_model,subset(trainSet, select = c("payment_plan_days","transaction_date","membership_expire_date","is_auto_renew","is_cancel"))) 
prob_nn_test <- neuralnet::compute(nn_model,subset(testSet, select = c("payment_plan_days","transaction_date","membership_expire_date","is_auto_renew","is_cancel"))) 

result_train <- prob_nn_train$net.result 
result_test <- prob_nn_test$net.result 

detach(package:neuralnet,unload = T)

#AUC on Train Data
nn_pred <-ROCR::prediction(result_train, trainSet$is_churn) 
AUC <- performance(nn_pred,"auc")
AUC@y.values[[1]] 

#AUC on Test Data
nn_pred <-ROCR::prediction(result_test, testSet$is_churn) 
AUC <- performance(nn_pred,"auc")
AUC@y.values[[1]] 






