install.packages('ggplot2')
library(dplyr)
library(data.table)
library(ggplot)
library(car)
library(adabag)
library(rpart)
require(caret)
library(foreach)
library(ROCR)


setwd("/Users/jasneekchugh/Desktop/DataScience/R-Programming/kk-box-churn-prediction")

#quickly import the prcocessed data
trainData <- fread('data/train_v2.csv', sep = ",", header=T, stringsAsFactors = T)
testData<- fread('data/sample_submission_v2.csv', sep = ",", header=T, stringsAsFactors = T)


#readfile
#tr <- fread("data/train.csv", sep = ",", header=T, stringsAsFactors = T)
#mb <- fread('data/members.csv', sep = ",", header=T, stringsAsFactors = T)
tn <- fread('data/transactions_v2.csv', sep = ",", header=T, stringsAsFactors = T)
#ul <- fread('data/user_logs.csv', sep = ",", header=T, stringsAsFactors = T)
head(ts)
class(mb)
table(tr$is_churn)

summary(trainData)
# combining all datasets in one
test <- inner_join(testData, tn)

data = inner_join(trainData, tn) # train data
# data = inner_join(data, membs) # i realized after training that I don't want to use these features
# data = inner_join(data, userlog)
head(data)
# Question 1.
# logistic regression on data. 
trainLength = nrow(data)/2
trainSet = data[1:trainLength,]
testSet = data[trainLength:1132036,] # the later half for test set as per question

# selecting few features only
cols = c("is_churn", "payment_plan_days", "plan_list_price" ,"actual_amount_paid", 
         "is_auto_renew", "transaction_date", "membership_expire_date", "is_cancel")
tr = select(trainSet, cols)
te = select(testSet, cols)

fit = glm(tr$is_churn ~ ., data = tr, family = "binomial")
op = predict(fit, newdata=te[-1], type="response")
op[op >= 0.5] = 1
op[op < 0.5] = 0

# performance on test dataset
accuracy = mean(te$is_churn == op) 
# 0.9351612

classification_error = 1 - accuracy
# 0.06483881

# AUC for test
op = predict(fit, newdata=te[-1], type="response")
auc = performance(prediction(op, te[1]), "auc")@y.values[[1]]
# 0.8067783

# Plotting AUC
plot(performance(prediction(op, te[1]), "tpr","fpr"))

