install.packages('ggplot2')
library(dplyr)
library(data.table)
library(ggplot2)
library(car)
library(adabag)
library(rpart)
library(caret)
library(foreach)
library(ROCR)
library(lubridate)
library(scales) 
library(grid) 
library(gridExtra) 
library(RColorBrewer) 
library(corrplot)

setwd("/Users/jasneekchugh/Desktop/DataScience/R-Programming/kk-box-churn-prediction")

#quickly import the prcocessed data
trainData <- fread('data/train_v2.csv', sep = ",", header=T, stringsAsFactors = T)
testData<- fread('data/sample_submission_v2.csv', sep = ",", header=T, stringsAsFactors = T)


#readfile
#tr <- fread("data/train.csv", sep = ",", header=T, stringsAsFactors = T)
members <- fread('data/members_v3.csv', sep = ",", header=T, stringsAsFactors = T)
trans <- fread('data/transactions_v2.csv', sep = ",", header=T, stringsAsFactors = T)
#ul <- fread('data/user_logs.csv', sep = ",", header=T, stringsAsFactors = T)

#summary of data
summary(trainData)
glimpse(trainData)

summary(members)
summary(trans)

sum(is.na(trainData))
sum(is.na(members))
sum(is.na(trans))

table(trainData$is_churn)
nrow(members[is.na(members$gender),])

#Reformating 
trainData <- trainData %>%
  mutate(is_churn = factor(is_churn))

trans <- trans %>%
  mutate(pay_met = factor(payment_method_id),
         auto_renew = factor(is_auto_renew),
         is_cancel = factor(is_cancel),
         trans_date = ymd(transaction_date),
         exp_date = ymd(membership_expire_date))

members <- members %>%
  mutate(city = factor(city),
         gender = factor(gender),
         reg_via = factor(registered_via),
         reg_init = ymd(registration_init_time))

#EDA
ggplot(data=trainData)+geom_bar(aes(x=is_churn, fill= is_churn))# The vast majority of users didn't churn
ggplot(data=members)+geom_bar(aes(x=gender, fill= gender))
ggplot(data=members)+geom_bar(aes(x=city, fill= city))




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

op# performance on test dataset
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

