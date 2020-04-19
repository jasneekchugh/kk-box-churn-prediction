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

#setwd("/Users/jasneekchugh/Desktop/DataScience/R-Programming/kk-box-churn-prediction")

#reading the required files
trainData <- fread('data/train_v2.csv', sep = ",", header=T, stringsAsFactors = T)
testData<- fread('data/sample_submission_v2.csv', sep = ",", header=T, stringsAsFactors = T)
members <- fread('data/members_v3.csv', sep = ",", header=T, stringsAsFactors = T)
trans <- fread('data/transactions_v2.csv', sep = ",", header=T, stringsAsFactors = T)
#ul <- fread('data/user_logs.csv', sep = ",", header=T, stringsAsFactors = T)

#summary of data
summary(trainData)
glimpse(trainData)

summary(members)
summary(trans)
glimpse(members)

sum(is.na(trainData))
sum(is.na(members))
sum(is.na(trans))

table(trainData$is_churn)
#Reformating 
trainData <- trainData %>%
  mutate(is_churn = factor(is_churn))

testData <- testData %>%
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
ggplot(data=members)+geom_bar(aes(x=reg_via, fill= reg_via))

# combining all datasets in one
tr_data<- trainData %>%      
  inner_join(trans,by="msno") %>%
  inner_join(members,by="msno")  #Inner join of trans data and members data to Train data

te_data<- testData %>%
  inner_join(trans,by="msno") %>%
  inner_join(members,by="msno")

length( unique(tr_data$msno) ) # to make sure the data is joined correctly
table(tr_data$is_churn)
sum(is.na(tr_data)) #to check if any NA's are present in the data
length(tr_data$msno)

xtabs(~is_churn+gender,data=tr_data)

# Question 1.
# logistic regression on data. 
tr_dataLength = nrow(tr_data)/2
trainSet = tr_data[1:tr_dataLength,]

testSet = tr_data[tr_dataLength:length(tr_data[,1]),] 

# selecting few features only
columns = c("is_churn", "payment_plan_days", "plan_list_price" ,"city","auto_renew",
         "trans_date", "exp_date", "is_cancel","reg_via")
kk_train_data = trainSet %>%
              select(columns)
kk_test_data = testSet%>%
          select(columns)

sum(is.na(kk_train_data$is_churn))#to make sure there are no NA's in the data
sum(is.na(kk_test_data$is_churn))

#model
churn_model = glm(is_churn ~ ., data = kk_train_data, family = binomial(link="logit"))
churn_prob<-predict(churn_model, newdata=kk_test_data[-1], type="response")
churn_pred <- ifelse(prob>=0.5,1,0)

table(churn_pred)

# Performance on test dataset
#Accuracy
accuracy = mean(kk_test_data$is_churn == churn_pred) 
accuracy
#Classification Error, it's calculated as 1-Accuracy 
class_error = 1 - accuracy
class_error
# Plotting ROC
pred<-prediction(churn_prob, kk_test_data[1])
plot(performance(pred, "tpr","fpr"), colorize=TRUE)

#AUC- Area under the curve
AUC=performance(pred,"auc")
AUC@y.values[[1]]

# Question 2.
columns = c("is_churn", "payment_plan_days", "plan_list_price" ,"city","auto_renew",
            "trans_date", "exp_date", "is_cancel","reg_via")
tr = tr_data %>% select( columns) %>% 
  sample_n(1000)
model<- train(is_churn ~ ., data = tr, method="rf", trControl=trainControl(method="cv", number=5,  verboseIter =TRUE))
model
 #the model was taking way too long to run the cross validation and resulted in intteruption
 #of R studio. Therefore, i have sampled the data to 1000 records and run cross validation on it
#Question3
columns = c("is_churn", "payment_plan_days", "plan_list_price" ,"city","auto_renew",
            "trans_date", "exp_date", "is_cancel","reg_via")

kk_tr_data = tr_data %>%  select(columns)
kk_te_data = te_data %>%  select(columns)

churn_model = glm(is_churn ~ ., data = kk_tr_data, family = binomial(link="logit"))
churn_prob<-predict(churn_model, newdata=kk_te_data[-1], type="response")
churn_pred <- ifelse(prob>=0.5,1,0)

kk_churn_prob<- data.frame(te_data, churn_prob)
kk_churn_prob<- kk_churn_prob %>% select(c("msno", "churn_prob"))
kk_churn_prob<- distinct(kk_churn_prob, msno, .keep_all = TRUE) 
write.csv(kk_churn_prob, "kk_churn_prob.csv", row.names = FALSE)  

  
 

