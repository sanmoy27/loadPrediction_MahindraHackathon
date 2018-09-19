getwd()
setwd("C:/F/NMIMS/DataScience/hackathon/data")
library(dplyr)
library(tidyr)
library(stringr)
library(caTools)
library(ISLR)
library(glmnet)
library(caret)
library(class)
library(FactoMineR)
library(car)
library(ggpubr)

data_hck <- read.csv("A.csv", stringsAsFactors = FALSE, header = TRUE)
head(data_hck)
tail(data_hck)
dim(data_hck)
data_hck_dmm_1 <- select(data_hck, loan_amnt,funded_amnt,funded_amnt_inv,term,int_rate,installment,grade,sub_grade,emp_title,emp_length,home_ownership,annual_inc,verification_status,issue_d,pymnt_plan,zip_code,addr_state,dti,delinq_2yrs,earliest_cr_line,inq_last_6mths,mths_since_last_delinq,mths_since_last_record,open_acc,pub_rec,revol_bal,revol_util,total_acc,initial_list_status,total_pymnt,total_pymnt_inv,total_rec_prncp,total_rec_int,total_rec_late_fee,recoveries,collection_recovery_fee,last_pymnt_d,last_pymnt_amnt,next_pymnt_d,acc_now_delinq,delinq_amnt,pub_rec_bankruptcies,tax_liens,debt_settlement_flag,debt_settlement_flag_date,settlement_status,settlement_date,settlement_amount,settlement_percentage,settlement_term,default_flag, -settlement_percentage, -settlement_term, -settlement_amount, -mths_since_last_record, -inq_last_6mths, -pub_rec, -zip_code, -addr_state, -pymnt_plan, -sub_grade, -emp_title, -issue_d, -earliest_cr_line, -initial_list_status, -last_pymnt_d, -next_pymnt_d, -settlement_date, -settlement_status, -total_rec_prncp, -total_rec_prncp, -total_pymnt_inv, -debt_settlement_flag_date)
dim(data_hck_dmm_1)
str(data_hck_dmm_1)

cat("\014")
#Detect NAs
detectNAs<-function(x){
  return(sum(is.na(x)))
}
sapply(data_hck_dmm_1, detectNAs)



##Clean delinq_2yrs
summarise(group_by(data_hck_dmm_1, delinq_2yrs), n())
data_hck_dmm_1$delinq_2yrs[is.na(data_hck_dmm_1$delinq_2yrs)] <- 0
class(data_hck_dmm_1$delinq_2yrs)


#clean annual_inc
data_hck_dmm_1$annual_inc[is.na(data_hck_dmm_1$annual_inc)] <- median(data_hck_dmm_1$annual_inc, na.rm=TRUE)
class(data_hck_dmm_1$annual_inc)

#clean open_acc
summarise(group_by(data_hck_dmm_1, open_acc), n())
data_hck_dmm_1 %>% 
  count(open_acc) %>%
  arrange(desc(n)) %>% 
  group_by(open_acc)

data_hck_dmm_1$open_acc[is.na(data_hck_dmm_1$open_acc)] <- 6

#clean total_acc
summarise(group_by(data_hck_dmm_1, total_acc), n())
data_hck_dmm_1 %>% 
  count(total_acc) %>%
  arrange(desc(n)) %>% 
  group_by(total_acc)

data_hck_dmm_1$total_acc[is.na(data_hck_dmm_1$total_acc)] <- 16

cor(data_hck_dmm_1$open_acc, data_hck_dmm_1$total_acc,  method = "pearson", use = "complete.obs")

#open_acc and open_acc are correlated with a correlation coeff of 0.7, so open_acc is dropped
dim(data_hck_dmm_1)
data_hck_dmm_1 <- select(data_hck_dmm_1, -open_acc)


#clean delinq_amnt
summarise(group_by(data_hck_dmm_1, delinq_amnt), n())
data_hck_dmm_1$delinq_amnt[is.na(data_hck_dmm_1$delinq_amnt)] <- 0
data_hck_dmm_1 <- select(data_hck_dmm_1, -delinq_amnt)

#clean acc_now_delinq
summarise(group_by(data_hck_dmm_1, acc_now_delinq), n())
data_hck_dmm_1$acc_now_delinq[is.na(data_hck_dmm_1$acc_now_delinq)] <- 0
data_hck_dmm_1 <- select(data_hck_dmm_1, -acc_now_delinq)

#clean pub_rec_bankruptcies
summarise(group_by(data_hck_dmm_1, pub_rec_bankruptcies), n())
data_hck_dmm_1$pub_rec_bankruptcies[is.na(data_hck_dmm_1$pub_rec_bankruptcies)] <- 0
data_hck_dmm_1 <- select(data_hck_dmm_1, -pub_rec_bankruptcies)

#clean tax_liens
summarise(group_by(data_hck_dmm_1, tax_liens), n())
data_hck_dmm_1$tax_liens[is.na(data_hck_dmm_1$tax_liens)] <- 0
data_hck_dmm_1 <- select(data_hck_dmm_1, -tax_liens)


#clean mths_since_last_delinq
summarise(group_by(data_hck_dmm_1, mths_since_last_delinq), n())
data_hck_dmm_1 %>% 
  count(mths_since_last_delinq) %>%
  arrange(desc(n)) %>% 
  group_by(mths_since_last_delinq)

data_hck_dmm_1$mths_since_last_delinq[is.na(data_hck_dmm_1$mths_since_last_delinq)] <- 0


summarise(group_by(data_hck_dmm_1, term), n())
data_hck_dmm_1$term <- as.character(str_trim(data_hck_dmm_1$term))
data_hck_dmm_1$term[data_hck_dmm_1$term=="36 months"] <- 0
data_hck_dmm_1$term[data_hck_dmm_1$term=="60 months"] <- 1
data_hck_dmm_1$term <- as.numeric(data_hck_dmm_1$term)

summarise(group_by(data_hck_dmm_1, grade), n())
data_hck_dmm_1$grade <- as.factor(data_hck_dmm_1$grade)

data_hck_dmm_1$int_rate <- as.numeric(substr(data_hck_dmm_1$int_rate, 1, nchar(data_hck_dmm_1$int_rate)-1))

summarise(group_by(data_hck_dmm_1, home_ownership), n())
data_hck_dmm_1$home_ownership <- as.factor(data_hck_dmm_1$home_ownership)



#clean emp_length
summarise(group_by(data_hck_dmm_1, emp_length), n())
data_hck_dmm_1$emp_length[data_hck_dmm_1$emp_length=="n/a"]<-"< 1 year"
data_hck_dmm_1$emp_length<-as.factor(data_hck_dmm_1$emp_length)


summarise(group_by(data_hck_dmm_1, verification_status), n())
data_hck_dmm_1$verification_status<-as.factor(data_hck_dmm_1$verification_status)


summarise(group_by(data_hck_dmm_1, revol_util), n())
class(data_hck_dmm_1$revol_util)
data_hck_dmm_1$revol_util[str_trim(data_hck_dmm_1$revol_util)==""] <- "0%"
data_hck_dmm_1$revol_util <- as.numeric(substr(data_hck_dmm_1$revol_util, 1, nchar(data_hck_dmm_1$revol_util)-1))

summarise(group_by(data_hck_dmm_1, debt_settlement_flag), n())
data_hck_dmm_1$debt_settlement_flag<-as.factor(data_hck_dmm_1$debt_settlement_flag)

data_hck_dmm_1$delinq_2yrs <- as.factor(data_hck_dmm_1$delinq_2yrs)

#cor(data_hck_dmm_1$total_rec_int, data_hck_dmm_1$total_rec_late_fee,  method = "pearson", use = "complete.obs")
data_hck_dmm_1$total_pymnt <- scale(data_hck_dmm_1$total_pymnt)
data_hck_dmm_1$total_pymnt <- as.numeric(data_hck_dmm_1$total_pymnt)

data_hck_dmm_1$loan_amnt <- scale(data_hck_dmm_1$loan_amnt)
data_hck_dmm_1$loan_amnt <- as.numeric(data_hck_dmm_1$loan_amnt)

data_hck_dmm_1$funded_amnt <- scale(data_hck_dmm_1$funded_amnt)
data_hck_dmm_1$funded_amnt <- as.numeric(data_hck_dmm_1$funded_amnt)

data_hck_dmm_1$funded_amnt_inv <- scale(data_hck_dmm_1$funded_amnt_inv)
data_hck_dmm_1$funded_amnt_inv <- as.numeric(data_hck_dmm_1$funded_amnt_inv)

data_hck_dmm_1$int_rate <- scale(data_hck_dmm_1$int_rate)
data_hck_dmm_1$funded_amnt_inv <- as.numeric(data_hck_dmm_1$funded_amnt_inv)

data_hck_dmm_1$funded_amnt <- scale(data_hck_dmm_1$funded_amnt)
data_hck_dmm_1$funded_amnt_inv <- as.numeric(data_hck_dmm_1$funded_amnt_inv)

data_hck_dmm_1$installment <- scale(data_hck_dmm_1$installment)
data_hck_dmm_1$funded_amnt_inv <- as.numeric(data_hck_dmm_1$funded_amnt_inv)

data_hck_dmm_1$annual_inc <- scale(data_hck_dmm_1$annual_inc)
data_hck_dmm_1$annual_inc <- as.numeric(data_hck_dmm_1$annual_inc)

data_hck_dmm_1$dti <- scale(data_hck_dmm_1$dti)
data_hck_dmm_1$dti <- as.numeric(data_hck_dmm_1$dti)

data_hck_dmm_1$revol_bal <- scale(data_hck_dmm_1$revol_bal)
data_hck_dmm_1$revol_bal <- as.numeric(data_hck_dmm_1$revol_bal)

data_hck_dmm_1$revol_util <- scale(data_hck_dmm_1$revol_util)
data_hck_dmm_1$revol_util <- as.numeric(data_hck_dmm_1$revol_util)

data_hck_dmm_1$mths_since_last_delinq <- scale(data_hck_dmm_1$mths_since_last_delinq)
data_hck_dmm_1$mths_since_last_delinq <- as.numeric(data_hck_dmm_1$mths_since_last_delinq)

data_hck_dmm_1$total_acc <- scale(data_hck_dmm_1$total_acc)
data_hck_dmm_1$total_acc <- as.numeric(data_hck_dmm_1$total_acc)

data_hck_dmm_1$total_rec_int <- scale(data_hck_dmm_1$total_rec_int)
data_hck_dmm_1$total_rec_int <- as.numeric(data_hck_dmm_1$total_rec_int)

data_hck_dmm_1$last_pymnt_amnt <- scale(data_hck_dmm_1$last_pymnt_amnt)
data_hck_dmm_1$last_pymnt_amnt <- as.numeric(data_hck_dmm_1$last_pymnt_amnt)

data_hck_dmm_1$total_rec_late_fee <- scale(data_hck_dmm_1$total_rec_late_fee)
data_hck_dmm_1$total_rec_late_fee <- as.numeric(data_hck_dmm_1$total_rec_late_fee)

data_hck_dmm_1$last_pymnt_amnt <- scale(data_hck_dmm_1$last_pymnt_amnt)
data_hck_dmm_1$last_pymnt_amnt <- as.numeric(data_hck_dmm_1$last_pymnt_amnt)

data_hck_dmm_1$collection_recovery_fee <- scale(data_hck_dmm_1$collection_recovery_fee)
data_hck_dmm_1$collection_recovery_fee <- as.numeric(data_hck_dmm_1$collection_recovery_fee)

data_hck_dmm_1$recoveries <- scale(data_hck_dmm_1$recoveries)
data_hck_dmm_1$recoveries <- as.numeric(data_hck_dmm_1$recoveries)
data_hck_dmm_1$int_rate <- as.numeric(data_hck_dmm_1$int_rate)
data_hck_dmm_1$installment <- as.numeric(data_hck_dmm_1$installment)
data_hck_dmm_1$funded_amnt <- as.numeric(data_hck_dmm_1$funded_amnt)
data_hck_dmm_1$verification_status <- as.factor(data_hck_dmm_1$verification_status)

class(data_hck_dmm_1$default_flag)
summarise(group_by(data_hck_dmm_1, default_flag), n())
data_hck_dmm_1$default_flag[data_hck_dmm_1$default_flag==1]<-'Y'
data_hck_dmm_1$default_flag[data_hck_dmm_1$default_flag==0]<-'N'
data_hck_dmm_1$default_flag<-as.factor(data_hck_dmm_1$default_flag)

head(data_hck_dmm_1)
class(data_hck_dmm_1)
str(data_hck_dmm_1)
dim(data_hck_dmm_1)


data_hck_dmm_lasso<-data_hck_dmm_1
data_hck_dmm_lasso$grade <- as.character(data_hck_dmm_lasso$grade)
data_hck_dmm_lasso$home_ownership <- as.character(data_hck_dmm_lasso$home_ownership)
data_hck_dmm_lasso$emp_length <- as.character(data_hck_dmm_lasso$emp_length)
data_hck_dmm_lasso$debt_settlement_flag<-as.character(data_hck_dmm_lasso$debt_settlement_flag)
data_hck_dmm_lasso$verification_status<-as.character(data_hck_dmm_lasso$verification_status)

data_hck_dmm_lasso$default_flag<-as.character(data_hck_dmm_lasso$default_flag)
data_hck_dmm_lasso$default_flag[data_hck_dmm_lasso$default_flag=="Y"]<-1
data_hck_dmm_lasso$default_flag[data_hck_dmm_lasso$default_flag=="N"]<-0
data_hck_dmm_lasso$default_flag<-as.numeric(data_hck_dmm_lasso$default_flag)

x=model.matrix(default_flag~.,data_hck_dmm_lasso)[,-1]  ##qualitative variable to quantitative
y=data_hck_dmm_lasso$default_flag
grid=10^seq(10,-2,length=100)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
###### The Lasso ##################################
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
set.seed(1)

cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]



data_hck_dmm_final <- select(data_hck_dmm_1, loan_amnt,funded_amnt,term,int_rate,default_flag)
#Defining the training controls for multiple models
fitControl <- trainControl( method = "cv",  number = 5,   savePredictions = 'final',  classProbs = T)

#Detect Class
detectClass<-function(x){
  return(class(x))
}


cat("\014")
set.seed(100) # set seed to replicate results
split<-sample.split(data_hck_dmm_final$default_flag, SplitRatio=0.8)
trainSet<-subset(data_hck_dmm_final, split==TRUE)
testSet<-subset(data_hck_dmm_final, split==FALSE)
sapply(data_hck_dmm_final, detectNAs)
sapply(trainSet, detectClass)

### Fitting Random Forest
model_dt<-train(default_flag~.,data=trainSet,method='rpart', trControl=fitControl,tuneLength=3)

#Predicting using random forest model
pred_dt<-predict(model_dt, testSet)

#Checking the accuracy of the random forest model
confusionMatrix(testSet$default_flag,pred_rf)


data_hck_dmm_knn<-data_hck_dmm_final
#data_hck_dmm_knn$grade <- as.character(data_hck_dmm_knn$grade)
#data_hck_dmm_knn$home_ownership <- as.character(data_hck_dmm_knn$home_ownership)
#data_hck_dmm_knn$emp_length <- as.character(data_hck_dmm_knn$emp_length)
#data_hck_dmm_knn$debt_settlement_flag<-as.character(data_hck_dmm_knn$debt_settlement_flag)
#data_hck_dmm_knn$verification_status<-as.character(data_hck_dmm_knn$verification_status)
#library(dummies)
#data_hck_dmm_knn<-dummy.data.frame(data_hck_dmm_knn,names=c("grade", "home_ownership", "emp_length", "debt_settlement_flag", "verification_status"), sep="_")
#head(data_hck_dmm_knn)
#class(data_hck_dmm_knn$default_flag)




cat("\014")
set.seed(100) # set seed to replicate results
split<-sample.split(data_hck_dmm_knn$default_flag, SplitRatio=0.8)
trainSet_knn<-subset(data_hck_dmm_knn, split==TRUE)
testSet_knn<-subset(data_hck_dmm_knn, split==FALSE)
sapply(data_hck_dmm_knn, detectNAs)
sapply(testSet_knn, detectClass)
### Fitting KNN
model_knn<-train(default_flag~.,data=trainSet_knn,method='knn', trControl=fitControl,tuneLength=3)

#Predicting using random forest model
pred_knn<-predict(model_knn, testSet_knn)

#Checking the accuracy of the random forest model
confusionMatrix(testSet_knn$default_flag,pred_knn)




cat("\014")

### Fitting KNN
model_glm<-train(default_flag~.,data=trainSet,method='glm', family="binomial", trControl=fitControl,tuneLength=3)

#Predicting using random forest model
pred_glm<-predict(model_glm, testSet)

#Checking the accuracy of the random forest model
confusionMatrix(testSet$default_flag,pred_glm)


#Predicting the probabilities
pred_dt_prob<-predict(model_dt,newdata = testSet,type='prob')
pred_knn_prob<-predict(model_knn,newdata = testSet_knn,type='prob')
pred_glm_prob<-predict(model_glm,newdata = testSet,type='prob')

#Taking weighted average of predictions
pred_weighted_avg<-((pred_dt_prob$Y*0.5)+(pred_knn_prob$Y*0.2)+(pred_glm_prob$Y*0.3))

#Splitting into binary classes at 0.22
pred_weighted_avg<-as.factor(ifelse(pred_weighted_avg>0.22,'Y','N'))
pred_weighted_avg
table(pred_weighted_avg, testSet$default_flag)
confusionMatrix(testSet$default_flag,pred_weighted_avg)











#####################################Test ###############################################
data_hck_test <- read.csv("B.csv", stringsAsFactors = FALSE, header = TRUE)
head(data_hck_test)
tail(data_hck_test)
dim(data_hck_test)

summarise(group_by(data_hck_test, term), n())
data_hck_test$term <- as.character(str_trim(data_hck_test$term))
data_hck_test$term[data_hck_test$term=="36 months"] <- 0
data_hck_test$term[data_hck_test$term=="60 months"] <- 1
data_hck_test$term <- as.numeric(data_hck_test$term)

data_hck_test$int_rate <- as.numeric(substr(data_hck_test$int_rate, 1, nchar(data_hck_test$int_rate)-1))

data_hck_test_final <- select(data_hck_test, loan_amnt,funded_amnt,term,int_rate)
dim(data_hck_test_final)
sapply(data_hck_test_final, detectNAs)


#Predicting the probabilities
pred_dt_test_prob<-predict(model_dt,newdata = data_hck_test_final,type='prob')
pred_knn_test_prob<-predict(model_knn,newdata = data_hck_test_final,type='prob')
pred_glm_test_prob<-predict(model_glm,newdata = data_hck_test_final,type='prob')

#Taking weighted average of predictions
pred_testweighted_avg<-((pred_dt_test_prob$Y*0.5)+(pred_knn_test_prob$Y*0.2)+(pred_glm_test_prob$Y*0.3))

#Splitting into binary classes at 0.22
pred_testweighted_avg<-as.factor(ifelse(pred_testweighted_avg>0.22,1,0))
pred_testweighted_avg


########################## Validation Set ################################
data_hck_val <- read.csv("validation.csv", stringsAsFactors = FALSE, header = TRUE)
head(data_hck_val)
tail(data_hck_val)
dim(data_hck_val)
data_hck_val_final <- select(data_hck_val, loan_amnt,funded_amnt,term,int_rate)
dim(data_hck_val_final)

summarise(group_by(data_hck_val_final, term), n())
data_hck_val_final$term <- as.character(str_trim(data_hck_val_final$term))
data_hck_val_final$term[data_hck_val_final$term=="36 months"] <- 0
data_hck_val_final$term[data_hck_val_final$term=="60 months"] <- 1
data_hck_val_final$term <- as.numeric(data_hck_val_final$term)

data_hck_val_final$int_rate <- as.numeric(substr(data_hck_val_final$int_rate, 1, nchar(data_hck_val_final$int_rate)-1))

sapply(data_hck_val_final, detectNAs)


#Predicting the probabilities
pred_dt_val_prob<-predict(model_dt,newdata = data_hck_val_final,type='prob')
pred_knn_val_prob<-predict(model_knn,newdata = data_hck_val_final,type='prob')
pred_glm_val_prob<-predict(model_glm,newdata = data_hck_val_final,type='prob')

#Taking weighted average of predictions
pred_valweighted_avg<-((pred_dt_val_prob$Y*0.5)+(pred_knn_val_prob$Y*0.2)+(pred_glm_val_prob$Y*0.3))

#Splitting into binary classes at 0.22
pred_valweighted_avg<-as.factor(ifelse(pred_valweighted_avg>0.22,1,0))
pred_valweighted_avg
