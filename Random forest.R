###############################################
##### AUTHOR : NAVEEN BALARAJU
##### UNIVERISTY AT BUFFALO
###############################################




age_data=read.csv("/Users/naveen/Desktop/project/Data_Age.csv")
## Feature selection
age_data[["Case.Control.Selected.on.10.22.2018"]]=NULL
age_data[["Date_of_Stool_Collection"]]=NULL
age_data[["Time_of_Stool_Collection"]]=NULL
age_data[["Data_Arrived_at_UB"]]=NULL
age_data[["Time_Arrived_at_UB"]]=NULL
age_data[["Date_put_in_Freezer"]]=NULL
age_data[["Time_put_in_Freezer"]]=NULL
age_data[["Person_who_Did_Tasks"]]=NULL
age_data[["Questionnaire_Revceived"]]=NULL
age_data[["Note"]]=NULL
age_data[["Column1"]]=NULL
age_data[["Issues_with_Questionnaire"]]=NULL



## To convert the column sample as the row names
row.names(age_data) = age_data$me_id
age_data[3]=NULL
dummy_data=age_data
str(age_data)

###############################################################################################################
########################################### Random forest Models ##############################################
###############################################################################################################

## CASE vs CONTROL
library(randomForest)
library(caret)
set.seed(123)

train=sample(1:nrow(age_data),0.7*nrow(age_data),replace = T)
rf.case.control =randomForest(ME_case_control~.,data=age_data,subset =train, importance =TRUE)
rf.case.control

pred.case.control = predict(rf.case.control, newdata=age_data[-train ,])
resp.test=age_data[-train ,"ME_case_control"]
caret::confusionMatrix(resp.test,pred.case.control)
#p-value: 1.267e-06
# accuracy :1 
#kappa : 1

## intermediate,control and advanced
rf.ICA =randomForest(AMD_ini_level_WS_c~.,data=age_data,subset =train, importance =TRUE)
rf.ICA

pred.ICA = predict(rf.ICA, newdata=age_data[-train ,])
resp.ICA=age_data[-train ,"AMD_ini_level_WS_c"]

caret::confusionMatrix(resp.ICA,pred.ICA)
# p value: 0.00185
# accuracy: 0.8095
# kappa : 0.7042


## Grouping control and intermediate into one class and advanced to other class
age_data[["AMD_ini_level_WS_c"]]=factor(ifelse(age_data$AMD_ini_level_WS_c=="Advanced AMD",1,2))
train=sample(1:nrow(age_data),0.7*nrow(age_data),replace = T)
rf.CIvsA =randomForest(AMD_ini_level_WS_c~.,data=age_data,subset =train, importance =TRUE)
rf.CIvsA

pred.CIvsA = predict(rf.CIvsA, newdata=age_data[-train ,])
resp.CIvsA=age_data[-train ,"AMD_ini_level_WS_c"]
caret::confusionMatrix(resp.CIvsA,pred.CIvsA)
# p value: 0.4049
# accuracy : 0.9
# kappa: 0.6078


## Eliminate intermediates and creating subsets to compare control vs advanced
ind=which(dummy_data$AMD_ini_level_WS_c=="Intermediate AMD")
dummy_data=dummy_data[-ind,]
dummy_data$AMD_ini_level_WS_c=factor(dummy_data$AMD_ini_level_WS_c) ## Re-ordering the levels
train=sample(1:nrow(dummy_data),0.7*nrow(dummy_data),replace = T)
rf.CvsA =randomForest(AMD_ini_level_WS_c~.,data=dummy_data,subset =train, importance =TRUE)
rf.CvsA

pred.CvsA = predict(rf.CvsA, newdata=dummy_data[-train ,])
resp.CvsA=dummy_data[-train ,"AMD_ini_level_WS_c"]
caret::confusionMatrix(resp.CvsA,pred.CvsA)
# pvalue: 0.03518
# accuracy : 1
# kappa: 1
