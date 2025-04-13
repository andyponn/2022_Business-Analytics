#HW3

setwd("~/Downloads/1102 R/HW/hw 3")

library(readr)
airline <- read_csv("airline_survey.csv")
airline$Index<-c(1:103904)

colnames(airline) <- gsub("/", "__", colnames(airline))
colnames(airline) <- gsub("-", "", colnames(airline))
colnames(airline) <- gsub(" ", "_", colnames(airline))

ls(airline)
airline<-na.omit(airline)

str(airline)
summary(airline)

##將data更改為factor step1
airline$satisfaction<-as.factor(airline$satisfaction)
airline$Gender<-as.factor(airline$Gender)
airline$Customer_Type<-as.factor(airline$Customer_Type)
airline$Type_of_Travel<-as.factor(airline$Type_of_Travel)
airline$Class<-as.factor(airline$Class)

## Use the dummy variable to predict factor variable
library(fastDummies)
data<-dummy_cols(airline)

#Delete original column and others（避免共線性）
ls(data)
d2<-data[ , -which(names(data) %in% c("Gender","Gender_Female","Type_of_Travel","Type_of_Travel_Personal Travel","satisfaction","satisfaction_neutral or dissatisfied","Class","Class_Eco",
                                  "Customer_Type_disloyal Customer","Customer_Type"))]
colnames(d2) <- gsub(" ", "_", colnames(d2))
summary(d2)

#變成factor，避免Random forest 噴錯（把類別型當成回歸去跑）
d2$satisfaction_satisfied<-as.factor(d2$satisfaction_satisfied)
d2$Class_Eco_Plus<-as.factor(d2$Class_Eco_Plus)
d2$Class_Business<-as.factor(d2$Class_Business)
d2$Type_of_Travel_Business_travel<-as.factor(d2$Type_of_Travel_Business_travel)
d2$Customer_Type_Loyal_Customer<-as.factor(d2$Customer_Type_Loyal_Customer)
d2$Gender_Male<-as.factor(d2$Gender_Male)
d2$id<-as.character(d2$id)


##訓練及測試集（縮減資料數量，怕跑不動）
data_tiny<-d2[sample(nrow(d2), 3000), ]

library(tidyverse)
train_df <- data_tiny %>% group_by(satisfaction_satisfied) %>% sample_frac(0.7)
test_df  <- anti_join(data_tiny, train_df, by ='Index')

train_rf <- train_df[,3:26]
test_rf  <- test_df[,3:26]

#str(train_rf)
table(train_df$satisfaction_satisfied)

##監督式學習 Random Forest配適模型

##利用train_rf data來作配適模型
library(randomForest)
rd_model <- randomForest(satisfaction_satisfied ~ ., data = train_rf, importance=TRUE) 
summary(rd_model$importance)
##用RF訓練出的模型來看test_rf data的預測結果(result.rf)
result.rf <- predict(rd_model,test_rf, type = "response")
result.rf
plot(result.rf)

##預測結果正確率
d2 = table(Predict = result.rf , Real= test_rf$satisfaction_satisfied)
(d2[1,1]+d2[2,2])/sum(d2)


##logistic regression配適模型
logit_model <- glm(satisfaction_satisfied ~ .,
                   train_rf, family=binomial(link="logit"))
summary(logit_model)
##用訓練出的模型來看test data的預測結果(result)
result <- predict(logit_model,test_rf, type = "response")
result
plot(result)
##########模型準確度
##計算threshold
library(InformationValue)
thres1=optimalCutoff(test_rf$satisfaction_satisfied, result)

##預測結果正確率
d1 = confusionMatrix(test_rf$satisfaction_satisfied, result, threshold = thres1)
(d1[1,1]+d1[2,2])/sum(d1)

