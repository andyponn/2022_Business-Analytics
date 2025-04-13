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
##將data更改為factor step2
airline$Gender <- ifelse(airline$Gender == "Male", 1, 0)
airline$Customer_Type <- ifelse(airline$Customer_Type == "Loyal Customer", 1, 0)
airline$Type_of_Travel <- ifelse(airline$Type_of_Travel== "Business travel", 1, 0)
airline$satisfaction <- ifelse(airline$satisfaction == "satisfied", 1, 0)

## Use the dummy variable to predict factor variable
library(fastDummies)
d2<-dummy_cols(airline)

#Delete original column and type_Eco
library(dummies)
##dummy 00 = type_Eco
d2<-d2[,c(-7,-27)]

colnames(d2) <- gsub(" ", "_", colnames(d2))

##訓練及測試集
library(tidyverse)
train_df <- d2 %>% group_by(satisfaction) %>% sample_frac(0.7)
test_df  <- anti_join(d2, train_df, by ='Index')

train_rf <- train_df[,3:26]
test_rf  <- test_df[,3:26]

#str(train_rf)
table(train_df$satisfaction)

##########logistic regression配適模型
logit_model <- glm(satisfaction ~ .,
                   train_rf, family=binomial(link="logit"))
summary(logit_model)
##用訓練出的模型來看test data的預測結果(result)
result <- predict(logit_model,test_rf, type = "response")
result
plot(result)
##########模型準確度
##計算threshold
library(InformationValue)
thres1=optimalCutoff(test_rf$satisfaction, result)

##預測結果正確率
d1 = confusionMatrix(test_rf$satisfaction, result, threshold = thres1)
(d1[1,1]+d1[2,2])/sum(d1)



##########2 非監督式學習
airline2 <- read_csv("airline_survey.csv")
airline2$Index<-c(1:103904)
airline2<-na.omit(airline2)

tinydf <- airline2  %>% sample_frac(0.01)

##決定分群k
library(cluster)
gap_stat <- clusGap(tinydf[,c(8,23,24)], FUN = kmeans, nstart = 50,
                    K.max = 10, B = 300)
fviz_gap_stat(gap_stat)

##
k = kmeans(tinydf[,c(8,23,24)], centers=2, nstart=50)
fviz_cluster(k, data = tinydf[,c(8,23,24)])


table(k$cluster, tinydf$satisfaction)
table(k$cluster, tinydf$`Customer Type`)
table(k$cluster, tinydf$Class)
table(k$cluster, tinydf$`Type of Travel`)
##########QQ監督式學習 Random Forest配適模型
##利用train_rf data來作配適模型
library(randomForest)
rd_model <- randomForest(satisfaction ~ ., data = train_rf, importance=TRUE) 
summary(rd_model$importance)
##用RF訓練出的模型來看test_rf data的預測結果(result.rf)
result.rf <- predict(rd_model,test_rf, type = "response")
result.rf
plot(result.rf)

