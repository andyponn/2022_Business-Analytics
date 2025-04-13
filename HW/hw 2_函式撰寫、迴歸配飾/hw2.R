#HW2

setwd("~/Downloads/1102 R/HW/hw 2")
#1a
##生成一筆資料
data <- data.frame(
  a = sample(1:10,20,replace=T),
  ε = rnorm(20,mean=0,sd=2))
##觀察sum（用來檢查ifelse的結果）
library(tidyverse)
data<- data%>%
  mutate(sum= a+ε)
##利用ifelse來確保X會落在0-11區間
data$X <- ifelse( data$a + data$ε >= 11, 11, 
                        ifelse( 0 <= data$a + data$ε, data$a + data$ε ,0))
##所求
X=data$X
head(X)

#1b
##Cauchy density function 取log及一階導數的function
Lcauchy <- function(theta){
  k = 0
  for (i in 1:length(X)){
    k <- k+(theta-X[i])/(1+(theta-X[i])^2)
  }
  return(-2*k)}

Lcauchy(theta)

#1c
##令theta=0.3，代入先前的X data
Lcauchy(0.3)



#2
##載入資料
houseprice.df <- read.csv("houseprice.csv")
##觀察資料並看看年份有沒有異常值
str(houseprice.df)
#2a
##方便操作
library(tidyverse)
houseprice.df<- mutate(houseprice.df, year_type=Build_year)
range(houseprice.df$year_type)
attach(houseprice.df)

##根據題目更改的條件
houseprice.df$year_type<- ifelse( year_type <= 1899 , "centennial" , 
          ifelse( 1960 <= year_type , "new","old"))
head(houseprice.df)

#################################
## Double check有沒有轉錯，不要跑

#which(year_type <= 1899)
#which(1960 <= year_type)

attach(houseprice.df)
which(year_type=="centennial")
which(year_type=="new")
which(year_type=="old")
#################################

## Preparation to change the variable type into factor variable
houseprice.df$Town<-as.factor(houseprice.df$Town)
houseprice.df$University<-as.factor(houseprice.df$University)
houseprice.df$year_type<-as.factor(houseprice.df$year_type)
houseprice.df$Type<-as.factor(houseprice.df$Type)

summary(houseprice.df)

## Find the each factor name
levels(houseprice.df$Type)
levels(houseprice.df$Town)
levels(houseprice.df$University)
levels(houseprice.df$year_type)


## Classify the area into short name
library(dplyr)
houseprice.df<-houseprice.df %>% separate(Town, c("TownF", "TownS",sep=","))

## check the data
str(houseprice.df)
## Delete date, build year, TownF and NA column
df<-houseprice.df[,c(-3,-9,-10,-12)]
str(df)
df$TownS<-as.factor(df$TownS)
levels(df$TownS)

## Use the dummy variable to predict factor variable
library(dummies)
dummies=dummy.data.frame(df)
str(dummies)

##Delete the one dummy of different factor 
#SingleFamily 
#AL 
#University ofWest Virginia 
#yeartype-old
dummies1.2<-dummies[,c(-9,-10,-99,-102)]


## SPLIT THE DATA INTO TRAINING AND TESTING 
train_df1 <- dummies1.2 %>% sample_frac(0.7)
test_df1  <- anti_join(dummies1.2, train_df1, by = 'Record')

which(is.na(train_df1))

train_df1 <-train_df1[,-1]
test_df1  <-test_df1[,-1] 
#Regression model
m1 <- lm(Sale_amount~ ., data=train_df1)
summary(m1)

ls(train_df1)

## Adjusted model
## Delete date, TownF, NA column and year type
df2<-houseprice.df[,c(-3,-10,-12,-14)]
str(df2)
df2$TownS<-as.factor(df2$TownS)
levels(df2$TownS)

dummies2.1=dummy.data.frame(df2)
str(dummies)

##Delete the one dummy of different factor 
#SingleFamily 
#AL 
#University ofWest Virginia 
dummies2.2<-dummies2.1[,c(-9,-11,-100)]

## SPLIT THE DATA INTO TRAINING AND TESTING 
train_df2 <- dummies2.2 %>% sample_frac(0.7)
test_df2  <- anti_join(dummies2.2, train_df2, by = 'Record')

which(is.na(train_df2))

train_df2 <-train_df2[,-1]
test_df2  <-test_df2[,-1]

#Regression model 2
m2 <- lm(Sale_amount~ ., data=train_df2)
summary(m2)


##plot
layout(matrix(c(1,2,3,4),2,2))
plot(m1)
plot(m2)


## test
predict=predict(m1,test_df1[,-1])
predict=predict(m1,test_df1)
RMSE=sqrt(mean(sum((test_df1$Sale_amount-predict)^2)))
RMSE

predict2=predict(m2,test_df2[,-1])
RMSE=sqrt(mean(sum((test_df2$Sale_amount-predict2)^2)))
RMSE





m2 <- lm(Sale_amount~ Beds+Baths+Sqft_home+Sqft_lot+Type+year_type, data=train_df)
summary(m2)



m3 <- lm(Sale_amount~ Beds+Baths+Sqft_home+Sqft_lot+Build_year+TownS+University, data=train_df)
summary(m3)

m4 <- lm(Sale_amount~ Beds+Baths+Sqft_home+Sqft_lot+Build_year+TownS, data=train_df)
summary(m3)


#6
layout(matrix(c(1,2,3,4),2,2))
plot(model)
####################
attach(houseprice.df)
plot(houseprice.df$Town,houseprice.df$Sale_amount)
plot(houseprice.df$University,houseprice.df$Sale_amount)
plot(houseprice.df$Type,houseprice.df$Sale_amount)

houseprice.df %>%
  filter(Type=="Multi Family")%>%
  summarise(boxplot(houseprice.df$Sale_amount))


plot(houseprice.df$year_type,houseprice.df$Sale_amount)

 


houseprice.df %>%
  group_by(year_type) %>%
  summarize(max.Sale_amount = which.max(Sale_amount), median.Sales_amount=mean(Sale_amount))

houseprice.df %>%
  group_by(Town) %>%
  summarize(max.Sale_amount = which.max(Sale_amount), median.Sales_amount=mean(Sale_amount))
houseprice.df %>%
  group_by(University) %>%
  summarize(max.Sale_amount = which.max(Sale_amount), median.Sales_amount=mean(Sale_amount))

##########
which.max(houseprice.df$Sale_amount & houseprice.df$Type=="Multi Family")


houseprice.df%>%
  filter(houseprice.df$Type=="Multi Family")%>%
  mean(Sale_amount)
##########





#fail
houseprice.df<- mutate(houseprice.df, place2=Beds+Baths  )
houseprice.df<- mutate(houseprice.df, place2=Sqft_home+Sqft_lot  )

library(corrplot)

cor=cor(houseprice.df[,c(2,4:7,9)])
cor
pairs(houseprice.df[,c(2,4:7,13)])




#############################
str(houseprice.df)
houseprice.df$year_type<-factor(year_type)

###為啥不行！？
houseprice.df$year_type<- ifelse( year_type <= 1899 , "centennial" , "N")

houseprice.df$year_type<- houseprice.df%>%
  ifelse( year_type <= 1899 , "centennial" ,
          ifelse( 1960 <= year_type , "new","old"))
