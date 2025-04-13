#資料financialdata.csv 有163間公司的財務指標，
setwd("~/Downloads/1102 R/HW/hw 4")

library(readr)
data.fin <- read.csv("financialdata.csv")
##########1.請先將資料探索，計算變數間correlation，並畫熱圖呈現。解釋哪個變數高度正相關或負相關。

##先將data變數整理為numeric
str(data.fin)
data.fin <- data.fin[,-1]
data.fin$op_profit_growth_rate <- as.numeric(data.fin$op_profit_growth_rate)
data.fin$current_ratio <- as.numeric(data.fin$current_ratio)
data.fin$quick_rartio <- as.numeric(data.fin$quick_rartio)


data.fin <- na.omit(data.fin)


##計算correlation
M = cor(data.fin)

##heatmap for correlation
library(tidyverse)
library(reshape2)
melted_cormat <- melt(M)
head(melted_cormat)

ggplot(data = melted_cormat,
       aes(Var1, Var2)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "firebrick4", high = "steelblue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Correlation")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())

##########2.以PCA或SPCA分析，找出每個主成份能解釋多少變異？大概需要多少個PC來解釋這筆資料？
library(stats)
pca<- prcomp(data.fin, center = TRUE, scale = TRUE)
names(pca) 
summary(pca)

##大概需要多少5個PC (eigenvalue>1)
plot(pca) #Variation	explained
abline(h=1, col="red")

##########3.找出前三個主成份分別重點變數為何並解釋。
pca.factor <- pca$rotation[,1:3]
pca.factor[,1:3]

#######################
pc_var=list()
for (i in 1:3){
  order=order(abs(pca.factor[,i]),decreasing = TRUE)
  pc_var[[i]]=pca.factor[,i][order][1:3]
}

pc_var[1]
pc_var[2]
pc_var[3]

ggplot(melt(pca.factor[,1:3]), aes(Var2, Var1)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "firebrick4", high = "steelblue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Coefficient")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())

##########4.找出適合投資的公司。
biplot(pca,scale = T,choices = c(1,3))
