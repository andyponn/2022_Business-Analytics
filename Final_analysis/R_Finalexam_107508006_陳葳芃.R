###[0-資料清理]
setwd("~/Downloads/1102 R/Final")
library(readr)
data <- read_csv("Data E.csv")
summary(data)
#缺失值疑除
data<-na.omit(data)

##資料觀察
summary(data)
ls(data)
colnames(data) <- gsub(" ", "_", colnames(data))


###[1-入榜頻道類型分析]
#頻道類型走向建議
data %>%
  group_by(category)%>% 
  count()

###[2-訂閱數觀看數分析]
##看看subscribers(訂閱數)及video_views(影片觀看數)關係走向
library(tidyverse)
data %>%  
  ggplot(aes(x=subscribers, y=video_views)) +
  geom_point()

###[3-找出流量密碼]
##找出每個種類下訂閱數字最多的人
top_subscribers<-data %>%
  group_by(category)%>%
  slice(which.max(subscribers))
top_subscribers
##找出每個種類下觀看次數最多的人
top_video_views<-data %>%
  group_by(category)%>%
  slice(which.max(video_views))
top_video_views

###[4-頻道建立趨勢]
##畫圖來觀看頻道種類頻道建立趨勢
ggplot(data=data, aes(x=started, y=video_views, fill=category)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()
##因為1970值年代太久遠，所以將其視為離群值移除
delete<-which(data$started==1970)
data2 <- data[-delete,]
##畫圖來觀看頻道種類趨勢2.0
ggplot(data=data2, aes(x=started, y=video_views, fill=category)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()



