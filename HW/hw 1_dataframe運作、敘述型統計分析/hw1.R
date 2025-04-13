#hw1
setwd("~/Downloads/1102 R/hw1")
sales.df <- read.csv("salesdata.csv")
prod.df <- read.csv("product_list.csv")
client.df <- read.csv("client_list.csv")

#Q1
library(tidyverse)
prod.df<- prod.df %>% 
  separate(Item, 
           into=c("Product", "Item"),
           sep = "_")

#Q2
sales.df$Product<- as.character(sales.df$Product)
full.table <- sales.df %>% 
  left_join(client.df, by = "Client") %>%  
  left_join(prod.df, by="Product")

#Q3
full.table<- full.table %>%
  mutate( spend = UnitPrice * Quantity )

#Q4
##資料進行分組
group1<-full.table %>%
  filter( Membership == "gold" | Membership =="diamond")
group2 <- full.table %>%
  filter( Membership != "gold" & Membership !="diamond") 

#平均年紀比較
group1 %>%
  summarise(mean(Age))
group2 %>%
  summarise(mean(Age))
#性別分布比較
group1 %>%
  group_by(Gender)%>%
    summarise(length(Gender)) 
group2 %>%
  group_by(Gender)%>%
  summarise(length(Gender)) 
#國家比較
group1 %>%
  group_by(Region)%>%
  summarise(length(Region)) 
group2 %>%
  group_by(Region)%>%
  summarise(length(Region))
#消費情況差異
group1 %>%
  summarise(mean(spend)) 
group1 %>%
  group_by(Item)%>%
  summarise(length(Item))

group2 %>%
  summarise(mean(spend)) 
group2 %>%
  group_by(Item)%>%
  summarise(length(Item))

#Q5
group_female<-full.table %>%
  filter( Gender == "female" )
#平均年紀
group_female%>%
  summary()
#國家
group_female %>%
  group_by(Region)%>%
  summarise(length(Region))
#消費情形
group_female %>%
  group_by(Membership)%>%
  count(Region)
##在不同產品的「總消費」畫圖分析
library(cowplot)
ii <- group_female %>%  
  ggplot(aes(x=Item, y=spend ,color=Item,shape=Item)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  geom_point(size=3) +
  theme_cowplot()
ggdraw(ii) + 
  draw_image("foto.jpg", x = 1, y = 1, width = 0.2, height = 0.2,hjust = 1, vjust = 1)
