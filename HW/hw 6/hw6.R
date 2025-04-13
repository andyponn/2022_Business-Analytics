setwd("~/Downloads/1102 R/HW/hw 6")
library(readr)
library(tidyverse)
data <- read.csv("hw6-fb.csv")
summary(data)

##變數類別整理
data <- data[,-1]
data$visit_date <- as.Date(data$visit_date)
data$condition <- as.factor(data$condition)
data$clicked_article <- as.factor(data$clicked_article)
data$clicked_like <- as.factor(data$clicked_like)
data$clicked_share <- as.factor(data$clicked_share)
data$gender <- as.factor(data$gender)

summary(data)

## condition difference
data %>%
  group_by(condition) %>%
  summarise(time_spent = mean(time_spent_homepage_sec))

## gender difference
data %>%
  group_by(gender) %>%
  summarise(time_spent = mean(time_spent_homepage_sec))


## （文章觀看停留時間分析）Hypothesis Test:
### t-test for two sample mean
### Ha:  mu_1(tips) - mu_(tools) >0
t.test(data[data$condition == "tips", ]$time_spent_homepage_sec,
       data[data$condition == "tools", ]$time_spent_homepage_sec,
       alternative = "greater")
#### conclude H0, the p-value does not less than the significance at the level of 0.05
#### tools 與 tips 的寫作方式並沒有顯著差距影響讀者在頁面停留時間

##使用ggplot 畫圖展示
ggplot(data, aes(x = condition, y = time_spent_homepage_sec)) +
  geom_boxplot() +
  xlab("Condition") + ylab("time_spent_homepage_sec") +
  ggtitle("Boxplot of time_spent_homepage_sec by Condition") +
  theme_bw()



##2種condition對於文章按讚率分析
# condition-tips: proportion of like
like_tips <- data %>% filter(condition == "tips" & clicked_like == "1")
number_like_tips <- nrow(like_tips)
visitors_tips <- nrow(data %>% filter(condition == "tips"))
phat_like_tips <-  (number_like_tips/visitors_tips) 

# condition-tools: proportion of like
like_tools <- data %>% filter(condition == "tools" & clicked_like == "1")
number_like_tools <- nrow(like_tools)
visitors_tools <- nrow(data %>% filter(condition == "tools"))
phat_like_tools <-  (number_like_tools/visitors_tools) 

##計算tips的按讚率高於tools的多少
uplift <- (phat_like_tips - phat_like_tools)/ phat_like_tools * 100
uplift  #140.74%
#tips的按讚率 is better than tools by 140%. 

#pooled proportion of click like
p_pool <- (number_like_tips + number_like_tools)/(visitors_tips + visitors_tools) 
SE_pool<- sqrt(p_pool*(1-p_pool) * ((1/visitors_tips) + (1/visitors_tools)))
d_hat <- phat_like_tips - phat_like_tools #Point Estimate or Difference in proportion
z_score <- d_hat/SE_pool
p_value <- pnorm(q = -z_score, mean = 0, sd = 1) * 2


#Run a 2-sampled test
print("H0:  proportion of click like(tips) = proportion of click like(tools)")
print("H1:  proportion of click like(tips) > proportion of click like(tools)")

prop.test(c(number_like_tips, number_like_tools), c(visitors_tips,visitors_tools))
print("result p-value < 2.2e-16")
