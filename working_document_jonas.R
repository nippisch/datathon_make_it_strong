# exploration script Jonas
library(tidyverse)
library(corrplot)
library(sf)


data[data$region == "AT31", ]
## 146 NUTS-coded regions of different level
# mean 83, min 6, max 721
mean(table(data$region))
min(table(data$region))
max(table(data$region))

# Maybe move to preprocessing? 
data$hincfel <- factor(data$hincfel, 
                       levels = c(1:4, 7, 8, 9),
                       labels = c(
                         "Living comfortably on present income",
                         "Coping on present income",
                         "Difficult on present income",
                         "Very difficult on present income",
                         "Refusal",
                         "Don't know",
                         "No answer"))

# Weights
sum(is.na(data$w1pspwght))
sum(is.na(data$w2pspwght)) # least (1705) NAs in wave 2 weights 
sum(is.na(data$w3pspwght))
sum(is.na(data$w4pspwght))
sum(is.na(data$w5pspwght))

weights_df <- data.frame(
  id = data$idno,
  w1 = data$w1pspwght, 
  w2 = data$w2pspwght, 
  w3 = data$w3pspwght,
  w4 = data$w4pspwght,
  w5 = data$w5pspwght
)

weights_df$mean_w <- rowMeans(weights_df[2:6], na.rm = TRUE)

View(weights_df)
# check max diff

# Observations with only NA weights?
count <- 0
for (row in 1:nrow(data)){
  if (is.na(data$w1pspwght[row])){
    if (is.na(data$w3pspwght[row])){
      count <- count + 1
    }
  }
}
# 827 missings in BOTH

for (row in 1:nrow(data)){
  if (is.na(data$w1pspwght[row])){
    if (is.na(data$w3pspwght[row])){
      count <- count + 1
    }
  }
}

# 8742 NO missing AT ALL 
count_2 <- 0
for (row in 1:nrow(data)){
  if (!is.na(data$w1pspwght[row])){
    if (!is.na(data$w3pspwght[row])){
      count_2 <- count_2 + 1
    }
  }
}
nrow(data)
nrow(data) - count_2
count_2

## Analysis of satisfaction with level of education I have reached (w1sq3)
# eduyrs, eisced, age, gndr, hincfel, hinctnta
cor_matrix <- select(data)

ivs_corr <- data %>% 
  select(
    eduyrs, 
    age, 
    eisced, 
    hincfel, hinctnta, 
    everyfair, ifair, 
    edu_satisf, edu_match) %>% 
  cor(use = "complete.obs") %>% 
  corrplot()

# 
summary(data$edu_satisf)
cor(data$edu_satisf, data$edu_match, use = "complete.obs")
cor(data$ifair, data$edu_satisf, use = "complete.obs")
cor(data$ifair, data$everyfair, use = "complete.obs")
cor(data$everyfair, data$edu_satisf, use = "complete.obs")

# Education level and income decile / feeling
cor(data$hinctnta, data$eisced, "complete.obs")
cor(data$hincfel, data$eisced, "complete.obs")

# 



ml <- lm(formula = "")
