# exploration script Jonas
library(tidyverse)
library(corrplot)
## Countries and regions
table(data$cntry)

length(unique(data$region))

data[data$region == "AT31", ]
## 146 NUTS-coded regions of different level
# mean 83, min 6, max 721
mean(table(data$region))
min(table(data$region))
max(table(data$region))


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
  select(, w1sq1:w3sq74) %>% 
  cor(use = "complete.obs") %>% 
  corrplot()

corrplot()

ivs_corr

