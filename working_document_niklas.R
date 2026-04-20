# exploration script Niklas
library(tidyverse)

data <- read.csv("data/cronos3_wave1.csv", sep = ",", header = TRUE)

data$w1sq3 <- na_if(data$w1sq3, 99)
data$agegroup35 <- na_if(data$agegroup35, 9)
data$age <- na_if(data$age, 999)
data$young <- ifelse(data$age < 26, 1, 0)
data$old <- ifelse(data$age > 64, 1, 0)
data$gndr <- na_if(data$gndr, 9)

data |> 
  group_by(cntry, young) |> 
  summarise(mean = mean(w1sq3, na.rm = TRUE)) |> 
  print(n = 100)

data_young <- data[data$age < 26, ]

m1 <- lm(w1sq3 ~ hincfel + gndr + eduyrs + age, data = data)
summary(m1)

data$age_wght <- data$age * data$w5pspwght
data$hinctnta_wght <- data$hinctnta * data$w5pspwght
library(survey)

weights <- svydesign(ids = ~1, weights = ~w5pspwght, data = data)
data <- data[!is.na(data$w5pspwght), ]


mean(data$age_wght, na.rm = TRUE)
svymean(data$age, design = weights, na.rm = TRUE)
lm <- ""
m1 <- svyglm(hinctnta ~ age, design = weights)
m2 <- lm(hinctnta_wght ~ age_wght, data = data)
summary(m1)
summary(m2)

svylm

data |> 
  ggplot(aes(x = age, y = w1sq3)) +
  geom_point()

data |> 
  filter(cntry == "AT") |> 
  group_by(region) |> 
  summarise(m1 = mean(w5sq14), m2 = mean(w5sq15), m3 = mean(w5sq16))

data |> 
  group_by(cntry) |> 
  summarise(corr = cor(w1sq3, age, use = "complete.obs"))

data |> 
  ggplot(aes(x = age, y = w1sq3, col = cntry)) +
  geom_point()
