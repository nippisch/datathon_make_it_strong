# exploration script Niklas (script was just for personal use and is not intendent to work on it's own)
library(tidyverse)
library(survey)
library(lme4)
library(lmerTest)
library(performance)
library(see)
library(broom)

data <- read.csv("data/cronos3_all.csv", sep = ",", header = TRUE)
load(file = "data_covariates_region.RData")

# defining missings
data$agegroup35 <- na_if(data$agegroup35, 9)
data$age <- na_if(data$age, 999)
data$w1sq2 <- na_if(data$w1sq2, 99)
data$w1sq3 <- na_if(data$w1sq3, 99)
data$gndr <- na_if(data$gndr, 9)
data$hincfel <- na_if(data$hincfel, 7)
data$hincfel <- na_if(data$hincfel, 8)
data$hincfel <- na_if(data$hincfel, 9)
data$eduyrs <- na_if(data$eduyrs, 77)
data$eduyrs <- na_if(data$eduyrs, 88)
data$eduyrs <- na_if(data$eduyrs, 99)
data$w3sq74 <- na_if(data$w3sq74, 6)
data$w3sq74 <- na_if(data$w3sq74, 9) 
data$w3dq53 <- na_if(data$w3dq53, 6)
data$w3dq53 <- na_if(data$w3dq53, 9)

# creating new variables
data$incbad <- ifelse(data$hincfel < 3, 1, 0)

# survey design
design_w5 <- svydesign(ids = ~1, weights = ~w5pspwght, data = data_w5)

# exploratory analyses

data |> 
  group_by(age) |> 
  summarise(mean_fairness = mean(ifair, na.rm = TRUE)) |> 
  ggplot(aes(x = age, y = mean_fairness)) +
  geom_point() +
  geom_smooth()

data |> 
  group_by(cntry, age) |> 
  summarise(mean_fairness = mean(w1sq3, na.rm = TRUE)) |> 
  ggplot(aes(x = age, y = mean_fairness)) +
  geom_point() +
  geom_smooth()



data |> 
  select(cntry, age, ifair) |> 
  group_by(cntry, age) |> 
  summarise(mean_w1sq2 = mean(ifair, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(x = age, y = mean_w1sq2)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~cntry)

data |> 
  group_by(w3sq74) |> 
  summarise(mean_fair = mean(w1sq2, na.rm = TRUE), mean_achieved = mean(w1sq3, na.rm = TRUE))

data |> 
  group_by(w3sq74) |> 
  summarise(mean_fair = mean(w1sq2, na.rm = TRUE)) |> 
  ggplot(aes(x = w3sq74, y = mean_fair)) +
  geom_point()

data$relate <- ifelse(data$w3sq74 == 3, 1, ifelse(data$w3sq74 < 3, 0, NA))

data |> 
  select(cntry, age, ifair) |> 
  group_by(cntry, age) |> 
  summarise(mean_ifair = mean(ifair, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(x = age, y = mean_ifair)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~cntry)

data |> 
  select(cntry, gndr, ifair) |> 
  group_by(cntry, gndr) |> 
  summarise(mean_ifair = mean(ifair, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(x = gndr, y = mean_ifair)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~cntry)

data |> 
  select(cntry, eduyrs, ifair) |> 
  group_by(cntry, eduyrs) |> 
  summarise(mean_ifair = mean(ifair, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(x = eduyrs, y = mean_ifair)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~cntry)

data |> 
  select(cntry, edu_match, ifair) |> 
  group_by(cntry, edu_match) |> 
  summarise(mean_ifair = mean(ifair, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(x = edu_match, y = mean_ifair)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~cntry)

data |> 
  select(cntry, inc_diff, ifair) |> 
  group_by(cntry, inc_diff) |> 
  summarise(mean_ifair = mean(ifair, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(x = inc_diff, y = mean_ifair)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~cntry)

data |> 
  select(cntry, ifair, edu_satisf) |> 
  group_by(cntry, edu_satisf) |> 
  summarise(mean_ifair = mean(ifair, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(x = edu_satisf, y = mean_ifair)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~cntry)

data |> 
  select(cntry, hincfel, ifair) |> 
  group_by(cntry, hincfel) |> 
  summarise(mean_ifair = mean(ifair, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(x = hincfel, y = mean_ifair)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~cntry)

data |> 
  select(cntry, w5hq5, ifair) |> 
  group_by(cntry, w5hq5) |> 
  summarise(mean_ifair = mean(ifair, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(x = w5hq5, y = mean_ifair)) +
  geom_line() +
  facet_wrap(~cntry)

data |> 
  select(cntry, w3dq53, ifair) |> 
  group_by(cntry, w3dq53) |> 
  summarise(mean_ifair = mean(ifair, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(x = w3dq53, y = mean_ifair)) +
  geom_line() +
  facet_wrap(~cntry)

data |> 
  select(cntry, w4dq8, ifair) |> 
  group_by(cntry, w4dq8) |> 
  summarise(mean_ifair = mean(ifair, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(x = w4dq8, y = mean_ifair)) +
  geom_line() +
  facet_wrap(~cntry)

data |> 
  select(age_cat, w5hq5, ifair) |> 
  group_by(age_cat, w5hq5) |> 
  summarise(mean_ifair = mean(ifair, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(x = w5hq5, y = mean_ifair)) +
  geom_line() +
  facet_wrap(~age_cat)


# mixed effects model
data$w5hq5 <- na_if(data$w5hq5, 9)
data$relate <- ifelse(data$edu_match == 3, 1, ifelse(data$edu_match < 3, 0, NA))
data$inc_diff <- ifelse(data$hincfel > 2, 1, 0)
data$felt_safe <- ifelse(data$w5hq5 == 1, 1, 0)
data$region <- substr(data$region, 1, 4)
data_all <- left_join(data, data_covariates_region, by = "region")

data_clean_lmm <- data_all |> 
  filter(!is.na(edu_satisf) & !is.na(age) & !is.na(eduyrs) & !is.na(relate) & !is.na(ifair) & !is.na(inc_diff) & !is.na(felt_safe) & 
           !is.na(early_leave) & !is.na(poverty_rate) & !is.na(youth_unemployment) & !is.na(cntry) & !is.na(region) & !is.na(w1pspwght))
m1 <- lmer(ifair ~ age + gndr + eduyrs + relate + edu_satisf + inc_diff + felt_safe + 
             early_leave + poverty_rate + youth_unemployment + 
             cntry + 
             (1 | region),
           data = data_clean_lmm,
           weights = w1pspwght)
m1_imp <- lmerTest::step(m1)
summary(m1)
model_performance(m1)



data_reg |> 
  ggplot(aes(x = poverty_rate, y = ifair)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ cntry)

data_reg |> 
  ggplot(aes(x = youth_unemployment, y = ifair)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ cntry)

data_reg |> 
  ggplot(aes(x = early_leave, y = ifair)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ cntry)

data |> 
  group_by(cntry, age) |> 
  summarise(mean_fairness = mean(ifair, na.rm = TRUE)) |> 
  ggplot(aes(x = age, y = mean_fairness)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ cntry)


data |> 
  ggplot(aes(x = ifair)) +
  geom_histogram(bins = 10)

data |> 
  ggplot(aes(x = ifair)) +
  geom_histogram(bins = 10) +
  facet_wrap(~ cntry)
