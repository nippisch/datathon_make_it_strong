# Script for the analysis

# loading relevant packages and data
library(tidyverse)
library(survey)
library(lme4)
library(lmerTest)
library(performance)
library(see)
library(broom)

data <- read.csv("data/cronos3_all.csv", sep = ",", header = TRUE)
load(file = "data_covariates_region.RData")

# preprocessing: creating new variables
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