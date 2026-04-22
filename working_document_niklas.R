# exploration script Niklas
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

m1 <- lmer(w1sq2 ~ age + w1sq3 + gndr + relate + eduyrs + cntry + (1 | region),
           data = data_clean,
           weights = w1pspwght)

m2 <- lmer(w1sq2 ~ age + gndr + relate + eduyrs + (1 | cntry),
           data = data,
           weights = w3pspwght)
summary(m1)
check_model(m1)
model_performance(m1)

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

# new approach: seperate models

## linear models without weighting
data_clean <- data_all |> 
  filter(!is.na(edu_satisf) & !is.na(age) & !is.na(eduyrs) & !is.na(relate) & !is.na(ifair) & !is.na(inc_diff) & !is.na(felt_safe) & 
           !is.na(gndr) & !is.na(w1pspwght))
linear_model <- lm(ifair ~ age + I(age^2) + gndr + eduyrs + I(eduyrs^2) + I(eduyrs^3) + relate + edu_satisf + inc_diff + felt_safe, data = data_clean)
linear_model_improved <- stats::step(linear_model)
summary(linear_model_improved)

## linear models with weighting
data_clean_lm <- data |> 
  filter(!is.na(w1pspwght) & !is.na(w3pspwght) & !is.na(w5pspwght))

design_lm <- svydesign(ids = ~1, weights = ~w5pspwght, data = data_clean_lm)

linear_model_wgt <- svyglm(ifair ~ age + I(age^2) + gndr + eduyrs + I(eduyrs^2) + I(eduyrs^3) + relate + edu_satisf + inc_diff + felt_safe,
                           design = design_lm)
lm_wgt_improved <- stats::step(linear_model_wgt)
summary(lm_wgt_improved)

## different models per country
data_clean_lm <- data |> 
  filter(!is.na(w1pspwght) & !is.na(w3pspwght) & !is.na(w5pspwght))
list_countries <- unique(data$cntry)
df_results <- data.frame(term = character(0), estimate = numeric(0), std.error = numeric(0), 
                         statistic = numeric(0), p.value = numeric(0), country = character(0))

for (i in 1:length(list_countries)) {
  # selecting only relevant observations for each country
  country <- list_countries[i]
  data_cntry <- data_clean_lm[data_clean_lm$cntry == country, ]
  
  # specifying design based on country
  design_cntry <- svydesign(ids = ~1, weights = ~w5pspwght, data = data_cntry)
  
  # fitting linear model
  linear_model_wgt <- svyglm(ifair ~ age + I(age^2) + gndr + eduyrs + I(eduyrs^2) + I(eduyrs^3) + relate + edu_satisf + inc_diff + felt_safe,
                             design = design_cntry)
  # lm_wgt_improved <- stats::step(linear_model_wgt)
  
  # extracting coefficients
  df_res <- broom::tidy(linear_model_wgt)
  df_res$country <- country
  
  df_results <- bind_rows(df_res, df_results)
}

df_plot <- df_results |> 
  filter(term != "(Intercept)") |>
  mutate(
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

ggplot(df_plot, aes(x = estimate, y = country)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ term, scales = "free_x") +
  labs(x = "Coefficient", y = "Country") +
  theme_minimal()

## model on region level
data_clean_lm_reg <- data |> 
  filter(!is.na(w2pspwght))
design_reg <- svydesign(ids = ~1, weights = ~w2pspwght, data = data_clean_lm_reg)

vars <- c("edu_satisf", "age", "gndr", "eduyrs", "relate", 
          "ifair", "inc_diff", "felt_safe")

data_reg <- svyby(
  formula = as.formula(paste("~", paste(vars, collapse = " + "))),
  by = ~cntry + region,
  design = design_reg,
  FUN = svymean,
  na.rm = TRUE) |> 
  as.data.frame()

data_reg <- left_join(data_reg, data_covariates_region, by = "region")

m2 <- lm(ifair ~ age + I(age^2) + gndrFemale + eduyrs + I(eduyrs^2) + I(eduyrs^3) + relate + edu_satisf + inc_diff + felt_safe + 
           early_leave + poverty_rate + youth_unemployment + cntry, 
         data = data_reg)
summary(m2)

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

# Sankey
data$age_cat <- ifelse(data$age < 30, 1, 
                       ifelse(data$age < 51, 2, 
                              ifelse(data$age > 50, 3, NA)))
data$ifair_cat <- ifelse(data$ifair < 4, 1, 
                         ifelse(data$ifair < 8, 2, 
                                ifelse(data$ifair > 7, 3, NA)))

## seperate models per age category
m_young <- lmer(ifair ~ gndr + eduyrs + relate + edu_satisf + inc_diff + felt_safe + 
             early_leave + poverty_rate + youth_unemployment + 
             cntry + 
             (1 | region),
           data = data_clean_lmm[data_clean_lmm$age_cat == 1, ],
           weights = w1pspwght)
model_performance(m_young)
summary(m_young)

m_mid <- lmer(ifair ~ gndr + eduyrs + relate + edu_satisf + inc_diff + felt_safe + 
             early_leave + poverty_rate + youth_unemployment + 
             cntry + 
             (1 | region),
           data = data_clean_lmm[data_clean_lmm$age_cat == 2, ],
           weights = w1pspwght)
model_performance(m_mid)
summary(m_mid)

m_old <- lmer(ifair ~ gndr + eduyrs + relate + edu_satisf + inc_diff + felt_safe + 
                early_leave + poverty_rate + youth_unemployment + 
                cntry + 
                (1 | region),
              data = data_clean_lmm[data_clean_lmm$age_cat == 3, ],
              weights = w1pspwght)
model_performance(m_old)
summary(m_old)

summary(m1)

data |> 
  ggplot(aes(x = ifair)) +
  geom_histogram(bins = 10)

data |> 
  ggplot(aes(x = ifair)) +
  geom_histogram(bins = 10) +
  facet_wrap(~ cntry)
