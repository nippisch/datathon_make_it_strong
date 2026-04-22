# robustness analysis and further exploration

# loading relevant packages and data
library(tidyverse)
library(survey)
library(lme4)
library(lmerTest)
library(performance)

load(file = "data/data_processed.RData")

# comparison with simpler models
m1 <- lmer(w1sq2 ~ age + w1sq3 + gndr + relate + eduyrs + cntry + (1 | region),
           data = data_clean,
           weights = w1pspwght)
summary(m1)
check_model(m1)
model_performance(m1)

m2 <- lmer(w1sq2 ~ age + gndr + relate + eduyrs + (1 | cntry),
           data = data,
           weights = w3pspwght)
summary(m2)
check_model(m2)
model_performance(m2)

# comparison models
m1 <- lmer(ifair ~ age + gndr + eduyrs + relate + edu_satisf + inc_diff + felt_safe + 
             early_leave + poverty_rate + youth_unemployment + 
             cntry + 
             (1 | region),
           data = data_clean_lmm,
           weights = w1pspwght)


m1_imp <- lmerTest::step(m1)
summary(m1)
model_performance(m1)

m3 <- lmer(ifair ~ age + gndr + eduyrs + relate + edu_satisf + inc_diff + felt_safe + financial_diffs + conflicts +
             early_leave + poverty_rate + youth_unemployment + 
             cntry + 
             (1 | region),
           data = data_clean_lmm,
           weights = w1pspwght)

summary(m3)
model_performance(m3)

# alternative approach: seperate models

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

## seperate models per age category
data$age_cat <- ifelse(data$age < 30, 1, 
                       ifelse(data$age < 51, 2, 
                              ifelse(data$age > 50, 3, NA)))
data$ifair_cat <- ifelse(data$ifair < 4, 1, 
                         ifelse(data$ifair < 8, 2, 
                                ifelse(data$ifair > 7, 3, NA)))

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