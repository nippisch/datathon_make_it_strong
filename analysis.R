# Script for the analysis

# loading relevant packages and data
library(tidyverse)
library(survey)
library(lme4)
library(lmerTest)
library(performance)

load(file = "data/data_processed.RData")
load(file = "data/data_covariates_region.RData")

# adding the two dataframes
data$region <- substr(data$region, 1, 4)
data_all <- left_join(data, data_covariates_region, by = "region")

# filtering only observations with weight for wave 1
data_clean_lmm <- data_all |> 
  filter(!is.na(w1pspwght))

# fitting the LMM and assessing the quality
lmm <- lmer(ifair ~ 0 +age + gndr + eduyrs + relate + edu_satisf + inc_diff + felt_safe + financial_diffs + conflicts +
             early_leave + poverty_rate + youth_unemployment + 
             cntry + 
             (1 | region),
           data = data_clean_lmm,
           weights = w1pspwght)

summary(lmm)
model_performance(lmm) # lowest BIC among all models tested

# function to extract the coefficients
extract_lmm_coefs <- function(model) {
  
  # extracting coefficients and saving them in dataframe
  coef_table <- summary(model)$coefficients
  df <- as.data.frame(coef_table)
  
  # adjusting colnames and order
  colnames(df) <- c("Estimate", "Std.Error", "df", "t.value", "p.value")
  df$Predictor <- rownames(df)
  rownames(df) <- NULL
  df <- df[, c("Predictor", "Estimate", "Std.Error", "df", "t.value", "p.value")]
  
  return(df)
}

# applying the function to the fitted LMM
dat_res <- extract_lmm_coefs(lmm)

# adjusting the results for graph below
dat_graph <- dat_res %>%
  transmute(
    term = Predictor,
    beta = Estimate,
    sd   = Std.Error,
    p    = p.value,
    sig  = ifelse(p < 0.05, 1L, 0L))

# selecting all coefficients but Intercept and countries
dat_graph <- dat_graph[2:nrow(dat_res), ]

# creating the graph
dat_graph |> 
  mutate(term = forcats::fct_reorder(term, beta, .desc = FALSE)) |> 
  ggplot(aes(x = beta, y = term)) +
  geom_point(position = position_dodge(width = 0.5), size = 4, colour = "#00457D") +
  geom_errorbarh(aes(
    xmin = beta - 1.96 * sd,
    xmax = beta + 1.96 * sd), 
    colour = "#00457D", height = 0.4, size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "",
    y = "",
    title = "") +
  scale_x_continuous(limits = c(-2.5, 2.5),
                     breaks = seq(-2.5, 2.5, by = 0.5),
                     expand = c(0,0.1)) +
  scale_y_discrete(labels = c(
    "cntryHU" = "Hungary",
    "cntryFI" = "Finland",
    "cntryCZ" = "Czechia",
    "cntryBE" = "Belgium", 
    "cntrySI" = "Slovenia",
    "cntryFR" = "France",
    "cntryPL" = "Poland",
    "cntryPT" = "Portugal",
    "age" = "Age (in years)",
    "gndrFemale" = "Female (1: Yes)",
    "eduyrs" = "Education (in years)",
    "relate" = "Current occupation matches \n education (1: Yes)",
    "edu_satisf" = "Satisfaction with own \n education (10: Very satisfied)",
    "inc_diff" = "Difficulties to live from \n current income (1: Yes)",
    "felt_safe" = "Felt safe with at least \n one carer in first 18 years (1: Yes)",
    "financial_diffs" = "Severe financial difficulties in family \n first 18 years (1: sometimes or more often)",
    "conflicts" = "Serious conflict in household first \n 18 years (1: sometimes or more often)",
    "early_leave" = "Early leavers from education (in %)",
    "poverty_rate" = "At-risk-of-poverty rate (in %)",
    "youth_unemployment" = "Young NEETs (in %)")) +
  theme_linedraw() +
  theme(
    panel.background = element_rect(fill = 'transparent', colour = 'NA'),
    plot.background = element_rect(fill = "transparent", colour = "NA"),
    axis.text.x = element_text(colour="#22444b", size = 10),
    axis.text.y = element_text(colour="#22444b", size = 16),
    legend.position = "none")

ggsave(filename = "coefficients_1.png", width = 40, height = 40, units = "cm")
