# import additional data

data$region <- substr(data$region, 1, 4)
df_codes <- data |> 
  group_by(region) |> 
  summarise(mean(age, na.rm = TRUE))

dat_help <- read.csv("data/helper_2.csv", sep = ";", header = TRUE)
dat_early_leave <- read.csv("data/early_leave.csv", sep = ",", header = TRUE)
dat_poverty <- read.csv("data/at_risk_poverty_rate.csv", sep = ",", header = TRUE)
dat_youth_unemployment <- read.csv("data/youth_unemployment.csv", sep = ",", header = TRUE)

dat_sel <- dat_youth_unemployment |> 
  select(geo, OBS_VALUE)

dat_both <- left_join(dat_help, dat_sel, join_by("labels" == "geo"))
dat_all <- left_join(df_codes, dat_both, join_by("region" == "codes"))

dat_all_clean <- dat_all |> 
  select(region, OBS_VALUE) |> 
  rename(youth_unemployment = OBS_VALUE)

#data_cov <- dat_all_clean
data_cov2 <- left_join(data_cov1, dat_all_clean, by = "region")
data_covariates_region <- data_cov2
save(data_covariates_region, file = "data_covariates_region.RData")