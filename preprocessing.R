# preprocessing
library(tidyverse)

data <- read_csv(file = "data/cronos3_all.csv")

# Selecting only relevant columns

data <- data |> 
  select(
    "cntry",
    "idno",
    "age",
    "agegroup35",
    "gndr",
    "eduyrs",
    "eisced" ,
    "hincfel",
    "hinctnta",
    "region",
    "w1pspwght",
    "w2pspwght",
    "w3pspwght",
    "w4pspwght",
    "w5pspwght",
    "w1sq1",
    "w1sq2",
    "w1sq3",
    "w3sq74"
  )

# Cleaning, removing NAs, factoring
## Sociodemographics
# age
table(data$age, useNA = "always") # 33 "999 -> into NA
data$age <- data$age %>% 
  na_if(y = 999)

# agegroup35
table(data$agegroup35, useNA = "always") # 33 "99" -> into NA
data$agegroup35 <- data$agegroup35 %>% 
  na_if(y = 9)

# Gender
data$gndr <- data$gndr |>
  na_if(y = 9) |>
  factor(labels = c("Male", "Female")) 

# Education
# eduyrs
table(data$eduyrs, useNA = "always") # bunch of 77, 88, 99 -> into NA
data$eduyrs <- data$eduyrs %>%
  na_if(y = 77) %>% 
  na_if(y = 88) %>% 
  na_if(y = 99)
# education level (eisced)
table(data$eisced, useNA = "always") # bunch of 55, 77, 88, 99 -> into NA
data$eisced <- data$eisced %>% 
  na_if(y = 55) %>% 
  na_if(y = 77) %>% 
  na_if(y = 88) %>% 
  na_if(y = 99)

# Income
# hincfel
table(data$hincfel, useNA = "always") # bunch of 7, 8, 9 -> into NA
data$hincfel <- data$hincfel %>% 
  na_if(y = 7) %>% 
  na_if(y = 8) %>% 
  na_if(y = 9)

# hinctnta, deciles
table(data$hinctnta, useNA = "always") # bunch of 77, 88, 99 -> into NA
data$hinctnta <- data$hinctnta %>% 
  na_if(y = 77) %>% 
  na_if(y = 88) %>% 
  na_if(y = 99)

## Items, IVs, DVs
# Everyone has fair chance to achieve education (w1sq1)
table(data$w1sq1, useNA = "always") # 204 "99" -> into NA
data$w1sq1 <- data$w1sq1 %>% 
  na_if(y = 99)

# Compared to other people, I had fair chance (w1sq2)
table(data$w1sq2, useNA = "always") # 275 "99" -> into NA
data$w1sq2 <- data$w1sq2 %>% 
  na_if(y = 99)

# Satisfaction with level of education (w1sq3)
table(data$w1sq3, useNA = "always") # 251 "99" -> into NA
data$w1sq3 <- data$w1sq3 %>% 
  na_if(y = 99)

# Correspondence between job and educational qualification (w3sq74)
table(data$w3sq74, useNA = "always") # bunch of 6, 9 -> into NA
data$w3sq74 <- data$w3sq74 %>% 
  na_if(y = 6) %>% 
  na_if(y = 9)

# Renaming columns
data <- data %>% 
  rename(
    everyfair = w1sq1,
    ifair = w1sq2,
    edu_satisf = w1sq3,
    edu_match = w3sq74
  )

