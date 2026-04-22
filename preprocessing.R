# preprocessing
library(tidyverse)

# importing dataframe with all CRONOS-3 data
data <- read_csv(file = "data/cronos3_all.csv")

# Selecting only relevant columns
data <- data |> 
  select(
    "cntry", # country
    "idno", # ID of respondent
    "age", # age of respondent
    "agegroup35", # agegroup
    "gndr", # gender
    "eduyrs", # education in years
    "eisced" , # education level
    "hincfel", # perceived income
    "hinctnta", # income in deciles
    "region", # region
    "w1pspwght", # weight of wave 1
    "w2pspwght", # weight of wave 2
    "w3pspwght", # weight of wave 3
    "w4pspwght", # weight of wave 4
    "w5pspwght", # weight of wave 5
    "w1sq1", # everyone in country has fair chance to achieve education they want
    "w1sq2", # I had fair chance to achieve education I wanted
    "w1sq3", # satisfaction with own education 
    "w3sq74", # match between education and occupation
    "w5hq5", # carer in childhood household
    "w5hq7", # financial difficulties in childhood household
    "w5hq8" # severe conflicts in childhood household
  )

# Cleaning, removing NAs, factoring
## Sociodemographics

### age
#table(data$age, useNA = "always") # 33 "999 -> into NA
data$age <- data$age %>% 
  na_if(y = 999)

### agegroup35
#table(data$agegroup35, useNA = "always") # 33 "99" -> into NA
data$agegroup35 <- data$agegroup35 %>% 
  na_if(y = 9)

### Gender
data$gndr <- data$gndr |>
  na_if(y = 9) |>
  factor(labels = c("Male", "Female")) 

## Education
### eduyrs
#table(data$eduyrs, useNA = "always") # bunch of 77, 88, 99 -> into NA
data$eduyrs <- data$eduyrs %>%
  na_if(y = 77) %>% 
  na_if(y = 88) %>% 
  na_if(y = 99)

### education level (eisced)
#table(data$eisced, useNA = "always") # bunch of 55, 77, 88, 99 -> into NA
data$eisced <- data$eisced %>% 
  na_if(y = 55) %>% 
  na_if(y = 77) %>% 
  na_if(y = 88) %>% 
  na_if(y = 99)

## other relevant variables
### matching edu and job
data$w3sq74 <- data$w3sq74 |> 
  na_if(y = 6) |> 
  na_if(y = 9)

### income, feeling
data$hincfel <- data$hincfel |> 
  na_if(y = 7) |> 
  na_if(y = 8) |> 
  na_if(y = 9)

### income, deciles
data$hinctnta <- data$hinctnta |> 
  na_if(y = 77) |> 
  na_if(y = 88) |> 
  na_if(y = 99) 

### Everyone in country fair chance achieve level of education they seek (w1sq1)
#table(data$w1sq1, useNA = "always")
data$w1sq1 <- data$w1sq1 |> 
  na_if(y = 99)

### Compared other people in country, fair chance achieve level of education I seek (w1sq2)
#table(data$w1sq2, useNA = "always")
data$w1sq2 <- data$w1sq2 |> 
  na_if(y = 99)

### Satisfaction with level of education you have reached (w1sq3)
#table(data$w1sq3, useNA = "always")
data$w1sq3 <- data$w1sq3 |> 
  na_if(y = 99)

### At least one carer in childhood household
#table(data$w5hq5, useNA = "always")
#table(data$w5hq5)
data$w5hq5 <- data$w5hq5 |> 
  na_if(y = 9)

### Severe conflicts in childhood household
data$w5hq7 <- na_if(data$w5hq7, 9)
data$conflicts <- ifelse(data$w5hq7 < 4, 1, 0)

### Severe financial difficulties in family during first 18 years of life, how often (w5hq8)
data$w5hq8 <- na_if(data$w5hq8, 9)
data$financial_diffs <- ifelse(data$w5hq8 < 4, 1, 0)

# Renaming columns
data <- data %>% 
  rename(
    everyfair = w1sq1,
    ifair = w1sq2,
    edu_satisf = w1sq3,
    edu_match = w3sq74
  )

# creating further variables
data$relate <- ifelse(data$edu_match == 3, 1, 
                      ifelse(data$edu_match < 3, 0, NA))
data$inc_diff <- ifelse(data$hincfel > 2, 1, 0) # values > 2 have difficulty on present income
data$felt_safe <- ifelse(data$w5hq5 == 1, 1, 0) # value 1 == yes, 0 == no 


# saving preprocessed file
save(data, file = "data/data_processed.RData")
