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
    "w4pspwght",
    "w5pspwght",
    "w1sq1",
    "w1sq2",
    "w1sq3",
    "w3sq74"
  )


data$gndr <- data$gndr |>
  na_if(y = 9) |>
  factor(labels = c("Male", "Female")) 
