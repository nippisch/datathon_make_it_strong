# preprocessing
library(tidyverse)

data <- read_csv(file = "cronos3_make_it_strong.csv")

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
    "w3sq61",
    "w4pspwght",
    "w4sq2",
    "w4sq11",
    "w5pspwght",
    "w5sq13",
    "w5sq14",
    "w5sq15",
    "w5sq16",
    "w5sq17_1",
  )

data$gndr <- data$gndr |>
  na_if(y = 9) |>
  factor(labels = c("Male", "Female")) 
