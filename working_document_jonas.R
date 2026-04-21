# exploration script Jonas

# Individual und Strukturebene: Einstellungen vs. KiTa-Coverage etc.

# https://eplp-dataset.org/ zu parenting leave policies
# https://ec.europa.eu/eurostat/web/income-and-living-conditions/database Child care arrangements#

library(tidyverse)
library(survey)
data <- read_csv(file = "cronos3_make_it_strong.csv")

ggplot(data = map) +
  geom_sf()

## Countries and regions
table(data$cntry)

length(unique(data$region))

data[data$region == "AT31", ]
## 146 NUTS-coded regions of different level
# mean 83, min 6, max 721
mean(table(data$region))
min(table(data$region))
max(table(data$region))


data$hincfel <- factor(data$hincfel, 
                       levels = c(1:4, 7, 8, 9),
                       labels = c(
                         "Living comfortably on present income",
                         "Coping on present income",
                         "Difficult on present income",
                         "Very difficult on present income",
                         "Refusal",
                         "Don't know",
                         "No answer"))


