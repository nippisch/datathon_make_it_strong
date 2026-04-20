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

### Influences on expectations on maternity/paternity leave (DV: w3sq61)
# Motivation: Child penalty and its anticipation
# IVs:
# - gender
# - age
# - years of education
# - age of youngest child number of children?

table(data$hincfel, data$gndr, useNA = "always")
class(data$hincfel)

data$gndr <- factor(data$gndr, labels = c("Male", "Female", "NA"))

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

ggplot(data = data, aes(x = hincfel, fill = gndr)) +
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
  
## Income by gender
table(data$hinctnta, data$gndr)

## Children economically possible? 
# Financial ability, w5sq13
# Suitable housing, w5sq14
# Satisfactory childcare availability, w5sq15
# sufficient paid parental leave, w5sq16
data$w5sq15 <- factor(data$w5sq15,
                      levels = c())

table(data$w5sq15, data$gndr)

ggplot(data = data, aes(x = w5sq15, fill = gndr)) +
  geom_bar(position = "dodge")

## Importance of children in partnership/marriage (w4sq11)
table(data$w4sq11, data$gndr)
# very important for most 

## Maternity / paternity leave
# Corr neg expectation and importance and felt income
table(data$w3sq61, data$gndr) 
# more women anticipate negative consequences
ggplot(data = data, aes(x = w3sq61, fill = gndr)) +
  geom_bar(position = "dodge")

# Women expect more negative consequences?
table(data$w5sq13, data$gndr)
summary(data$w5sq13)


# Make it digital
data_digital <- read.csv("cronos3_make_it_digital.csv")
table(data_digtial$w4dq18) 

# Select only weights to check for differences
dat <- select(data, w1pspwght, w2pspwght, w3pspwght, w4pspwght, w5pspwght)
dat
