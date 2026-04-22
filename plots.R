library(tidyverse)
library(survey)

load(file = "data/data_processed.RData")

# Means for each country
weights_data <- data |> 
  filter(!is.na(w2pspwght))

design <- svydesign(
  ids = ~1,
  strata = ~cntry,
  weights = ~w2pspwght,
  data = weights_data,
)

# Calculate weighted means of ifair by country AND by country + gender
mean_total <- svyby(~ifair, ~cntry, design = design, FUN = svymean, na.rm = TRUE)
mean_gender <- svyby(~ifair, ~cntry + ~gndr, design = design, FUN = svymean, na.rm = TRUE)

# Combine mean dataframes - long
dumbbell_long <- bind_rows(
  mean_total |>
    select(cntry, ifair) |>
    mutate(group = "Total"),
  mean_gender |>
    select(cntry, gndr, ifair) |>
    mutate(group = as.character(gndr)) |>
    select(-gndr)
)

# Pivot - wide
dumbbell_wide <- dumbbell_long |>
  filter(group != "Total") |>
  pivot_wider(names_from = group, values_from = ifair)

# Order countries by overall mean
country_order <- mean_total |>
  arrange(ifair) |>
  pull(cntry)

# Country as factor to sort plot  
dumbbell_long <- dumbbell_long |> 
  mutate(cntry = factor(cntry, levels = country_order))
dumbbell_wide <- dumbbell_wide |> 
  mutate(cntry = factor(cntry, levels = country_order))

# Dumbbell dot plot: ifair by country, total vs male vs female

ggplot() +
  # Plot line from female to male
  geom_segment(
    data = dumbbell_wide,
    aes(x = Female, xend = Male, y = cntry, yend = cntry),
    color = "grey70", linewidth = 1
  ) +
  # Plot points for total, male and female mean
  geom_point(
    data = dumbbell_long,
    aes(x = ifair, y = cntry, color = group, shape = group),
    size = 3
  ) +
  scale_color_manual(values = c("Total" = "black", "Male" = "#2196F3", "Female" = "#E91E63")) +
  scale_shape_manual(values = c("Total" = 18, "Male" = 16, "Female" = 16)) +
  scale_x_continuous(limits = c(5, 8.5)) +
  labs(
    x = "Mean perceived fairness",
    y = NULL,
    color = NULL, shape = NULL,
 ) +
  theme_linedraw(base_family = "Luminari") +
  theme(
    panel.background = element_rect(fill = "white", colour = "white"),
    plot.background = element_rect(fill = "#FFF7F5", colour = "#FFF7F5"),
    plot.title = element_text(size = 20, face = "bold", colour = "#22444b"),
    axis.text.x = element_text(colour="#22444b", size = 10, family = "Luminari"),
    axis.title.x = element_text(colour = "#22444b", size = 10, family = "Luminari"),
    axis.text.y = element_text(colour="#22444b", size = 10, family = "Luminari"),
    legend.text = element_text(colour="#22444b", size = 10, family = "Luminari"),
    legend.position = "top",
    legend.background = element_rect(fill = "#FFF7F5"),
    legend.key = element_rect(fill = "#FFF7F5"),
  )

# 10cm height * 16cm width on poster
# ggsave(filename = "dotplot_xzoomed.png", width = 16, height = 10, units = "cm")
