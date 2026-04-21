library(tidyverse)
library(sf)

## Maps
# EU NUTS-2 Karte (2021)
map <- st_read("data/NUTS_RG_20M_2021_3035.shp/")
map <- map[map$LEVL_CODE == 2, ]
overseas_codes <- c("FRY1", "FRY2", "FRY3", "FRY4", "FRY5")
map <- map[!map$NUTS_ID %in% overseas_codes, ]
map <- rename(map, region = NUTS_ID)

# UK ITL-2 Karte (TLC3 → UKC3 etc.)
map_uk <- st_read("data/ITL2_JAN_2025_UK_BFE_8224540410987116282/")
map_uk <- st_transform(map_uk, crs = st_crs(map))
map_uk_clean <- map_uk %>%
  mutate(region     = str_replace(ITL225CD, "^TL", "UK"),
         CNTR_CODE  = "GB") %>%   # ESS codiert UK als "GB"
  select(region, CNTR_CODE, geometry)

# combine
map_combined <- bind_rows(
  map %>% select(region, CNTR_CODE, geometry),
  map_uk_clean
)

unique(data$region[data$cntry == "GB"])

# --- Karte: Durchschnittliches Alter pro NUTS-2 Region ---

# NUTS-3 → NUTS-2 (erste 4 Zeichen des Codes)
data_region <- data %>%
  mutate(region_nuts2 = substr(region, 1, 4)) %>%
  group_by(region_nuts2) %>%
  summarise(mean_age = mean(age, na.rm = TRUE), .groups = "drop") %>%
  rename(region = region_nuts2)

# Karte auf die 11 Länder im Datensatz einschränken
countries_in_data <- unique(data$cntry)
map_filtered <- map_combined[map_combined$CNTR_CODE %in% countries_in_data, ]

combine_age <- left_join(map_filtered, data_region, by = "region")

ggplot(data = combine_age, aes(fill = mean_age)) +
  geom_sf(color = "white", linewidth = 0.2) +
  coord_sf(datum = st_crs(3035)) +
  scale_fill_viridis_c(
    name = "Ø Alter (Jahre)",
    option = "plasma",
    na.value = "grey80"
  ) +
  labs(
    title = "Durchschnittliches Alter pro NUTS-2 Region",
    caption = "Quelle: CRONOS-3"
  ) +
  theme_bw() +
  theme(
    axis.text  = element_blank(),
    axis.ticks = element_blank()
  )

unique(data$region)
unique(data$region[data$cntry == "PT"])



data_region <- data %>%
  mutate(region_nuts2 = substr(region, 1, 4),
         region_nuts2 = recode(region_nuts2, !!!nuts_crosswalk)) %>%
  group_by(region_nuts2) %>%
  summarise(mean_age = mean(age, na.rm = TRUE), .groups = "drop") %>%
  rename(region = region_nuts2)

map$region[substr(map$region, 1, 2) == "PT"]
unique(data$region[data$cntry == "PT"])
## Countries and regions
table(data$cntry)

unique(data$region[data$cntry == "PT"])
unique(map$region[map$CNTR_CODE == "PT"])

data[data$cntry == "PT", ]