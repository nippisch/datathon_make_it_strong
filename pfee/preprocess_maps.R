library(sf)
library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load("data/data_processed.RData")

if (file.exists("data/map_regions_v3.rds") && file.exists("data/map_countries_v3.rds")) {
  map_regions   <- readRDS("data/map_regions_v3.rds")
  map_countries <- readRDS("data/map_countries_v3.rds")
} else {
  message("Processing shapefiles from source...")

  # EU map data
  map_eu <- st_read("data/NUTS_RG_20M_2021_3035.shp/", quiet = TRUE) %>%
    filter(LEVL_CODE == 2,
           !NUTS_ID %in% c("FRY1", "FRY2", "FRY3", "FRY4", "FRY5")) %>%
    rename(region = NUTS_ID) %>%
    select(region, CNTR_CODE, geometry) %>%
    st_transform(4326) %>%
    st_simplify(preserveTopology = TRUE, dTolerance = 1000)

  # UK map data
  map_uk <- st_read("data/ITL2_JAN_2025_UK_BFE_8224540410987116282/", quiet = TRUE) %>%
    st_transform(4326) %>%
    st_simplify(preserveTopology = TRUE, dTolerance = 1000) %>%
    mutate(nuts1 = substr(str_replace(ITL225CD, "^TL", "UK"), 1, 3)) %>%
    group_by(region = nuts1) %>%
    summarise(geometry = st_union(geometry), .groups = "drop") %>%
    mutate(CNTR_CODE = "GB")
    
  # Combine regional map data
  map_regions <- bind_rows(map_eu, map_uk) %>%
    filter(CNTR_CODE %in% unique(data$cntry))

  # Combine country level map data
  map_countries <- map_regions %>%
    group_by(CNTR_CODE) %>%
    summarise(geometry = st_union(geometry), .groups = "drop")
}

saveRDS(as(map_regions,   "Spatial"), "data/map_regions_sp.rds")
saveRDS(as(map_countries, "Spatial"), "data/map_countries_sp.rds")
message("Done. map_regions_sp.rds and map_countries_sp.rds saved to data/")
