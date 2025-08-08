
library(arrow)
library(data.table)
library(dplyr)
library(tidycensus)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(mapview)
library(sf)
library(stringr)
library(viridisLite)
library(esquisse)
library(plotly)
options(tigris_use_cache = TRUE)

#load data
arrow_table <- arrow:: read_parquet("~/Library/CloudStorage/OneDrive-BrownUniversity/Summer Project version 1/Datasets/predictions.parquet")

xispredictions_dt <- as.data.table(arrow_table)

xispredictions_dt <- rename(xispredictions_dt, GEOID = county)

xispredictions_dt <- xispredictions_dt %>%
  mutate(GEOID = stringr::str_pad(GEOID, width = 5, pad = "0"))

#CDD calculations
xispredictions_dt <- xispredictions_dt %>%
  mutate(
    temp_f = (1.8 * ((temp_mean_cK / 100) - 273.15)) + 32,
    temp_cdd = pmax(temp_f - 65, 0),
    heat_f = (1.8 * ((heatindex_mean_cK / 100) - 273.15)) + 32,
    heat_cdd = pmax(heat_f - 65, 0)
  )

#sum temp and heat CDDs by GEOID
xis_cdd_summary <- xispredictions_dt %>%
  group_by(GEOID) %>%
  summarise(
    temp_cdd_sum = sum(temp_cdd, na.rm = TRUE),
    heat_cdd_sum = sum(heat_cdd, na.rm = TRUE)
  ) %>%
  ungroup()

#bring into main code
xis_us_continuous <- county_age_data %>%
  left_join(xis_cdd_sum_dt, by = "GEOID") %>%
  left_join(xis_heatindex_cdd_sum_dt, by = "GEOID")

esquisse::esquisser(data = xis_us_continuous)

#preliminarymap
cdd_heat_6_27 <- ggplot(xis_us_continuous) +
  aes(fill = heatindex_cdd_sum.y) +
  geom_sf() +
scale_fill_viridis_c(
  option = "C", 
  name = "Cooling Degree Days") +
  labs(title = "Excess Heat May-Sep 2023 (cumulative CDDs)",
    fill = "Cumulative CDDs (Heat Index)"
  ) +
  theme_minimal()

ggsave("cdd_heat_6_27.png", plot = cdd_heat_6_27, width = 10, height = 6, dpi = 300)





