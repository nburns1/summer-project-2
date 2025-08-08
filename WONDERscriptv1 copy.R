library(tidycensus)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(mapview)
library(sf)
options(tigris_use_cache = TRUE)

wonder_data <- read.table("~/Library/CloudStorage/OneDrive-BrownUniversity/Summer Project version 1/Datasets/6_17_25_Underlying Cause of Death_2018-2023_suppressed copy.txt", header = TRUE, sep = "\t")

wonder_data <- rename(wonder_data, GEOID = County.Code)

wonder_for_calculation <- wonder_data %>%
  mutate(GEOID = str_pad(GEOID, width = 5, pad = "0")) %>%

mutate(Deaths = str_trim(Deaths),
         Deaths = str_replace_all(Deaths, ",", "")) %>%

filter(!Deaths %in% c("0", "Suppressed")) %>%
  
mutate(Deaths = as.numeric(Deaths))

wonder_for_calculation <- wonder_for_calculation %>%
  left_join(county_age_data, by = "GEOID") 
  
#deathrate
wonder_for_calculation <- wonder_for_calculation %>%
mutate(death_rate_65plus = Deaths / pop_65plus)

median_death_rate <- median(wonder_for_calculation$death_rate_65plus, na.rm = TRUE)
median_death_rate

#impute with the median 
wonder_data_with_pop <- wonder_data %>%
  mutate(GEOID = str_pad(GEOID, 5, pad = "0")) %>%
  left_join(county_age_data, by = "GEOID")

wonder_data_imputed <- wonder_data_with_pop %>%
  mutate(
    Deaths_imputed = case_when(
      Deaths %in% c("Suppressed", "0") ~ round(median_death_rate * as.numeric(pop_65plus)),
      TRUE ~ as.numeric(str_replace_all(str_trim(Deaths), ",", ""))
    )
  )

wonder_data_imputed <- wonder_data_imputed %>%
  select(GEOID, County, Deaths, Deaths_imputed)



