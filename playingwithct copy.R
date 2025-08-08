library(tidycensus)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(mapview)
library(sf)
library(stringr)
options(tigris_use_cache = TRUE)

#total pop. by tract
total_county_popct <- get_acs(
  geography = "tract",
  variables = "B01003_001",
  year = 2023,
  state = "CT",
  geometry = TRUE
) %>%
  select(GEOID, NAME, total_pop = estimate)

# 65 plus variables
over65_vars <- c(
  "B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025",  
  "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049"   
)

# 65+ population by tract
over65_tract_popct <- get_acs(
  geography = "tract",
  variables = over65_vars,
  year = 2023,
  state = "CT",
  geometry = TRUE
)

acs_pop_65plusct <- over65_tract_popct %>%
  st_drop_geometry() %>%    
  group_by(GEOID, NAME) %>%
  summarise(acs_pop_65plusct = sum(estimate, na.rm = TRUE), .groups = "drop")

#join
tracts_with_65plus <- total_county_popct %>%
  left_join(acs_pop_65plusct, by = c("GEOID", "NAME"))

# crosswalk clean
ct_crosswalk <- read.csv("~/Library/CloudStorage/OneDrive-BrownUniversity/Summer Project version 1/Datasets/2022tractcrosswalk.csv", header = TRUE) %>%
  mutate(Tract_fips_2022 = str_pad(Tract_fips_2022, width = 11, pad = "0")) %>%
  rename(GEOID = Tract_fips_2022)

#join acs and crosswalk
ct_acs <- tracts_with_65plus %>%
  left_join(ct_crosswalk, by = "GEOID")

#numeric check
ct_acs <- ct_acs %>%
  mutate(
    total_pop = as.numeric(total_pop),
    acs_pop_65plusct = as.numeric(acs_pop_65plusct)
  )

# add geometry back
ct_acs_counties <- ct_acs %>%
  group_by(county_name) %>%
  summarise(
    total_pop = sum(total_pop, na.rm = TRUE),
    pop_65plus = sum(acs_pop_65plusct, na.rm = TRUE),
    geometry = st_union(geometry),
    .groups = "drop"
  )

# countyGEOIDs
ct_county_geoids <- tribble(
  ~county_name,    ~GEOID,  ~NAME,
  "Fairfield",     "09001", "Fairfield County, Connecticut",
  "Hartford",      "09003", "Hartford County, Connecticut",
  "Litchfield",    "09005", "Litchfield County, Connecticut",
  "Middlesex",     "09007", "Middlesex County, Connecticut",
  "New Haven",     "09009", "New Haven County, Connecticut",
  "New London",    "09011", "New London County, Connecticut",
  "Tolland",       "09013", "Tolland County, Connecticut",
  "Windham",       "09015", "Windham County, Connecticut"
)

ct_acs_counties <- ct_acs_counties %>%
  left_join(ct_county_geoids, by = "county_name") %>%
  filter(!is.na(county_name) & county_name != "") %>%
  mutate(state_abbr = "CT") %>%
  select(GEOID, NAME, total_pop, pop_65plus, geometry, state_abbr)

