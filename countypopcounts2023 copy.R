library(tidycensus)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(mapview)
library(sf)
library(stringr)
options(tigris_use_cache = TRUE)

#Total population w/geometry
total_county_pop <- get_acs(
  geography = "county",
  variables = "B01003_001",
  year = 2023,
  geometry = TRUE
) %>%
  select(GEOID, NAME, total_pop = estimate)

#over 65 variables
over65_vars <- c(
  "B01001_020", "B01001_021", "B01001_022", "B01001_023",
  "B01001_024", "B01001_025", "B01001_044", "B01001_045",
  "B01001_046", "B01001_047", "B01001_048", "B01001_049"
)

#over-65 population w/ geometry
over65_county_pop <- get_acs(
  geography = "county",
  variables = over65_vars,
  year = 2023,
  geometry = TRUE
)

#aggregate by GEOID and name
county_65plus <- over65_county_pop %>%
  group_by(GEOID, NAME) %>%
  summarise(pop_65plus = sum(estimate, na.rm = TRUE), geometry = st_union(geometry), .groups = "drop")

#add CT abbrv
ct_acs_counties <- ct_acs_counties %>%
  mutate(state_abbr = "CT") %>%
  select(GEOID, NAME, total_pop, pop_65plus, geometry, state_abbr)

#remove planning region data
county_age_data <- county_age_data %>%
  filter(state_abbr != "CT")

#append data
county_age_data <- bind_rows(county_age_data, ct_acs_counties)

#remove Alaska, Hawaii, Puerto Rico
county_age_data <- county_age_data %>%
  filter(!str_detect(NAME, "Alaska|Hawaii|Puerto Rico"))

#setup for climate region later
county_age_data <- county_age_data %>%
  mutate(
    state_abbr = state.abb[match(str_extract(NAME, "[^,]+$") %>% str_trim(), state.name)]
  )


