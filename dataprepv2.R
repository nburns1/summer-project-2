#data import and cleaning file
#CT, ACS, WONDER, XIS,
#run every line and then it will create

#Libraries
library(tidycensus)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(mapview)
library(sf)
library(stringr)
options(tigris_use_cache = TRUE)


####################
#2. CT: independently call ACS data, using CT crosswalk to drop planning regions and add counties
#total pop. by tract
total_county_popct <- get_acs(
  geography = "tract",
  variables = "B01003_001",
  year = 2023,
  state = "CT",
  geometry = TRUE
) %>%
  select(GEOID, NAME, total_pop = estimate)

#65 plus variables
over65_vars <- c(
  "B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025",  
  "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049"   
)

#65+ population by tract
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

#crosswalk clean
ct_crosswalk <- read.csv("~/Library/CloudStorage/OneDrive-BrownUniversity/Summer Project Version 2/Datasets copy/2022tractcrosswalk.csv", header = TRUE) %>%
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

#add geometry back
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

############################
#1. ACS remaining states

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

# add state abbreviation
county_65plus <- county_65plus %>%
  mutate(
    state_abbr = state.abb[match(str_extract(NAME, "[^,]+$") %>% str_trim(), state.name)]
  )

# filter out CT
county_age_data <- county_65plus %>%
  filter(state_abbr != "CT")


#add CT abbrv
ct_acs_counties <- ct_acs_counties %>%
  mutate(state_abbr = "CT") %>%
  select(GEOID, NAME, total_pop, pop_65plus, geometry, state_abbr)


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



#########################
#CDC WONDER data
wonder_data <- read.table("~/Library/CloudStorage/OneDrive-BrownUniversity/Summer Project Version 2/Datasets copy/6_17_25_Underlying Cause of Death_2018-2023_suppressed copy.txt", header = TRUE, sep = "\t")

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

#######################
#XIS 
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
arrow_table <- arrow:: read_parquet("~/Library/CloudStorage/OneDrive-BrownUniversity/Summer Project Version 2/Datasets copy/predictions.parquet")

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
  left_join(xis_cdd_summary, by = "GEOID")
