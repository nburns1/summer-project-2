library(tidycensus)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(mapview)
library(sf)
library(stringr)
library(viridisLite)
library("esquisse")
library(esquisse)
library(gnm)
library(geofacet)
library(scales)
options(tigris_use_cache = TRUE)

#Step 1: build out merged data table

#merge table
merged_acs_wonder_xis <- county_age_data %>%   
  left_join(xis_cdd_summary, by = "GEOID") %>%
  left_join(wonder_data_imputed, by = "GEOID")

#NOAA climate regions
merged_acs_wonder_xis <- merged_acs_wonder_xis %>%
  mutate(
    climate_region = case_when(
      state_abbr %in% c("IL", "IN", "KY", "MO", "OH", "TN", "WV") ~ "Central",
      state_abbr %in% c("IA", "MI", "MN", "WI") ~ "East North Central",
      state_abbr %in% c("DC", "CT", "DE", "ME", "MD", "MA", "NH", "NJ", "NY", "PA", "RI", "VT") ~ "Northeast",
      state_abbr %in% c("ID", "OR", "WA") ~ "Northwest",
      state_abbr %in% c("AR", "KS", "LA", "MS", "OK", "TX") ~ "South",
      state_abbr %in% c("AL", "FL", "GA", "NC", "SC", "VA") ~ "Southeast",
      state_abbr %in% c("AZ", "CO", "NM", "UT") ~ "Southwest",
      state_abbr %in% c("CA", "NV") ~ "West",
      state_abbr %in% c("MT", "NE", "ND", "SD", "WY") ~ "West North Central",
      TRUE ~ NA_character_ 
    )
  )

#% of deaths (imputed)
merged_acs_wonder_xis <- merged_acs_wonder_xis %>%
  mutate(
    death_pct = if_else(
      !is.na(Deaths_imputed) & !is.na(pop_65plus) & pop_65plus > 0,
      (Deaths_imputed / pop_65plus) * 100,
      NA_real_
    )
  )

#heat_cdd_scaled variable = heat_cdd_sum/100
merged_acs_wonder_xis <- merged_acs_wonder_xis %>%
  mutate(heat_cdd_scaled = heat_cdd_sum / 100)

#Map 1: ggplotversion temp cdd

cdd_temp_0708 <- ggplot(merged_acs_wonder_xis) +
  aes(fill = temp_cdd_sum) +
  geom_sf() +
  scale_fill_viridis_c(
    option = "C", 
    name = "Cooling Degree Days") +
  labs(
    title = "Cumulative Cooling Degree Days (Temp) May-Sep 2023 ",
    fill = "Cooling Degree Days"
  ) +
  theme_void()
print(cdd_temp_0708)

ggsave("cdd_temp_0708.png", plot = cdd_temp_0708, width = 10, height = 6, dpi = 300)

#Map 2: heatindex_ggplotmap
summary(merged_acs_wonder_xis$heat_cdd_sum)

cdd_hi_0708 <- ggplot(merged_acs_wonder_xis) +
  aes(fill = heat_cdd_sum) +
  geom_sf() +
  scale_fill_viridis_c(
    option = "C", 
    name = "Cooling Degree Days",
    na.value = "grey80"
    ) +
  labs(
    title = "Cumulative Cooling Degree Days (Heat) May-Sep 2023 ",
    fill = "Cooling Degree Days"
  ) +
  theme_void()
print(cdd_hi_0708)

ggsave("cdd_hi_0708.png", plot = cdd_hi_0708, width = 10, height = 6, dpi = 300)

esquisse::esquisser(data = merged_acs_wonder_xis)

#temp
temp_722 <- ggplot(data = merged_acs_wonder_xis) +
  geom_sf(aes(fill = temp_cdd_sum), color = NA) +
  scale_fill_viridis_c(
    option = "C", 
    name = "Cooling Degree Days",
    labels = label_comma()) +
  labs(
    title = "Sum of Cooling Degree Days (Excess Temperature)",
    subtitle = "May–Sep 2023"
  ) +
  theme_void() +
  theme(
    axis.text = element_blank(),        
    axis.ticks = element_blank(),      
    axis.line = element_blank(),
    plot.margin = margin(0,0,0,0),
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5, size = 16),
    legend.margin = margin(0, 0, 0, 0), # turned off for alignment
    legend.justification.top = "left",
    legend.justification.left = "top",
    legend.justification.bottom = "right",
    legend.justification.inside = c(1, 1),
    legend.location = "plot",
    plot.title.position = "plot"
  )
print(temp_722)
ggsave("tempmap_722.png", plot = temp_722, width = 10, height = 6, dpi = 300)

#hicdd
heat_722 <- ggplot(data = merged_acs_wonder_xis) +
  geom_sf(aes(fill = heat_cdd_sum)) +
  scale_fill_viridis_c(
    option = "C", 
    name = "Cooling Degree Days",
    labels = label_comma()) +
  labs(
    title = "Sum of Cooling Degree Days (Excess Heat Index)",
    subtitle = "May–Sep 2023"
  ) +
  theme_void() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    plot.title = element_text(hjust = 0.5, size = 24),
    plot.subtitle = element_text(hjust = 0.5, size = 18),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.margin = margin(0, 0, 0, 0),
    legend.justification.top = "left",
    legend.justification.left = "top",
    legend.justification.bottom = "right",
    legend.justification.inside = c(1, 1),
    legend.location = "plot",
    plot.title.position = "plot"
  )
print(heat_722)
ggsave("heatmap_722.png", plot = heat_722, width = 10, height = 6, dpi = 300)
#deathmap
deathpctmap <- ggplot(merged_acs_wonder_xis) +
  geom_sf(aes(fill = death_pct), color = NA) +
  scale_fill_viridis_c(
    name = "Death %",
    limits = c(0, 1.75)
  ) +
  labs(
    title = "Percent of Population 65+ with Circulatory Cause of Death",
    subtitle = "May–Sep 2023"
  ) +
  theme_void() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    plot.title = element_text(hjust = 0.5, size = 24),
    plot.subtitle = element_text(hjust = 0.5, size = 18),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.margin = margin(0, 0, 0, 0),
    legend.justification.top = "left",
    legend.justification.left = "top",
    legend.justification.bottom = "right",
    legend.justification.inside = c(1, 1),
    legend.location = "plot",
    plot.title.position = "plot"
  )

print(deathpctmap)

ggsave("death_pct_722.png", plot = deathpctmap, width = 10, height = 6, dpi = 300)

#data characterization stats
merged_acs_wonder_xis %>%
  summarise(
    mean_pop_65plus = mean(pop_65plus, na.rm = TRUE),
    sd_pop_65plus = sd(pop_65plus, na.rm = TRUE),
    min_pop_65plus = min(pop_65plus, na.rm = TRUE),
    max_pop_65plus = max(pop_65plus, na.rm = TRUE),
    median_pop_65plus = median(pop_65plus, na.rm = TRUE),
    count = n()
  )

#temp scatterplot
temp_scatterplot <- ggplot(merged_acs_wonder_xis, aes(x = temp_cdd_sum, y = death_pct, color = climate_region)) + 
  geom_point(alpha = 0.7) +
  geom_smooth(aes(weight=pop_65plus), color = "black") + 
  labs(title = "Circulatory Mortality % in Populations 65+ and Cooling Degree Days (Temp)", subtitle = "Population Weighted, May-Sep 2023 in the Contiguous U.S.",
       x = "Excess Temp (CDD)", y = "% Deaths", color = "Climate Region") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
print(temp_scatterplot)
ggsave("temp_scatterplot_071525.png", plot = temp_scatterplot, width = 10, height = 6, dpi = 300)

#heatindex
merged_acs_wonder_xis$climate_region <- factor(
  merged_acs_wonder_xis$climate_region,
  levels = c(
    "Northwest", "West", "West North Central",
    "East North Central", "Central", "Northeast",
    "Southwest", "South", "Southeast"
  )
)
hi_scatterplot <- ggplot(merged_acs_wonder_xis, aes(x = heat_cdd_sum, y = death_pct, color = climate_region)) + 
  geom_point(alpha = 0.7) +
  geom_smooth(aes(weight= pop_65plus), color = "black") + 
  labs(title = "Circulatory Mortality % in Populations 65+ and Cooling Degree Days", subtitle = "Population Weighted, May-Sep 2023 in the Contiguous U.S.",
       x = "Excess Heat Index (CDD)", y = "% Deaths", color = "Climate Region") +
  scale_x_continuous(labels = comma) +   
  scale_y_continuous(labels = comma) +  
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)
  )
print(hi_scatterplot)

ggsave("hi_scatterplot_722.png", plot = hi_scatterplot, width = 10, height = 6, dpi = 300)

#hi no line
merged_acs_wonder_xis$climate_region <- factor(
  merged_acs_wonder_xis$climate_region,
  levels = c(
    "Northwest", "West", "West North Central",
    "East North Central", "Central", "Northeast",
    "Southwest", "South", "Southeast"
  )
)
hi_scatterplot_noline <- ggplot(merged_acs_wonder_xis, aes(x = heat_cdd_sum, y = death_pct, color = climate_region)) + 
  geom_point(alpha = 0.7) +
  labs(title = "Circulatory Mortality % in Populations 65+ and Cooling Degree Days", subtitle = "May-Sep 2023 in the Contiguous U.S.",
       x = "Excess Heat Index (CDD)", y = "% Deaths", color = "Climate Region") +
  scale_x_continuous(labels = comma) +   
  scale_y_continuous(labels = comma) +  
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)
  )
print(hi_scatterplot_noline)

ggsave("hi_scatterplot_noline_722.png", plot = hi_scatterplot_noline, width = 10, height = 6, dpi = 300)

#histogram
cddhistogram <- ggplot(merged_acs_wonder_xis, aes(x = heat_cdd_sum)) + 
  geom_histogram(binwidth = 200, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Cooling Degree Days (Heat)",
    subtitle = "May–Sep 2023",
    x = "Cooling Degree Days (CDD)",
    y = "Count"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    plot.subtitle = element_text(hjust = 0.5, size = 11)
  )

print(cddhistogram)
ggsave("cddhistogram_071525.png", plot = cddhistogram, width = 10, height = 6, dpi = 300)

#heatindexscatterplot modified lines
hi_multiple_lines <- ggplot(merged_acs_wonder_xis, aes(x = heat_cdd_sum, y = death_pct, color = climate_region)) + 
  #geom_point(alpha = 0.7) +
  geom_smooth(aes(weight=pop_65plus), se = FALSE) + 
  labs(
    title = "Circulatory Mortality % in Populations 65+ and Cooling Degree Days",
    subtitle = "May–Sep 2023 in the Contiguous U.S.",
    x = "Excess Heat (CDD)",
    y = "% Deaths",
    color = "Climate Region"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    plot.subtitle = element_text(hjust = 0.5, size = 11)
  )
print(hi_multiple_lines)
ggsave("hi_multiple_lines_071425.png", plot = hi_multiple_lines, width = 10, height = 6, dpi = 300)

#hi tiles
merged_acs_wonder_xis$climate_region <- factor(
  merged_acs_wonder_xis$climate_region,
  levels = c(
    "Northwest", "West", "West North Central",
    "East North Central", "Central", "Northeast",
    "Southwest", "South", "Southeast"
  )
)

tile_region_scatterplots <- ggplot(merged_acs_wonder_xis, aes(x = heat_cdd_sum, y = death_pct)) + 
  geom_point(aes(color = climate_region, size = pop_65plus), alpha = 0.7) +
  geom_smooth(aes(weight= pop_65plus), color = "black") + 
  scale_x_continuous(labels = scales::comma, limits = c(0, 5000))+
  ylim(0, 1.6) +
  facet_wrap(~ climate_region, nrow = 3, ncol = 3, scales = "fixed") +
  labs(
    title = "Circulatory Mortality % in Populations 65+ and Cooling Degree Days (Heat)",
    subtitle = "May-Sep 2023, by NOAA Climate Region",
    x = "Excess Heat (CDD)",
    y = "% Deaths",
    color = "Climate Region"
  ) +
  theme_classic() +
  theme(
    strip.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 16),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )+
  scale_size_continuous(name = "Population 65+", range = c(0.1, 5), labels = scales ::comma)

print(tile_region_scatterplots)

ggsave("tile_region_1017.png", plot = tile_region_scatterplots, width = 10, height = 8, dpi = 300)

#temp tiles
temp_region_scatterplots <- ggplot(merged_acs_wonder_xis, aes(x = temp_cdd_sum, y = death_pct)) + 
  geom_point(aes(color = climate_region, size = pop_65plus), alpha = 0.7) +
  geom_smooth(aes(weight= pop_65plus), color = "black") + 
  xlim(0, 5000) +
  ylim(0, 1.6) +
  facet_wrap(~ climate_region, nrow = 3, ncol = 3, scales = "fixed") +
  labs(
    title = "Circulatory Mortality % in populations 65+ and Cooling Degree Days (Temp)",
    subtitle = "May-Sep 2023, by NOAA Climate Region",
    x = "Excess Temp (CDD)",
    y = "% Deaths",
    color = "Climate Region"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    strip.text = element_text(size = 10) 
  )+
  scale_size_continuous(name = "Population 65+", range = c(0.1, 5))

print(temp_region_scatterplots)

ggsave("temp_tiles_071525.png", plot = temp_region_scatterplots, width = 10, height = 6, dpi = 300)

#heatindex excluding places under 10 deaths
ggplot(filteredwonder_lessthan10imputed, 
       aes(x = heat_cdd_sum, y = death_pct, color = climate_region, size = pop_65plus)) + 
  geom_point(alpha = 0.7) +  # Optional: makes overlapping points easier to see
  geom_smooth(method = lm, color = "black") + 
  labs(
    title = "Circulatory Mortality % in Populations 65+ and Cooling Degree Days\nMay–Sep 2023 (Counties with ≥10 Deaths)",
    x = "Excess Heat (CDD)", 
    y = "% Deaths", 
    color = "Climate Region",
    size = "Pop 65+"
  ) +
  theme_classic()

#heatindex with deaths plus 1 pct
heatindexplus1test <- ggplot(merged_acs_wonder_xis, aes(x = heat_cdd_sum, y = death_pctplus1, color = climate_region)) + 
  geom_point(size = 1.5) +
  geom_smooth(method = lm, color = "black") + 
  labs(title = "Circulatory Mortality % in populations 65+ (PLUS 1 TEST) and Cooling Degree Days May-Sep 2023 in the Contiguous U.S.",
       x = "Excess Heat (CDD)", y = "% Deaths (PLUS 1 TEST)", color = "Climate Region") +
  theme_classic()
ggsave("7_7_heatindexplus1test.png")

# % deaths by pop
ggplot(merged_acs_wonder_xis, aes(x = death_pct, y = pop_65plus, color = climate_region)) + 
  geom_point(size = 1.5) +
  geom_smooth(method = lm, color = "black") + 
  labs(title = "Circulatory Mortality % in populations 65+ May-Sep 2023 in the Contiguous U.S.",
       x = "death pct", y = "pop 65", color = "Climate Region") +
  theme_classic()

merged_acs_wonder_xis %>%
  summarise(
    mean_deaths = mean(Deaths_imputed, na.rm = TRUE),
    sd_deaths = sd(Deaths_imputed, na.rm = TRUE),
    min_deaths = min(Deaths_imputed, na.rm = TRUE),
    max_deaths = max(Deaths_imputed, na.rm = TRUE),
    median_deaths = median(Deaths_imputed, na.rm = TRUE),
    count = n()
  )

wonder_data %>%
  summarise(total_deaths_modified = sum(deaths_modified, na.rm = TRUE))

#by region
#northwest
northwest_data <- merged_acs_wonder_xis %>%
  filter(climate_region == "Northwest")

model <- lm(death_pct ~ heat_cdd_sum + pop_65plus, data = northwest_data)
summary(model)

#west
west_data <- merged_acs_wonder_xis %>%
  filter(climate_region == "West")

model <- lm(death_pct ~ heat_cdd_sum, data = west_data)
summary(model)

#southwest
southwest_data <- merged_acs_wonder_xis %>%
  filter(climate_region == "Southwest")

model <- lm(death_pct ~ heat_cdd_sum + pop_65plus, data = southwest_data)
summary(model)

#south
south_data <- merged_acs_wonder_xis %>%
  filter(climate_region == "South")

model <- lm(death_pct ~ heat_cdd_sum, data = south_data)
summary(model)

#southeast
southeast_data <- merged_acs_wonder_xis %>%
  filter(climate_region == "Southeast")

model <- lm(death_pct ~ heat_cdd_sum + pop_65plus, data = southeast_data)
summary(model)

#northeast
northeast_data <- merged_acs_wonder_xis %>%
  filter(climate_region == "Northeast")

model <- lm(death_pct ~ heat_cdd_sum, data = northeast_data)
summary(model)

#west north central
westnorthcentral_data <- merged_acs_wonder_xis %>%
  filter(climate_region == "West North Central")

model <- lm(death_pct ~ heat_cdd_sum +pop_65plus, data = westnorthcentral_data)
summary(model)

#east north central
eastnorthcentral_data <- merged_acs_wonder_xis %>%
  filter(climate_region == "East North Central")

model <- lm(death_pct ~ heat_cdd_sum, data = eastnorthcentral_data)
summary(model)

#central
central_data <- merged_acs_wonder_xis %>%
  filter(climate_region == "Central")

model <- lm(death_pct ~ heat_cdd_sum + pop_65plus, data = central_data)
summary(model)

#linear regression
Model <- lm(outcome~exposure + covariate, data = datatable 
            
#quasipoisson-- overdispersed data (variance is greater than 1)
var(merged_acs_wonder_xis$Deaths_imputed, na.rm=T)/mean(merged_acs_wonder_xis$Deaths_imputed, na.rm=T)
var(merged_acs_wonder_xis$death_pct, na.rm=T)/mean(merged_acs_wonder_xis$death_pct, na.rm=T)

#701.7448 >1
model <- gnm(Deaths_imputed ~ heat_cdd_scaled +offset(log(pop_65plus)), family = quasipoisson, data = merged_acs_wonder_xis)
summary(model)

#
model <- gnm(Deaths_imputed ~ heat_cdd_sum +offset(log(pop_65plus)), family = quasipoisson, data = merged_acs_wonder_xis)
summary(model)

#carBayes
library(CARBayes)
library(spdep)
library(sf)

merged_acs_wonder_xis_nona <- merged_acs_wonder_xis %>%  filter(is.na(heat_cdd_sum) == F & !GEOID %in% c("25007","53055","36085","25019"))

W.nb <- poly2nb(merged_acs_wonder_xis_nona, row.names = merged_acs_wonder_xis_nona$GEOID)
matrix1 <- nb2mat(W.nb, style = "B", zero.policy = TRUE)

model_CARBayes_bym <- S.CARbym(
  formula = Deaths_imputed ~ heat_cdd_scaled + offset(log(pop_65plus)),
  data = merged_acs_wonder_xis_nona,
  family = "poisson",
  W = matrix1,
  burnin = 20000,
  n.sample = 100000,
  thin = 10,
  verbose = TRUE
)

print(model_CARBayes_bym)

#Leroux
model_CARBayes_leroux <- S.CARleroux(
  formula = Deaths_imputed ~ heat_cdd_scaled + offset(log(pop_65plus)),
  data = merged_acs_wonder_xis_nona,
  family = "poisson",
  W = matrix1,
  burnin = 20000,
  n.sample = 100000,
  thin = 10,
  verbose = TRUE
)

print(model_CARBayes_leroux)

y.fit_bym <- model_CARBayes_bym$samples$fitted
estimate_ratio_bym <- t(t(y.fit_bym) / median(merged_acs_wonder_xis_nona$Deaths_imputed, na.rm=T))
merged_acs_wonder_xis_nona$estimate_ratio_median_bym <- apply(estimate_ratio_bym, 2, median)

#leroux
y.fit_leroux <- model_CARBayes_leroux$samples$fitted
estimate_ratio_leroux <- t(t(y.fit_leroux) / median(merged_acs_wonder_xis_nona$Deaths_imputed, na.rm=T))
merged_acs_wonder_xis_nona$estimate_ratio_median_leroux <- apply(estimate_ratio_leroux, 2, median, na.rm=T)

#bym
carbmap_bym <- ggplot(merged_acs_wonder_xis_nona) +
  geom_sf(aes(fill = estimate_ratio_median_bym)) +
  scale_fill_viridis_c(
    name = "Relative Risk",
    limits = c(0, 250)
  ) +
  labs(
    title = "CARBayes BYM Model"
  ) +
theme_void() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    plot.title = element_text(hjust = 0.5, size = 24),
    plot.subtitle = element_text(hjust = 0.5, size = 16),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.margin = margin(0, 0, 0, 0),
    legend.justification.top = "left",
    legend.justification.left = "top",
    legend.justification.bottom = "right",
    legend.justification.inside = c(1, 1),
    legend.location = "plot",
    plot.title.position = "plot"
  )

print(carbmap_bym)
ggsave("carbmap_bym_722.png", plot = carbmap_bym, width = 10, height = 6, dpi = 300)

#plus1 death test

merged_acs_wonder_xis <- merged_acs_wonder_xis %>%
  mutate(deaths_imputed_plus1 = Deaths_imputed + 1)

merged_acs_wonder_xis <- merged_acs_wonder_xis %>%
  mutate(
    death_pctplus1 = if_else(
      !is.na(deaths_imputed_plus1) & !is.na(pop_65plus) & pop_65plus > 0,
      (deaths_imputed_plus1 / pop_65plus) * 100,
      NA_real_
    )
  )
#exclude deaths less than 10
filteredwonder_lessthan10imputed <- merged_acs_wonder_xis %>%
  filter(Deaths_imputed >= 10)

filtereddeathpctmap <- ggplot(filteredwonder_lessthan10imputed) +
  geom_sf(aes(fill = death_pct), color = NA) +
  scale_fill_viridis_c(
    name = "Death %",
    na.value = "grey80"
    
  ) +
  labs(
    title = "Percent of Pop 65+ with Circulatory Cause of Death",
    subtitle = "May–Sep 2023, 10> Imputed Deaths excluded"
  ) +
  theme_minimal()

print(filtereddeathpctmap)


#deathplus1map
library(scales)

deathpctmapplus1 <- ggplot(merged_acs_wonder_xis) +
  geom_sf(aes(fill = squish(death_pctplus1, c(0, 4))), color = NA) +
  scale_fill_viridis_c(
    name = "Death plus 1 %",
    limits = c(0, 4)
  ) +
  labs(
    title = "Percent of Pop 65+ with Circulatory Cause of Death (PLUS 1 TEST)",
    subtitle = "May–Sep 2023",
    fill = "Death %"
  ) +
  theme_minimal()

print(deathpctmapplus1)
ggsave("death_pctplus1_070725.png", plot = deathpctmapplus1, width = 10, height = 6, dpi = 300)

mapview(merged_acs_wonder_xis, zcol = "death_pct")
#climate region tests
#climate_regions_modified
merged_acs_wonder_xis <- merged_acs_wonder_xis %>%
  mutate(
    climate_region_modified = case_when(
      state_abbr %in% c("IL", "IN", "KY", "MO", "OH", "TN", "WV") ~ "Central",
      state_abbr %in% c("IA", "MI", "MN", "WI") ~ "East North Central",
      state_abbr %in% c("DC", "CT", "DE", "ME", "MD", "MA", "NH", "NJ", "NY", "PA", "RI", "VT") ~ "Northeast",
      state_abbr %in% c("ID", "OR", "WA") ~ "Northwest",
      state_abbr %in% c("AR", "KS", "LA", "MS", "OK", "TX") ~ "South",
      state_abbr %in% c("AL", "FL", "GA", "NC", "SC", "VA") ~ "Southeast",
      state_abbr %in% c("CO", "NM", "UT") ~ "Southwest",
      state_abbr %in% c("CA", "NV") ~ "West",
      state_abbr %in% c("MT", "NE", "ND", "SD", "WY") ~ "West North Central",
      TRUE ~ NA_character_ 
    )
  )

#southwest test-- Southwest as defined by NOAA
sw_scatterplot <- ggplot(southwest_data, aes(x = heat_cdd_sum, y = death_pct, size=)) + 
  geom_point(alpha = 0.7) +
  geom_smooth(aes(weight= pop_65plus), color = "black") + 
  labs(title = "SOUTHWEST: Circulatory Mortality % in Populations 65+ and Cooling Degree Days", subtitle = "Population Weighted, May-Sep 2023",
       x = "Excess Heat (CDD)", y = "% Deaths") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        plot.subtitle = element_text(hjust = 0.5, size = 11))
print(sw_scatterplot)
ggsave("sw_scatterplot_071525.png", plot = sw_scatterplot, width = 10, height = 6, dpi = 300)

#sw minus Maricopa
southwest_data_no_maricopa <- merged_acs_wonder_xis %>%
  filter(climate_region == "Southwest" & !(County == "Maricopa County, AZ"))

nomaricopa_scatterplot <- ggplot(southwest_data_no_maricopa, 
                         aes(x = heat_cdd_sum, y = death_pct)) + 
  geom_point(alpha = 0.7) +
  geom_smooth(aes(weight = pop_65plus), color = "black") + 
  labs(
    title = "SOUTHWEST NO MARICOPA: Circulatory Mortality % in Populations 65+ and Cooling Degree Days", 
    subtitle = "Population Weighted, May–Sep 2023",
    x = "Excess Heat (CDD)", 
    y = "% Deaths"
  ) +
  scale_size_continuous(name = "Population 65+", range = c(0.1, 5)) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    plot.subtitle = element_text(hjust = 0.5, size = 11)
  )

print(nomaricopa_scatterplot)

ggsave("nomaricopa_071525.png", plot = nomaricopa_scatterplot, width = 10, height = 6, dpi = 300)

#sw minus AZ
sw_modified_data <- merged_acs_wonder_xis %>%
  filter(climate_region_modified == "Southwest")
#southwest test-- Southwest minus AZ 
sw_modified_scatterplot <- ggplot(sw_modified_data, aes(x = heat_cdd_sum, y = death_pct)) + 
  geom_point(alpha = 0.7) +
  geom_smooth(aes(weight= pop_65plus), color = "black") + 
  labs(title = "SOUTHWEST - AZ: Circulatory Mortality % in Populations 65+ and Cooling Degree Days", subtitle = "Population Weighted, May-Sep 2023 in the Contiguous U.S.",
       x = "Excess Heat (CDD)", y = "% Deaths", color = "Climate Region") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        plot.subtitle = element_text(hjust = 0.5, size = 11))
print(sw_modified_scatterplot)

ggsave("sw_minusAZ_scatterplot_071525.png", plot = sw_modified_scatterplot, width = 10, height = 6, dpi = 300)
