library(tidyverse)
library(tidycensus)
library(dplyr)
library(ggmap)
library(choroplethrZip)
library(sf)
library(extrafont)
library(viridis)
data(zip.regions)
options(tigris_use_cache = TRUE)

south_zc = c("60655","60633","60827","60628","60643","60617",
             "60649","60637","60615","60653","60609","60632",
             "60638","60629","60636","60621","60652","60620",
             "60619")

TNC_drivers = read.csv("TNC Drivers 04-2024 12-2019.csv")
CA_sf = st_read("Boundaries - Community Areas/geo_export_ca2499f2-5635-434c-ab80-5247bfba606e.shp")
CA_sf_no_AP = CA_sf %>% filter(area_num_1 != 56 & area_num_1 != 76)
ZC_sf = st_read("cb_2020_us_zcta520_500k/cb_2020_us_zcta520_500k.shp")
CA_demo = read.csv("Community_Area_Demographics.csv") %>% 
  rowwise() %>% mutate(highest_race = c("White","Hispanic","Black","Asian","Other")[which.max(c_across(WHITE:OTHER))])
CA_prof = read.csv("Community_Area_profiles_2023.csv") %>% 
  mutate(percent_rent = RENT_OCC_HU/HU_TOT)
equivalencies = read.csv("Community area and zip code equivalency.csv")
CA_sf = CA_sf %>% mutate(GEOID = as.numeric(area_num_1)) %>% left_join(CA_demo, by = "GEOID") %>% left_join(CA_prof, by = "GEOID")

zcta_data = get_acs(geography = "zcta", zcta = equivalencies$ZCTA5, variables = c("B25008_001","B25008_002","B25008_003")) 
zcta_processed = zcta_data %>% 
  mutate(variable_def = case_when(variable == "B25008_001" ~ "tot_units",
                                  variable == "B25008_002" ~ "own_units",
                                  .default = "rent_units")) %>% 
  select(-NAME,-variable) %>% 
  pivot_wider(names_from = variable_def, values_from = c(estimate,moe)) %>% 
  mutate(percent_rent = estimate_rent_units/estimate_tot_units, percent_own = estimate_own_units/estimate_tot_units)

grouped_trips = TNC_drivers %>% 
  drop_na() %>% 
  group_by(ZIP,MONTH_REPORTED) %>% 
  summarize(avg_monthly_trips = mean(NUMBER_OF_TRIPS),
            num_drivers = n())

# filtering ZCTA for community areas
ZC_CA = ZC_sf %>%
  filter(ZCTA5CE20 %in% equivalencies$ZCTA5) %>% 
  filter(ZCTA5CE20 != 60007)

joined_zips = ZC_CA %>% 
  left_join(grouped_trips, by = c("ZCTA5CE20" = "ZIP")) %>% 
  left_join(zcta_processed, by = c("ZCTA5CE20" = "GEOID"))

joined_zips_monthly = joined_zips %>% 
  filter(MONTH_REPORTED == "2024-04") 

ggplot() +
  geom_sf(data = joined_zips_monthly, aes(fill = avg_monthly_trips)) +
  #geom_sf(data = CA_sf,inherit.aes = FALSE, alpha = 0, linewidth = .3, aes(color = highest_race)) +
  geom_sf(data = CA_sf,inherit.aes = FALSE, alpha = 0, linewidth = .3, color = "black") +
  scale_y_continuous(breaks = c(41.7, 41.8, 41.9, 42), limits = c(41.61, 42.03)) +
  scale_x_continuous(breaks = seq(-88,-87.5,by = .1), limits = c(-88,-87.5)) +
  scale_fill_viridis_c(limits = c(0,max(joined_zips_monthly$avg_monthly_trips))) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(fill = "Average Monthly\nTrips: December 2019") +
  theme(axis.text=element_text(size=11)) +
  theme(text = element_text(family = "Times New Roman")) +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), axis.title = element_text(size = 13))
ggplot() +
  geom_sf(data = joined_zips_monthly, aes(fill = percent_rent)) +
  #geom_sf(data = CA_sf,inherit.aes = FALSE, alpha = 0, linewidth = .3, aes(color = highest_race)) +
  geom_sf(data = CA_sf,inherit.aes = FALSE, alpha = 0, linewidth = .3, color = "black") +
  scale_y_continuous(breaks = c(41.7, 41.8, 41.9, 42), limits = c(41.61, 42.03)) +
  scale_x_continuous(breaks = seq(-88,-87.5,by = .1), limits = c(-88,-87.5)) +
  scale_fill_viridis_c(limits = c(0,max(joined_zips_monthly$percent_rent))) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(fill = "% of Housing Units\nthat are Rented") +
  theme(axis.text=element_text(size=11)) +
  theme(text = element_text(family = "Times New Roman")) +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), axis.title = element_text(size = 13))

tot_drivers = sum(joined_zips_monthly$num_drivers)
joined_zips_monthly = joined_zips_monthly %>% 
  mutate(pct_drivers = num_drivers/tot_drivers,
         pct_drivers_rent = pct_drivers*percent_rent,
         pct_drivers_own = pct_drivers*percent_own)
pct_drivers_rent = sum(joined_zips_monthly$pct_drivers_rent)
pct_drivers_own = sum(joined_zips_monthly$pct_drivers_own)
