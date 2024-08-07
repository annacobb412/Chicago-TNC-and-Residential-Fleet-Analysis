library(tidyverse)
library(tidycensus)
library(dplyr)
library(ggmap)
library(choroplethrZip)
library(sf)
data(zip.regions)
options(tigris_use_cache = TRUE)

south_zc = c("60655","60633","60827","60628","60643","60617",
             "60649","60637","60615","60653","60609","60632",
             "60638","60629","60636","60621","60652","60620",
             "60619")

TNC_drivers = read.csv("Transportation_Network_Providers_-_Drivers.csv")
CA_sf = st_read("Boundaries - Community Areas/geo_export_ca2499f2-5635-434c-ab80-5247bfba606e.shp")

# filtering driver data for those from Chicago (and the desired months/years reported)
TNC_drivers_new = TNC_drivers %>% filter(CITY == "Chicago") %>% filter(MONTH_REPORTED == "2023-01") 
TNC_drivers_old = TNC_drivers %>% filter(CITY == "Chicago") %>% filter(MONTH_REPORTED == "2019-11")

# grouping & getting counts of the drivers by zip code
driver_count_zip_new = TNC_drivers_new %>% group_by(ZIP) %>% summarize(total_new = n())
driver_count_zip_new_s = TNC_drivers_new %>% mutate(in_south = case_when(ZIP %in% south_zc ~ "south",
                                                                        .default = "not south")) %>% 
  group_by(in_south) %>% summarize(total_new = n())
driver_count_zip_old = TNC_drivers_old %>% group_by(ZIP) %>% summarize(total_old = n()) 

# setting the count to zero for anywhere it's NA (confused how this would happen)
driver_count_zip_new$total_new[is.na(driver_count_zip_new$total_new)] = 0
driver_count_zip_old$total_old[is.na(driver_count_zip_old$total_old)] = 0

# getting the percent of drivers in each zip code
driver_count_zip_new = driver_count_zip_new %>% 
  mutate(total_new_pc = 100*total_new/sum(total_new))
driver_count_zip_old = driver_count_zip_old %>% 
  mutate(total_old_pc = 100*total_old/sum(total_old))

# filtering the zip code data for cook county (and Illinois)
chicago_zips = zip.regions %>% filter(county.name == "cook" & state.name == "illinois")
chicago_zips[is.na(chicago_zips)] = 0

joined_zips = chicago_zips %>% 
  left_join(driver_count_zip_new, by = c("region" = "ZIP")) %>% 
  left_join(driver_count_zip_old, by = c("region" = "ZIP")) %>% 
  mutate(pc_diff = total_new_pc - total_old_pc)
drivers_with_zip = joined_zips %>% 
  select(region,total_new_pc) %>% 
  rename(value = total_new_pc) %>% 
  distinct()
drivers_with_zip$value[is.na(drivers_with_zip$value)] = 0
list_of_chicago_zips = chicago_zips$region

# For plot of just one month of reporting:
lat_val_south_dt = 41.845 # this value marks southern border of downtown
lat_val_north_dt = 41.91 # this value marks northern border of downtown
dt_CA = CA_sf %>% filter(area_num_1 %in% c(8,32,33))
CA_sf_no_AP = CA_sf %>% filter(!(area_num_1 %in% c(56,76)))
zips_unmatched = subset(joined_zips,!(region %in% list_of_chicago_zips))
zip_choropleth(drivers_with_zip,
               legend = " % of Total TNC\nDriver Residences",
               title = "",
               num_colors = 1) +
  geom_sf(data = CA_sf_no_AP,inherit.aes = FALSE, alpha = 0, linewidth = .2, color = "black") +
  # geom_sf(data = dt_CA,inherit.aes = FALSE, linewidth = .6, alpha = 0, color = "red") +
  # geom_hline(yintercept = lat_val_north_dt, linetype = "dashed") +
  # geom_hline(yintercept = lat_val_south_dt, linetype = "dashed") +
  scale_y_continuous(breaks = c(41.7, 41.8, 41.9, 42), limits = c(41.61, 42.03)) +
  scale_x_continuous(breaks = seq(-87.85,-87.55,by = .1), limits = c(-87.87,-87.5)) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(axis.text=element_text(size=11)) +
  theme(text = element_text(family = "Times New Roman")) +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), axis.title = element_text(size = 13))

# anna figuring things out:
me = c(rep(0,100),rep(5,74))
meep = joined_zips %>% 
  select(region) %>% 
  mutate(value = case_when(region %in% south_zc ~ 0, .default = 5))
zip_choropleth(meep,
               legend = "confusion",
               title = "",
               num_colors = 1) +
  geom_sf(data = CA_sf_no_AP,inherit.aes = FALSE, alpha = 0, linewidth = .2, color = "black") +
  geom_sf(data = dt_CA,inherit.aes = FALSE, linewidth = .6, alpha = 0, color = "red") +
  geom_hline(yintercept = lat_val_north_dt, linetype = "dashed") +
  geom_hline(yintercept = lat_val_south_dt, linetype = "dashed") +
  scale_y_continuous(breaks = c(41.7, 41.8, 41.9, 42), limits = c(41.61, 42.03)) +
  scale_x_continuous(breaks = seq(-87.85,-87.55,by = .1), limits = c(-87.87,-87.5)) +
  theme_bw() +
  theme(axis.text=element_text(size=11)) +
  theme(text = element_text(family = "Times New Roman")) +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), axis.title = element_text(size = 13))


# For plot of difference between two time months of reporting:
change_drivers_with_zip = joined_zips %>% 
  select(region,pc_diff) %>% 
  rename(value = pc_diff) %>% 
  distinct()
zip_choropleth(change_drivers_with_zip,
               county_zoom = "17031",
               legend = "% Change in \nDriver Residences",
               num_colors = 1,
               title = "Changes from January 2020 to February 2023") +
  geom_sf(data = CA_sf_no_AP,inherit.aes = FALSE, alpha = 0, linewidth = .2, color = "black") +
  geom_sf(data = dt_CA,inherit.aes = FALSE, linewidth = .6, alpha = 0, color = "red") +
  geom_hline(yintercept = lat_val_north_dt, linetype = "dashed") +
  geom_hline(yintercept = lat_val_south_dt, linetype = "dashed") +
  scale_y_continuous(breaks = c(41.7, 41.8, 41.9, 42), limits = c(41.61, 42.03)) +
  scale_x_continuous(breaks = seq(-87.85,-87.55,by = .1), limits = c(-87.87,-87.5)) +
  theme_bw() +
  theme(axis.text=element_text(size=11)) +
  theme(text = element_text(family = "Times New Roman")) +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), axis.title = element_text(size = 13))

