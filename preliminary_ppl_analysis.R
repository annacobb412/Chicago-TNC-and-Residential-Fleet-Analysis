library(dplyr)
library(tidyverse)

ppl_vehicles = read.csv("public_passenger_license_data.csv")
ppl_filtered = ppl_vehicles %>% 
  filter(vehicle_type %in% c("Taxi","Livery","Jitney")) %>% 
  mutate(last_valid_datetime = as.POSIXct(last_valid_date,format = "%Y-%m-%d %H:%M:%S")) %>% 
  arrange(public_vehicle_number,last_valid_datetime) %>% 
  mutate(first_valid_datetime = case_when(lag(public_vehicle_number) == public_vehicle_number ~ lag(last_valid_datetime),
                                          .default = last_valid_datetime - months(1))) %>% 
  select(-X) %>% 
  filter(status == "ACTIVE") %>% 
  mutate()