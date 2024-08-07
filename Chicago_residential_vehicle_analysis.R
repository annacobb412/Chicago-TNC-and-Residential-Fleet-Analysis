# Date:
# Author:
# Purpose: 

library(dplyr)
library(tidyverse)

# reading in files with vehicle counts for chicago over time
ev_count = read.csv("Chicago_EV_Counts.csv")
ff_count = read.csv("Chicago_FlexFuel_Count.csv")
h_count = read.csv("Chicago_Hybrid_Count.csv")
all_count = read.csv("Chicago_all_count.csv")

# processing
alt_veh = rbind(ev_count %>% mutate(powertrain = "EV"),
                ff_count %>% mutate(powertrain = "Flex_Fuel"),
                h_count %>% mutate(powertrain = "Hybrid"))
alt_veh_count = alt_veh %>% 
  mutate(count = rowSums(alt_veh %>% select(starts_with("X")),na.rm = TRUE),
         date_format = case_when(powertrain == "EV" ~ paste0(substr(Date,1,nchar(Date)-2),"20",substr(Date,nchar(Date)-1,nchar(Date))),
                                 .default = Date),
         date = case_when(powertrain == "EV" ~ as.POSIXct(date_format, format = "%m/%d/%Y"),
                          .default = as.POSIXct(date_format, format = "%Y-%m-%d"))) %>% 
  select(date,powertrain,count) %>% 
  filter(date > as.POSIXct("2017-12-15",format = "%Y-%m-%d")) %>% 
  pivot_wider(names_from = powertrain, values_from = count)
all_count_new = data.frame(date = alt_veh_count$date,all = c(rep(all_count$Passenger_Vehicle_Count[1],12),
                                                               rep(all_count$Passenger_Vehicle_Count[2],11),
                                                               rep(all_count$Passenger_Vehicle_Count[3],12),
                                                               rep(all_count$Passenger_Vehicle_Count[4],12),
                                                               rep(all_count$Passenger_Vehicle_Count[5],12),
                                                               rep(all_count$Passenger_Vehicle_Count[6],12),
                                                               rep(all_count$Passenger_Vehicle_Count[7],5)))
all_veh = left_join(alt_veh_count,all_count_new, by = join_by(date)) %>% 
  mutate(ICE = all - EV - Hybrid,
         pct_ICE = 100*round(ICE/all,4),
         pct_EV = 100*round(EV/all,4),
         pct_Hybrid = 100*round(Hybrid/all,4)) %>% 
  pivot_longer(cols = pct_ICE:pct_Hybrid,
               names_to = "powertrain",
               values_to = "vehicle_pct") %>% 
  mutate(powertrain = case_when(powertrain == "pct_EV" ~ "'Electric'",
                                powertrain == "pct_Hybrid" ~ "'Hybrid'",
                                .default = "ICE"))

breaks = seq.POSIXt(from = as.POSIXct("2018-06-01", format = "%Y-%m-%d"), to = as.POSIXct("2024-04-01", format = "%Y-%m-%d"), by = "6 months")
ggplot(data = all_veh, aes(x = date, y = vehicle_pct, color = powertrain)) +
  geom_line() +
  ylab("% of Total Vehicle Fleet") +
  xlab("Month Reported") +
  labs(color = "Powertrain") +
  ggtitle("Chicago Residential Vehicles Powertrain Evolution over Time") +
  scale_x_datetime(breaks = breaks, date_labels = "%m-%Y") +
  scale_y_continuous(breaks = seq(0,100, by = 10), limits = c(0,100)) +
  geom_vline(xintercept = as.POSIXct("2022-05-01",format = "%Y-%m-%d"), color = "blue", linetype = "dashed") +
  geom_vline(xintercept = as.POSIXct("2022-09-15",format = "%Y-%m-%d"), color = "blue") +
  geom_vline(xintercept = as.POSIXct("2020-09-01",format = "%Y-%m-%d"), color = "black", linetype = "dashed") +
  geom_vline(xintercept = as.POSIXct("2023-06-01",format = "%Y-%m-%d"), color = "black") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

tnc_fleet = read.csv("TNC_monthly_composition_IL_all_years.csv") %>% 
  mutate(datetime = as.POSIXct(paste0(MONTH_REPORTED,"-15"), format = "%Y-%m-%d")) %>% 
  select(datetime,ICE,EV,Hybrid,PHEV,total_vehicles) %>% 
  rename(ICE_TNC = ICE, EV_TNC = EV, Hybrid_TNC = Hybrid, PHEV_TNC = PHEV, total_TNC = total_vehicles)

tnc_res = tnc_fleet %>% 
  distinct() %>% 
  left_join(all_veh %>% select(-vehicle_pct,-powertrain) %>%  distinct(), 
            by = join_by("datetime" == "date")) %>% 
  mutate(EV_overall = EV - EV_TNC,
         hybrid_overall = Hybrid - Hybrid_TNC - PHEV_TNC, # CHANGE THIS IF YOU FIND OUT WHERE PHEVs GO
         ICE_overall = ICE - ICE_TNC)

all_veh_adjusted = tnc_res %>% 
  select(datetime,EV_overall, hybrid_overall,ICE_overall) %>% 
  mutate(total = EV_overall + hybrid_overall + ICE_overall,
         pct_ICE = 100*round(ICE_overall/total,4),
         pct_EV = 100*round(EV_overall/total,4),
         pct_hybrid = 100*round(hybrid_overall/total,4)) %>% 
  pivot_longer(cols = pct_ICE:pct_hybrid,
               names_to = "powertrain",
               values_to = "vehicle_pct") %>% 
  mutate(powertrain = case_when(powertrain == "pct_ICE" ~ "ICE",
                                powertrain == "pct_hybrid" ~ "'Hybrid'",
                                .default = "'Electric'"))

formatted_all_veh = all_veh %>% 
  rename(datetime = date, EV_overall = EV, hybrid_overall = Hybrid,
         ICE_overall = ICE, total = all) %>% 
  select(-Flex_Fuel) %>% 
  select(datetime,EV_overall,hybrid_overall,ICE_overall,total, powertrain,vehicle_pct)
  
all_res_data_combined = rbind(all_veh_adjusted %>% mutate(status = "adjusted"),
                          formatted_all_veh %>% mutate(status = "unadjusted"))
ggplot(data = all_res_data_combined, aes(x = datetime, y = vehicle_pct, color = powertrain,
                                         linetype = status)) +
  geom_line() +
  ylab("% of Total Vehicle Fleet") +
  xlab("Month Reported") +
  labs(color = "Powertrain") +
  ggtitle("Chicago Residential Vehicles Powertrain Evolution over Time\n[with TNCs registered in IL subtracted]") +
  scale_x_datetime(breaks = breaks, date_labels = "%m-%Y") +
  scale_y_continuous(breaks = seq(0,100, by = 10), limits = c(0,100)) +
  geom_vline(xintercept = as.POSIXct("2022-05-01",format = "%Y-%m-%d"), color = "blue", linetype = "dashed") +
  geom_vline(xintercept = as.POSIXct("2022-09-15",format = "%Y-%m-%d"), color = "blue") +
  geom_vline(xintercept = as.POSIXct("2020-09-01",format = "%Y-%m-%d"), color = "black", linetype = "dashed") +
  geom_vline(xintercept = as.POSIXct("2023-06-01",format = "%Y-%m-%d"), color = "black") +
  theme_bw() +
  labs(linetype = "Data Status") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

