# Date: 6/26/24
# Author: Anna Cobb
# Purpose: Generating preliminary plots and comparisons of Chicago TNC fleet
# composition and residential fleet composition

library(dplyr)
library(tidyverse)

tnc_breakdown = read.csv("TNC_monthly_composition_all_years.csv")

res_all_count = read.csv("Chicago_all_count.csv")
res_EV_count = read.csv("Chicago_EV_Counts.csv")
res_FF_count = read.csv("Chicago_FlexFuel_Count.csv")
res_hybrid_count = read.csv("Chicago_Hybrid_Count.csv")

# a bit of data formatting
tnc_formatted = tnc_breakdown %>% 
  mutate(fleet_count = round(total_vehicles*fleet_pct)) %>% 
  select(-ICE,-EV,-PHEV,-Hybrid,-X) %>% 
  mutate(powertrain = case_when(powertrain == "pct_ICE" ~ "ICE",
                                powertrain == "pct_EV" ~ "EV",
                                powertrain == "pct_PHEV" ~ "PHEV",
                                .default = "Hybrid"))
res_EV_count = 

ggplot(data = monthly_composition) +
  geom_line(aes(x = date, y = fleet_pct, color = powertrain, group = powertrain)) +
  scale_x_datetime(breaks = "6 months", date_labels = "%m-%Y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.8, hjust = .8)) +
  xlab("") +
  ylab("Composition [%]") +
  ggtitle("Composition of TNC Fleet in Chicago") +
  labs(color = "Powertrain")