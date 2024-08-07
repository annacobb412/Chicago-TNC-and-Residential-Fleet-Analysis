# Date: 08/01/24
# Author: Anna Cobb
# Purpose: this code is supposed to plot the results from the 
# Chicago_TNC_vehicle_analysis.R script. The result from that script was a CSV
# containing TNC vehicle fleet powertrain breakdown on a monthly basis over time.

library(tidyverse)
library(dplyr)

tnc_fleet = read.csv("TNC_monthly_composition_all_years.csv") %>% 
  mutate(datetime = as.POSIXct(date, format = "%Y-%m-%d"),
         powertrain = case_when(powertrain == "pct_EV" ~ "Battery Electric",
                                powertrain == "pct_Hybrid" ~ "Hybrid",
                                powertrain == "pct_ICE" ~ "ICE",
                                powertrain == "pct_no_ID" ~ "Not Identified",
                                .default = "Plug-in Hybrid"))
breaks = seq.POSIXt(from = as.POSIXct("2018-06-01", format = "%Y-%m-%d"), to = as.POSIXct("2024-04-01", format = "%Y-%m-%d"), by = "6 months")
ggplot(data = tnc_fleet, aes(x = datetime, y= fleet_pct, color = powertrain)) +
  geom_line() +
  ylab("% of Total Vehicle Fleet") +
  xlab("Month Reported") +
  labs(color = "Powertrain") +
  ggtitle("Chicago TNC Vehicle Fleet Powertrain Evolution over Time") +
  scale_x_datetime(breaks = breaks, date_labels = "%m-%Y") +
  scale_y_continuous(breaks = seq(0,100, by = 10), limits = c(0,100)) +
  geom_vline(xintercept = as.POSIXct("2022-05-01",format = "%Y-%m-%d"), color = "blue", linetype = "dashed") +
  geom_vline(xintercept = as.POSIXct("2022-09-15",format = "%Y-%m-%d"), color = "blue") +
  geom_vline(xintercept = as.POSIXct("2020-09-01",format = "%Y-%m-%d"), color = "black", linetype = "dashed") +
  geom_vline(xintercept = as.POSIXct("2023-06-01",format = "%Y-%m-%d"), color = "black") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
