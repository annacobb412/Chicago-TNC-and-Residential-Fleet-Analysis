library(tidyverse)
library(dplyr)

vehicles = read.csv("Transportation_Network_Providers_-_Vehicles_20240516.csv") %>% 
  mutate(date_with_day = paste0(MONTH_REPORTED,"-01"),
         date = as.POSIXct(date_with_day,tz = "", "%Y-%m-%d")) %>% 
  select(-date_with_day,-MONTH_REPORTED)

trips = read.csv("avg_daily_trip_count_by_month.csv") %>% 
  mutate(date_with_day = paste0(month_year,"-01"),
         date = as.POSIXct(date_with_day,tz = "", "%m-%Y-%d")) %>% 
  select(-date_with_day,-month_year)

unique_MM = vehicles %>% distinct(MAKE,MODEL)

unique_HEV = vehicles %>% 
  filter(grepl("hybrid",MODEL,ignore.case = TRUE)) %>% 
  distinct(MAKE,MODEL)

unique_tesla = vehicles %>% 
  filter(MAKE == "Tesla") %>% 
  distinct(MODEL)
unique_honda = vehicles %>% 
  filter(grepl("honda",MAKE,ignore.case = TRUE)) %>% 
  distinct(MODEL,.keep_all = TRUE)

honda_crv_count = vehicles %>% 
  filter(grepl("Honda",MAKE,ignore.case = TRUE)) %>% 
  filter(MODEL %in% c("Cr-V","Crv","Cr V")) %>% 
  group_by(date) %>% 
  summarize(total_CRVs = sum(count))

teslas_count = vehicles %>% 
  filter(grepl("Tesla",MAKE,ignore.case = TRUE)) %>% 
  filter(!(MODEL %in% c("Prius", "Grand Caravan"))) %>% 
  group_by(date) %>% 
  summarize(total_teslas = sum(count))

joint = left_join(teslas_count,honda_crv_count, by = join_by(date))

coeff = 110
ggplot(data = trips) +
  geom_line(aes(x = date, y = avg_daily_trip_count), color = "lightblue") +
  scale_y_continuous(name = "Average Daily TNC Trip Count",sec.axis = sec_axis( trans=~./coeff, name="Number of Vehicles")) +
  geom_line(data = teslas_count, aes(x = date, y = total_teslas*coeff)) +
  geom_line(data = honda_crv_count, aes(x = date, y = total_CRVs*coeff), linewidth = 1.5) +
  geom_vline(xintercept = as.POSIXct("2022-05-01","%Y-%m-%d"), color = "purple", linetype = "dashed") +
  geom_vline(xintercept = as.POSIXct("2022-09-15","%Y-%m-%d"), color = "purple") +
  geom_vline(xintercept = as.POSIXct("2020-09-01","%Y-%m-%d"), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.POSIXct("2023-06-01","%Y-%m-%d"), color = "red") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Total Tesla Vehicles and Honda CR-Vs \nRegistered to Drive for a TNC in Chicago") +
  xlab("Date [month-year]") +
  ylab(" ")

ggplot(data = honda_crv_count) +
  geom_line(aes(x = date, y = total_CRVs)) +
  geom_vline(xintercept = as.POSIXct("2022-05-01","%Y-%m-%d"), color = "blue") +
  geom_vline(xintercept = as.POSIXct("2022-09-15","%Y-%m-%d"), color = "red") +
  geom_vline(xintercept = as.POSIXct("2020-09-01","%Y-%m-%d"), color = "purple") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Total Honda CRVs Registered with a TNC in Chicago") +
  xlab("Date [month-year]") +
  ylab(" ")
