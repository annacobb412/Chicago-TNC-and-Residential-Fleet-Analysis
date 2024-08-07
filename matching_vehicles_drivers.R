library(tidyverse)
library(dplyr)

drivers = read.csv("TNC Drivers 2024 01 04.csv")
vehicles = read.csv("TNC Vehicles 2024 01 04.csv")

drivers_chicago = drivers %>% 
  filter(CITY == "Chicago") 

drivers_w = drivers_chicago %>% 
  pivot_wider(id_cols = c(DRIVER_START_MONTH,CITY,STATE,ZIP,MULTIPLE_TNPS),
              names_from = MONTH_REPORTED,
              values_from = NUMBER_OF_TRIPS)

unique_drivers = drivers_w %>% filter(sapply(`2024-01`,length) == 1 &
                                        sapply(`2024-02`,length) == 1 &
                                        sapply(`2024-03`,length) == 1 &
                                        sapply(`2024-04`,length) == 1)
unique_drivers_no_0s = unique_drivers %>% filter(`2024-01` != 0 | `2024-02` != 0 | `2024-03` != 0 | `2024-04` != 0)

vehicles_w = vehicles %>% 
  pivot_wider(id_cols = c(MAKE,MODEL,LAST_INSPECTION_MONTH,STATE,COLOR,YEAR,MULTIPLE_TNPS),
              names_from = MONTH_REPORTED,
              values_from = NUMBER_OF_TRIPS) 

unique_vehicles = vehicles_w %>% filter(sapply(`2024-01`,length) == 1 &
                                          sapply(`2024-02`,length) == 1 &
                                          sapply(`2024-03`,length) == 1 &
                                          sapply(`2024-04`,length) == 1)
unique_vehicles_no_0s = unique_vehicles %>% filter(`2024-01` != 0 | `2024-02` != 0 | `2024-03` != 0 | `2024-04` != 0)


vehicles_w = vehicles_w %>% 
  mutate(predicted_start_month = case_when(sapply(`2024-01`, is.null) & sapply(`2024-02`, is.null) & sapply(`2024-03`, is.null) ~ "2024-04",
                                         sapply(`2024-01`, is.null) & sapply(`2024-02`, is.null) ~ "2024-03",
                                         sapply(`2024-01`, is.null) ~ "2024-02",
                                         .default = "unknown"))
drivers_w = drivers_w %>% 
  mutate(predicted_start_month = case_when(sapply(`2024-01`, is.null) & sapply(`2024-02`, is.null) & sapply(`2024-03`, is.null) ~ "2024-04",
                                               sapply(`2024-01`, is.null) & sapply(`2024-02`, is.null) ~ "2024-03",
                                               sapply(`2024-01`, is.null) ~ "2024-02",
                                               .default = "unknown"))

recent_vehicles = vehicles_w %>% filter(vehicle_start_month != "unknown")
vehicles_02_2024 = vehicles_w %>% filter(predicted_start_month == "2024-02")
recent_drivers = drivers_w %>% filter(str_detect(DRIVER_START_MONTH,"2024"))
drivers_02_2024 = drivers_w %>% filter(predicted_start_month == "2024-02")

# THIS IS WHERE I STOPPED WORKING ON JUNE 19, 2024

vehicles_added_04_2024 = vehicles_w %>% 
  filter(sapply(`2024-01`, is.null) & sapply(`2024-02`, is.null) & sapply(`2024-03`, is.null))

vehicles_added_04_2024 = vehicles_w %>% 
  filter(sapply(`2024-03`, function(x) is.na(x)))

vehicles_no_all_0s = vehicles_w %>% 
  filter(sapply(`2024-01`, function(x) if (length(x) == 1) {x != 0} else {TRUE}) &
           sapply(`2024-02`, function(x) if (length(x) == 1) {x != 0} else {TRUE}) &
           sapply(`2024-03`, function(x) if (length(x) == 1) {x != 0} else {TRUE}) &
           sapply(`2024-04`, function(x) if (length(x) == 1) {x != 0} else {TRUE}))

vehicles_all_0s = vehicles_w %>% 
  filter(!(sapply(`2024-01`, function(x) if (length(x) == 1) {x != 0} else {TRUE}) &
           sapply(`2024-02`, function(x) if (length(x) == 1) {x != 0} else {TRUE}) &
           sapply(`2024-03`, function(x) if (length(x) == 1) {x != 0} else {TRUE}) &
           sapply(`2024-04`, function(x) if (length(x) == 1) {x != 0} else {TRUE})))
  

vehicles_no_0s = vehicles_w %>% 
  drop_na() %>% 
  filter(`2024-01` != 0 & `2024-02` != 0 & `2024-03` != 0 & `2024-04` != 0)
  

vehicles_w_2_TNCs = vehicles_all_years_no_0s %>% 
  filter(MULTIPLE_TNPS == "true")
vehicles_w_1_TNC = vehicles_all_years_no_0s %>% 
  filter(MULTIPLE_TNPS == "false")

# match_count = 0
# multiple_matches = 0
# matched_rows = data.frame()
# for (ind in 1:nrow(drivers_w)) {
#   
#   one_driver = drivers_w[ind,]
#   eps = 0
#   
#   if (one_driver$MULTIPLE_TNPS == "true") {
    matched = vehicles_w_2_TNCs %>%
      filter(`2024-01` %in% one_driver$`2024-01` &
               `2024-02` %in% one_driver$`2024-02` &
               `2024-03` %in% one_driver$`2024-03` &
               `2024-04` %in% one_driver$`2024-04`)
#     matched = vehicles_w_2_TNCs %>%
#       filter(((`2024-01` + eps) %in% unlist(one_driver$`2024-01`) | (`2024-01` - eps) %in% unlist(one_driver$`2024-01`)) &
#                ((`2024-02` + eps) %in% unlist(one_driver$`2024-02`) | (`2024-02` - eps) %in% unlist(one_driver$`2024-02`)) &
#                ((`2024-03` + eps) %in% unlist(one_driver$`2024-03`) | (`2024-03` - eps) %in% unlist(one_driver$`2024-03`)) &
#                ((`2024-04` + eps) %in% unlist(one_driver$`2024-04`) | (`2024-04` - eps) %in% unlist(one_driver$`2024-04`)))
# 
#   } else {
#     matched = vehicles_w_1_TNC %>%
#       filter(((`2024-01` + eps) %in% unlist(one_driver$`2024-01`) | (`2024-01` - eps) %in% unlist(one_driver$`2024-01`)) &
#                ((`2024-02` + eps) %in% unlist(one_driver$`2024-02`) | (`2024-02` - eps) %in% unlist(one_driver$`2024-02`)) &
#                ((`2024-03` + eps) %in% unlist(one_driver$`2024-03`) | (`2024-03` - eps) %in% unlist(one_driver$`2024-03`)) &
#                ((`2024-04` + eps) %in% unlist(one_driver$`2024-04`) | (`2024-04` - eps) %in% unlist(one_driver$`2024-04`)))
#   }
#     
#   if (nrow(matched) == 1) {
#     phrase = paste("found a match for driver",ind)
#     print(phrase)
#     match_count = match_count + 1
#     new_row = cbind(one_driver,matched)
#     matched_rows = rbind(matched_rows,new_row)
#   } else if (nrow(matched) > 1) {
#     multiple_matches = multiple_matches + 1
#   } else {
#     phrase = paste("done with driver",ind)
#     print(phrase)
#   }
# 
# }

match_count_v2 = 0
multiple_matches_v2 = 0
matched_rows_v2 = data.frame()
m_matched_rows_v2 = data.frame()
unmatched_rows_v2 = data.frame()
for (ind in 1:nrow(vehicles_no_all_0s)) {
  
  vehicle = vehicles_no_all_0s[ind,]
  
  matched = drivers_w %>% 
    filter(sapply(`2024-04`, function(x) vehicle$`2024-04` %in% x) &
             sapply(`2024-03`, function(x) vehicle$`2024-03` %in% x) &
             sapply(`2024-02`, function(x) vehicle$`2024-02` %in% x) &
             sapply(`2024-01`, function(x) vehicle$`2024-01` %in% x) &
             MULTIPLE_TNPS == vehicle$MULTIPLE_TNPS)
  
  if (nrow(matched) == 1) {
    phrase = paste("found a match for vehicle",ind)
    print(phrase)
    match_count_v2 = match_count_v2 + 1
    new_row = cbind(vehicle,matched)
    matched_rows_v2 = rbind(matched_rows_v2,new_row)
  } else if (nrow(matched) > 1) {
    multiple_matches_v2 = multiple_matches_v2 + 1
    new_row = cbind(vehicle,matched)
    m_matched_rows_v2 = rbind(m_matched_rows_v2,new_row)
    phrase = paste("found multiple matches for vehicle",ind)
    print(phrase)
  } else {
    phrase = paste("done with vehicle",ind)
    print(phrase)
  }
  
}

colnames(matched_rows_v2) = c("MAKE","MODEL","LAST INSPECTION MONTH","STATE",
                              "COLOR","YEAR","MULTIPLE_TNPS","ROW","2024-04","2024-03",
                              "2024-02","2024-01","DRIVER_START_MONTH","DRIVER_CITY",
                              "DRIVER_STATE","DRIVER_ZIP","DRIVER_MULTIPLE_TNPS",
                              "DRIVER_2024-04","DRIVER_2024-03","DRIVER_2024-02","DRIVER_2024-01")
colnames(m_matched_rows_v2) = c("MAKE","MODEL","LAST INSPECTION MONTH","STATE",
                              "COLOR","YEAR","MULTIPLE_TNPS","ROW","2024-04","2024-03",
                              "2024-02","2024-01","DRIVER_START_MONTH","DRIVER_CITY",
                              "DRIVER_STATE","DRIVER_ZIP","DRIVER_MULTIPLE_TNPS",
                              "DRIVER_2024-04","DRIVER_2024-03","DRIVER_2024-02","DRIVER_2024-01")
mrv2 = matched_rows_v2 %>% 
  select(-`DRIVER_2024-01`,-`DRIVER_2024-03`,-`DRIVER_2024-02`,-`DRIVER_2024-04`)
write.csv(mrv2,"matched_2024_01_04.csv")

matched_veh = mrv2 %>% select(-DRIVER_START_MONTH,-DRIVER_CITY,-DRIVER_MULTIPLE_TNPS,-DRIVER_STATE,-DRIVER_ZIP)
m_matched_veh = m_matched_rows_v2 %>% select(-DRIVER_START_MONTH,-DRIVER_CITY,-DRIVER_MULTIPLE_TNPS,-DRIVER_STATE,-DRIVER_ZIP,-`DRIVER_2024-01`,-`DRIVER_2024-03`,-`DRIVER_2024-02`,-`DRIVER_2024-04`)
unmatched_veh = vehicles_no_all_0s %>% anti_join(matched_veh) %>% anti_join(m_matched_veh)
