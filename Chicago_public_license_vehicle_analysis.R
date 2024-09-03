# Date: 08/20/24
# Author: Anna Cobb
# Purpose: using dataset from fueleconomy.gov to classify the Chicago TNC vehicles
# from all years that they were recorded. HEADS UP: this code takes quite a while
# to run.

library(dplyr)
library(tidyverse)
library(stringdist)

#------------------------------DEFINING FUNCTIONS-------------------------------
# DESCRIPTION: Function below deals with long names that may have been shortened 
# (e.g., C40 Recharge vs. C40 Recharge Twin)
# INPUTS: 
# - tnc_models: all TNC models of a given powertrain that were able to be matched 
# to a vehicle listed in the FE data (includes make)
# - tnc_model_years: the same as tnc_models but includes the year of the vehicle
# - atv_label: given powertrain you are looking at ("hybrid","ev",etc.)
# OUTPUTS:
# - tnc_model_years: s
add_shortened_names = function(tnc_models,tnc_model_years,atv_label) {
  not_captured = anti_join(tnc_models,tnc_model_years,by = "make_model_year") 
  # ^^these are now all models that don't actually exist for the year listed 
  # (as in they hand't been manufactured yet in the year listed) AND models
  # incorrectly captured by not including the year (see more details in hybrid section)
  not_captured_models = not_captured %>% group_by(make_model_year,make_model,YEAR) %>% summarize(count_models = sum(count))
  not_captured_models_v2 = not_captured_models
  nc_count = sum(not_captured_models$count_models)
  print(paste("Initial discrepancy in model classifications:",nc_count,"vehicles"))
  for (ind in 1:nrow(not_captured_models)) {
    one_make_model = not_captured_models$make_model[ind]
    vehicle_yr = not_captured_models$YEAR[ind]
    model_listed = fe_vehicles %>% filter(str_detect(make_model,regex(one_make_model,ignore_case = TRUE)))
    atv_types = model_listed %>% group_by(atvType) %>% summarize(count = n())
    if (nrow(atv_types) == 1 & vehicle_yr %in% model_listed$year) {
      if (atv_types$atvType[1] == atv_label) {
        new_rows = models_matched %>% filter(str_detect(make_model_year,not_captured_models$make_model_year[ind]))
        tnc_model_years = rbind(tnc_model_years,new_rows)
        not_captured_models_v2 = not_captured_models_v2 %>% filter(!(make_model_year == not_captured_models$make_model_year[ind]))
      }
    }
  }
  nc_count_v2 = sum(not_captured_models_v2$count_models)
  print(paste("Final discrepancy in model classifications:",nc_count_v2,"vehicles"))
  return(tnc_model_years)
}

#----------------READING IN TNC VEHICLE DATA & PROCESSING-----------------------
ppl_vehicles = read.csv("public_passenger_license_data.csv")
ppl_filtered = ppl_vehicles %>% 
  filter(vehicle_type %in% c("Taxi","Livery","Jitney") & status == "ACTIVE") %>% 
  mutate(last_valid_datetime = as.POSIXct(last_valid_date,format = "%Y-%m-%d %H:%M:%S")) %>% 
  arrange(public_vehicle_number,last_valid_datetime) %>% 
  mutate(first_valid_datetime = case_when(lag(public_vehicle_number) == public_vehicle_number ~ lag(last_valid_datetime),
                                          .default = last_valid_datetime - months(1))) %>% 
  select(-X)
#----------READING IN FUELECONOMY.GOV VEHICLE DATASET & PROCESSING--------------
fe_vehicles = read.csv("fueleconomy_vehicles_list.csv") %>% 
  mutate(make = case_when(make == "Chevy" ~ "Chevrolet",
                          .default = make)) %>% 
  rename(old_model = model) %>% 
  mutate(model_no_WD = str_remove(old_model,regex(" [a-zA-Z]WD| \\dWD")),
         model = str_remove(model_no_WD,regex(" \\(\\d+ inch Wheels\\)",ignore_case = TRUE)),
         make_model = tolower(paste(make,model)),
         make_base_model = tolower(paste(make,baseModel))) %>% 
  relocate(make,model,year,atvType,fuelType,make_model,make_base_model)

atv_order = c("", "FFV", "Diesel", "Hybrid", "Plug-in Hybrid", "EV", "Bifuel (CNG)", "Bifuel (LPG)", "CNG", "FCV", NA)
fe_vehicles = fe_vehicles %>% 
  mutate(atvType = factor(atvType, levels = atv_order, ordered = TRUE)) %>% 
  group_by(make_model,year) %>% 
  slice_min(order_by = atvType, n = 1, with_ties = FALSE) %>% 
  ungroup() %>% 
  mutate(atvType = as.character(atvType))

# filling in the gaps in make + model manufacturing years (e.g., if in the FE
# dataset, toyota prius is listed for the years 2008 & 2010-2024, I want to fill
# in 2009)
fe_vehicles = fe_vehicles %>%
  group_by(make_model) %>%
  complete(year = full_seq(c(min(year), 2024), 1)) %>%
  fill(everything(), .direction = "down") %>%
  ungroup() %>% 
  mutate(make_model_year = paste(make_model,year),
         make_base_model_year = paste(make_base_model,year)) %>% 
  filter(!(is.na(make_model)))

# filtering out makes from TNC vehicles that are not in fuel economy list
tnc_makes = tnc_vehicles %>% group_by(MAKE) %>% summarize(tnc_count = n())
fe_makes = fe_vehicles %>% group_by(make) %>% summarize(count = n())
not_captured_make = tnc_makes %>% filter(!(str_detect(MAKE,regex(paste(fe_makes$make,collapse = "|"),ignore_case = TRUE))))
not_captured_make_count = sum(not_captured_make$tnc_count)
tnc_vehicles = tnc_vehicles %>% filter(!(MAKE %in% not_captured_make$MAKE))

#------------------CORRECTING KNOWN TYPOS IN TNC MAKE & MODEL NAMES-------------
tnc_vehicles = tnc_vehicles %>% 
  mutate(MAKE = case_when(str_detect(MAKE,regex("chev|chv",ignore_case = TRUE)) ~ "Chevrolet",
                          str_detect(MAKE,regex("Chr",ignore_case = TRUE)) ~ "Chrysler",
                          str_detect(MAKE,regex("toy",ignore_case = TRUE)) ~ "Toyota",
                          str_detect(MAKE,regex("ford",ignore_case = TRUE)) ~ "Ford",
                          str_detect(MAKE,regex("ram",ignore_case = TRUE)) ~ "Ram",
                          str_detect(MAKE,regex("cardillac|cadilac",ignore_case = TRUE)) ~ "Cadillac",
                          str_detect(MAKE,regex("doge",ignore_case = TRUE)) ~ "Dodge",
                          str_detect(MAKE,regex("hoda",ignore_case = TRUE)) ~ "Honda",
                          str_detect(MAKE,regex("Hydundia",ignore_case = TRUE)) ~ "Hyundai",
                          str_detect(MAKE,regex("infinity",ignore_case = TRUE)) ~ "Infiniti",
                          str_detect(MAKE,regex("jmc|general motors|gmc",ignore_case = TRUE)) ~ "GMC",
                          str_detect(MAKE,regex("licoin|lincoin",ignore_case = TRUE)) ~ "Lincoln",
                          str_detect(MAKE,regex("Mecede|Mercede",ignore_case = TRUE)) ~ "Mercedes-Benz",
                          str_detect(MAKE,regex("volkswagon",ignore_case = TRUE)) ~ "Volkswagen",
                          str_detect(MAKE,regex("satum",ignore_case = TRUE)) ~ "Saturn",
                          str_detect(MAKE,regex("rolls",ignore_case = TRUE)) ~ "Rolls-Royce",
                          str_detect(MAKE,regex("Mazada|Naza",ignore_case = TRUE)) ~ "Mazda",
                          str_detect(MAKE,regex("kia",ignore_case = TRUE)) ~ "Kia",
                          str_detect(MAKE,regex("baic|buick",ignore_case = TRUE)) ~ "Buick",
                          str_detect(MAKE,regex("bmw|baw",ignore_case = TRUE)) ~ "BMW",
                          .default = MAKE)) %>% 
  mutate(MODEL = case_when(str_detect(MODEL,regex("\\dseries|\\d series|\\d-series",ignore_case = TRUE)) ~ paste(str_extract(MODEL,regex("\\d")),"Series"),
                           str_detect(MODEL,regex("bolt",ignore_case = TRUE)) ~ "Bolt EV", # fe vehicles don't include bolt along and "bolt" will match to "volt" before "bolt EV"
                           str_detect(MODEL,regex("E-Niro|E Niro",ignore_case = TRUE)) ~ "Niro Electric",
                           str_detect(MODEL,regex("town",ignore_case = TRUE)) & MAKE == "Chrysler" ~ "Town and Country",
                           str_detect(MODEL,regex("mazda",ignore_case = TRUE)) ~ str_extract(MODEL,regex("\\d")),
                           str_detect(MODEL,regex("caravan",ignore_case = TRUE)) & MAKE == "Dodge" ~ "Caravan/Ram Van",
                           str_detect(MODEL,regex("G sedan",ignore_case = TRUE)) & MAKE == "Infiniti" ~ "G20",
                           str_detect(MODEL,regex("c max energi|cmax energi|c-max energi",ignore_case = TRUE)) ~ "C-MAX Energi Plug-in Hybrid",
                           .default = MODEL))
#----------------CORRECTING UNKNOWN TYPOS IN TNC MAKE & MODEL NAMES-------------
make_model_fe = fe_vehicles %>% distinct(make_model,.keep_all = TRUE) 
make_model_tnc = tnc_vehicles %>% 
  group_by(MAKE,MODEL,MONTH_REPORTED) %>% 
  summarize(count = n()) %>% 
  mutate(make_model = paste(MAKE,MODEL))
# these are the make + model combos listed in the TNC vehicle fleet that are not 
# in the FE vehicle data (potentially due to typos)
not_captured_make_model = make_model_tnc %>% 
  filter(!(str_detect(make_model,regex(paste(make_model_fe$make_model,collapse = "|"),ignore_case = TRUE)) | 
             str_detect(make_model,regex(paste(make_model_fe$make_base_model,collapse = "|"),ignore_case = TRUE)))) %>% 
  drop_na()

for (ind in 1:nrow(not_captured_make_model)) {
  if (ind %% 10 == 0) {print(paste("Vehicle", ind, "/", nrow(not_captured_make_model)))}
  make_model_nc = not_captured_make_model$make_model[ind]
  # use stringsim function to determine how similar the "not captured" make model
  # combos are to all make model combos in the fuel economy dataset
  meep = make_model_fe %>% 
    mutate(mm_nc = make_model_nc) %>% 
    select(make_model,make_base_model,mm_nc) %>% 
    mutate(sim_mm = stringsim(tolower(make_model),tolower(mm_nc),method = "jw"),
           sim_mm_base = stringsim(tolower(make_base_model),tolower(mm_nc),method = "jw")) %>% 
    arrange(desc(sim_mm)) %>% 
    drop_na()
  max_mm = max(meep$sim_mm)
  max_mm_base = max(meep$sim_mm_base)
  not_captured_make_model[ind,"max_sim_mm_value"] = max_mm
  not_captured_make_model[ind,"max_sim_mm"] = meep$make_model[which.max(meep$sim_mm)]
  not_captured_make_model[ind,"max_sim_mm_base_value"] = max_mm_base
  not_captured_make_model[ind,"max_sim_mm_base"] = meep$make_base_model[which.max(meep$sim_mm_base)]
}
# safe_matches are all the TNC make model combos that are similar enough to make
# model combos in the fuel economy dataset that I deem it safe to assume that 
# what's listed in the fuel economy dataset is what the TNC driver meant
safe_matches = not_captured_make_model %>% 
  filter(max_sim_mm_value >= 0.9 | max_sim_mm_base_value >= 0.9) %>% 
  mutate(final_make_model = case_when(max_sim_mm_value > max_sim_mm_base_value ~ max_sim_mm,
                                      max_sim_mm_value < max_sim_mm_base_value ~ max_sim_mm_base,
                                      .default = max_sim_mm))
# these are the leftover vehicles that can't be classified due to typos or 
# incorrectly supplied make/model (e.g., Acura Camry)
leftover = not_captured_make_model %>% filter(max_sim_mm_value < 0.9 & max_sim_mm_base_value < 0.9)
leftover_count = sum(leftover$count)
print(paste("# of TNC vehicles with no match after checking for both known & unknown typos is:",as.character(leftover_count)))
leftover_vehicles = leftover %>% 
  group_by(MONTH_REPORTED) %>% 
  summarize(vehicle_count = sum(count)) %>% 
  mutate(powertrain = "not_identified") %>% 
  select(powertrain,MONTH_REPORTED,vehicle_count)

# creating lists of make model combos to match to the tnc vehicle dataset directly
make_model_fe_patterns <- paste(make_model_fe$make_model, collapse = "|")
make_base_model_patterns <- paste(make_model_fe$make_base_model, collapse = "|")
safe_matches_patterns <- paste(safe_matches$make_model, collapse = "|")

# specifying for each vehicle whether or not it has a match in the three lists 
# created above. If it's one of the not captured models that we figured out a 
# safe match for, we replace the original make_model with the make model combo 
# we determined is a safe match
all_models <- tnc_vehicles %>%
  group_by(MAKE, MODEL, YEAR, MONTH_REPORTED) %>% 
  summarize(count = n(), .groups = 'drop') %>%
  drop_na() %>% 
  mutate(make_model_v1 = paste(MAKE, MODEL)) %>%
  mutate(make_model = case_when(str_detect(make_model_v1, regex(make_model_fe_patterns, ignore_case = TRUE)) | str_detect(make_model_v1, regex(make_base_model_patterns, ignore_case = TRUE)) ~ make_model_v1,
                                str_detect(make_model_v1, regex(safe_matches_patterns)) ~ map_chr(make_model_v1, ~ {match_index <- which(str_detect(.x, safe_matches$make_model))
                                if (length(match_index) > 0) safe_matches$final_make_model[match_index[1]] else "no match"}), TRUE ~ "no match"))
models_matched = all_models %>% 
  filter(make_model != "no match") %>% 
  mutate(make_model_year = paste(make_model,YEAR))
#----------------CREATING FUEL ECONOMY MODEL LISTS BY POWERTRAIN----------------
ev_list = fe_vehicles %>% filter(atvType == "EV") %>% filter(make != "Polestar") # getting rid of the model "2" 
phev_list = fe_vehicles %>% filter(atvType == "Plug-in Hybrid")
hybrid_list = fe_vehicles %>% filter(atvType == "Hybrid")
fc_list = fe_vehicles %>% filter(atvType == "FCV") # fuel cell
ice_list = fe_vehicles %>% filter(atvType == "")
#-----------------------------SELECTING HYBRIDS---------------------------------
# interesting note: having the make model year (so you get something like Hyundai Ioniq 2024) prevents similarly named cars with different
# powertrains from getting selected (Hyundai Ioniq 5 2020) whereas without the year, Hyundai Ioniq (a hybrid) on the hybrid list would 
# select Hyundai Ioniq 5 from the models_matched list
hybrid_model_year_list = hybrid_list$make_model_year
tnc_hybrid_model_years = models_matched %>% 
  filter((str_detect(make_model_year,regex(paste(hybrid_model_year_list,collapse = "|"),ignore_case = TRUE)) |
           str_detect(MODEL,regex("hybrid|hybird|hibrid|hybryd",ignore_case = TRUE))) & 
           !(str_detect(MODEL,regex("plug in|plug-in|plugin|phev| e-tron| electric| ev",ignore_case = TRUE))))
hybrid_model_list = hybrid_list$make_model
tnc_hybrid_models = models_matched %>%
  filter((str_detect(make_model,regex(paste(hybrid_model_list,collapse = "|"),ignore_case = TRUE)) |
            str_detect(MODEL,regex("hybrid|hybird|hibrid|hybryd",ignore_case = TRUE))) &
            !(str_detect(MODEL,regex("plug in|plug-in|plugin|phev| e-tron| electric| ev",ignore_case = TRUE))))
tnc_hybrid_model_years = add_shortened_names(tnc_models =  tnc_hybrid_models,
                                             tnc_model_years = tnc_hybrid_model_years,
                                             atv_label = "Hybrid")
tnc_hybrid_model_years = tnc_hybrid_model_years %>% 
  mutate(powertrain = "Hybrid")

#--------------------------------SELECTING BEVS---------------------------------
ev_model_year_list = ev_list$make_model_year
tnc_ev_model_years = models_matched %>% 
  filter((str_detect(make_model_year,regex(paste(ev_model_year_list,collapse = "|"),ignore_case = TRUE)) |
           str_detect(MAKE,regex("Tesla|Rivian|Polestar",ignore_case = TRUE))   |
           str_detect(MODEL,regex("bolt| EV$|electric|e-tron",ignore_case = TRUE))) &
           !(str_detect(MODEL,regex("Caravan/Grand Caravan|I30|I35"))))
ev_model_list = ev_list$make_model
tnc_ev_models = models_matched %>% 
  filter((str_detect(make_model,regex(paste(ev_model_list,collapse = "|"),ignore_case = TRUE)) |
            str_detect(MAKE,regex("Tesla|Rivian|Polestar",ignore_case = TRUE))   |
            str_detect(MODEL,regex("bolt| EV$|electric|e-tron",ignore_case = TRUE))) &
           !(str_detect(MODEL,regex("Caravan/Grand Caravan|I30|I35"))))
tnc_ev_model_years = add_shortened_names(tnc_ev_models,tnc_ev_model_years,"EV")
tnc_ev_model_years = tnc_ev_model_years %>% 
  mutate(powertrain = "EV")

#-------------------------------SELECTING PHEVS---------------------------------
phev_model_year_list = phev_list$make_model_year
tnc_phev_model_years = models_matched %>% 
  filter(str_detect(make_model_year,regex(paste(phev_model_year_list,collapse = "|"),ignore_case = TRUE)) |
           str_detect(MODEL,regex("plug-in|plug in|phev",ignore_case = TRUE)) |
           str_detect(MODEL,regex("530e|330e|745e|Edrive|energi"))) # these are specific models I've checked for
phev_model_list = phev_list$make_model
tnc_phev_models = models_matched %>% 
  filter(str_detect(make_model,regex(paste(phev_model_list,collapse = "|"),ignore_case = TRUE)) |
           str_detect(MODEL,regex("plug-in|plug in|phev",ignore_case = TRUE)) |
           str_detect(MODEL,regex("530e|330e|745e|Edrive|energi"))) # these are specific models I've checked for
tnc_phev_model_years = add_shortened_names(tnc_phev_models,tnc_phev_model_years,"Plug-in Hybrid")
tnc_phev_model_years = tnc_phev_model_years %>% 
  mutate(powertrain = "PHEV")

#-------------------------------SELECTING FCEVS---------------------------------
fc_model_year_list = fc_list$make_model_year
tnc_fc_model_years = models_matched %>% 
  filter(str_detect(make_model_year,regex(paste(fc_model_year_list,collapse = "|"),ignore_case = TRUE)))
tnc_fc_model_years = tnc_fc_model_years %>% 
  mutate(powertrain = "FCV")

#-----------------------------SELECTING ICE VEHICLES----------------------------
tnc_ice_vehicles = anti_join(models_matched,tnc_ev_model_years) %>% 
  anti_join(tnc_fc_model_years) %>% 
  anti_join(tnc_hybrid_model_years) %>% 
  anti_join(tnc_phev_model_years) %>% 
  mutate(powertrain = "ICE")

#--------PUTTING ALL VEHICLE COUNTS TOGETHER AND WRITING TO CSV-----------------
tnc_vehicles = rbind(tnc_ice_vehicles,
                     tnc_ev_model_years,
                     tnc_phev_model_years,
                     tnc_hybrid_model_years,
                     tnc_fc_model_years)
# write.csv(tnc_vehicles,"TNC_vehicles_classified_all_years.csv")
monthly_composition = tnc_vehicles %>% 
  filter(powertrain != "FCV") %>% 
  group_by(powertrain,MONTH_REPORTED) %>% 
  summarize(vehicle_count = sum(count)) %>% 
  rbind(leftover_vehicles) %>% 
  pivot_wider(names_from = powertrain, values_from = vehicle_count) %>% 
  rowwise() %>% 
  mutate(total_vehicles = sum(ICE,EV,PHEV,Hybrid,not_identified),
         pct_ICE = 100*round(ICE/total_vehicles,4),
         pct_EV = 100*round(EV/total_vehicles,4),
         pct_Hybrid = 100*round(Hybrid/total_vehicles,4),
         pct_PHEV = 100*round(PHEV/total_vehicles,4),
         pct_no_ID = 100*round(not_identified/total_vehicles,4),
         date = as.POSIXct(paste0(MONTH_REPORTED,"-01"),format = "%Y-%m-%d")) %>% 
  pivot_longer(cols = c(pct_ICE,pct_EV,pct_PHEV,pct_Hybrid,pct_no_ID), names_to = "powertrain", values_to = "fleet_pct") %>% 
  ungroup()
write.csv(monthly_composition,"TNC_monthly_composition_IL_all_years.csv")
