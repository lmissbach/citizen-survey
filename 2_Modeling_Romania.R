# 0     General ####
# Author: L. Missbach (missbach@mcc-berlin.net)
# Date: 15th of November 2024

# 0.1   Packages ####

if(!require("pacman")) install.packages("pacman")

p_load("boot", "broom", "fixest", "ggpubr", "ggrepel",
       "ggsci", "Hmisc", "knitr", "kableExtra", "openxlsx", "rattle", "scales", "tidymodels", "tidyverse", "xtable")

options(scipen=999)

# 0.2   Load data ####

path_0 <- "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Romania"

# 0.2.1 Household budget survey data ####

expenditures <- read_csv(sprintf("%s/1_Data_Clean/expenditures_items_Romania.csv", path_0))

household_information <- read_csv(sprintf("%s/1_Data_Clean/household_information_Romania.csv", path_0))

# 0.2.2 Carbon intensities ####

carbon_intensities_0 <- read.xlsx("../2_Data/Carbon_Intensities_Full_All_Gas.xlsx", sheet = "Romania")

GTAP_code            <- read_delim("../2_Data/GTAP10.csv", ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

carbon_intensities   <- left_join(GTAP_code, carbon_intensities_0, by = c("Number"="GTAP"))%>%
  select(-Explanation, - Number)%>%
  mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))%>%
  group_by(GTAP)%>%
  summarise(across(CO2_Mt:Total_HH_Consumption_MUSD, ~ sum(.)))%>%
  ungroup()%>%
  mutate(CO2_direct_Gas = ifelse(GTAP == "gasgdt", CO2_direct,0))%>%
  mutate(CO2_t_per_dollar_global      = CO2_Mt/            Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_national    = CO2_Mt_within/     Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_electricity = CO2_Mt_Electricity/Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_transport   = CO2_Mt_Transport/  Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_gas         = CO2_Mt_Gas/        Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_gas_direct  = CO2_direct_Gas/    Total_HH_Consumption_MUSD)%>%
  select(GTAP, starts_with("CO2_t"))

rm(carbon_intensities_0, GTAP_code)

# 0.2.3 Matching tables ####

item_codes <- read.xlsx(sprintf("%s/3_Matching_Tables/Item_Codes_Description_Romania.xlsx", path_0))

item_fuels <- read.xlsx(sprintf("%s/3_Matching_Tables/Item_Fuel_Concordance_Romania.xlsx", path_0), colNames = FALSE)%>%
  rename(item_code = X2, fuel = X1, Description = X3)

item_gtap <- read.xlsx(sprintf("%s/3_Matching_Tables/Item_GTAP_Concordance_Romania.xlsx", path_0))%>%
  select(-Explanation)%>%
  pivot_longer(-GTAP, values_to = "item_code", names_to = "drop")%>%
  filter(!is.na(item_code))%>%
  select(-drop)%>%
  arrange(GTAP, item_code)%>%
  mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))%>%
  distinct()

# 0.2.4 Codes ####

# TBA

District.Code    <- read_csv("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Spain/2_Codes/District.Code.csv")
Province.Code    <- read_csv("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Spain/2_Codes/Province.Code.csv")
Urban.1.Code     <- read_csv("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Spain/2_Codes/Urban.Code.1.csv")
Urban.2.Code     <- read_csv("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Spain/2_Codes/Urban.Code.2.csv")
Gender.Code      <- read_csv("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Spain/2_Codes/Gender.Code.csv")
Nationality.Code <- read_csv("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Spain/2_Codes/Nationality.Code.csv")
Education.Code   <- read_csv("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Spain/2_Codes/Education.Code.csv")
Occupation.Code  <- read_csv("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Spain/2_Codes/Occupation.Code.csv")
Industry.Code    <- read_csv("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Spain/2_Codes/Industry.Code.csv")
Tenant.Code      <- read_csv("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Spain/2_Codes/Tenant.Code.csv")
Housing.Code     <- read_csv("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Spain/2_Codes/Housing.Code.csv")
Water.Code       <- read_csv("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Spain/2_Codes/Water.Code.csv")
Heating.Code     <- read_csv("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Spain/2_Codes/Heating.Code.csv")

# 0.2.5 Supplementary data ####

exchange.rate  <- 0.24676177 # Source as usual
inflation.rate <- 1/1.01344 # Source: IMF

# 1   Transform and clean expenditures ####

# TBA

data_0.1 <- expenditures %>%
  left_join(item_gtap, by = "item_code")%>%
  filter(GTAP != "deleted")%>%
  # deleting 30% of total expenditures ad 131,797 observations
  # After check: Cleaning at item-level not strictly necessary
  # group_by(item_code)%>%
  # mutate(mean_0   = mean(expenditures_year),
  #       sd_0      = sd(expenditures_year),
  #       median_0  = quantile(expenditures_year, prob = 0.5))%>%
  # ungroup()%>%
  # mutate(z_score = (expenditures_year - mean_0)/sd_0)
  # After check: Cleaning at expenditure-level not strictly necessary
  # group_by(hh_id)%>%
  # mutate(tot_exp = sum(expenditures_year))%>%
# ungroup()
left_join(select(carbon_intensities, GTAP, CO2_t_per_dollar_national, CO2_t_per_dollar_transport, CO2_t_per_dollar_gas, CO2_t_per_dollar_gas_direct), by = "GTAP")%>%
  mutate(CO2_t_per_dollar_national   = ifelse(is.na(CO2_t_per_dollar_national),  0,CO2_t_per_dollar_national),
         CO2_t_per_dollar_transport  = ifelse(is.na(CO2_t_per_dollar_transport), 0,CO2_t_per_dollar_transport),
         CO2_t_per_dollar_gas        = ifelse(is.na(CO2_t_per_dollar_gas),       0,CO2_t_per_dollar_gas),
         CO2_t_per_dollar_gas_direct = ifelse(is.na(CO2_t_per_dollar_gas_direct),0,CO2_t_per_dollar_gas_direct))%>%
  # Adjusting for dollar/Euro
  mutate(CO2_t_per_euro_national   = CO2_t_per_dollar_national/exchange.rate,
         CO2_t_per_euro_transport  = CO2_t_per_dollar_transport/exchange.rate,
         CO2_t_per_euro_gas        = CO2_t_per_dollar_gas/exchange.rate,
         CO2_t_per_euro_gas_direct = CO2_t_per_dollar_gas_direct/exchange.rate)%>%
  mutate(CO2_t_national           = CO2_t_per_euro_national*expenditures_year*inflation.rate,
         CO2_t_transport          = CO2_t_per_euro_transport*expenditures_year*inflation.rate,
         # Applying carbon pricing in gas sector everywhere warranted, given that also manufacturing firms may require gas for heating
         # Also little difference --> latter is true, but still
         CO2_t_gas                = CO2_t_per_euro_gas*expenditures_year*inflation.rate,
         CO2_t_gas_direct         = CO2_t_per_euro_gas_direct*expenditures_year*inflation.rate)

data_0.2 <- data_0.1 %>%
  group_by(hh_id)%>%
  summarise(hh_expenditures_EURO_2018 = sum(expenditures_year),
            CO2_t_national            = sum(CO2_t_national),
            CO2_t_transport           = sum(CO2_t_transport),
            CO2_t_gas                 = sum(CO2_t_gas),
            CO2_t_gas_direct          = sum(CO2_t_gas_direct))%>%
  ungroup()

data_0.3 <- data_0.1 %>%
  left_join(select(item_fuels, item_code, fuel), by = "item_code")%>%
  filter(!is.na(fuel))%>%
  group_by(hh_id, fuel)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()%>%
  pivot_wider(names_from = "fuel", values_from = "expenditures_year", values_fill = 0, names_prefix = "Exp_")

# 1.2   Transform household information ####

# TBA

household_information <- household_information %>%
  left_join(District.Code)%>%   
  left_join(Province.Code)%>%   
  left_join(Urban.1.Code)%>%     
  left_join(Urban.2.Code)%>%     
  left_join(Gender.Code)%>%      
  left_join(Nationality.Code)%>% 
  left_join(Education.Code)%>%   
  left_join(Occupation.Code)%>%  
  left_join(Industry.Code)%>%    
  left_join(Tenant.Code)%>%      
  left_join(Housing.Code)%>%     
  left_join(Water.Code)%>%       
  left_join(Heating.Code)%>%
  rename_all(tolower)%>%
  #TBA
  select()

rm(District.Code, Province.Code, Urban.1.Code, Urban.2.Code, Gender.Code, Nationality.Code, Education.Code, Occupation.Code, Industry.Code,   
   Tenant.Code, Housing.Code, Water.Code, Heating.Code)

# 1.3   Compile final dataset ####

data_0 <- left_join(data_0.2, data_0.3, by = "hh_id")%>%
  mutate_at(vars(starts_with("Exp_")), ~ ifelse(is.na(.),0,.))

data_1 <- left_join(household_information, data_0, by = "hh_id")%>%
  mutate(household_expenditures_pc = hh_expenditures_EURO_2018/hh_size)%>%
  mutate(Expenditure_Group_5       = as.numeric(binning(household_expenditures_pc, bins = 5,  method = c("wtd.quantile"), weights = hh_weights)),
         Expenditure_Group_10      = as.numeric(binning(household_expenditures_pc, bins = 10, method = c("wtd.quantile"), weights = hh_weights)))%>%
  select(-household_expenditures_pc, -household_income_pc)

rm(data_0.1, data_0.2, data_0.3, data_0, expenditures_items, household_information,
   item_gtap, item_fuels, item_codes, carbon_intensities,
   exchange.rate, inflation.rate)

# 2     Microsimulation ####

# 2.1   Simulating a carbon price (in the transport and heating sector) ####

data_1.1 <- data_1 %>%
  # No carbon price in 2018 yet
  mutate(exp_CO2_transport           = CO2_t_transport*45,
         exp_CO2_national            = CO2_t_national*45,
         exp_CO2_gas                 = CO2_t_gas*45,
         exp_CO2_gas_direct          = CO2_t_gas_direct*45)%>%
  mutate(exp_CO2_price               = exp_CO2_transport + exp_CO2_gas_direct)%>%
  mutate(burden_CO2_transport        = exp_CO2_transport/hh_expenditures_EURO_2018,
         burden_CO2_national         = exp_CO2_national/hh_expenditures_EURO_2018,
         burden_CO2_price            = exp_CO2_price/hh_expenditures_EURO_2018)%>%
  mutate(t_weighted = (CO2_t_gas_direct+CO2_t_transport)*hh_weights, # 898,975,547 = 898 MtO2 --> Emissionen sind zu hoch
         t_weighted_national = CO2_t_national*hh_weights,            # 1,302,834,950 = 1,3 MtCO2 --> Emissionen sind zu hoch 
         Exp_Benzin = Exp_Benzin*hh_weights)                         # 47 Milliarden Euro --> kommt ungef?hr hin.

# TBA 
data_1.2 <- data_1.1 %>%
  select(hh_id, hh_size, hh_weights, bundesland, urban_type, urban_01, ost_west,
         age_hhh, gender_hhh, nationality, education, ausbildung, employment, industry,
         building_year, building_type, renting, space, heating_type, heating_fuel,
         motorcycle.01, tv.01, refrigerator.01, dishwasher.01, washing_machine.01, stove.e.01, number_of_cars, hh_expenditures_EURO_2018,
         starts_with("CO2_"), starts_with("Exp_"), Expenditure_Group_5, Expenditure_Group_10)

write_rds(data_1.2, "H:/6_Citizen_Survey/2_Data/Microdata/Microdata_Transformed_Romania.rds")

rm(data_1, data_1.1, data_1.2)