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

appliances <- read_csv(sprintf("%s/1_Data_Clean/appliances_0_1_Romania.csv", path_0))

# 0.2.2 Carbon intensities ####

carbon_intensities_0 <- read.xlsx("../2_Data/Carbon_Intensities_Full_All_Gas_EU.xlsx", sheet = "Romania")

GTAP_code            <- read_delim("../2_Data/GTAP10.csv", ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

carbon_intensities   <- left_join(GTAP_code, carbon_intensities_0, by = c("Number"="GTAP"))%>%
  select(-Explanation, - Number)%>%
  mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))%>%
  group_by(GTAP)%>%
  summarise(across(CO2_Mt:Total_HH_Consumption_MUSD_P, ~ sum(.)))%>%
  ungroup()%>%
  mutate(CO2_direct_Gas = ifelse(GTAP == "gasgdt", CO2_direct,0))%>%
  mutate(
    # CO2_t_per_dollar_global       = CO2_Mt/            Total_HH_Consumption_MUSD,
    CO2_t_per_dollar_national     = CO2_Mt_within/     Total_HH_Consumption_MUSD,
    CO2_t_per_dollar_national_P   = CO2_Mt_within/     Total_HH_Consumption_MUSD_P,
    # CO2_t_per_dollar_electricity  = CO2_Mt_Electricity/Total_HH_Consumption_MUSD,
    CO2_t_per_dollar_transport    = CO2_Mt_Transport/  Total_HH_Consumption_MUSD,
    CO2_t_per_dollar_transport_P  = CO2_Mt_Transport/  Total_HH_Consumption_MUSD_P,
    # CO2_t_per_dollar_gas          = CO2_Mt_Gas/        Total_HH_Consumption_MUSD,
    CO2_t_per_dollar_gas_direct   = CO2_direct_Gas/    Total_HH_Consumption_MUSD,
    CO2_t_per_dollar_gas_direct_P = CO2_direct_Gas/    Total_HH_Consumption_MUSD_P)%>%
  select(GTAP, starts_with("CO2_t"))

rm(carbon_intensities_0, GTAP_code)

# 0.2.3 Matching tables ####

item_codes <- read.xlsx(sprintf("%s/3_Matching_Tables/Item_Codes_Description_Romania.xlsx", path_0))

item_fuels <- read.xlsx(sprintf("%s/3_Matching_Tables/Item_Fuel_Concordance_Romania.xlsx", path_0), colNames = FALSE)%>%
  rename(item_code = X2, fuel = X1, Description = X3)

item_gtap <- read.xlsx(sprintf("%s/3_Matching_Tables/Item_GTAP_Concordance_Romania.xlsx", path_0))%>%
  mutate_at(vars(X40:X49), ~ as.character(.))%>%
  select(-Explanation)%>%
  pivot_longer(-GTAP, values_to = "item_code", names_to = "drop")%>%
  filter(!is.na(item_code))%>%
  select(-drop)%>%
  arrange(GTAP, item_code)%>%
  mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))%>%
  distinct()

# 0.2.4 Codes ####

Province.Code      <- read_csv(sprintf("%s/2_Codes/Province.Code.csv",      path_0), show_col_types = FALSE)
Gender.Code        <- read_csv(sprintf("%s/2_Codes/Gender.Code.csv",        path_0), show_col_types = FALSE)
Nationality.Code   <- read_csv(sprintf("%s/2_Codes/Nationality.Code.csv",   path_0), show_col_types = FALSE)
Nationality.1.Code <- read_csv(sprintf("%s/2_Codes/Nationality.1.Code.csv", path_0), show_col_types = FALSE)
Education.Code     <- read_csv(sprintf("%s/2_Codes/Education.Code.csv",     path_0), show_col_types = FALSE)
Occupation.Code    <- read_csv(sprintf("%s/2_Codes/Occupation.Code.csv",    path_0), show_col_types = FALSE)
Housing.Code       <- read_csv(sprintf("%s/2_Codes/Housing.Code.csv",       path_0), show_col_types = FALSE)
Cooking.Code       <- read_csv(sprintf("%s/2_Codes/Cooking.Code.csv",       path_0), show_col_types = FALSE)
Heating.Code       <- read_csv(sprintf("%s/2_Codes/Heating.Code.csv",       path_0), show_col_types = FALSE)
Lighting.Code      <- read_csv(sprintf("%s/2_Codes/Lighting.Code.csv",      path_0), show_col_types = FALSE)

# 0.2.5 Supplementary data ####

exchange.rate  <- 0.24676177 # Source as usual - in Dollar per Lei
inflation.rate <- 1/1.046 # Source: IMF

# 0.2.6 Remove duplicates ####

household_information_1 <- household_information %>%
  group_by_at(vars(-hh_id))%>%
  mutate(number = n(),
         flag = ifelse(number > 1,1,0))%>%
  ungroup()

hh_duplicates_information <- household_information_1 %>%
  filter(flag != 0)%>%
  select(hh_id)

# Exact duplicates for expenditures
expenditure_information_1 <- expenditures %>%
  pivot_wider(names_from = "item_code", values_from = "expenditures_year")%>%
  group_by_at(vars(-hh_id))%>%
  mutate(number = n(),
         flag = ifelse(number > 1,1,0))%>%
  ungroup()%>%
  arrange(desc(flag))

hh_duplicates_expenditures_1 <- expenditure_information_1 %>%
  filter(flag != 0)%>%
  select(hh_id)

household_information <- household_information %>%
  filter(!hh_id %in% hh_duplicates_information$hh_id)%>%
  filter(!hh_id %in% hh_duplicates_expenditures_1$hh_id)

expenditures <- expenditures %>%
  filter(!hh_id %in% hh_duplicates_information$hh_id)%>%
  filter(!hh_id %in% hh_duplicates_expenditures_1$hh_id)

rm(hh_duplicates_expenditures_1, expenditure_information_1, household_information_1, hh_duplicates_information)

# 1   Transform and clean expenditures ####

data_0.1 <- expenditures %>%
  left_join(item_gtap, by = "item_code")%>%
  # deleting 24% of total, i.e., 448,145 observations
  filter(GTAP != "deleted")%>%
  left_join(select(household_information, hh_id, hh_weights))%>%
  filter(!is.na(expenditures_year) & expenditures_year > 0)%>%
  group_by(item_code)%>%
  mutate(outlier_99 = wtd.quantile(expenditures_year, weights = hh_weights, probs = 0.99),
         median_exp = wtd.quantile(expenditures_year, weights = hh_weights, probs = 0.5))%>%
  ungroup()%>%
  mutate(flag_outlier_99 = ifelse(expenditures_year>= outlier_99,1,0))%>%
  # this line replaces all expenditures which are above the 99th percentile for each item to the median
  mutate(expenditures_year = ifelse(flag_outlier_99 == 1, median_exp, expenditures_year))%>%
  # After check: Cleaning at expenditure-level not strictly necessary
  select(hh_id, item_code, GTAP, expenditures_year)%>%
  # group_by(hh_id)%>%
  # mutate(sum_expenditures_year = sum(expenditures_year))%>%
  # ungroup()%>%
  # mutate(share = expenditures_year/sum_expenditures_year)%>%
  # filter(item_code == 344)
  left_join(select(carbon_intensities, GTAP, 
                   CO2_t_per_dollar_national, CO2_t_per_dollar_transport, CO2_t_per_dollar_gas_direct,
                   CO2_t_per_dollar_national_P, CO2_t_per_dollar_transport_P, CO2_t_per_dollar_gas_direct_P), by = "GTAP")%>%
  mutate(CO2_t_per_dollar_national   = ifelse(is.na(CO2_t_per_dollar_national),  0,CO2_t_per_dollar_national),
         CO2_t_per_dollar_transport  = ifelse(is.na(CO2_t_per_dollar_transport), 0,CO2_t_per_dollar_transport),
         CO2_t_per_dollar_gas_direct = ifelse(is.na(CO2_t_per_dollar_gas_direct),0,CO2_t_per_dollar_gas_direct),
         CO2_t_per_dollar_national_P   = ifelse(is.na(CO2_t_per_dollar_national_P),  0,CO2_t_per_dollar_national_P),
         CO2_t_per_dollar_transport_P  = ifelse(is.na(CO2_t_per_dollar_transport_P), 0,CO2_t_per_dollar_transport_P),
         CO2_t_per_dollar_gas_direct_P = ifelse(is.na(CO2_t_per_dollar_gas_direct_P),0,CO2_t_per_dollar_gas_direct_P))%>%
  # Adjusting for dollar/Romanian Lei
  mutate(CO2_t_per_LEI_national     = CO2_t_per_dollar_national*exchange.rate,
         CO2_t_per_LEI_transport    = CO2_t_per_dollar_transport*exchange.rate,
         CO2_t_per_LEI_gas_direct   = CO2_t_per_dollar_gas_direct*exchange.rate,
         CO2_t_per_LEI_national_P   = CO2_t_per_dollar_national_P*exchange.rate,
         CO2_t_per_LEI_transport_P  = CO2_t_per_dollar_transport_P*exchange.rate,
         CO2_t_per_LEI_gas_direct_P = CO2_t_per_dollar_gas_direct_P*exchange.rate)%>%
  mutate(CO2_t_national           = CO2_t_per_LEI_national*expenditures_year*inflation.rate,
         CO2_t_transport          = CO2_t_per_LEI_transport*expenditures_year*inflation.rate,
         CO2_t_gas_direct         = CO2_t_per_LEI_gas_direct*expenditures_year*inflation.rate,
         CO2_t_national_P         = CO2_t_per_LEI_national_P*expenditures_year*inflation.rate,
         CO2_t_transport_P        = CO2_t_per_LEI_transport_P*expenditures_year*inflation.rate,
         CO2_t_gas_direct_P       = CO2_t_per_LEI_gas_direct_P*expenditures_year*inflation.rate)

data_0.2 <- data_0.1 %>%
  group_by(hh_id)%>%
  summarise(hh_expenditures_LEI_2018 = sum(expenditures_year),
            CO2_t_national            = sum(CO2_t_national),
            CO2_t_transport           = sum(CO2_t_transport),
            CO2_t_gas_direct          = sum(CO2_t_gas_direct),
            CO2_t_national_P          = sum(CO2_t_national_P),
            CO2_t_transport_P         = sum(CO2_t_transport_P),
            CO2_t_gas_direct_P        = sum(CO2_t_gas_direct_P))%>%
  ungroup()

data_0.3 <- data_0.1 %>%
  left_join(select(item_fuels, item_code, fuel), by = "item_code")%>%
  filter(!is.na(fuel))%>%
  group_by(hh_id, fuel)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()%>%
  pivot_wider(names_from = "fuel", values_from = "expenditures_year", values_fill = 0, names_prefix = "Exp_")

# 1.2   Transform household information ####

household_information <- household_information %>%
  left_join(Province.Code,      by = "province")%>%   
  left_join(Gender.Code,        by = "sex_hhh")%>%      
  left_join(Nationality.Code,   by = "nationality")%>% 
  left_join(Nationality.1.Code, by = "nationality_1")%>% 
  left_join(Education.Code,     by = "edu_hhh")%>%   
  left_join(Occupation.Code,    by = "hhh_occupation")%>%  
  left_join(Housing.Code,       by = "house_type")%>%     
  left_join(Lighting.Code,      by = "lighting_fuel")%>%       
  left_join(Heating.Code,       by = "heating_fuel")%>%
  left_join(Cooking.Code,       by = "cooking_fuel")%>%
  left_join(appliances,         by = "hh_id")%>%
  select(hh_id, hh_size, adults, children, hh_weights, urban_01, Province,
         Gender, Nationality, Nationality_1, Education, Occupation,
         area, construction_year, rooms, Housing_Type,
         Lighting_Fuel, Heating_Fuel, Cooking_Fuel, refrigerator.01, freezer.01, washing_machine.01, motorcycle.01, tv.01, number_of_cars)%>%
  rename_all(tolower)

rm(Province.Code, Gender.Code, Nationality.Code, Nationality.1.Code, Education.Code, Occupation.Code, Housing.Code, Cooking.Code, Heating.Code, Lighting.Code)

# 1.3   Compile final dataset ####

data_0 <- left_join(data_0.2, data_0.3, by = "hh_id")%>%
  mutate_at(vars(starts_with("Exp_")), ~ ifelse(is.na(.),0,.))

data_1 <- left_join(household_information, data_0, by = "hh_id")%>%
  filter(!is.na(hh_expenditures_LEI_2018))%>%
  mutate(household_expenditures_pc = hh_expenditures_LEI_2018/hh_size)%>%
  mutate(Expenditure_Group_5       = as.numeric(binning(household_expenditures_pc, bins = 5,  method = c("wtd.quantile"), weights = hh_weights)),
         Expenditure_Group_10      = as.numeric(binning(household_expenditures_pc, bins = 10, method = c("wtd.quantile"), weights = hh_weights)))%>%
  select(-household_expenditures_pc)

rm(data_0.1, data_0.2, data_0.3, data_0, expenditures, household_information, appliances,
   item_gtap, item_fuels, item_codes, carbon_intensities,
   exchange.rate, inflation.rate)

# 2     Microsimulation ####

# 2.1   Simulating a carbon price (in the transport and heating sector) ####

data_1.1 <- data_1 %>%
  # No carbon price in 2018 yet - carbon price in LEI
  mutate(exp_CO2_transport           = CO2_t_transport*182,
         exp_CO2_national            = CO2_t_national*182,
         exp_CO2_gas_direct          = CO2_t_gas_direct*182)%>%
  mutate(exp_CO2_price               = exp_CO2_transport + exp_CO2_gas_direct)%>%
  mutate(burden_CO2_transport        = exp_CO2_transport/hh_expenditures_LEI_2018,
         burden_CO2_national         = exp_CO2_national/hh_expenditures_LEI_2018,
         burden_CO2_price            = exp_CO2_price/hh_expenditures_LEI_2018)%>%
  mutate(t_weighted = (CO2_t_gas_direct+CO2_t_transport)*hh_weights, # 34,257,168 = 34 MtO2 --> Emissionen sind zu niedrig (50%)
         t_weighted_national = CO2_t_national*hh_weights)            # 45,081,191 = 25 MtCO2 --> Emissionen sind zu niedrig (50%)

data_1.2 <- data_1.1 %>%
  select(hh_id:number_of_cars, hh_expenditures_LEI_2018,
         starts_with("CO2_"), starts_with("Exp_"), Expenditure_Group_5, Expenditure_Group_10)

write_rds(data_1.2, "H:/6_Citizen_Survey/2_Data/Microdata/Microdata_Transformed_Romania.rds")

rm(data_1, data_1.1, data_1.2)
