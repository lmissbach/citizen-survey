# 0     General ####
# Author: L. Missbach (missbach@mcc-berlin.net)
# Date: 15th of November 2024

# 0.1   Packages ####

if(!require("pacman")) install.packages("pacman")

p_load("boot", "broom", "fixest", "ggpubr", "ggrepel",
       "ggsci", "Hmisc", "knitr", "kableExtra", "openxlsx", "rattle", "scales", "tidymodels", "tidyverse", "xtable")

options(scipen=999)

# 0.2   Load data ####

path_0 <- "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_France"

# 0.2.1 Household budget survey data ####

expenditures <- read_csv(sprintf("%s/1_Data_Clean/expenditures_items_France.csv", path_0), show_col_types = FALSE)

household_information <- read_csv(sprintf("%s/1_Data_Clean/household_information_France.csv", path_0), show_col_types = FALSE)

appliances <- read_csv(sprintf("%s/1_Data_Clean/appliances_0_1_France.csv", path_0), show_col_types = FALSE)

# 0.2.2 Carbon intensities ####

carbon_intensities_0 <- read.xlsx("../2_Data/Carbon_Intensities_Full_All_Gas_EU.xlsx", sheet = "France")

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

item_codes <- read.xlsx(sprintf("%s/3_Matching_Tables/Item_Codes_Description_France.xlsx", path_0))

item_fuels <- read.xlsx(sprintf("%s/3_Matching_Tables/Item_Fuel_Concordance_France.xlsx", path_0), colNames = FALSE)%>%
  rename(item_code = X2, fuel = X1, Description = X3)

item_gtap <- read.xlsx(sprintf("%s/3_Matching_Tables/Item_GTAP_Concordance_France.xlsx", path_0))%>%
  mutate(X46 = as.character(X46))%>%
  select(-Explanation)%>%
  pivot_longer(-GTAP, values_to = "item_code", names_to = "drop")%>%
  filter(!is.na(item_code))%>%
  select(-drop)%>%
  arrange(GTAP, item_code)%>%
  mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))%>%
  distinct()

# 0.2.4 Codes ####

Roof.Code              <- read_csv(sprintf("%s/2_Codes/Roof.Code.csv",              path_0), show_col_types = FALSE)
Province.Code          <- read_csv(sprintf("%s/2_Codes/Province.Code.csv",          path_0), show_col_types = FALSE)
Urban.1.Code           <- read_csv(sprintf("%s/2_Codes/Urban_1_Code.csv",           path_0), show_col_types = FALSE)
Urban.2.Code           <- read_csv(sprintf("%s/2_Codes/Urban_2_Code.csv",           path_0), show_col_types = FALSE)
Wall.Code              <- read_csv(sprintf("%s/2_Codes/Wall.Code.csv",              path_0), show_col_types = FALSE)
Tenant.Code            <- read_csv(sprintf("%s/2_Codes/Tenant.Code.csv",            path_0), show_col_types = FALSE)
Gender.Code            <- read_csv(sprintf("%s/2_Codes/Gender.Code.csv",            path_0), show_col_types = FALSE)
Nationality.Code       <- read_csv(sprintf("%s/2_Codes/Nationality.Code.csv",       path_0), show_col_types = FALSE)
Education.Code         <- read_csv(sprintf("%s/2_Codes/Education.Code.csv",         path_0), show_col_types = FALSE)
Occupation.Code        <- read_csv(sprintf("%s/2_Codes/Occupation.Code.csv",        path_0), show_col_types = FALSE)
Construction.Year.Code <- read_csv(sprintf("%s/2_Codes/Construction.Year.Code.csv", path_0), show_col_types = FALSE)
Floor.Code             <- read_csv(sprintf("%s/2_Codes/Floor.Code.csv",             path_0), show_col_types = FALSE)
House.Code             <- read_csv(sprintf("%s/2_Codes/House.Code.csv",             path_0), show_col_types = FALSE)
Housing.Type.Code      <- read_csv(sprintf("%s/2_Codes/Housing.Type.Code.csv",      path_0), show_col_types = FALSE)
Housing.Type.2.Code    <- read_csv(sprintf("%s/2_Codes/Housing.Type.2.Code.csv",    path_0), show_col_types = FALSE)
Heating.Code           <- read_csv(sprintf("%s/2_Codes/Heating.Code.csv",           path_0), show_col_types = FALSE)

# 0.2.5 Supplementary data ####

exchange.rate  <- 1.12968118 # Source as usual
inflation.rate <- 1 # Survey year is 2017 - no adjustment necessary

# 1   Transform and clean expenditures ####

data_0.1 <- expenditures %>%
  left_join(item_gtap, by = "item_code")%>%
  filter(GTAP != "deleted")
  # deleting 6% of total information and 41,104 observations

# Cleaning on item-level
data_0.1.1 <- data_0.1 %>%
  left_join(select(household_information, hh_id, hh_weights), by = "hh_id")%>%
  filter(!is.na(expenditures_year) & expenditures_year > 0 )%>%
  group_by(item_code)%>%
  mutate(outlier_99 = wtd.quantile(expenditures_year, weights = hh_weights, probs = 0.99),
         median_exp = wtd.quantile(expenditures_year, weights = hh_weights, probs = 0.5))%>%
  ungroup()%>%
  mutate(flag_outlier_99 = ifelse(expenditures_year>= outlier_99,1,0))%>%
  # this line replaces all expenditures which are above the 99th percentile for each item to the median
  mutate(expenditures_year = ifelse(flag_outlier_99 == 1, median_exp, expenditures_year))%>%
  select(hh_id, item_code, expenditures_year, GTAP)
# Cleaning at household expenditure level not necessary

data_0.1 <- data_0.1.1 %>%
  left_join(select(carbon_intensities, GTAP, 
                   CO2_t_per_dollar_national, CO2_t_per_dollar_transport, CO2_t_per_dollar_gas_direct,
                   CO2_t_per_dollar_national_P, CO2_t_per_dollar_transport_P, CO2_t_per_dollar_gas_direct_P), by = "GTAP")%>%
  mutate(CO2_t_per_dollar_national   = ifelse(is.na(CO2_t_per_dollar_national),  0,CO2_t_per_dollar_national),
         CO2_t_per_dollar_transport  = ifelse(is.na(CO2_t_per_dollar_transport), 0,CO2_t_per_dollar_transport),
         CO2_t_per_dollar_gas_direct = ifelse(is.na(CO2_t_per_dollar_gas_direct),0,CO2_t_per_dollar_gas_direct),
         CO2_t_per_dollar_national_P   = ifelse(is.na(CO2_t_per_dollar_national_P),  0,CO2_t_per_dollar_national_P),
         CO2_t_per_dollar_transport_P  = ifelse(is.na(CO2_t_per_dollar_transport_P), 0,CO2_t_per_dollar_transport_P),
         CO2_t_per_dollar_gas_direct_P = ifelse(is.na(CO2_t_per_dollar_gas_direct_P),0,CO2_t_per_dollar_gas_direct_P))%>%
  # Adjusting for dollar/Euro
  mutate(CO2_t_per_euro_national   = CO2_t_per_dollar_national*exchange.rate,
         CO2_t_per_euro_transport  = CO2_t_per_dollar_transport*exchange.rate,
         CO2_t_per_euro_gas_direct = CO2_t_per_dollar_gas_direct*exchange.rate,
         CO2_t_per_euro_national_P   = CO2_t_per_dollar_national_P*exchange.rate,
         CO2_t_per_euro_transport_P  = CO2_t_per_dollar_transport_P*exchange.rate,
         CO2_t_per_euro_gas_direct_P = CO2_t_per_dollar_gas_direct_P*exchange.rate)%>%
  mutate(CO2_t_national           = CO2_t_per_euro_national*expenditures_year*inflation.rate,
         CO2_t_transport          = CO2_t_per_euro_transport*expenditures_year*inflation.rate,
         # Applying carbon pricing in gas sector everywhere warranted, given that also manufacturing firms may require gas for heating
         # Also little difference --> latter is true, but still
         # CO2_t_gas                = CO2_t_per_euro_gas*expenditures_year*inflation.rate,
         CO2_t_gas_direct         = CO2_t_per_euro_gas_direct*expenditures_year*inflation.rate,
         CO2_t_national_P         = CO2_t_per_euro_national_P*expenditures_year*inflation.rate,
         CO2_t_transport_P        = CO2_t_per_euro_transport_P*expenditures_year*inflation.rate,
         CO2_t_gas_direct_P       = CO2_t_per_euro_gas_direct_P*expenditures_year*inflation.rate)

data_0.2 <- data_0.1 %>%
  group_by(hh_id)%>%
  summarise(hh_expenditures_EURO_2018 = sum(expenditures_year),
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
  left_join(Roof.Code,              by = "roof")%>%   
  left_join(Province.Code,          by = "province")%>%   
  left_join(Urban.1.Code,           by = "urban_type")%>%     
  left_join(Urban.2.Code,           by = "urban_type_2")%>%
  left_join(Wall.Code,              by = "wall")%>%
  left_join(Tenant.Code,            by = "tenant")%>%      
  left_join(Gender.Code,            by = "sex_hhh")%>%      
  left_join(Nationality.Code,       by = "nationality")%>% 
  left_join(Education.Code,         by = "edu_hhh")%>%   
  left_join(Occupation.Code,        by = "occupation_hhh")%>%  
  left_join(Construction.Year.Code, by = "construction_year")%>%    
  left_join(Floor.Code,             by = "floor")%>%   
  left_join(House.Code,             by = "house_type")%>%     
  left_join(Housing.Type.Code,      by = "housing_type")%>%  
  left_join(Housing.Type.2.Code,    by = "housing_type_2")%>%    
  left_join(Heating.Code,           by = "heating_fuel")%>%
  filter(hh_id %in% expenditures$hh_id)%>%
  left_join(appliances, by = "hh_id")%>%
  select(hh_id, hh_size, hh_weights, children, Province, Urban_Type, Urban_Type_2,
         age_hhh, Education, Nationality, Gender, Occupation,
         House_Type, Housing_Type, Housing_Type_2, Construction_Year, Wall, Roof, Floor, Heating_Fuel, Tenant, area,
         refrigerator.01, freezer.01, washing_machine.01, dryer.01, dishwasher.01, ac.01, tv.01, motorcycle.01, number_of_cars)%>%
  rename_all(tolower)

rm(Roof.Code, Province.Code, Urban.1.Code, Urban.2.Code, Wall.Code, Gender.Code, Nationality.Code, Education.Code, Occupation.Code, Construction.Year.Code, Floor.Code,   
   Tenant.Code, House.Code, Housing.Type.Code, Housing.Type.2.Code, Heating.Code, appliances)

# 1.3   Compile final dataset ####

data_0 <- left_join(data_0.2, data_0.3, by = "hh_id")%>%
  mutate_at(vars(starts_with("Exp_")), ~ ifelse(is.na(.),0,.))

data_1 <- left_join(household_information, data_0, by = "hh_id")%>%
  # Deletes two households
  filter(!is.na(hh_expenditures_EURO_2018))%>%
  mutate(household_expenditures_pc = hh_expenditures_EURO_2018/hh_size)%>%
  mutate(Expenditure_Group_5       = as.numeric(binning(household_expenditures_pc, bins = 5,  method = c("wtd.quantile"), weights = hh_weights)),
         Expenditure_Group_10      = as.numeric(binning(household_expenditures_pc, bins = 10, method = c("wtd.quantile"), weights = hh_weights)))%>%
  select(-household_expenditures_pc)

rm(data_0.1, data_0.1.1, data_0.2, data_0.3, data_0, expenditures, household_information,
   item_gtap, item_fuels, item_codes, carbon_intensities,
   exchange.rate, inflation.rate)

# 2     Microsimulation ####

# 2.1   Simulating a carbon price (in the transport and heating sector) ####

data_1.1 <- data_1 %>%
  # No carbon price in 2018 yet
  mutate(exp_CO2_transport           = CO2_t_transport*45,
         exp_CO2_national            = CO2_t_national*45,
         exp_CO2_gas_direct          = CO2_t_gas_direct*45)%>%
  mutate(exp_CO2_price               = exp_CO2_transport + exp_CO2_gas_direct)%>%
  mutate(burden_CO2_transport        = exp_CO2_transport/hh_expenditures_EURO_2018,
         burden_CO2_national         = exp_CO2_national/hh_expenditures_EURO_2018,
         burden_CO2_price            = exp_CO2_price/hh_expenditures_EURO_2018)%>%
  mutate(t_weighted = (CO2_t_gas_direct+CO2_t_transport)*hh_weights, # 358,466,099 = 177 MtO2
         t_weighted_national = CO2_t_national*hh_weights)            # 651,668,248  = 651 MtCO2 --> Looks too high - should be 338,584,770

data_1.2 <- data_1.1 %>%
  select(hh_id, hh_size, hh_weights, children, hh_expenditures_EURO_2018,
         province, urban_type, urban_type_2, 
         age_hhh, gender, nationality, education, occupation,
         tenant, house_type, housing_type, housing_type_2, construction_year, wall, roof, floor, heating_fuel, area, ends_with(".01"), number_of_cars,
         starts_with("CO2_"), starts_with("Exp_"), Expenditure_Group_5, Expenditure_Group_10, -starts_with("exp_CO2"))

write_rds(data_1.2, "H:/6_Citizen_Survey/2_Data/Microdata/Microdata_Transformed_France.rds")

rm(data_1, data_1.1, data_1.2)
