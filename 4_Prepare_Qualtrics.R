# 0     General ####
# Author: L. Missbach (missbach@mcc-berlin.net)
# Date: 23rd of April 2025

# 0.1   Packages ####

if(!require("pacman")) install.packages("pacman")

p_load("arrow", "boot", "broom", "extrafont", "fixest", "ggpubr", "ggrepel",
       "ggsci", "Hmisc", "jsonlite", "knitr", "kableExtra", "openxlsx", "rattle", "readxl", "scales", "showtext", "tidymodels", "tidyverse", "xtable")

options(scipen=999)

# 0.2   Load data ####

combinations_ESP <- read_parquet("../2_Data/Output/Output data/Combinations_Spain_P.parquet")%>%
  mutate_at(vars(hh_expenditures_EURO_2018, age_hhh), ~ as.character(.))%>%
  rename(hh_expenditures = hh_expenditures_EURO_2018)
combinations_GER <- read_parquet("../2_Data/Output/Output data/Combinations_Germany_P.parquet")%>%
  mutate_at(vars(space, hh_expenditures_EURO_2018), ~ as.character(.))%>%
  rename(hh_expenditures = hh_expenditures_EURO_2018)
combinations_FRA <- read_parquet("../2_Data/Output/Output data/Combinations_France_P.parquet")%>%
  mutate_at(vars(hh_expenditures_EURO_2018), ~ as.character(.))%>%
  rename(hh_expenditures = hh_expenditures_EURO_2018)
combinations_ROM <- read_parquet("../2_Data/Output/Output data/Combinations_Romania_P.parquet")%>%
  mutate_at(vars(area, hh_expenditures_LEI_2018), ~ as.character(.))%>%
  rename(space = area, hh_expenditures = hh_expenditures_LEI_2018)

data_ESP_0 <- read_rds("H:/6_Citizen_Survey/2_Data/Microdata/Microdata_Transformed_Spain.rds")
data_GER_0 <- read_rds("H:/6_Citizen_Survey/2_Data/Microdata/Microdata_Transformed_Germany.rds")
data_FRA_0 <- read_rds("H:/6_Citizen_Survey/2_Data/Microdata/Microdata_Transformed_France.rds")
data_ROM_0 <- read_rds("H:/6_Citizen_Survey/2_Data/Microdata/Microdata_Transformed_Romania.rds")

# 1     Data transformation ####

# Income adjustment for inflation
# Data source: https://www.imf.org/external/datamapper/PCPIPCH@WEO/OEMDC/ADVEC/WEOWORLD

inflation_0 <- read_excel("../2_Data/Supplementary/imf-dm-export-20250527.xls")%>%
  rename(Country = "Inflation rate, average consumer prices (Annual percent change)")%>%
  filter(Country %in% c("Germany", "France", "Spain", "Romania"))%>%
  pivot_longer(-Country, names_to = "year", values_to = "rate")%>%
  filter(year < 2026 & year > 2016)%>%
  filter((Country == "Germany" & year > 2018)|(Country == "France")|(Country == "Romania" & year > 2019)|(Country == "Spain" & year > 2018))%>%
  mutate(inflation_rate = 1+as.numeric(rate)/100)%>%
  group_by(Country)%>%
  summarise(inflation_rate = prod(inflation_rate))%>%
  ungroup()

inflation_ESP <- inflation_0$inflation_rate[inflation_0$Country == "Spain"]
inflation_GER <- inflation_0$inflation_rate[inflation_0$Country == "Germany"]
inflation_FRA <- inflation_0$inflation_rate[inflation_0$Country == "France"]
inflation_ROM <- inflation_0$inflation_rate[inflation_0$Country == "Romania"]

# 1.1   Change price to 45€ instead of 40€ ####

combinations_ESP_1 <- combinations_ESP %>%
  mutate(relative_45 = .pred*45/40,
         absolute_45 = absolute*45*inflation_ESP/40,
         relative_85 = .pred*85/40,
         absolute_85 = absolute*85*inflation_ESP/40,
         relative_125 = .pred*125/40,
         absolute_125 = absolute*125*inflation_ESP/40,
         relative_165 = .pred*165/40,
         absolute_165 = absolute*165*inflation_ESP/40)%>%
  mutate(CO2_t = round(absolute*inflation_ESP/40,1))%>%
  select(everything(), starts_with("absolute"), starts_with("relative"), Percentile, -.pred)%>%
  mutate_at(vars(starts_with("absolute_")), ~ paste0(format(round(.,0), big.mark = ",", trim = "TRUE", decimal.mark = "."),"€"))%>%
  mutate_at(vars(starts_with("relative_")), ~ paste0(round(.,3)*100, "%"))

combinations_GER_1 <- combinations_GER %>%
  mutate(relative_45 = .pred*45/40,
         absolute_45 = absolute*45*inflation_GER/40,
         relative_85 = .pred*85/40,
         absolute_85 = absolute*85*inflation_GER/40,
         relative_125 = .pred*125/40,
         absolute_125 = absolute*125*inflation_GER/40,
         relative_165 = .pred*165/40,
         absolute_165 = absolute*165*inflation_GER/40)%>%
  mutate(CO2_t = round(absolute*inflation_GER/40,1))%>%
  select(everything(), starts_with("absolute"), starts_with("relative"), Percentile, -.pred)%>%
  mutate_at(vars(starts_with("absolute_")), ~ paste0(format(round(.,0), big.mark = ",", trim = "TRUE", decimal.mark = "."),"€"))%>%
  mutate_at(vars(starts_with("relative_")), ~ paste0(round(.,3)*100, "%"))

combinations_FRA_1 <- combinations_FRA %>%
  mutate(relative_45 = .pred*45/40,
         absolute_45 = absolute*45*inflation_FRA/40,
         relative_85 = .pred*85/40,
         absolute_85 = absolute*85*inflation_FRA/40,
         relative_125 = .pred*125/40,
         absolute_125 = absolute*125*inflation_FRA/40,
         relative_165 = .pred*165/40,
         absolute_165 = absolute*165*inflation_FRA/40)%>%
  mutate(CO2_t = round(absolute*inflation_FRA/40,1))%>%
  select(everything(), starts_with("absolute"), starts_with("relative"), Percentile, -.pred)%>%
  mutate_at(vars(starts_with("absolute_")), ~ paste0(format(round(.,0), big.mark = ",", trim = "TRUE", decimal.mark = "."),"€"))%>%
  mutate_at(vars(starts_with("relative_")), ~ paste0(round(.,3)*100, "%"))

combinations_ROM_1 <- combinations_ROM %>%
  mutate(relative_45 = .pred*45/40,
         absolute_45 = absolute*45*inflation_ROM/40,
         relative_85 = .pred*85/40,
         absolute_85 = absolute*85*inflation_ROM/40,
         relative_125 = .pred*125/40,
         absolute_125 = absolute*125*inflation_ROM/40,
         relative_165 = .pred*165/40,
         absolute_165 = absolute*165*inflation_ROM/40)%>%
  mutate(CO2_t = round(absolute*inflation_ROM/(202),1))%>%
  select(everything(), starts_with("absolute"), starts_with("relative"), Percentile, -.pred)%>%
  mutate_at(vars(starts_with("absolute_")), ~ paste0(format(round(.,0), big.mark = ",", trim = "TRUE", decimal.mark = "."),"€"))%>%
  mutate_at(vars(starts_with("relative_")), ~ paste0(round(.,3)*100, "%"))

rm(combinations_ESP, combinations_GER, combinations_FRA, combinations_ROM)

# 1.2 Change numeric columns ####

# Spain

# Income groups

income_groups_ESP <- data_ESP_0 %>%
  mutate(Income_Group_10 = as.numeric(binning(hh_expenditures_EURO_2018, bins = 10, weights = hh_weights, method = "wtd.quantile")))%>%
  group_by(Income_Group_10)%>%
  summarise(min                       = min(hh_expenditures_EURO_2018)/12,
            max                       = max(hh_expenditures_EURO_2018)/12,
            hh_expenditures_EURO_2018 = wtd.mean(hh_expenditures_EURO_2018, hh_weights))%>%
  ungroup()%>%
  # Adjust for inflation
  mutate(min = min*inflation_ESP,
         max = max*inflation_ESP)%>%
  # Round to nearest 100
  mutate(max = round(max/100)*100)%>%
  mutate(min = lag(max)+1)%>%
  mutate(IG = ifelse(Income_Group_10 == 1, paste0("menos que ", max, "€"),
                     ifelse(Income_Group_10 == 10, paste0("más que ", format(min, big.mark = ",", trim = TRUE), "€"),
                            paste0("desde ", format(min, big.mark = ",", trim = "TRUE"), "€ hasta ", format(max, big.mark = ",", trim = "TRUE"), "€"))))%>%
  # mutate(IG = URLencode(IG, reserved = TRUE))%>%
  select(hh_expenditures_EURO_2018, IG)%>%
  mutate(hh_expenditures = as.factor(as.character(round(hh_expenditures_EURO_2018))))%>%
  select(hh_expenditures, IG)

age_ESP <- data_ESP_0 %>%
  mutate(age_3 = as.numeric(binning(age_hhh, bins = 3, weights = hh_weights, method = "wtd.quantile")))%>%
  group_by(age_3)%>%
  summarise(min_age  = round(min(age_hhh)),
            max_age  = round(max(age_hhh)),
            age_hhh  = round(wtd.mean(age_hhh, hh_weights),1))%>%
  ungroup()%>%
  mutate(AGE = ifelse(age_3 == 1, paste0("hasta ", max_age, " años"),
                      ifelse(age_3 == 2, paste0(min_age, " a ", max_age, " años"),
                             paste0("más de ", min_age, " años"))))%>%
  # mutate(AGE = URLencode(AGE, reserved = TRUE))%>%
  select(age_hhh, AGE)%>%
  mutate(age_hhh = as.character(age_hhh))

combinations_ESP_2 <- left_join(combinations_ESP_1, income_groups_ESP)%>%
  mutate(hh_expenditures = ifelse(!is.na(IG), IG, hh_expenditures))%>%
  left_join(age_ESP)%>%
  select(-age_hhh, -IG)%>%
  rename(age_hhh = AGE)%>%
  select(heating_fuel:hh_expenditures, age_hhh, CO2_t, starts_with("absolute"), starts_with("relative"), Percentile)

combinations_ESP_2[] <- lapply(combinations_ESP_2, function(x) if (is.character(x)) as.factor(x) else x)

rm(combinations_ESP_1)

# Germany

income_groups_GER <- data_GER_0 %>%
  mutate(Income_Group_10 = as.numeric(binning(hh_expenditures_EURO_2018, bins = 10, weights = hh_weights, method = "wtd.quantile")))%>%
  group_by(Income_Group_10)%>%
  summarise(min                       = min(hh_expenditures_EURO_2018)/12,
            max                       = max(hh_expenditures_EURO_2018)/12,
            hh_expenditures_EURO_2018 = wtd.mean(hh_expenditures_EURO_2018, hh_weights))%>%
  ungroup()%>%
  # Adjust for inflation
  mutate(min = min*inflation_GER,
         max = max*inflation_GER)%>%
  # Round to nearest 100
  mutate(max = round(max/100)*100)%>%
  mutate(min = lag(max)+1)%>%
  mutate(IG = ifelse(Income_Group_10 == 1, paste0("weniger als ", format(max, big.mark = ".", trim = "TRUE", decimal.mark = ","), "€"),
                     ifelse(Income_Group_10 == 10, paste0("mehr als ", format(min, big.mark = ".", trim = "TRUE", decimal.mark = ","), "€"),
                            paste0("zwischen ", format(min, big.mark = ".", trim = "TRUE", decimal.mark = ","), "€ und ", format(max, big.mark = ".", trim = "TRUE", decimal.mark = ","), "€"))))%>%
  # mutate(IG = URLencode(IG, reserved = TRUE))%>%
  select(hh_expenditures_EURO_2018, IG)%>%
  mutate(hh_expenditures = as.factor(as.character(round(hh_expenditures_EURO_2018))))%>%
  select(hh_expenditures, IG)

space_GER <- data_GER_0 %>%
  mutate(space_5 = as.numeric(binning(space, bins = 5, weights = hh_weights, method = "wtd.quantile")))%>%
  group_by(space_5)%>%
  summarise(min_space  = round(min(space)),
            max_space  = round(max(space)),
            space = round(wtd.mean(space, hh_weights),1))%>%
  ungroup()%>%
  mutate(Space = ifelse(space_5 == 1, paste0("bis ", max_space, " m2"),
                      ifelse(space_5 %in% c(2,3,4), paste0(min_space, " bis ", max_space, " m2"),
                             paste0("mehr als ", min_space, " m2"))))%>%
  select(space, Space)%>%
  mutate(space = as.character(round(space)))

combinations_GER_2 <- left_join(combinations_GER_1, income_groups_GER)%>%
  mutate(hh_expenditures = ifelse(!is.na(IG), IG, hh_expenditures))%>%
  left_join(space_GER)%>%
  select(-space, -IG)%>%
  rename(space = Space)%>%
  select(heating_fuel:hh_expenditures, space, CO2_t, starts_with("absolute"), starts_with("relative"), Percentile)

combinations_GER_2[] <- lapply(combinations_GER_2, function(x) if (is.character(x)) as.factor(x) else x)

rm(combinations_GER_1)

# France
income_groups_FRA <- data_FRA_0 %>%
  mutate(Income_Group_10 = as.numeric(binning(hh_expenditures_EURO_2018, bins = 10, weights = hh_weights, method = "wtd.quantile")))%>%
  group_by(Income_Group_10)%>%
  summarise(min                       = min(hh_expenditures_EURO_2018)/12,
            max                       = max(hh_expenditures_EURO_2018)/12,
            hh_expenditures_EURO_2018 = wtd.mean(hh_expenditures_EURO_2018, hh_weights))%>%
  ungroup()%>%
  # Adjust for inflation
  mutate(min = min*inflation_FRA,
         max = max*inflation_FRA)%>%
  # Round to nearest 100
  mutate(max = round(max/100)*100)%>%
  mutate(min = lag(max)+1)%>%
  mutate(IG = ifelse(Income_Group_10 == 1, paste0("moins de ", format(max, big.mark = ",", trim = "TRUE"), "€"),
                     ifelse(Income_Group_10 == 10, paste0("plus de ", format(min, big.mark = ",", trim = "TRUE"), "€"),
                            paste0("entre ", format(min, big.mark = ",", trim = "TRUE"), "€ et ", format(max, big.mark = ",", trim = "TRUE"), "€"))))%>%
  # mutate(IG = URLencode(IG, reserved = TRUE))%>%
  select(hh_expenditures_EURO_2018, IG)%>%
  mutate(hh_expenditures = as.factor(as.character(round(hh_expenditures_EURO_2018))))%>%
  select(hh_expenditures, IG)

combinations_FRA_2 <- left_join(combinations_FRA_1, income_groups_FRA)%>%
  mutate(hh_expenditures = ifelse(!is.na(IG), IG, hh_expenditures))%>%
  select(-IG)%>%
  select(everything(), CO2_t, starts_with("absolute"), starts_with("relative"), Percentile)

combinations_FRA_2[] <- lapply(combinations_FRA_2, function(x) if (is.character(x)) as.factor(x) else x)

rm(combinations_FRA_1)

# Romania

income_groups_ROM <- data_ROM_0 %>%
  mutate(Income_Group_10 = as.numeric(binning(hh_expenditures_LEI_2018, bins = 10, weights = hh_weights, method = "wtd.quantile")))%>%
  group_by(Income_Group_10)%>%
  summarise(min                       = min(hh_expenditures_LEI_2018)/12,
            max                       = max(hh_expenditures_LEI_2018)/12,
            hh_expenditures_LEI_2018 = wtd.mean(hh_expenditures_LEI_2018, hh_weights))%>%
  ungroup()%>%
  # Adjust for inflation
  mutate(min = min*inflation_ROM,
         max = max*inflation_ROM)%>%
  # Round to nearest 100
  mutate(max = round(max/100)*100)%>%
  mutate(min = lag(max)+1)%>%
  mutate(IG = ifelse(Income_Group_10 == 1, paste0("Mai putin de ", format(max, big.mark = ",", trim = "TRUE"), " de lei"),
                     ifelse(Income_Group_10 == 10, paste0("Peste ", format(min, big.mark = ",", trim = "TRUE"), " de lei"),
                            paste0("Intre ", format(min, big.mark = ",", trim = "TRUE"), " de lei si ", format(max, big.mark = ",", trim = "TRUE"), " de lei"))))%>%
  # mutate(IG = URLencode(IG, reserved = TRUE))%>%
  select(hh_expenditures_LEI_2018, IG)%>%
  mutate(hh_expenditures = as.factor(as.character(round(hh_expenditures_LEI_2018))))%>%
  select(hh_expenditures, IG)

space_ROM <- data_ROM_0 %>%
  mutate(space_5 = as.numeric(binning(area, bins = 5, weights = hh_weights, method = "wtd.quantile")))%>%
  group_by(space_5)%>%
  summarise(min_space  = round(min(area)),
            space = round(wtd.mean(area, hh_weights),1),
            max_space  = round(max(area)))%>%
  ungroup()%>%
  mutate(Space = ifelse(space_5 == 1, paste0("pana la ", max_space, " de m2"),
                        ifelse(space_5 %in% c(2,3,4), paste0("de la ",min_space, " de la ", max_space, " de m2"),
                               paste0("Peste ", min_space, " de m2"))))%>%
  select(space, Space)%>%
  mutate(space = as.character(space))

combinations_ROM_2 <- left_join(combinations_ROM_1, income_groups_ROM)%>%
  mutate(hh_expenditures = ifelse(!is.na(IG), IG, hh_expenditures))%>%
  mutate(space = ifelse(space != "Nu stiu asta", as.character(round(as.numeric(space),1)), space))%>%
  left_join(space_ROM)%>%
  mutate(space = ifelse(!is.na(Space), Space, space))%>%
  select(-IG)%>%
  select(heating_fuel:hh_expenditures, space, CO2_t, starts_with("absolute"), starts_with("relative"), Percentile)

combinations_ROM_2[] <- lapply(combinations_ROM_2, function(x) if (is.character(x)) as.factor(x) else x)

rm(combinations_ROM_1)

# 1.3  Translation Romania ####

ROM_HF_1 <- distinct(combinations_ROM_2, heating_fuel)%>%
  mutate(Heating_Fuel = c("Gaz natural", "Lemn, carbune sau petrol", "Incalzire centralizata", "Electricitate", "Alte tipuri de incalzire", "Nu stiu asta"))%>%
  mutate(Heating_Fuel = as.factor(Heating_Fuel))

ROM_OC_1 <- distinct(combinations_ROM_2, occupation)%>%
  mutate(Occupation = c("Pensionar", "Angajat", "Somerii", "Lucrator independent în agricultura", "Lucrator independent în activitati neagricole", "Alte"))%>%
  mutate(Occupation = as.factor(Occupation))

ROM_CF_1 <- distinct(combinations_ROM_2, cooking_fuel)%>%
  mutate(Cooking_Fuel = c("Nu stiu asta", "Gaz natural", "GPL", "Alte combustibili"))%>%
  mutate(Cooking_Fuel = as.factor(Cooking_Fuel))

ROM_HT_1 <- distinct(combinations_ROM_2, housing_type)%>%
  mutate(Housing_Type = c("Nu stiu asta", "Apartament", "Casa unifamlilala", "Alta cladire"))%>%
  mutate(Housing_Type = as.factor(Housing_Type))

combinations_ROM_3 <- combinations_ROM_2 %>%
  left_join(ROM_HF_1)%>%
  left_join(ROM_OC_1)%>%
  left_join(ROM_CF_1)%>%
  left_join(ROM_HT_1)%>%
  select(-heating_fuel, -occupation, -cooking_fuel, -housing_type)%>%
  rename(heating_fuel = Heating_Fuel, occupation = Occupation, cooking_fuel = Cooking_Fuel, housing_type = Housing_Type)%>%
  select(heating_fuel:housing_type, number_of_cars:hh_expenditures, CO2_t, starts_with("absolute"), starts_with("relative"), Percentile)

rm(ROM_HF_1, ROM_OC_1, ROM_CF_1, ROM_HT_1, combinations_ROM_2)

# 1.3.1 Adjustment Spain ####

levels(combinations_ESP_2$occupation)[levels(combinations_ESP_2$occupation) == "Jubilado.a..retirado.a.anticipadamente"] <- "Jubilado/a, retirado/a anticipadamente" 

combinations_ESP_2.1 <- combinations_ESP_2 %>%
  rename(province = district, urban = urban_identif)

# 1.4  Output data ####

write_parquet(combinations_GER_2, "../2_Data/Output/Output data/Combinations_Qualtrics_Germany_250703.parquet", compression = "gzip")  
write_parquet(combinations_FRA_2, "../2_Data/Output/Output data/Combinations_Qualtrics_France_250703.parquet",  compression = "gzip")  
write_parquet(combinations_ESP_2, "../2_Data/Output/Output data/Combinations_Qualtrics_Spain_250703.parquet",   compression = "gzip")  
write_parquet(combinations_ROM_3, "../2_Data/Output/Output data/Combinations_Qualtrics_Romania_250703.parquet", compression = "gzip")  

rm(combinations_GER_2, combinations_FRA_2, combinations_ESP_2, combinations_ROM_3)

# 1.5  Supplementary analyses ####

# For question - brackets

data_ESP_1.5 <- data_ESP_0 %>%
  mutate(CO2_interest = CO2_t_gas_direct_P + CO2_t_transport_P,
         abs_interest_45 = CO2_interest*45*inflation_ESP)%>%
  mutate(abs_Quintiles = as.numeric(binning(abs_interest_45, bins = 5, method = "wtd.quantile", weights = hh_weights)))%>%
  group_by(abs_Quintiles)%>%
  summarise(min = round(min(abs_interest_45),-1),
            max = round(max(abs_interest_45),-1))%>%
  ungroup()%>%
  mutate(min = ifelse(abs_Quintiles == 1, NA, min),
         max = ifelse(abs_Quintiles == 5, NA, max))%>%
  mutate(min_85 = round(min*85/45,-1),
         max_85 = round(max*85/45,-1),
         min_125 = round(min*125/45,-1),
         max_125 = round(max*125/45,-1),
         min_165 = round(min*165/45,-1),
         max_165 = round(max*165/45,-1))

data_ESP_1.6 <- data_ESP_0 %>%
  mutate(CO2_interest = CO2_t_gas_direct_P + CO2_t_transport_P,
         abs_interest_45 = CO2_interest*45*inflation_ESP)%>%
  summarise(median_45 = round(wtd.quantile(abs_interest_45, hh_weights, probs = 0.5),-1))%>%
  mutate(median_85 = round(median_45*85/45,-1),
         median_125 = round(median_45*125/45,-1))%>%
  mutate(Country = "Spain")

data_GER_1.5 <- data_GER_0 %>%
  mutate(CO2_interest = CO2_t_gas_direct_P + CO2_t_transport_P,
         abs_interest_45 = CO2_interest*45*inflation_GER)%>%
  mutate(abs_Quintiles = as.numeric(binning(abs_interest_45, bins = 5, method = "wtd.quantile", weights = hh_weights)))%>%
  group_by(abs_Quintiles)%>%
  summarise(min = round(min(abs_interest_45),-1),
            max = round(max(abs_interest_45),-1))%>%
  ungroup()%>%
  mutate(min = ifelse(abs_Quintiles == 1, NA, min),
         max = ifelse(abs_Quintiles == 5, NA, max))%>%
  mutate(min_85 = round(min*85/45,-1),
         max_85 = round(max*85/45,-1),
         min_125 = round(min*125/45,-1),
         max_125 = round(max*125/45,-1),
         min_165 = round(min*165/45,-1),
         max_165 = round(max*165/45,-1))

data_GER_1.6 <- data_GER_0 %>%
  mutate(CO2_interest = CO2_t_gas_direct_P + CO2_t_transport_P,
         abs_interest_45 = CO2_interest*45*inflation_ESP)%>%
  summarise(median_45 = round(wtd.quantile(abs_interest_45, hh_weights, probs = 0.5),-1))%>%
  mutate(median_85 = round(median_45*85/45,-1),
         median_125 = round(median_45*125/45,-1))%>%
  mutate(Country = "Germany")

data_ROM_1.5 <- data_ROM_0 %>%
  mutate(CO2_interest = CO2_t_gas_direct_P + CO2_t_transport_P,
         abs_interest_45 = CO2_interest*45*inflation_ROM)%>%
  mutate(abs_Quintiles = as.numeric(binning(abs_interest_45, bins = 5, method = "wtd.quantile", weights = hh_weights)))%>%
  group_by(abs_Quintiles)%>%
  summarise(min = round(min(abs_interest_45),-1),
            max = round(max(abs_interest_45),-1))%>%
  ungroup()%>%
  mutate(min = ifelse(abs_Quintiles == 1, NA, min),
         max = ifelse(abs_Quintiles == 5, NA, max))%>%
  mutate(min_85 = round(min*85/45,-1),
         max_85 = round(max*85/45,-1),
         min_125 = round(min*125/45,-1),
         max_125 = round(max*125/45,-1),
         min_165 = round(min*165/45,-1),
         max_165 = round(max*165/45,-1))

data_ROM_1.6 <- data_ROM_0 %>%
  mutate(CO2_interest = CO2_t_gas_direct_P + CO2_t_transport_P,
         abs_interest_45 = CO2_interest*202*inflation_ESP)%>%
  summarise(median_45 = round(wtd.quantile(abs_interest_45, hh_weights, probs = 0.5),-1))%>%
  mutate(median_85 = round(median_45*85/45,-1),
         median_125 = round(median_45*125/45,-1))%>%
  mutate(Country = "Romania")

data_FRA_1.5 <- data_FRA_0 %>%
  mutate(CO2_interest = CO2_t_gas_direct_P + CO2_t_transport_P,
         abs_interest_45 = CO2_interest*45*inflation_FRA)%>%
  mutate(abs_Quintiles = as.numeric(binning(abs_interest_45, bins = 5, method = "wtd.quantile", weights = hh_weights)))%>%
  group_by(abs_Quintiles)%>%
  summarise(min = round(min(abs_interest_45),-1),
            max = round(max(abs_interest_45),-1))%>%
  ungroup()%>%
  mutate(min = ifelse(abs_Quintiles == 1, NA, min),
         max = ifelse(abs_Quintiles == 5, NA, max))%>%
  mutate(min_85 = round(min*85/45,-1),
         max_85 = round(max*85/45,-1),
         min_125 = round(min*125/45,-1),
         max_125 = round(max*125/45,-1),
         min_165 = round(min*165/45,-1),
         max_165 = round(max*165/45,-1))

data_FRA_1.6 <- data_FRA_0 %>%
  mutate(CO2_interest = CO2_t_gas_direct_P + CO2_t_transport_P,
         abs_interest_45 = CO2_interest*45*inflation_ESP)%>%
  summarise(median_45 = round(wtd.quantile(abs_interest_45, hh_weights, probs = 0.5),-1))%>%
  mutate(median_85 = round(median_45*85/45,-1),
         median_125 = round(median_45*125/45,-1))%>%
  mutate(Country = "France")

data_1.6 <- bind_rows(data_ESP_1.6, data_FRA_1.6, data_GER_1.6, data_ROM_1.6)

write.xlsx(data_1.6, "../2_data/Supplementary/Median_Costs_Countries.xlsx")

# 1.   Transform data for Qualtrics / Google Sheets ####

# ESP_1.1 <- distinct(combinations_ESP, tenant)%>%
#   mutate(tenant_0 = URLencode(tenant, reserved = TRUE))
# 
# ESP_1.2 <- distinct(combinations_ESP, water_energy)%>%
#   mutate(water_energy_0 = URLencode(water_energy, reserved = TRUE))
# 
# ESP_1.3 <- distinct(combinations_ESP, heating_fuel)%>%
#   mutate(heating_fuel_0 = URLencode(heating_fuel, reserved = TRUE))
# 
# ESP_1.4 <- distinct(combinations_ESP, urban_identif)%>%
#   mutate(urban_identif_0 = URLencode(urban_identif, reserved = TRUE))
# 
# ESP_1.5 <- distinct(combinations_ESP, urban_identif_2)%>%
#   mutate(urban_identif_2_0 = URLencode(urban_identif_2, reserved = TRUE))
# 
# ESP_1.6 <- distinct(combinations_ESP, district)%>%
#   mutate(district_0 = URLencode(district, reserved = TRUE))
# 
# ESP_1.7 <- distinct(combinations_ESP, gender)%>%
#   mutate(gender_0 = URLencode(gender, reserved = TRUE))
# 
# ESP_1.8 <- distinct(combinations_ESP, occupation)%>%
#   mutate(occupation_0 = URLencode(occupation, reserved = TRUE))
# 
# combinations_ESP_1 <- combinations_ESP %>%
#   rename(relative = .pred)%>%
#   left_join(ESP_1.1, by = "tenant")%>%
#   left_join(ESP_1.2, by = "water_energy")%>%
#   left_join(ESP_1.3, by = "heating_fuel")%>%
#   left_join(ESP_1.4, by = "urban_identif")%>%
#   left_join(ESP_1.5, by = "urban_identif_2")%>%
#   left_join(ESP_1.6, by = "district")%>%
#   left_join(ESP_1.7, by = "gender")%>%
#   left_join(ESP_1.8, by = "occupation")%>%
#   select(-tenant, -water_energy, -heating_fuel, -urban_identif,-urban_identif_2, -district,-gender,-occupation)%>%
#   rename(tenant = tenant_0, water_energy = water_energy_0, heating_fuel = heating_fuel_0, urban_identif = urban_identif_0, urban_identif_2 = urban_identif_2_0, district = district_0, gender = gender_0,
#          occupation = occupation_0)%>%
#   mutate(hh_expenditures_EURO_2018 = round(hh_expenditures_EURO_2018))%>%
#   left_join(income_groups, by = c("hh_expenditures_EURO_2018" = "mean"))%>%
#   select(-hh_expenditures_EURO_2018)%>%
#   left_join(age, by = c("age_hhh" = "mean_age"))%>%
#   select(-age_hhh)%>%
#   select(heating_fuel, tenant, water_energy, urban_identif, district, urban_identif_2, occupation, gender, IG, AGE, absolute, relative, Percentile, everything())%>%
#   mutate(absolute = round(absolute),
#          relative = round(relative,4))
# 
#   distinct()%>%
#   slice_head(n = 10)
# 
# write_csv(combinations_ESP_1, "../2_Data/Output/Output data/Combinations_Spain_Test.csv")
# 
# combinations_ESP_2 <- combinations_ESP %>%
#   rename(relative = .pred)%>%
#   mutate_at(vars(heating_fuel, water_energy, urban_identif, district, urban_identif_2, gender), ~ as.character(.))%>%
#   mutate(hh_expenditures_EURO_2018 = round(hh_expenditures_EURO_2018))%>%
#   left_join(income_groups, by = c("hh_expenditures_EURO_2018" = "mean"))%>%
#   select(-hh_expenditures_EURO_2018)%>%
#   left_join(age, by = c("age_hhh" = "mean_age"))%>%
#   select(-age_hhh)%>%
#   select(heating_fuel, tenant, water_energy, urban_identif, district, urban_identif_2, occupation, gender, IG, AGE, absolute, relative, Percentile, everything())%>%
#   mutate(absolute = round(absolute),
#          relative = round(relative,4))%>%
#   group_by(tenant, gender)%>%
#   summarise(absolute = mean(absolute),
#             relative = mean(relative),
#             Percentile = first(Percentile))%>%
#   ungroup()
#   slice_head(n = 1000)
# 
# # input <- c("heating_fuel", "tenant", "water_energy", "urban_identif", "district", "urban_identif_2", "occupation", "gender", "IG", "AGE")
# input <- c("tenant", "gender")
# 
# outcome <- c("absolute", "relative", "Percentile")
# 
# combinations_ESP_2$input <- apply(combinations_ESP_2[input],1,function(row){
#   toJSON(list(
#     #heating_fuel    = row["heating_fuel"],
#     tenant          = row["tenant"],
#     #water_energy    = row["water_energy"],
#     #urban_identif   = row["urban_identif"],
#     #district        = row["district"],
#     #urban_identif_2 = row["urban_identif_2"],
#     #occupation      = row["occupation"],
#     gender          = row["gender"]#,
#     #IG              = row["IG"],
#     #AGE             = row["AGE"]
#   ), auto_unbox = TRUE)
# })
# 
# combinations_ESP_2$outcome <- apply(combinations_ESP_2[outcome],1,function(row){
#   toJSON(list(
#     absolute = as.numeric(row[["absolute"]]),
#     relative = as.numeric(row[["relative"]]),
#     Percentile = as.numeric(row[["Percentile"]])
#   ), auto_unbox = TRUE)
# })
# 
# outcome_ESP_2 <- combinations_ESP_2 %>%
#   select(input, outcome)
# 
# write.csv(outcome_ESP_2, "../2_Data/Output/Output data/Combinations_Spain_Test_JSON.csv", row.names = FALSE)
