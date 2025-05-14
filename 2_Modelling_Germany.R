# 0     General ####
# Author: L. Missbach (missbach@mcc-berlin.net)
# Date: 18th of September 2023

# 0.1   Packages ####

if(!require("pacman")) install.packages("pacman")

p_load("boot", "broom", "fixest", "ggpubr", "ggrepel",
       "ggsci", "Hmisc", "knitr", "kableExtra", "openxlsx", "rattle", "scales", "tidymodels", "tidyverse", "xtable")

options(scipen=999)

# 0.2   Load data ####

path_0 <- "H:/6_Elite_Survey/2_Data"

# 0.2.1 EVS-Data ####

# 2023_EVS_2018

# data_0 <- read_delim("K:/2023_EVS_2018/23D024/suf_evs_2018_ngt_gf4_slr/daten/evs_ngt2018_slr.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE) 
# 10.351 * 548 --> What is this?

data_0 <- read_delim("K:/2022_DESTATIS_Mikrozensen2014+2016+2019+EVS2013+2018/MCC/suf_evs_2018_aagshb_gf3_slr/daten/evs_aagshb2018_slr.csv",
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
# 42.226 * 1198

# data_0.1 <- read_delim("K:/2020_DESTATIS_EVS_2018/MCC/suf_evs_2018_aagshb_gf3_slr/daten/evs_aagshb2018_slr.csv",
#                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

# 0.2.2 Carbon intensities ####

carbon_intensities_0 <- read.xlsx("H:/6_Citizen_Survey/2_Data/Carbon_Intensities_Full_All_Gas.xlsx", sheet = "Germany")
  
GTAP_code            <- read_delim("H:/6_Citizen_Survey/2_Data/GTAP10.csv", ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
  
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

item_codes <- read.xlsx(sprintf("%s/EVS 2018/Item_Codes_Description_EVS.xlsx", path_0))

item_fuels <- read.xlsx(sprintf("%s/EVS 2018/Item_Fuel_Concordance_EVS.xlsx", path_0), colNames = FALSE)%>%
  rename(item_code = X2, fuel = X1, Description = X3)

item_gtap <- read.xlsx(sprintf("%s/EVS 2018/Item_GTAP_Concordance_EVS.xlsx", path_0))%>%
  select(-Explanation)%>%
  pivot_longer(-GTAP, values_to = "item_code", names_to = "drop")%>%
  filter(!is.na(item_code))%>%
  select(-drop)%>%
  arrange(GTAP, item_code)%>%
  mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))%>%
  distinct()

# 0.2.4 Codes ####

bundesland.code <- data.frame(bundesland = seq(1,16,1),
                              Bundesland = c("Schleswig-Holstein", "Hamburg", "Niedersachsen", "Bremen", "Nordrhein-Westfalen", "Hessen",
                                             "Rheinland-Pfalz", "Baden-Wuerttemberg", "Bayern", "Saarland", "Berlin", "Brandenburg",
                                             "Mecklenburg-Vorpommern", "Sachsen", "Sachsen-Anhalt", "Thueringen"))
urban.code <- data.frame(urban_type = c(1,2,3),
                         Urban_type = c("Agglomerationsraum", "Verstaedterter Raum", "Laendlicher Raum"))

gender.code <- data.frame(gender_hhh = c(1,2),
                          Gender_hhh = c("Maennlich", "Weiblich"))

nationality.code <- data.frame(nationality = c(1,2,3),
                               Nationality = c("Deutsch", "EU", "Nicht-EU"))

education.code <- data.frame(edu_hhh   = seq(1,7,1),
                             Education = c("Kein Abschluss", "Abschluss nach sieben Jahren", "Hauptschule", "Realschule", "Realschule",
                                           "Fachhochschulreife", "Abitur"))

education.code.2 <- data.frame(edu_hhh_2 = seq(1,15,1),
                               Ausbildung = c("Keine Ausbildung", "Anlernausbildung", "Berufsausbildung", "Vorbereitungsdienst",
                                              "Ausbildung Gesundheit- und Sozialberufe", "Ausbildung Gesundheit- und Sozialberufe",
                                              "ErzieherIn", "MeisterIn", "TechnikerIn", "Fachschule DDR", "Fachakademie Bayern",
                                              "Berufsakademie", "Fachhochschule", "Universitaet", "Promotion"))

employment.code <- data.frame(employment = seq(1,11,1),
                              Employment = c("Erwerbstaetigkeit", "Altersteilzeit", "Einkuenfte des Partners",
                                             "Vermoegen, Vermietung, Verpachtung", "Renten", "Pensionen", "Betriebsrente",
                                             "Arbeitslosengeld I", "Arbeitslosengeld II", "Sozialhilfe", "Sonstiges"))

industry.code <- data.frame(industry = seq(0,22,1),
                            Industry = c("NA", "Land- und Forstwirtschaft, Fischerei", 
                                         "Bergbau und Gewinnung von Erdoel, Erdgas, Steinen und Erden",
                                         "Verarbeitendes Gewerbe / Herstellung von Waren",
                                         "Energieversorgung",
                                         "Wasserversorgung; Abwasser- und Abfallentsorgung und Beseitigung von Umweltverschmutzung",
                                         "Baugewerbe, Hoch- und Tiefbau",
                                         "Gross- und Einzelhandel; Instandhaltung und Reparatur von Kraftfahrzeugen",
                                         "Personen- und Gueterverkehr; Lagerei (einschliesslich Post- und Kurierdienst)",
                                         "Gastgewerbe / Beherbergung und Gastronomie",
                                         "Information und Kommunikation",
                                         "Banken / Finanz- und Versicherungsdienstleister",
                                         "Grundstuecks- und Wohnungswesen",
                                         "Freiberufliche, wissenschaftliche und technische Dienstleistungen",
                                         "Sonstige wirtschaftliche Dienstleistungen fuer Unternehmen und Privatpersonen",
                                         "Öffentliche Verwaltung, Gerichte, Öffentliche Sicherheit und Ordnung, Verteidigung, Sozialversicherung",
                                         "Erziehung und Unterricht",
                                         "Gesundheits- und Sozialwesen",
                                         "Sonstige ueberwiegend personenbezogene Dienstleistungen; allgemeine Reparaturen von Waren und Geraeten",
                                         "Kunst, Unterhaltung, Sport und Erholung",
                                         "Gewerkschaft, Verband, Partei und sonstige Interessenvertretung, kirchliche und religioese Vereinigung",
                                         "Konsulat, Botschaft, internationale und supranationale Organisation",
                                         "Privater Haushalt mit Beschaeftigten"))

building_year.code <- data.frame(building_year = c(1,2,3,4,5),
                                 Building_year = c("<1949", "1949-1990", "1991-2000", "2001-2010","2011-2018"))

building_type.code <- data.frame(building_type = c(1,2,3,4,5),
                                 Building_type = c("Einfamilienhaus", "Doppelhaushaelfte", "Zweifamilienhaus", "Wohngebaeude", "Sonstiges"))

# renting.code <- data.frame(renting = c(1,2,3,4,5),
#                            Renting = c("Eigent?mer", "Eigent?mer", "Mieter", "Mietfrei", "Mietfrei"))

heating_type.code <- data.frame(heating_type = c(1,2,3,4),
                                Heating_type = c("Fernheizung", "Block- o. Zentralheizung", "Etagenheizung", "Einzeloefen"))

heating_fuel.code <- data.frame(heating_fuel = c(0,1,2,3,4,5),
                                Heating_fuel = c("NA", "Strom", "Gas", "Heizoel", "Feste Brennstoffe", "Sonstiges"))

renting.code <- data.frame(renting = c(1,2,3),
                           Renting = c("Mieter", "Eigentuemer", "Mietfrei"))

# 0.2.5 Supplementary data ####

exchange.rate <- 1.12968118 # Source as usual
inflation.rate <- 1/1.01711 # Source: IMF

# 1     Data transformation ####

# Household information

data_0.1 <- data_0 %>%
  rename(hh_id = EF2U2, bundesland = EF2U1, urban_type = EF5, hh_size = EF7, ost_west = EF1,
         gender_hhh = EF8U2, nationality = EF8U5, edu_hhh = EF8U6, edu_hhh_2 = EF8U7, year_hhh = EF8U3, employment = EF8U13, industry = EF8U20,
         building_year = EF18, building_type = EF19, 
         # renting = EF20, 
         space = EF21, heating_type = EF22, heating_fuel = EF23, renting = EF50,
         income_4 = EF56, # Einkommen aus ?ffentlichen Transferzahlungen
         income_5 = EF60, # Haushaltsbruttoeinkommen
         income_6 = EF62, # Haushaltsnettoeinkommen
         hh_weights = EF107, 
         motorcycle.01 = EF534, tv.01 = EF537, refrigerator.01 = EF556, dishwasher.01 = EF558, washing_machine.01 = EF560,
         stove.e.01 = EF562, stove.g.01 = EF563)%>%
  select(hh_id, everything())%>%
  arrange(hh_id)%>%
  mutate(number_of_cars = EF531 + EF532 + EF533,
         urban_01       = ifelse(urban_type == 3,0,1),
         ost_west       = ifelse(ost_west == 33, "West", "Ost"))%>%
  mutate_at(vars(motorcycle.01, tv.01, refrigerator.01, dishwasher.01, washing_machine.01, stove.e.01, stove.g.01), ~ ifelse(. > 0,1,0))%>%
  select(hh_id, hh_size, hh_weights, 
         bundesland, urban_type, urban_01, ost_west,
         gender_hhh, nationality, edu_hhh, edu_hhh_2, year_hhh, employment, industry, building_year, building_type, renting, space, heating_type, heating_fuel,
         starts_with("income"), 
         motorcycle.01, tv.01, refrigerator.01, dishwasher.01, washing_machine.01, stove.e.01, stove.g.01, number_of_cars)%>%
  mutate(age_hhh = 2018 - year_hhh,
         income_4 = income_4*4,
         income_5 = income_5*4,
         income_6 = income_6*4)%>%
  left_join(bundesland.code,    by = "bundesland")%>%
  left_join(urban.code,         by = "urban_type")%>%
  left_join(gender.code,        by = "gender_hhh")%>%
  left_join(nationality.code,   by = "nationality")%>%
  left_join(education.code,     by = "edu_hhh")%>%
  left_join(education.code.2,   by = "edu_hhh_2")%>%
  left_join(employment.code,    by = "employment")%>%
  left_join(industry.code,      by = "industry")%>%
  left_join(building_year.code, by = "building_year")%>%
  left_join(building_type.code, by = "building_type")%>%
  left_join(heating_type.code,  by = "heating_type")%>%
  left_join(heating_fuel.code,  by = "heating_fuel")%>%
  left_join(renting.code,       by = "renting")%>%
  select(hh_id, hh_size, hh_weights, 
         Bundesland, Urban_type, urban_01, ost_west, age_hhh,
         Gender_hhh, Nationality, Education, Ausbildung, year_hhh, Employment, Industry, Building_year, Building_type, 
         Renting, space, Heating_type, Heating_fuel,
         starts_with("income"), 
         motorcycle.01, tv.01, refrigerator.01, dishwasher.01, washing_machine.01, stove.e.01, stove.g.01, number_of_cars)%>%
  rename_all(tolower)%>%
  filter(income_5 != 0)%>%
  # remove households with negative income
  filter(income_6 > 0)

rm(building_type.code, building_year.code, bundesland.code, education.code, education.code.2, employment.code, gender.code,
   heating_fuel.code, heating_type.code, industry.code, nationality.code, renting.code, urban.code)

# Expenditure information

data_0.2 <- data_0 %>%
  rename(hh_id = EF2U2)%>%
  select(hh_id, EF242:EF529)%>%
  arrange(hh_id)%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures")%>%
  left_join(select(data_0.1, hh_id, hh_weights))%>%
  group_by(item_code)%>%
  mutate(outlier_99 = wtd.quantile(expenditures, weights = hh_weights, probs = 0.99),
        median_exp  = wtd.quantile(expenditures, weights = hh_weights, probs = 0.5))%>%
  ungroup()%>%
  mutate(flag_outlier_99 = ifelse(expenditures>= outlier_99,1,0))%>%
  # this line replaces all expenditures which are above the 99th percentile for each item to the median
  mutate(expenditures = ifelse(flag_outlier_99 == 1, median_exp, expenditures))%>%
  # After check: Cleaning at expenditure-level not strictly necessary
  mutate(expenditures_year = expenditures*4)%>%
  filter(expenditures_year > 0)

rm(data_0)

# 1.1   Transform and clean expenditures ####

data_0.2.1 <- data_0.2 %>%
  left_join(item_gtap, by = "item_code")%>%
  filter(GTAP != "deleted")%>%
  left_join(select(carbon_intensities, GTAP, CO2_t_per_dollar_national, CO2_t_per_dollar_transport, CO2_t_per_dollar_gas, CO2_t_per_dollar_gas_direct), by = "GTAP")%>%
  mutate(CO2_t_per_dollar_national   = ifelse(is.na(CO2_t_per_dollar_national),  0,CO2_t_per_dollar_national),
         CO2_t_per_dollar_transport  = ifelse(is.na(CO2_t_per_dollar_transport), 0,CO2_t_per_dollar_transport),
         CO2_t_per_dollar_gas        = ifelse(is.na(CO2_t_per_dollar_gas),       0,CO2_t_per_dollar_gas),
         CO2_t_per_dollar_gas_direct = ifelse(is.na(CO2_t_per_dollar_gas_direct),0,CO2_t_per_dollar_gas_direct))%>%
  # Adjusting for dollar/Euro
  mutate(CO2_t_per_euro_national   = CO2_t_per_dollar_national*exchange.rate,
         CO2_t_per_euro_transport  = CO2_t_per_dollar_transport*exchange.rate,
         CO2_t_per_euro_gas        = CO2_t_per_dollar_gas*exchange.rate,
         CO2_t_per_euro_gas_direct = CO2_t_per_dollar_gas_direct*exchange.rate)%>%
  mutate(CO2_t_national            = CO2_t_per_euro_national*expenditures_year*inflation.rate,
         CO2_t_transport           = CO2_t_per_euro_transport*expenditures_year*inflation.rate,
         # Applying carbon pricing in gas sector everywhere warranted, given that also manufacturing firms may require gas for heating
         # Also little difference --> latter is true, but still
         CO2_t_gas                = CO2_t_per_euro_gas*expenditures_year*inflation.rate,
         CO2_t_gas_direct         = CO2_t_per_euro_gas_direct*expenditures_year*inflation.rate)

data_0.2.2 <- data_0.2.1 %>%
  group_by(hh_id)%>%
  summarise(hh_expenditures_EURO_2018 = sum(expenditures_year),
            CO2_t_national            = sum(CO2_t_national),
            CO2_t_transport           = sum(CO2_t_transport),
            CO2_t_gas                 = sum(CO2_t_gas),
            CO2_t_gas_direct          = sum(CO2_t_gas_direct))%>%
  ungroup()

# data_0.2.2_supervision <- data_0.2.1 %>%
#   mutate(interest = CO2_t_transport + CO2_t_gas_direct)%>%
#   group_by(hh_id)%>%
#   mutate(interest_sum = sum(interest))%>%
#   ungroup()%>%
#   mutate(share = interest/interest_sum)%>%
#   group_by(item_code)%>%
#   summarise(share = mean(share))%>%
#   ungroup()%>%
#   arrange(desc(share))

# Keine irregulären Items
# Kohle, Holz und andere feste Brennstoffe
# Kraftstoffe, Autogas, Strom für Elektroauto, Schmiermittel
# Erdgas
# Gaszentralheizung und Wasser
# Öl
# Heizöl
# Erdgas
# Fernwärme

data_0.2.3 <- data_0.2 %>%
  left_join(select(item_fuels, item_code, fuel), by = "item_code")%>%
  filter(!is.na(fuel))%>%
  group_by(hh_id, fuel)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()%>%
  pivot_wider(names_from = "fuel", values_from = "expenditures_year", values_fill = 0, names_prefix = "Exp_")

# 1.2   Compile final dataset ####

data_0.2.4 <- left_join(data_0.2.2, data_0.2.3, by = "hh_id")%>%
  mutate_at(vars(starts_with("Exp_")), ~ ifelse(is.na(.),0,.))

data_1 <- left_join(data_0.1, data_0.2.4, by = "hh_id")%>%
  mutate(household_expenditures_pc = hh_expenditures_EURO_2018/hh_size,
         household_income_pc       = income_6/hh_size)%>%
  mutate(Income_Group_5            = as.numeric(binning(household_income_pc, bins = 5,  method = c("wtd.quantile"), weights = hh_weights)),
         Income_Group_10           = as.numeric(binning(household_income_pc, bins = 10, method = c("wtd.quantile"), weights = hh_weights)))%>%
  mutate(Expenditure_Group_5       = as.numeric(binning(household_expenditures_pc, bins = 5,  method = c("wtd.quantile"), weights = hh_weights)),
         Expenditure_Group_10      = as.numeric(binning(household_expenditures_pc, bins = 10, method = c("wtd.quantile"), weights = hh_weights)))%>%
  select(-household_expenditures_pc, -household_income_pc)

rm(data_0.1, data_0.2, data_0.2.1, data_0.2.2, data_0.2.3, data_0.2.4,
   item_gtap, item_fuels, item_codes, carbon_intensities,
   exchange.rate, inflation.rate, path_0)

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
  mutate(t_weighted          = (CO2_t_gas_direct+CO2_t_transport)*hh_weights, # 966,928,027 = 898 MtO2 --> Emissionen sind zu hoch
         t_weighted_national = CO2_t_national*hh_weights,                     # 1,260,998,520 = 1,2 MtCO2 --> Emissionen sind zu hoch 
         Exp_Benzin          = Exp_Benzin*hh_weights)                         # 47 Milliarden Euro --> kommt ungef?hr hin.

data_1.2 <- data_1.1 %>%
  select(hh_id, hh_size, hh_weights, bundesland, urban_type, urban_01, ost_west,
         age_hhh, gender_hhh, nationality, education, ausbildung, employment, industry,
         building_year, building_type, renting, space, heating_type, heating_fuel,
         motorcycle.01, tv.01, refrigerator.01, dishwasher.01, washing_machine.01, stove.e.01, number_of_cars, hh_expenditures_EURO_2018,
         starts_with("CO2_"), starts_with("Exp_"), Expenditure_Group_5, Expenditure_Group_10)

write_rds(data_1.2, "H:/6_Citizen_Survey/2_Data/Microdata/Microdata_Transformed_Germany.rds")

rm(data_1, data_1.1, data_1.2)


# Check carbon intensities ####

carbon_intensities_1 <- data.frame()

for(i in c("Germany", "France", "Romania", "Spain")){
  carbon_intensities_0 <- read.xlsx("../2_Data/Carbon_Intensities_Full_All_Gas.xlsx", sheet = i)
  
  GTAP_code            <- read_delim("../2_Data/GTAP10.csv", ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
  
  carbon_intensities   <- left_join(GTAP_code, carbon_intensities_0, by = c("Number"="GTAP"))%>%
    select(-Explanation, - Number)%>%
    mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))%>%
    group_by(GTAP)%>%
    summarise(across(CO2_Mt:Total_HH_Consumption_MUSD, ~ sum(.)))%>%
    ungroup()%>%
    mutate(CO2_direct_Gas = ifelse(GTAP == "gasgdt", CO2_direct,0))%>%
    mutate(CO2_direct_Gas = ifelse(GTAP == "gas" | GTAP == "gdt", CO2_direct,0))%>%
    mutate(CO2_t_per_dollar_global      = CO2_Mt/            Total_HH_Consumption_MUSD,
           CO2_t_per_dollar_national    = CO2_Mt_within/     Total_HH_Consumption_MUSD,
           CO2_t_per_dollar_electricity = CO2_Mt_Electricity/Total_HH_Consumption_MUSD,
           CO2_t_per_dollar_transport   = CO2_Mt_Transport/  Total_HH_Consumption_MUSD,
           CO2_t_per_dollar_gas         = CO2_Mt_Gas/        Total_HH_Consumption_MUSD,
           CO2_t_per_dollar_gas_direct  = CO2_direct_Gas/    Total_HH_Consumption_MUSD)%>%
    select(GTAP, CO2_t_per_dollar_national, CO2_t_per_dollar_gas_direct, CO2_t_per_dollar_transport)%>%
    mutate(Country = i)
  
  carbon_intensities_1 <- carbon_intensities_1 %>%
    bind_rows(carbon_intensities)%>%
    arrange(GTAP, CO2_t_per_dollar_national)
  
  rm(carbon_intensities_0, GTAP_code)
}

# Gasintensität für Deutschland etwas hoch, aber nicht außergewöhnlich hoch.