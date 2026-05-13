# 0     General ####
# Author: L. Missbach (leonard.missbach@pik-potsdam.de)
# Date: 28.10.2026

# 0.1   Packages ####

if(!require("pacman")) install.packages("pacman")

p_load("arrow", "boot", "broom", "extrafont", "fixest", "ggpubr", "ggrepel",
       "ggsci", "Hmisc", "jsonlite", "knitr", "kableExtra", "openxlsx", "rattle", "readxl", "scales", "showtext", "stringi", "tidymodels", "tidyverse", "xtable")

options(scipen=999)

# 0.2   Load data ####

# Pilot data
# data_0_ESP <- read_csv("../2_Data/0_Qualtrics_Output/20250826_Pilot/Spanish/Spanish_26. August 2025_04.16.csv")
# data_0_FRA <- read_csv("../2_Data/0_Qualtrics_Output/20250826_Pilot/French/French_26. August 2025_04.19.csv")
# data_0_GER <- read_csv("../2_Data/0_Qualtrics_Output/20250826_Pilot/German/German_26. August 2025_04.18.csv")
# data_0_ROM <- read_csv("../2_Data/0_Qualtrics_Output/20250826_Pilot/Romanian/Romanian_26. August 2025_04.18.csv")

# Survey data
data_0_ESP <- read_csv("../2_Data/0_Qualtrics_Output/20260309_Final/Spanish/Spanish_5.+Mai+2026_15.02.csv")
data_0_FRA <- read_csv("../2_Data/0_Qualtrics_Output/20260309_Final/French/French_9.+März+2026_09.31.csv")
data_0_GER <- read_csv("../2_Data/0_Qualtrics_Output/20260309_Final/German/German_9.+März+2026_09.31.csv")
data_0_ROM <- read_csv("../2_Data/0_Qualtrics_Output/20260309_Final/Romanian/Romanian_5.+Mai+2026_15.02.csv")

com_0_ESP <- read_parquet("../2_Data/Output/Output data/Combinations_Qualtrics_Spain_251117.parquet")
com_0_FRA <- read_parquet("../2_Data/Output/Output data/Combinations_Qualtrics_France_251117.parquet")
com_0_GER <- read_parquet("../2_Data/Output/Output data/Combinations_Qualtrics_Germany_251117.parquet")
com_0_ROM <- read_parquet("../2_Data/Output/Output data/Combinations_Qualtrics_Romania_251117.parquet")

# Median costs
median_costs <- read.xlsx("../2_Data/Supplementary/Median_Costs_Countries.xlsx")

# 1     Data transformation ####
# 1.1   Spain ####

data_1_ESP <- data_0_ESP %>%
  filter(Status == "IP-Adresse")%>%
  # Convert date
  mutate(StartDate = as.POSIXct(StartDate, format = "%Y-%m-%d %H:%M:%S"))%>%
  mutate(Date = format(StartDate, "%Y-%m-%d"))%>%
  filter(as.Date(Date) > as.Date("2025-12-17"))%>%   # TBA
  # Create basic columns
  mutate(Country = "Spain")%>%
  mutate(ID = 1:n())%>%
  select(Country, ID, everything())%>%
  rename(time = "Duration (in seconds)")%>%
  filter(Q02 != "No")%>%
  filter(Finished == "Wahr")%>%
  # Attention Check 1
  filter(Q38A == "De acuerdo")%>%
  # For now
  filter(Q60A == "Energía")%>%
  # Order of columns
  select(Country, ID, time, Q10:Date)

# Label-file
data_1.1.1_ESP <- data_1_ESP %>%
  select(Country, ID, Label1:Label5)%>%
  pivot_longer(Label1:Label5, names_to = "Label", values_to = "Value", names_prefix = "Label")

# Institution-file
data_1.1.2_ESP <- data_1_ESP %>%
  select(Country, ID, Institution1:Institution3)%>%
  pivot_longer(Institution1:Institution3, names_to = "Institution", values_to = "Value_Institution", names_prefix = "Institution")

# Institutional trust values
data_1.1_ESP <- data_1_ESP %>%
  select(Country, ID, Q31_1:Q31_3)%>%
  pivot_longer(Q31_1:Q31_3, names_to = "Variable", values_to = "Value_raw")%>%
  # filter(!is.na(Value_raw))%>% # TBA
  mutate(Label = ifelse(str_detect(Value_raw, "1"),"1",
                        ifelse(str_detect(Value_raw, "2"), "2",
                               ifelse(str_detect(Value_raw, "3"), "3",
                                      ifelse(str_detect(Value_raw, "4"), "4",
                                             ifelse(str_detect(Value_raw, "5"), "5", Value_raw))))))%>%
  mutate(Institution = str_sub(Variable,-1,-1))%>%
  mutate(Variable = str_sub(Variable,1,-3))%>%
  left_join(data_1.1.1_ESP)%>%
  left_join(data_1.1.2_ESP)%>%
  mutate(Institution = ifelse(Value_Institution == "El gobierno español", "Gov_nat",
                              ifelse(Value_Institution == "Su gobierno local", "Gov_loc",
                                     ifelse(Value_Institution == "La Comisión Europea", "EU_Comm", Value_Institution))))%>%
  mutate(Column = paste0(Variable, "_", Institution))%>%
  select(Country, ID, Column, Value)%>%
  pivot_wider(names_from = "Column", values_from = "Value")

data_1.2_ESP <- data_1_ESP %>%
  select(-(Q31_1:Q31_3), -(Institution1:Label5))%>%
  left_join(data_1.1_ESP)%>%
  select(Country:Q30_3, starts_with("Q31"), everything())

# Cost range
data_1.2.1_ESP <- data_1.2_ESP %>%
  select(Country, ID, Q42_1, Q42_2)%>%
  pivot_longer(Q42_1:Q42_2)%>%
  left_join(select(data_1.2_ESP, Country, ID, t1:t6))%>%
  mutate(value = ifelse(str_detect(value, "t1"), str_replace(value, fixed("${e://Field/t1}"), t1), value),
         value = ifelse(str_detect(value, "t2"), str_replace(value, fixed("${e://Field/t2}"), t2), value),
         value = ifelse(str_detect(value, "t3"), str_replace(value, fixed("${e://Field/t3}"), t3), value),
         value = ifelse(str_detect(value, "t4"), str_replace(value, fixed("${e://Field/t4}"), t4), value),
         value = ifelse(str_detect(value, "t5"), str_replace(value, fixed("${e://Field/t5}"), t5), value),
         value = ifelse(str_detect(value, "t6"), str_replace(value, fixed("${e://Field/t6}"), t6), value))%>%
  select(Country, ID, name, value)%>%
  pivot_wider()

data_1.3_ESP <- data_1.2_ESP %>%
  rename("Q42_1_true" = "Q42_1", "Q42_2_true" = "Q42_2", policy = "Policy description")%>%
  left_join(data_1.2.1_ESP)%>%
  select(Country:Q42_1_true, Q42_1, Q43_1:Q42_2_true, Q42_2, everything())%>%
  # Remove some columns
  select(-(heating:spending), -(FairnessPerception:"Create New Field or Choose From Dropdown..."))%>%
  # mutate_at(vars(Effectiveness_1, Effectiveness_2, Expected_cost_1_true, Expected_cost_2_true), ~ ifelse(is.na(.), "No lo sé",.))%>%
  # Introducing factors
  mutate(Q41_1 = factor(Q41_1, levels = c("En absoluto eficaz", "Probablemente no sea eficaz", "Probablemente eficaz", "Sin duda eficaz")),
         Q41_2 = factor(Q41_2, levels = c("En absoluto eficaz", "Probablemente no sea eficaz", "Probablemente eficaz", "Sin duda eficaz")),
         Q42_1_true = factor(Q42_1_true, levels = c("Menos de ${e://Field/t1}€", "Entre ${e://Field/t1}€ y ${e://Field/t2}€", "Entre ${e://Field/t2}€ y ${e://Field/t3}€", "Entre ${e://Field/t3}€ y ${e://Field/t4}€", "Entre ${e://Field/t4}€ y ${e://Field/t5}€", "Entre ${e://Field/t5}€ y ${e://Field/t6}€", "Más de ${e://Field/t6}€")),
         Q42_2_true = factor(Q42_2_true, levels = c("Menos de ${e://Field/t1}€", "Entre ${e://Field/t1}€ y ${e://Field/t2}€", "Entre ${e://Field/t2}€ y ${e://Field/t3}€", "Entre ${e://Field/t3}€ y ${e://Field/t4}€", "Entre ${e://Field/t4}€ y ${e://Field/t5}€", "Entre ${e://Field/t5}€ y ${e://Field/t6}€", "Más de ${e://Field/t6}€")),
         Q43_1 = factor(Q43_1, levels = c("Mucho más que a un hogar típico","Algo más que a un hogar típico", "Más o menos lo mismo que a un hogar típico", "Algo menos que a un hogar típico", "Mucho menos que a un hogar típico")),
         Q43_2 = factor(Q43_2, levels = c("Mucho más que a un hogar típico","Algo más que a un hogar típico", "Más o menos lo mismo que a un hogar típico", "Algo menos que a un hogar típico", "Mucho menos que a un hogar típico")),
         Q44_1 = factor(Q44_1, levels = c("Los perjudicará", "Ni los ayudará ni los perjudicará", "Los ayudará")),
         Q44_2 = factor(Q44_2, levels = c("Los perjudicará", "Ni los ayudará ni los perjudicará", "Los ayudará")),
         Q45_1 = factor(Q45_1, levels = c("Injusta", "Ni justa ni injusta", "Justa")),
         Q45_2 = factor(Q45_2, levels = c("Injusta", "Ni justa ni injusta", "Justa")),
         Q46_1 = factor(Q46_1, levels = c("Me opongo firmemente", "Me opongo en parte", "Ni la apoyo ni me opongo", "La apoyo en parte", "La apoyo firmemente")),
         Q46_2 = factor(Q46_2, levels = c("Me opongo firmemente", "Me opongo en parte", "Ni la apoyo ni me opongo", "La apoyo en parte", "La apoyo firmemente")),
         Q30_1 = factor(Q30_1, levels = c("En absoluto", "Un poco", "Algo", "Bastante", "Completamente")),
         Q30_2 = factor(Q30_2, levels = c("En absoluto", "Un poco", "Algo", "Bastante", "Completamente")),
         Q30_3 = factor(Q30_3, levels = c("En absoluto", "Un poco", "Algo", "Bastante", "Completamente")))%>%
  mutate(PedTreatmentGrp = ifelse(PedTreatmentGrp == "Treat", "Treatment", PedTreatmentGrp))%>%
  mutate(Treatment_A = factor(policy, levels = c("EU", "nonEU")),
         Treatment_B = factor(PedTreatmentGrp,  levels = c("Control", "Treatment")),
         Treatment_C = factor(CostTreatmentGrp, labels = c("C1", "C2", "C3", "C4", "Control")))%>%
  select(-PedTreatmentGrp, -CostTreatmentGrp,-policy)%>%
  arrange(ID)

# Join cost estimates 

com_1_ESP <- com_0_ESP %>%
  rename(Q20 = "heating_fuel", Q21 = "tenant", Q29A = "water_energy", Q27 = "urban_identif",
         Q28 = "hh_expenditures")%>%
  mutate_at(vars(Q20:age_hhh), ~ as.character(.))

data_1.4_ESP <- data_1.3_ESP %>%
  mutate(age_hhh = ifelse(Q10 == "Más de 63 años", "más de 63 años",
                          ifelse(Q10 %in% c("Entre 45 y 54 años", "Entre 55 y 63 años"), "47 a 62 años", "hasta 46 años")),
         gender = ifelse(Q11 == "Otros", "Mujer", Q11),
         occupation = ifelse(Q13 %in% c("Inactivo (no en búsqueda de trabajo)"), "Dedicado/a a las labores del hogar",
                             ifelse(Q13 %in% c("Jubilado"), "Jubilado/a, retirado/a anticipadamente",
                                    ifelse(Q13 %in% c("Estudiante"), "Otra situación",
                                           ifelse(Q13 %in% c("Prefiero no revelarlo"), "Prefiero no revelarlo", "Trabajando al menos una hora")))),
         district = case_when(Q26 == "Comunidad Foral de Navarra" ~ "Navarra, Comunidad Foral de",
                              Q26 == "Comunidad de Madrid" ~ "Madrid, Comunidad de",
                              Q26 == "Illes Balears" ~ "Balears, Illes",
                              Q26 == "Principado de Asturias" ~ "Asturias, Principado d",
                              Q26 == "Región de Murcia" ~ "Murcia, Región de",
                              Q26 == "La Rioja" ~ "Rioja, La",
                              TRUE ~ Q26))%>%
  left_join(com_1_ESP)%>%
  mutate(absolute_value = ifelse(Pricelevel == "45", as.character(absolute_45),
                                 ifelse(Pricelevel == "85", as.character(absolute_85),
                                        ifelse(Pricelevel == "125", as.character(absolute_125), NA))),
         relative_value = ifelse(Pricelevel == "45", as.character(relative_45),
                                 ifelse(Pricelevel == "85", as.character(relative_85),
                                        ifelse(Pricelevel == "125", as.character(relative_125), NA))))%>%
  mutate(absolute_value = as.numeric(str_replace_all(absolute_value, "[€,]", "")),
         relative_value = as.numeric(str_replace_all(relative_value, "%",""))/100)%>%
  select(-(absolute:relative_165), -(age_hhh:district))%>%
  left_join(median_costs)%>%
  mutate(above_median = ifelse((absolute_value > median_45 & Pricelevel == "45") | (absolute_value > median_85 & Pricelevel == "85") | (absolute_value > median_125 & Pricelevel == "125"),1,0))

# Edit single columns or filter
data_1.4_ESP <- data_1.4_ESP %>%
  # Time in seconds - slowest 2% and fastest 5%
  mutate(time = as.numeric(time))%>%
  mutate(filter_1a = ifelse(time <= quantile(time, probs = 0.05, na.rm = TRUE),1,0),
         filter_1b = ifelse(time >= quantile(time, probs = 0.98, na.rm = TRUE),1,0))%>%
  # NA in every Q41_1 to Q46_1
  mutate(filter_2a = ifelse(is.na(Q41_1) & is.na(Q42_1) & is.na(Q43_1) & is.na(Q44_1) & is.na(Q45_1) & is.na(Q46_1),1,0),
  # NA in every Q41_2 to Q46_2       
         filter_2b = ifelse(is.na(Q41_2) & is.na(Q42_2) & is.na(Q43_2) & is.na(Q44_2) & is.na(Q45_2) & is.na(Q46_2),1,0),
  # NA in every Q62 to Q68
         filter_2c = ifelse(is.na(Q62) & is.na(Q63) & is.na(Q64) & is.na(Q65) & is.na(Q66) & is.na(Q67) & is.na(Q68),1,0))%>%
  # NA in every C1_1, C1_2, C1_3, C1_4
  mutate(filter_3a = ifelse(is.na(C1_1) & is.na(C2_1) & is.na(C3_1) & is.na(C4_1),1,0))

rm(data_1_ESP, data_1.1_ESP, data_1.1.1_ESP, data_1.1.2_ESP, data_1.2_ESP, data_1.2.1_ESP, data_1.3_ESP, data_0_ESP, com_1_ESP, com_0_ESP)

# 1.2   France ####

data_1_FRA <- data_0_FRA %>%
  filter(Status == "IP-Adresse")%>%
  # Convert date
  mutate(StartDate = as.POSIXct(StartDate, format = "%Y-%m-%d %H:%M:%S"))%>%
  mutate(Date = format(StartDate, "%Y-%m-%d"))%>%
  filter(as.Date(Date) > as.Date("2025-12-17"))%>% # TBA
  # Create basic columns
  mutate(Country = "France")%>%
  mutate(ID = 1:n())%>%
  select(Country, ID, everything())%>%
  rename(time = "Duration (in seconds)")%>%
  filter(Q02 != "Non")%>%
  filter(Finished == "Wahr")%>%
  # Attention Check 1
  filter(Q38A == "D’accord")%>%
  # For now
  filter(Q60A == "Énergie")%>%
  # Order of columns
  select(Country, ID, time, Q10:Date)

# Label-file
data_1.1.1_FRA <- data_1_FRA %>%
  select(Country, ID, Label1:Label5)%>%
  pivot_longer(Label1:Label5, names_to = "Label", values_to = "Value", names_prefix = "Label")

# Institution-file
data_1.1.2_FRA <- data_1_FRA %>%
  select(Country, ID, Institution1:Institution3)%>%
  pivot_longer(Institution1:Institution3, names_to = "Institution", values_to = "Value_Institution", names_prefix = "Institution")

# Institutional trust values
data_1.1_FRA <- data_1_FRA %>%
  select(Country, ID, Q31_1:Q31_3)%>%
  pivot_longer(Q31_1:Q31_3, names_to = "Variable", values_to = "Value_raw")%>%
  # filter(!is.na(Value_raw))%>% # TBA
  # Decision: NAs == 5 "I don't know"
  mutate(Value_raw = ifelse(is.na(Value_raw), "${e://Field/Label5}", Value_raw))%>%
  mutate(Label = ifelse(str_detect(Value_raw, "1"),"1",
                        ifelse(str_detect(Value_raw, "2"), "2",
                               ifelse(str_detect(Value_raw, "3"), "3",
                                      ifelse(str_detect(Value_raw, "4"), "4",
                                             ifelse(str_detect(Value_raw, "5"), "5", Value_raw))))))%>%
  mutate(Institution = str_sub(Variable,-1,-1))%>%
  mutate(Variable = str_sub(Variable,1,-3))%>%
  left_join(data_1.1.1_FRA)%>%
  left_join(data_1.1.2_FRA)%>%
  mutate(Institution = ifelse(Value_Institution == "Le gouvernement français", "Gov_nat",
                              ifelse(Value_Institution == "Votre administration locale", "Gov_loc",
                                     ifelse(Value_Institution == "La Commission européenne", "EU_Comm", Value_Institution))))%>%
  mutate(Column = paste0(Variable, "_", Institution))%>%
  select(Country, ID, Column, Value)%>%
  pivot_wider(names_from = "Column", values_from = "Value")

data_1.2_FRA <- data_1_FRA %>%
  select(-(Q31_1:Q31_3), -(Institution1:Label5))%>%
  left_join(data_1.1_FRA)%>%
  select(Country:Q30_3, starts_with("Q31"), everything())

# Cost range
data_1.2.1_FRA <- data_1.2_FRA %>%
  select(Country, ID, Q42_1, Q42_2)%>%
  pivot_longer(Q42_1:Q42_2)%>%
  left_join(select(data_1.2_FRA, Country, ID, t1:t6))%>%
  mutate(value = ifelse(str_detect(value, "t1"), str_replace(value, fixed("${e://Field/t1}"), t1), value),
         value = ifelse(str_detect(value, "t2"), str_replace(value, fixed("${e://Field/t2}"), t2), value),
         value = ifelse(str_detect(value, "t3"), str_replace(value, fixed("${e://Field/t3}"), t3), value),
         value = ifelse(str_detect(value, "t4"), str_replace(value, fixed("${e://Field/t4}"), t4), value),
         value = ifelse(str_detect(value, "t5"), str_replace(value, fixed("${e://Field/t5}"), t5), value),
         value = ifelse(str_detect(value, "t6"), str_replace(value, fixed("${e://Field/t6}"), t6), value))%>%
  select(Country, ID, name, value)%>%
  pivot_wider()

data_1.3_FRA <- data_1.2_FRA %>%
  rename("Q42_1_true" = "Q42_1", "Q42_2_true" = "Q42_2", policy = "Policy description")%>%
  left_join(data_1.2.1_FRA)%>%
  select(Country:Q42_1_true, Q42_1, Q43_1:Q42_2_true, Q42_2, everything())%>%
  # Remove some columns
  select(-(FairnessPerception:PolicySupport), -"Create New Field or Choose From Dropdon...")%>%
  # mutate_at(vars(Effectiveness_1, Effectiveness_2, Expected_cost_1_true, Expected_cost_2_true), ~ ifelse(is.na(.), "No lo sé",.))%>%
  # Introducing factors
  mutate(Q41_1 = factor(Q41_1, levels = c("Certainement inefficace", "Probablement inefficace", "Probablement efficace", "Certainement efficace")),
         Q41_2 = factor(Q41_2, levels = c("Certainement inefficace", "Probablement inefficace", "Probablement efficace", "Certainement efficace")),
         Q42_1_true = factor(Q42_1_true, levels = c("Moins de ${e://Field/t1}€", "Entre ${e://Field/t1}€ et ${e://Field/t2}€", "Entre ${e://Field/t2}€ et ${e://Field/t3}€", "Entre ${e://Field/t3}€ et ${e://Field/t4}€", "Entre ${e://Field/t4}€ et ${e://Field/t5}€", "Entre ${e://Field/t5}€ et ${e://Field/t6}€", "Plus de ${e://Field/t6}€")),
         Q42_2_true = factor(Q42_2_true, levels = c("Moins de ${e://Field/t1}€", "Entre ${e://Field/t1}€ et ${e://Field/t2}€", "Entre ${e://Field/t2}€ et ${e://Field/t3}€", "Entre ${e://Field/t3}€ et ${e://Field/t4}€", "Entre ${e://Field/t4}€ et ${e://Field/t5}€", "Entre ${e://Field/t5}€ et ${e://Field/t6}€", "Plus de ${e://Field/t6}€")),
         Q43_1 = factor(Q43_1, levels = c("Beaucoup plus qu’un ménage type","Un peu plus qu'un ménage type","À peu près autant qu’un ménage type", "Un peu moins qu’un ménage type","Beaucoup moins qu'un ménage type")),
         Q43_2 = factor(Q43_2, levels = c("Beaucoup plus qu’un ménage type","Un peu plus qu'un ménage type","À peu près autant qu’un ménage type", "Un peu moins qu'un ménage type","Beaucoup moins qu’un ménage type")),
         Q44_1 = factor(Q44_1, levels = c("Nuisible", "Ni nuisible ni utile", "Utile")),
         Q44_2 = factor(Q44_2, levels = c("Nuisible", "Ni nuisible ni utile", "Utile")),
         Q45_1 = factor(Q45_1, levels = c("Injuste", "Ni juste ni injuste", "Juste")),
         Q45_2 = factor(Q45_2, levels = c("Injuste", "Ni juste ni injuste", "Juste")),
         Q46_1 = factor(Q46_1, levels = c("Je suis tout à fait contre", "Je suis plutôt contre", "Je ne suis ni pour ni contre", "Je suis plutôt pour", "Je suis tout à fait pour")),
         Q46_2 = factor(Q46_2, levels = c("Je suis tout à fait contre", "Je suis plutôt contre", "Je ne suis ni pour ni contre", "Je suis plutôt pour", "Je suis tout à fait pour")),
         Q30_1 = factor(Q30_1, levels = c("Pas du tout", "Un peu", "Assez", "Largement", "Totalement")),
         Q30_2 = factor(Q30_2, levels = c("Pas du tout", "Un peu", "Assez", "Largement", "Totalement")),
         Q30_3 = factor(Q30_3, levels = c("Pas du tout", "Un peu", "Assez", "Largement", "Totalement")))%>%
  mutate(PedTreatmentGrp = ifelse(PedTreatmentGrp == "Treat", "Treatment", PedTreatmentGrp))%>%
  mutate(Treatment_A = factor(policy, levels = c("EU", "nonEU")),
         Treatment_B = factor(PedTreatmentGrp,  levels = c("Control", "Treatment")),
         Treatment_C = factor(CostTreatmentGrp, labels = c("C1", "C2", "C3", "C4", "Control")))%>%
  select(-PedTreatmentGrp, -CostTreatmentGrp,-policy)%>%
  arrange(ID)

# Join cost estimates 

com_1_FRA <- com_0_FRA %>%
  rename(Q20 = "heating_fuel", Q21 = "tenant", Q27 = "urban_type", Q22 = "housing_type", Q26 = "province", Q23 = "construction_year",
         Q28 = "hh_expenditures", Q25 = "number_of_cars")%>%
  mutate_at(vars(Q20:Q23), ~ as.character(.))

data_1.4_FRA <- data_1.3_FRA %>%
  left_join(com_1_FRA)%>%
  mutate(absolute_value = ifelse(Pricelevel == "45", as.character(absolute_45),
                                 ifelse(Pricelevel == "85", as.character(absolute_85),
                                        ifelse(Pricelevel == "125", as.character(absolute_125), NA))),
         relative_value = ifelse(Pricelevel == "45", as.character(relative_45),
                                 ifelse(Pricelevel == "85", as.character(relative_85),
                                        ifelse(Pricelevel == "125", as.character(relative_125), NA))))%>%
  mutate(absolute_value = as.numeric(str_replace_all(absolute_value, "[€,]", "")),
         relative_value = as.numeric(str_replace_all(relative_value, "%",""))/100)%>%
  select(-(absolute:relative_165))%>%
  left_join(median_costs)%>%
  mutate(above_median = ifelse((absolute_value > median_45 & Pricelevel == "45") | (absolute_value > median_85 & Pricelevel == "85") | (absolute_value > median_125 & Pricelevel == "125"),1,0))

# Edit single columns or filter
data_1.4_FRA <- data_1.4_FRA %>%
  # Time in seconds - slowest 2% and fastest 5%
  mutate(time = as.numeric(time))%>%
  mutate(filter_1a = ifelse(time <= quantile(time, probs = 0.05, na.rm = TRUE),1,0),
         filter_1b = ifelse(time >= quantile(time, probs = 0.98, na.rm = TRUE),1,0))%>%
  # NA in every Q41_1 to Q46_1
  mutate(filter_2a = ifelse(is.na(Q41_1) & is.na(Q42_1) & is.na(Q43_1) & is.na(Q44_1) & is.na(Q45_1) & is.na(Q46_1),1,0),
  # NA in every Q41_2 to Q46_2
         filter_2b = ifelse(is.na(Q41_2) & is.na(Q42_2) & is.na(Q43_2) & is.na(Q44_2) & is.na(Q45_2) & is.na(Q46_2),1,0),
  # NA in every Q62 to Q68
         filter_2c = ifelse(is.na(Q62) & is.na(Q63) & is.na(Q64) & is.na(Q65) & is.na(Q66) & is.na(Q67) & is.na(Q68),1,0))%>%
  # NA in every C1_1, C1_2, C1_3, C1_4
  mutate(filter_3a = ifelse(is.na(C1_1) & is.na(C2_1) & is.na(C3_1) & is.na(C4_1),1,0))

rm(data_1_FRA, data_1.1_FRA, data_1.1.1_FRA, data_1.1.2_FRA, data_1.2_FRA, data_1.2.1_FRA, data_1.3_FRA, data_0_FRA, com_1_FRA, com_0_FRA)

# 1.3   Germany ####

data_1_GER <- data_0_GER %>%
  filter(Status == "IP-Adresse")%>%
  # Convert date
  mutate(StartDate = as.POSIXct(StartDate, format = "%Y-%m-%d %H:%M:%S"))%>%
  mutate(Date = format(StartDate, "%Y-%m-%d"))%>%
  filter(as.Date(Date) > as.Date("2025-12-17"))%>% # TBA
  # Create basic columns
  mutate(Country = "Germany")%>%
  mutate(ID = 1:n())%>%
  select(Country, ID, everything())%>%
  rename(time = "Duration (in seconds)")%>%
  filter(Q02 != "Nein")%>%
  filter(Q10 != "Unter 18")%>%
  filter(Finished == "Wahr")%>%
  # Attention Check 1:
  filter(Q38A == "Stimme zu")%>%
  # For now: Attention Check 2:
  filter(Q60A == "Energie")%>%
  # Order of columns
  select(Country, ID, time, Q10:Date)

# Label-file
data_1.1.1_GER <- data_1_GER %>%
  select(Country, ID, Label1:Label5)%>%
  pivot_longer(Label1:Label5, names_to = "Label", values_to = "Value", names_prefix = "Label")

# Institution-file
data_1.1.2_GER <- data_1_GER %>%
  select(Country, ID, Institution1:Institution3)%>%
  pivot_longer(Institution1:Institution3, names_to = "Institution", values_to = "Value_Institution", names_prefix = "Institution")

# Institutional trust values
data_1.1_GER <- data_1_GER %>%
  select(Country, ID, Q31_1:Q31_3)%>%
  pivot_longer(Q31_1:Q31_3, names_to = "Variable", values_to = "Value_raw")%>%
  # filter(!is.na(Value_raw))%>%
  mutate(Label = ifelse(str_detect(Value_raw, "1"),"1",
                        ifelse(str_detect(Value_raw, "2"), "2",
                               ifelse(str_detect(Value_raw, "3"), "3",
                                      ifelse(str_detect(Value_raw, "4"), "4",
                                             ifelse(str_detect(Value_raw, "5"), "5", Value_raw))))))%>%
  mutate(Institution = str_sub(Variable,-1,-1))%>%
  mutate(Variable = str_sub(Variable,1,-3))%>%
  left_join(data_1.1.1_GER)%>%
  left_join(data_1.1.2_GER)%>%
  mutate(Institution = ifelse(Value_Institution == "Die deutsche Bundesregierung", "Gov_nat",
                              ifelse(Value_Institution == "Ihre lokale Regierung", "Gov_loc",
                                     ifelse(Value_Institution == "Die EU-Kommission", "EU_Comm", Value_Institution))))%>%
  mutate(Column = paste0(Variable, "_", Institution))%>%
  select(Country, ID, Column, Value)%>%
  pivot_wider(names_from = "Column", values_from = "Value")

data_1.2_GER <- data_1_GER %>%
  select(-(Q31_1:Q31_3), -(Institution1:Label5))%>%
  left_join(data_1.1_GER)%>%
  select(Country:Q30_3, starts_with("Q31"), everything())

# Cost range
data_1.2.1_GER <- data_1.2_GER %>%
  select(Country, ID, Q42_1, Q42_2)%>%
  pivot_longer(Q42_1:Q42_2)%>%
  left_join(select(data_1.2_GER, Country, ID, t1:t6))%>%
  mutate(value = ifelse(str_detect(value, "t1"), str_replace(value, fixed("${e://Field/t1}"), t1), value),
         value = ifelse(str_detect(value, "t2"), str_replace(value, fixed("${e://Field/t2}"), t2), value),
         value = ifelse(str_detect(value, "t3"), str_replace(value, fixed("${e://Field/t3}"), t3), value),
         value = ifelse(str_detect(value, "t4"), str_replace(value, fixed("${e://Field/t4}"), t4), value),
         value = ifelse(str_detect(value, "t5"), str_replace(value, fixed("${e://Field/t5}"), t5), value),
         value = ifelse(str_detect(value, "t6"), str_replace(value, fixed("${e://Field/t6}"), t6), value))%>%
  select(Country, ID, name, value)%>%
  pivot_wider()

data_1.3_GER <- data_1.2_GER %>%
  rename("Q42_1_true" = "Q42_1", "Q42_2_true" = "Q42_2", policy = "Policy description")%>%
  left_join(data_1.2.1_GER)%>%
  select(Country:Q42_1_true, Q42_1, Q43_1:Q42_2_true, Q42_2, everything())%>%
  # Remove some columns
  select(-(FairnessPerception:PolicySupport))%>%
  # mutate_at(vars(Effectiveness_1, Effectiveness_2, Expected_cost_1_true, Expected_cost_2_true), ~ ifelse(is.na(.), "No lo sé",.))%>%
  # Introducing factors
  mutate(Q41_1 = factor(Q41_1, levels = c("Auf keinen Fall", "Vermutlich nicht", "Vermutlich", "Auf jeden Fall")),
         Q41_2 = factor(Q41_2, levels = c("Auf keinen Fall", "Vermutlich nicht", "Vermutlich", "Auf jeden Fall")),
         Q42_1_true = factor(Q42_1_true, levels = c("um weniger als ${e://Field/t1}€", "um ${e://Field/t1}€ bis ${e://Field/t2}€", "um ${e://Field/t2}€ bis ${e://Field/t3}€", "um ${e://Field/t3}€ bis ${e://Field/t4}€", "um ${e://Field/t4}€ bis ${e://Field/t5}€", "um ${e://Field/t5}€ bis ${e://Field/t6}€", "mehr als ${e://Field/t6}€")),
         Q42_2_true = factor(Q42_2_true, levels = c("um weniger als ${e://Field/t1}€", "um ${e://Field/t1}€ bis ${e://Field/t2}€", "um ${e://Field/t2}€ bis ${e://Field/t3}€", "um ${e://Field/t3}€ bis ${e://Field/t4}€", "um ${e://Field/t4}€ bis ${e://Field/t5}€", "um ${e://Field/t5}€ bis ${e://Field/t6}€", "um mehr als ${e://Field/t6}€")),
         Q43_1 = factor(Q43_1, levels = c("Viel höher als bei einem durchschnittlichen Haushalt","Etwas höher als bei einem durchschnittlichen Haushalt", "Ungefähr so hoch wie bei einem durchschnittlichen Haushalt", "Etwas niedriger als bei einem durchschnittlichen Haushalt", "Viel niedriger als bei einem durchschnittlichen Haushalt")),
         Q43_2 = factor(Q43_2, levels = c("Viel höher als bei einem typischen Haushalt","Etwas höher als bei einem typischen Haushalt", "Ungefähr so hoch wie bei einem typischen Haushalt", "Etwas niedriger als bei einem typischen Haushalt", "Viel niedriger als bei einem typischen Haushalt")),
         Q44_1 = factor(Q44_1, levels = c("Sie wird weder eher schaden", "Sie wird weder schaden noch helfen", "Sie wird eher helfen")),
         Q44_2 = factor(Q44_2, levels = c("Sie wird eher schaden", "Sie wird weder schaden noch helfen", "Sie wird eher helfen")),
         Q45_1 = factor(Q45_1, levels = c("Ich finde sie ungerecht", "Ich finde sie weder gerecht noch ungerecht", "Ich finde sie gerecht")),
         Q45_2 = factor(Q45_2, levels = c("Ich finde sie ungerecht", "Ich finde sie weder gerecht noch ungerecht", "Ich finde sie gerecht")),
         Q46_1 = factor(Q46_1, levels = c("Ich lehne sie entschieden ab", "Ich bin eher dagegen", "Ich bin weder dafür noch dagegen", "Ich befürworte sie in gewissem Maße", "Ich befürworte sie entschieden")),
         Q46_2 = factor(Q46_2, levels = c("Ich lehne sie entschieden ab", "Ich bin eher dagegen", "Ich bin weder dafür noch dagegen", "Ich befürworte sie in gewissem Maße", "Ich befürworte sie entschieden")),
         Q30_1 = factor(Q30_1, levels = c("Überhaupt nicht", "Ein wenig", "Einigermaßen", "Weitgehend", "Vollständig")),
         Q30_2 = factor(Q30_2, levels = c("Überhaupt nicht", "Ein wenig", "Einigermaßen", "Weitgehend", "Vollständig")),
         Q30_3 = factor(Q30_3, levels = c("Überhaupt nicht", "Ein wenig", "Einigermaßen", "Weitgehend", "Vollständig")))%>%
  mutate(PedTreatmentGrp = ifelse(PedTreatmentGrp == "Treat", "Treatment", PedTreatmentGrp))%>%
  mutate(Treatment_A = factor(policy, levels = c("EU", "nonEU")),
         Treatment_B = factor(PedTreatmentGrp,  levels = c("Control", "Treatment")),
         Treatment_C = factor(CostTreatmentGrp, labels = c("C1", "C2", "C3", "C4", "Control")))%>%
  select(-PedTreatmentGrp, -CostTreatmentGrp,-policy)%>%
  #select(-all_of(starts_with("QT")), all_of(starts_with("QT")))%>%
  arrange(ID)

# Join cost estimates 

com_1_GER <- com_0_GER %>%
  rename(Q20 = "heating_fuel", Q21 = "renting", Q27 = "urban_type", Q22 = "building_type", Q26 = "bundesland", Q23 = "building_year",
         Q28 = "hh_expenditures", Q25 = "number_of_cars", Q24 = "space")%>%
  mutate_at(vars(Q20:Q24), ~ as.character(.))

data_1.4_GER <- data_1.3_GER %>%
  left_join(com_1_GER)%>%
  mutate(absolute_value = ifelse(Pricelevel == "45", as.character(absolute_45),
                                 ifelse(Pricelevel == "85", as.character(absolute_85),
                                        ifelse(Pricelevel == "125", as.character(absolute_125), NA))),
         relative_value = ifelse(Pricelevel == "45", as.character(relative_45),
                                 ifelse(Pricelevel == "85", as.character(relative_85),
                                        ifelse(Pricelevel == "125", as.character(relative_125), NA))))%>%
  mutate(absolute_value = as.numeric(str_replace_all(absolute_value, "[€,]", "")),
         relative_value = as.numeric(str_replace_all(relative_value, "%",""))/100)%>%
  select(-(absolute:relative_165))%>%
  left_join(median_costs)%>%
  mutate(above_median = ifelse((absolute_value > median_45 & Pricelevel == "45") | (absolute_value > median_85 & Pricelevel == "85") | (absolute_value > median_125 & Pricelevel == "125"),1,0))

# Edit or filter single columns
data_1.4_GER <- data_1.4_GER %>%
  # Time in seconds - slowest 2% and fastest 5%
  mutate(time = as.numeric(time))%>%
  mutate(filter_1a = ifelse(time <= quantile(time, probs = 0.05, na.rm = TRUE),1,0),
         filter_1b = ifelse(time >= quantile(time, probs = 0.98, na.rm = TRUE),1,0))%>%
  # NA in every Q41_1 to Q46_1
  mutate(filter_2a = ifelse(is.na(Q41_1) & is.na(Q42_1) & is.na(Q43_1) & is.na(Q44_1) & is.na(Q45_1) & is.na(Q46_1),1,0),
         # NA in every Q41_2 to Q46_2
         filter_2b = ifelse(is.na(Q41_2) & is.na(Q42_2) & is.na(Q43_2) & is.na(Q44_2) & is.na(Q45_2) & is.na(Q46_2),1,0),
         # NA in every Q62 to Q68
         filter_2c = ifelse(is.na(Q62) & is.na(Q63) & is.na(Q64) & is.na(Q65) & is.na(Q66) & is.na(Q67) & is.na(Q68),1,0))%>%
  # NA in every C1_1, C1_2, C1_3, C1_4
  mutate(filter_3a = ifelse(is.na(C1_1) & is.na(C2_1) & is.na(C3_1) & is.na(C4_1),1,0))

rm(data_1_GER, data_1.1_GER, data_1.1.1_GER, data_1.1.2_GER, data_1.2_GER, data_1.2.1_GER, data_1.3_GER, data_0_GER, com_1_GER, com_0_GER)

# 1.4   Romania ####

data_1_ROM <- data_0_ROM %>%
  filter(Status == "IP-Adresse")%>%
  # Convert date
  mutate(StartDate = as.POSIXct(StartDate, format = "%Y-%m-%d %H:%M:%S"))%>%
  mutate(Date = format(StartDate, "%Y-%m-%d"))%>%
  filter(as.Date(Date) > as.Date("2025-12-17"))%>%
  # Create basic columns
  mutate(Country = "Romania")%>%
  mutate(ID = 1:n())%>%
  select(Country, ID, everything())%>%
  rename(time = "Duration (in seconds)")%>%
  filter(Q02 != "Nu")%>%
  filter(Q10 != "Sub 18")%>%
  filter(Finished == "Wahr")%>%
  # Attention Check 1 %>%
  filter(Q38A == "De acord")%>%
  # For now
  filter(Q60A == "Energie")%>%
  # Order of columns
  select(Country, ID, time, Q10:Date)

# Label-file
data_1.1.1_ROM <- data_1_ROM %>%
  select(Country, ID, Label1:Label5)%>%
  pivot_longer(Label1:Label5, names_to = "Label", values_to = "Value", names_prefix = "Label")

# Institution-file
data_1.1.2_ROM <- data_1_ROM %>%
  select(Country, ID, Institution1:Institution3)%>%
  pivot_longer(Institution1:Institution3, names_to = "Institution", values_to = "Value_Institution", names_prefix = "Institution")

# Institutional trust values
data_1.1_ROM <- data_1_ROM %>%
  select(Country, ID, Q31_1:Q31_3)%>%
  pivot_longer(Q31_1:Q31_3, names_to = "Variable", values_to = "Value_raw")%>%
  # filter(!is.na(Value_raw))%>%
  # Decision: NAs == 5 "I don't know"
  mutate(Label = ifelse(str_detect(Value_raw, "1"),"1",
                        ifelse(str_detect(Value_raw, "2"), "2",
                               ifelse(str_detect(Value_raw, "3"), "3",
                                      ifelse(str_detect(Value_raw, "4"), "4",
                                             ifelse(str_detect(Value_raw, "5"), "5", Value_raw))))))%>%
  mutate(Institution = str_sub(Variable,-1,-1))%>%
  mutate(Variable = str_sub(Variable,1,-3))%>%
  left_join(data_1.1.1_ROM)%>%
  left_join(data_1.1.2_ROM)%>%
  mutate(Institution = ifelse(Value_Institution == "Guvernul Romaniei", "Gov_nat",
                              ifelse(grepl("Administr", Value_Institution), "Gov_loc",
                                     ifelse(grepl("Comisia", Value_Institution), "EU_Comm", Value_Institution))))%>%
  mutate(Column = paste0(Variable, "_", Institution))%>%
  select(Country, ID, Column, Value)%>%
  pivot_wider(names_from = "Column", values_from = "Value")

data_1.2_ROM <- data_1_ROM %>%
  select(-(Q31_1:Q31_3), -(Institution1:Label5))%>%
  left_join(data_1.1_ROM)%>%
  select(Country:Q30_3, starts_with("Q31"), everything())

# Cost range
data_1.2.1_ROM <- data_1.2_ROM %>%
  select(Country, ID, Q42_1, Q42_2)%>%
  pivot_longer(Q42_1:Q42_2)%>%
  left_join(select(data_1.2_ROM, Country, ID, t1:t6))%>%
  mutate(value = ifelse(str_detect(value, "t1"), str_replace(value, fixed("${e://Field/t1}"), t1), value),
         value = ifelse(str_detect(value, "t2"), str_replace(value, fixed("${e://Field/t2}"), t2), value),
         value = ifelse(str_detect(value, "t3"), str_replace(value, fixed("${e://Field/t3}"), t3), value),
         value = ifelse(str_detect(value, "t4"), str_replace(value, fixed("${e://Field/t4}"), t4), value),
         value = ifelse(str_detect(value, "t5"), str_replace(value, fixed("${e://Field/t5}"), t5), value),
         value = ifelse(str_detect(value, "t6"), str_replace(value, fixed("${e://Field/t6}"), t6), value))%>%
  select(Country, ID, name, value)%>%
  pivot_wider()

data_1.3_ROM <- data_1.2_ROM %>%
  rename("Q42_1_true" = "Q42_1", "Q42_2_true" = "Q42_2", policy = "Policy description")%>%
  left_join(data_1.2.1_ROM)%>%
  select(Country:Q42_1_true, Q42_1, Q43_1:Q42_2_true, Q42_2, everything())%>%
  # Remove some columns
  select(-(FairnessPerception:PolicySupport))%>%
  mutate_at(vars(Q41_1:Q46_1, Q41_2:Q46_2, Q30_1, Q30_2, Q30_3), ~ stri_trans_general(., "Latin-ASCII"))%>%
  # mutate_at(vars(Effectiveness_1, Effectiveness_2, Expected_cost_1_true, Expected_cost_2_true), ~ ifelse(is.na(.), "No lo sé",.))%>%
  # Introducing factors
  mutate(Q41_1      = factor(Q41_1,      levels = c("In mod sigur nu va fi eficienta", "Probabil nu va fi eficienta", "Probabil va fi eficienta", "In mod sigur va fi eficienta")),
         Q41_2      = factor(Q41_2,      levels = c("In mod sigur nu va fi eficienta", "Probabil nu va fi eficienta", "Probabil va fi eficienta", "In mod sigur va fi eficienta")),
         Q42_1_true = factor(Q42_1_true, levels = c("Mai putin de ${e://Field/t1} de lei", "Intre ${e://Field/t1} de lei si ${e://Field/t2} de lei", "Intre ${e://Field/t2} de lei si ${e://Field/t3} de lei", "Intre ${e://Field/t3} de lei si ${e://Field/t4} de lei", "Intre ${e://Field/t4} de lei si ${e://Field/t5} de lei", "Intre ${e://Field/t5} de lei si ${e://Field/t6} de lei", "Mai mult de ${e://Field/t6} de lei")),
         Q42_2_true = factor(Q42_2_true, levels = c("Mai putin de ${e://Field/t1} de lei", "Intre ${e://Field/t1} de lei si ${e://Field/t2} de lei", "Intre ${e://Field/t2} de lei si ${e://Field/t3} de lei", "Intre ${e://Field/t3} de lei si ${e://Field/t4} de lei", "Intre ${e://Field/t4} de lei si ${e://Field/t5} de lei", "Intre ${e://Field/t5} de lei si ${e://Field/t6} de lei", "Mai mult de ${e://Field/t6} de lei")),
         Q43_1      = factor(Q43_1,      levels = c("Mult mai mult decat pe o gospodarie obisnuita","Putin mai mult decat pe o gospodarie obisnuita", "Cam la fel ca pe o gospodarie obisnuita", "Putin mai putin decat pe o gospodarie obisnuita","Mult mai putin decat pe o gospodarie obisnuita")),
         Q43_2      = factor(Q43_2,      levels = c("Mult mai mult decat pe o gospodarie obisnuita","Putin mai mult decat pe o gospodarie obisnuita", "Cam la fel ca pe o gospodarie obisnuita", "Putin mai putin decat pe o gospodarie obisnuita","Mult mai putin decat pe o gospodarie obisnuita")),
         Q44_1      = factor(Q44_1,      levels = c("Va afecta", "Nici nu va afecta, nici nu va ajuta", "Va ajuta")),
         Q44_2      = factor(Q44_2,      levels = c("Va afecta", "Nici nu va afecta, nici nu va ajuta", "Va ajuta")),
         Q45_1      = factor(Q45_1,      levels = c("Incorecta", "Nici corecta nici incorecta", "Corecta")),
         Q45_2      = factor(Q45_2,      levels = c("Incorecta", "Nici corecta nici incorecta", "Corecta")),
         Q46_1      = factor(Q46_1,      levels = c("Ma opun cu tarie", "Ma opun oarecum", "Nici nu o sustin, nici nu ma opun", "O sustin oarecum", "O sustin cu tarie")),
         Q46_2      = factor(Q46_2,      levels = c("Ma opun cu tarie", "Ma opun oarecum", "Nici nu o sustin, nici nu ma opun", "O sustin oarecum", "O sustin cu tarie")),
         Q30_1      = factor(Q30_1,      levels = c("Deloc", "Putin", "Oarecum", "Mult", "Complet")),
         Q30_2      = factor(Q30_2,      levels = c("Deloc", "Putin", "Oarecum", "Mult", "Complet")),
         Q30_3      = factor(Q30_3,      levels = c("Deloc", "Putin", "Oarecum", "Mult", "Complet")))%>%
  mutate(PedTreatmentGrp = ifelse(PedTreatmentGrp == "Treat", "Treatment", PedTreatmentGrp))%>%
  mutate(Treatment_A = factor(policy, levels = c("EU", "nonEU")),
         Treatment_B = factor(PedTreatmentGrp,  levels = c("Control", "Treatment")),
         Treatment_C = factor(CostTreatmentGrp, labels = c("C1", "C2", "C3", "C4", "Control")))%>%
  select(-PedTreatmentGrp, -CostTreatmentGrp,-policy)%>%
  arrange(ID)

# Join cost estimates 

com_1_ROM <- com_0_ROM %>%
  rename(Q20  = "heating_fuel", 
         Q29B = "cooking_fuel",
         Q13  = "occupation",
         Q22  = "housing_type", 
         Q26  = "province", 
         Q28  = "hh_expenditures", 
         Q25  = "number_of_cars", 
         Q24  = "space")%>%
  mutate_at(vars(Q20:Q24), ~ as.character(.))%>%
  mutate_at(vars(Q20:Q24,Q13), ~ stri_trans_general(., "Latin-ASCII"))%>%
  mutate(Q28 = case_when(Q28 == "Intre 1,301 de lei si 1,800 de lei"  ~ "intre 1,301 de lei si 1,800 de lei",
                         Q28 == "Intre 1,801 de lei si 2,500 de lei"  ~ "intre 1,801 de lei si 2,500 de lei",
                         Q28 == "Intre 2,501 de lei si 3,300 de lei"  ~ "intre 2,501 de lei si 3,300 de lei",
                         Q28 == "Intre 3,301 de lei si 4,200 de lei"  ~ "intre 3,301 de lei si 4,200 de lei",
                         Q28 == "Intre 4,201 de lei si 5,200 de lei"  ~ "intre 4,201 de lei si 5,200 de lei",
                         Q28 == "Intre 5,201 de lei si 6,600 de lei"  ~ "intre 5,201 de lei si 6,600 de lei",
                         Q28 == "Intre 6,601 de lei si 8,200 de lei"  ~ "intre 6,601 de lei si 8,200 de lei",
                         Q28 == "Intre 8,201 de lei si 10,300 de lei" ~ "intre 8,201 de lei si 10,300 de lei",
                         Q28 == "Mai putin de 1,300 de lei"           ~ "mai putin de 1,300 de lei",
                         Q28 == "Nu stiu asta"                        ~ "Nu stiu asta",
                         Q28 == "Peste 10,301 de lei"                 ~ "peste 10,301 de lei"))%>%
  mutate(Q25 = case_when(Q25 == "Am doua masini."               ~ "Am doua masini",
                         Q25 == "Am o masina."                  ~ "Am o masina",
                         Q25 == "Am trei sau mai multe masini." ~ "Am trei sau mai multe masini",
                         Q25 == "Nu am o masina."               ~ "Nu am o masina"))%>%
  mutate(Q24 = ifelse(Q24 == "Peste 67 de m2", "peste 67 de m2", Q24),
         Q22 = ifelse(Q22 == "Casa unifamlilala", "Casa unifamiliala", Q22))

data_1.4_ROM <- data_1.3_ROM %>%
  mutate_at(vars(Q20, Q13, Q29B, Q22, Q26, Q28, Q25, Q24), ~ stri_trans_general(., "Latin-ASCII"))%>%
  left_join(com_1_ROM)%>%
  mutate(absolute_value = ifelse(Priceleveleuro == "45", as.character(absolute_45),
                                 ifelse(Priceleveleuro == "85", as.character(absolute_85),
                                        ifelse(Priceleveleuro == "125", as.character(absolute_125), NA))),
         relative_value = ifelse(Priceleveleuro == "45", as.character(relative_45),
                                 ifelse(Priceleveleuro == "85", as.character(relative_85),
                                        ifelse(Priceleveleuro == "125", as.character(relative_125), NA))))%>%
  mutate(absolute_value = as.numeric(str_replace_all(absolute_value, "[de lei,]", "")),
         relative_value = as.numeric(str_replace_all(relative_value, "%",""))/100)%>%
  select(-(absolute:relative_165))%>%
  left_join(median_costs)%>%
  mutate(above_median = ifelse((absolute_value > median_45 & Priceleveleuro == "45") | (absolute_value > median_85 & Priceleveleuro == "85") | (absolute_value > median_125 & Priceleveleuro == "125"),1,0))

data_1.4_ROM <- data_1.4_ROM %>%
  # Time in seconds - slowest 2% and fastest 5%
  mutate(time = as.numeric(time))%>%
  mutate(filter_1a = ifelse(time <= quantile(time, probs = 0.05, na.rm = TRUE),1,0),
         filter_1b = ifelse(time >= quantile(time, probs = 0.98, na.rm = TRUE),1,0))%>%
  # NA in every Q41_1 to Q46_1
  mutate(filter_2a = ifelse(is.na(Q41_1) & is.na(Q42_1) & is.na(Q43_1) & is.na(Q44_1) & is.na(Q45_1) & is.na(Q46_1),1,0),
         # NA in every Q41_2 to Q46_2
         filter_2b = ifelse(is.na(Q41_2) & is.na(Q42_2) & is.na(Q43_2) & is.na(Q44_2) & is.na(Q45_2) & is.na(Q46_2),1,0),
         # NA in every Q62 to Q68
         filter_2c = ifelse(is.na(Q62) & is.na(Q63) & is.na(Q64) & is.na(Q65) & is.na(Q66) & is.na(Q67) & is.na(Q68),1,0))%>%
  # NA in every C1_1, C1_2, C1_3, C1_4
  mutate(filter_3a = ifelse(is.na(C1_1) & is.na(C2_1) & is.na(C3_1) & is.na(C4_1),1,0))

rm(data_1_ROM, data_1.1_ROM, data_1.1.1_ROM, data_1.1.2_ROM, data_1.2_ROM, data_1.2.1_ROM, data_1.3_ROM, data_0_ROM, com_1_ROM, com_0_ROM)

# 1.5   Editing the conjoint experiment data ####

extract_conjoint <- function(data_1.4.0){
  data_1.5.1 <- data_1.4.0 %>%
    select(ID, starts_with("profile"))%>%
    drop_na()%>%
    pivot_longer(starts_with("profile"), names_to = "names", values_to = "values")%>%
    mutate(Profile = case_when(grepl("profileA", names) ~ "A",
                               grepl("profileB", names) ~ "B"),
           Task    = case_when(grepl("task0", names) ~ "0",
                               grepl("task1", names) ~ "1",
                               grepl("task2", names) ~ "2",
                               grepl("task3", names) ~ "3"))%>%
    select(-names)%>%
    filter(!is.na(values))%>%
    mutate(parsed = map(values, \(x) {
      x <- gsub('^"|"$', '', x)                                  # remove outer quotes
      parts <- strsplit(x, "\\}\\s*\\{")[[1]]                    # split multiple JSONs
      parts <- paste0("{", gsub("(^\\{|\\}$)", "", parts), "}")  # fix braces
      jsons <- map(parts, safely(fromJSON))                      # safely parse each
      bind_rows(map(jsons, "result"))                            # combine parsed pieces
    })) %>%
    unnest(parsed)%>%
    mutate_at(vars(-c(ID:Task)), ~ ifelse(is.na(.), "Missing", .))%>%
    arrange(ID, Task, Profile)%>%
    select(-values)%>%
    select(ID, Task, Profile, everything())
  
  data_1.5.1_b <- expand_grid("ID" = unique(data_1.5.1$ID), Task = c("0","1","2","3"))%>%
    mutate(Profile = "C")
  
  data_1.5.1 <- data_1.5.1 %>%
    bind_rows(data_1.5.1_b)%>%
    arrange(ID, Task, Profile)%>%
    mutate_all(., ~ ifelse(is.na(.), "Repeal",.))
  
  data_1.5.2 <- data_1.4.0 %>%
    select(ID, starts_with("C1_1"), starts_with("C2_1"), starts_with("C3_1"), starts_with("C4_1"))%>%
    pivot_longer(-ID, names_to = "names", values_to = "Choice")%>%
    mutate(Task = case_when(grepl("C1_1", names) ~ "0",
                            grepl("C2_1", names) ~ "1",
                            grepl("C3_1", names) ~ "2",
                            grepl("C4_1", names) ~ "3"))%>%
    # mutate(Profile = case_when(grepl("_1", names) ~ "A",
    #                            grepl("_2", names) ~ "B",
    #                            grepl("_3", names) ~ "C"))%>%
    select(ID, Task, Choice, -names)
  
  data_1.5.3 <- data_1.4.0 %>%
    select(ID, Q62:Q68, filter_3a)
  # TBD

  data_1.5.4 <- data_1.4.0 %>%
    select(ID, starts_with("C1_2"), starts_with("C2_2"), starts_with("C3_2"), starts_with("C4_2"))%>%
    pivot_longer(-ID, names_to = "names", values_to = "values")%>%
    mutate(Task = case_when(grepl("C1", names) ~ "0",
                            grepl("C2", names) ~ "1",
                            grepl("C3", names) ~ "2",
                            grepl("C4", names) ~ "3"))%>%
    mutate(Choice_2 = case_when(grepl("enario A", values) ~ "A",
                                grepl("enario B", values) ~ "B",
                                grepl("enario C", values) ~ "C"))%>%
    mutate(Choice_3 = case_when(grepl("enario A", values) & grepl("_2B", names) ~ "C",
                                grepl("enario B", values) & grepl("_2C", names) ~ "A",
                                grepl("enario C", values) & grepl("_2A", names) ~ "B",
                                grepl("enario A", values) & grepl("_2C", names) ~ "B",
                                grepl("enario B", values) & grepl("_2A", names) ~ "C",
                                grepl("enario C", values) & grepl("_2B", names) ~ "A"))%>%
    filter(!is.na(values))%>%
    select(ID, Task, Choice_2, Choice_3)
  
  data_1.5 <- left_join(data_1.5.2, data_1.5.1)%>%
    left_join(data_1.5.4)%>%
    filter(!is.na(Profile))%>%
    select(ID, Task, Profile, budget_and_funding, budget_control, household_support, information, infrastructure_ownership, worker_support, community_mobility_support, everything())%>%
    mutate_at(vars(budget_and_funding:community_mobility_support), ~ ifelse(. == "Missing", NA,.))%>%
    mutate(Preferred        = ifelse(Profile == Choice,  1,0),
           Preferred_Second = ifelse(Profile == Choice_2,1,0),
           Preferred_Least  = ifelse(Profile == Choice_3,1,0))%>%
    left_join(data_1.5.3)%>%
    filter(filter_3a == 0)
  
  return(data_1.5)
}

data_conjoint_ESP <- extract_conjoint(data_1.4_ESP)%>%
  mutate_at(vars(budget_and_funding:community_mobility_support), ~ str_trim(.))%>%
  mutate(budget_and_funding = factor(budget_and_funding, levels = c("Los ingresos de la política de tarificación del carbono, pero sin presupuesto adicional",
                                                                    "Un presupuesto mayor (para apoyar a las personas de una forma más amplia) financiado mediante endeudamiento",
                                                                    "Un presupuesto mayor (para apoyar a las personas de una forma más amplia) financiado por un impuesto sobre la riqueza al 10% de la población más rica",
                                                                    "Repeal")),
         budget_control = factor(budget_control, levels = c("El gobierno, como ocurre con cualquier otro ingreso público",
                                                            "Un fondo protegido que garantice que el gobierno invierta en la transición durante las próximas dos décadas",
                                                            "Un fondo protegido que garantice que el gobierno invierta en la transición durante las próximas dos décadas, con ciudadanos en el consejo para decidir cómo se utiliza el dinero",
                                                            "Repeal")),
         household_support = factor(household_support, levels = c("El 15% de los costes adicionales",
                                                                  "El 50% de los costes adicionales",
                                                                  "El 90% de los costes adicionales",
                                                                  "Repeal")),
         information = factor(information, levels = c("A través de las páginas web gubernamentales",
                                                      "A través de un centro climático local, donde un asesor pueda orientarle",
                                                      "Repeal")),
         infrastructure_ownership = factor(infrastructure_ownership, levels = c("Cualquier empresa dispuesta a invertir",
                                                                                "Preferentemente proyectos energéticos en los que participen residentes locales como copropietarios",
                                                                                "Empresas públicas que reinvierten sus beneficios en la transición",
                                                                                "Repeal")),
         worker_support = factor(worker_support, levels = c("La asistencia social existente (seguro de desempleo y servicios de formación y empleo)",
                                                            "Curso de formación totalmente financiado durante un máximo de 1 año",
                                                            "Curso de formación totalmente financiado durante un máximo de 1 año y un salario garantizado durante 3 años",
                                                            "Repeal")),
         community_mobility_support = factor(community_mobility_support, levels = c("Mantener la calidad del transporte público existente",
                                                                                    "Mayor inversión en transporte público limpio, frecuente y asequible en todas las comunidades",
                                                                                    "Mayor inversión en transporte público limpio, frecuente y asequible en todas las comunidades, así como en trenes rápidos interurbanos",
                                                                                    "Repeal")))

data_conjoint_FRA <- extract_conjoint(data_1.4_FRA)%>%
  mutate_at(vars(budget_and_funding:community_mobility_support), ~ str_trim(.))%>%
  mutate(household_support = str_replace_all(household_support, "\u00a0", " "))%>%
  mutate(budget_and_funding = factor(budget_and_funding, levels = c("Les recettes de la politique de tarification du carbone, mais sans budget supplémentaire",
                                                                    "Un budget plus important (pour aider un plus grand nombre de personnes) financé par l’emprunt",
                                                                    "Un budget plus important (pour aider un plus grand nombre de personnes) financé par un impôt sur la fortune des 10 % les plus riches",
                                                                    "Repeal")),
         budget_control = factor(budget_control, levels = c("Le gouvernement, comme pour toute autre recette publique",
                                                            "Un fonds protégé qui garantit que le gouvernement consacrera les dépenses à la transition au cours des deux prochaines décennies",
                                                            "Un fonds protégé qui garantit que le gouvernement consacrera les dépenses à la transition au cours des deux prochaines décennies et dont la gestion sera supervisée par un conseil composé de citoyens",
                                                            "Repeal")),
         household_support = factor(household_support, levels = c("15 % des coûts supplémentaires",
                                                                  "50 % des coûts supplémentaires",
                                                                  "90 % des coûts supplémentaires",
                                                                  "Repeal")),
         information = factor(information, levels = c("Sur les sites web du gouvernement",
                                                      "Dans un centre d'information local, où un conseiller peut vous guider",
                                                      "Repeal")),
         infrastructure_ownership = factor(infrastructure_ownership, levels = c("Toute entreprise disposée à investir",
                                                                                "Des projets à propriété partagée avec les habitants du territoire",
                                                                                "Les entreprises publiques réinvestissant leurs bénéfices dans la transition",
                                                                                "Repeal")),
         worker_support = factor(worker_support, levels = c("Aide sociale existante (assurance chômage et services de formation et d’emploi)",
                                                            "Formation entièrement financée pendant un an maximum",
                                                            "Formation entièrement financée pendant un an maximum et salaire garanti pendant trois ans",
                                                            "Repeal")),
         community_mobility_support = factor(community_mobility_support, levels = c("Maintien de la qualité des transports publics existants",
                                                                                    "Augmentation des investissements dans des transports publics propres, fréquents et abordables dans chaque communauté",
                                                                                    "Augmentation des investissements dans des transports publics propres, fréquents et abordables dans chaque communauté et dans des trains interurbains rapides",
                                                                                    "Repeal")))

data_conjoint_GER <- extract_conjoint(data_1.4_GER)%>%
  mutate(budget_control           = str_replace(budget_control, fixed("Ein geschützter Fonds, der garantiert"), "Ein Fond, der garantiert"),
         infrastructure_ownership = str_replace(infrastructure_ownership, fixed("in den Übergang"), "in der Transformation"))%>%
  mutate_at(vars(budget_and_funding:community_mobility_support), ~ str_trim(.))%>%
  mutate(budget_and_funding = factor(budget_and_funding,                  levels = c("Die Einnahmen aus der CO2-Bepreisung, aber kein zusätzliches Budget",
                                                                                     "Ein größeres Budget (um mehr Menschen zu unterstützen), das durch Kreditaufnahme finanziert wird",
                                                                                     "Ein größeres Budget (um mehr Menschen zu unterstützen), das durch eine  Steuer auf das Vermögen der reichsten 10 % bezahlt wird",
                                                                                     "Repeal")),
         budget_control = factor(budget_control,                          levels = c("Die Regierung, wie bei allen anderen staatlichen Einnahmen auch",
                                                                                     "Ein Fond, der garantiert, dass die Regierung die Mittel in den nächsten zwei Jahrzehnten für die Transformation ausgibt",
                                                                                     "Ein Fond, der garantiert, dass die Regierung die Mittel in den nächsten zwei Jahrzehnten Geld für die Transformation ausgibt, wobei die Bürgerinnen und Bürger über die Verwendung des Geldes mitentscheiden.",
                                                                                     "Repeal")),
         household_support = factor(household_support,                    levels = c("15 % der Mehrkosten", "50 % der Mehrkosten", "90 % der Mehrkosten", "Repeal")),
         information = factor(information,                                levels = c("Von Websites der Regierung",
                                                                                     "Von einem lokalen Informationzentrum, in dem Sie sich beraten lassen können",
                                                                                     "Repeal")),
         infrastructure_ownership = factor(infrastructure_ownership,      levels = c("Jedes Unternehmen, das bereit ist zu investieren",
                                                                                     "Vorzugsweise Energieprojekte im Miteigentum der ortsansässigen Bevölkerung",
                                                                                     "Staatseigene Unternehmen, die ihre Gewinne in der Transformation reinvestieren",
                                                                                     "Repeal")),
         worker_support = factor(worker_support,                          levels = c("Bestehende Sozialleistungen (Arbeitslosenversicherung, Schulungen und Arbeitsvermittlung)",
                                                                                     "Vollständig finanzierte Schulung für bis zu einem Jahr",
                                                                                     "Vollständig finanzierte Schulung für bis zu einem Jahr und eine Lohngarantie für 3 Jahre",
                                                                                     "Repeal")),
         community_mobility_support = factor(community_mobility_support,  levels = c("Beibehaltung der Qualität der bestehenden öffentlichen Verkehrsmittel",
                                                                                     "Verstärkte Investitionen in einen nachhaltigen, häufig verkehrenden und erschwinglichen ÖPNV in  jeder Gemeinde",
                                                                                     "Verstärkte Investitionen in einen nachhaltigen, häufig verkehrenden und erschwinglichen ÖPNV in  jeder Gemeinde und in schnelle Intercity-Zugverbindungen",
                                                                                     "Repeal")))
data_conjoint_ROM <- extract_conjoint(data_1.4_ROM)%>%
  mutate_at(vars(budget_and_funding:community_mobility_support), ~ str_trim(.))%>%
  mutate_at(vars(budget_and_funding:community_mobility_support), ~ stri_trans_general(., "Latin-ASCII"))%>%
  mutate(budget_and_funding = factor(budget_and_funding, levels = c("Veniturile generate de politica de stabilire a pretului carbonului, fara niciun buget suplimentar",
                                                                    "Un buget mai mare (pentru a sprijini mai multe persoane), finantat prin imprumuturi",
                                                                    "Un buget mai mare (pentru a sprijini mai multe persoane), finantat printr-un impozit pe averea celor mai bogati 10% din populatie",
                                                                    "Repeal")),
         budget_control = factor(budget_control, levels = c("Guvernul, ca pe orice alte venituri publice",
                                                            "Un fond protejat, care garanteaza ca guvernul va cheltui banii pentru tranzitie in urmatoarele doua decenii",
                                                            "Un fond protejat, care garanteaza ca guvernul va cheltui banii pentru tranzitie in urmatoarele doua decenii, si care va avea in consiliul de conducere cetateni care vor decide modul in care vor fi utilizati acesti bani",
                                                            "Repeal")),
         household_support = factor(household_support, levels = c("15% din costurile suplimentare",
                                                                  "50% din costurile suplimentare",
                                                                  "90% din costurile suplimentare",
                                                                  "Repeal")),
         information = factor(information, levels = c("De pe site-urile guvernamentale",
                                                      "Un centru local de informare, unde un consilier va poate indruma",
                                                      "Repeal")),
         infrastructure_ownership = factor(infrastructure_ownership, levels = c("Orice companie dispusa sa investeasca",
                                                                                "Proiect energetic preferential, detinut in comun de catre locuitorii din zona",
                                                                                "Companiile de stat care reinvestesc profiturile in tranzitie",
                                                                                "Repeal")),
         worker_support = factor(worker_support, levels = c("Asistenta sociala existenta (asigurarea de somaj si servicii de formare profesionala si ocupare a fortei de munca)",
                                                            "Curs de formare profesionala finantat integral, cu durata de pana la 1 an",
                                                            "Curs de formare profesionala finantat integral, cu durata de pana la 1 an, si salariu garantat pe o perioada de 3 ani",
                                                            "Repeal")),
         community_mobility_support = factor(community_mobility_support, levels = c("Mentinerea calitatii transportului public existent",
                                                                                    "Cresterea investitiilor in transportul public curat, frecvent si accesibil in fiecare comunitate",
                                                                                    "Cresterea investitiilor in transportul public curat, frecvent si accesibil in fiecare comunitate si in transportul feroviar interurban rapid",
                                                                                    "Repeal")))

data_1.5_ESP <- data_1.4_ESP %>%
  select(-starts_with("profile"))
data_1.5_FRA <- data_1.4_FRA %>%
  select(-starts_with("profile"), - c(Q62:`QT11_Click Count`), -d1, -d2)%>%
  select(-all_of(starts_with("QT")), -all_of(starts_with("Q139")), all_of(starts_with("Q139")), all_of(starts_with("QT")))
data_1.5_GER <- data_1.4_GER %>%
  select(-starts_with("profile"), - c(Q62:`QT11_Click Count`), -d1, -d2)%>%
  select(-all_of(starts_with("QT")), all_of(starts_with("QT")))
data_1.5_ROM <- data_1.4_ROM %>%
  select(-starts_with("profile"))

rm(data_1.4_ESP, data_1.4_FRA, data_1.4_GER, data_1.4_ROM, median_costs, extract_conjoint)

# 1.6   Create outcomes ####

create_outcomes <- function(data_1.5_0){
  data_1.6.0 <- data_1.5_0 %>%
    mutate(Q41_1N = as.numeric(Q41_1),
           Q41_2N = as.numeric(Q41_2),
           Q42_1N = as.numeric(Q42_1_true),
           Q42_2N = as.numeric(Q42_2_true),
           Q43_1N = as.numeric(Q43_1),
           Q43_2N = as.numeric(Q43_2),
           Q44_1N = as.numeric(Q44_1),
           Q44_2N = as.numeric(Q44_2),
           Q45_1N = as.numeric(Q45_1),
           Q45_2N = as.numeric(Q45_2),
           Q46_1N = as.numeric(Q46_1),
           Q46_2N = as.numeric(Q46_2),
           Q30_1N = as.numeric(Q30_1),
           Q30_2N = as.numeric(Q30_2),
           Q30_3N = as.numeric(Q30_3))%>%
    mutate_at(vars(t1:t6), ~ as.numeric(str_remove(., "\\.")))%>%
    mutate(absolute_t       = ifelse(absolute_value < t1, 1,
                                     ifelse(absolute_value < t2, 2,
                                            ifelse(absolute_value < t3, 3,
                                                   ifelse(absolute_value < t4, 4,
                                                          ifelse(absolute_value < t5, 5,
                                                                 ifelse(absolute_value < t6, 6,
                                                                        ifelse(absolute_value >= t6, 7,absolute_value))))))))%>%
    # \tilde{l}
    # Positive values: Overestimation
    mutate(Dif_cost_1 = Q42_1N - absolute_t,
           Dif_cost_2 = Q42_2N - absolute_t)%>%
    # \tilde{lr}
    mutate(Percentile_abs = as.numeric(as.character(Percentile)))%>%
    mutate(Quintile = case_when(Percentile_abs<= 20                       ~ 5,
                                Percentile_abs>  20 & Percentile_abs<= 40 ~ 4,
                                Percentile_abs>  40 & Percentile_abs<= 60 ~ 3,
                                Percentile_abs>  60 & Percentile_abs<= 80 ~ 2,
                                Percentile_abs>  80                       ~ 1))%>%
    # Positive values: Overestimation - thinks they are more affected than they are.
    mutate(Dif_Percentile_1 = Q43_1N - Quintile,
           Dif_Percentile_2 = Q43_2N - Quintile)%>%
    mutate(Dif_Percentile_1_ABS = abs(Dif_Percentile_1),
           Dif_Percentile_2_ABS = abs(Dif_Percentile_2),
           Dif_cost_1_ABS       = abs(Dif_cost_1),
           Dif_cost_2_ABS       = abs(Dif_cost_2))
    
    return(data_1.6.0)
}

data_1.6_ESP <- create_outcomes(data_1.5_ESP)
data_1.6_FRA <- create_outcomes(data_1.5_FRA)
data_1.6_GER <- create_outcomes(data_1.5_GER)
data_1.6_ROM <- create_outcomes(data_1.5_ROM)

rm(data_1.5_ESP, data_1.5_GER, data_1.5_FRA, data_1.5_ROM, create_outcomes)

# 1.7   Filters ####

# Wrong second attention check

data_1.6_ESP <- data_1.6_ESP %>%
  filter(Q60A == "Energía")%>%
  filter(filter_1a == 0 & filter_1b == 0)%>%
  filter(filter_2a == 0 | filter_2b == 0)%>%
  mutate(Inclusion = 1)

data_1.6_FRA <- data_1.6_FRA %>%
  filter(Q60A == "Énergie")%>%
  filter(filter_1a == 0 & filter_1b == 0)%>%
  filter(filter_2a == 0 | filter_2b == 0)%>%
  mutate(Inclusion = 1)

data_1.6_GER <- data_1.6_GER %>%
  filter(Q60A == "Energie")%>%
  filter(filter_1a == 0 & filter_1b == 0)%>%
  filter(filter_2a == 0 | filter_2b == 0)%>%
  mutate(Inclusion = 1)

data_1.6_ROM <- data_1.6_ROM %>%
  filter(Q60A == "Energie")%>%
  filter(filter_1a == 0 & filter_1b == 0)%>%
  filter(filter_2a == 0 | filter_2b == 0)%>%
  mutate(Inclusion = 1)

# 2     DESCRIPTIVE STATISTICS ####

data_2 <- bind_rows(data_1.6_FRA, data_1.6_GER)%>%
  bind_rows(data_1.6_ROM)%>%
  bind_rows(data_1.6_ESP)

# 2.1   Baseline outcome distribution ####
# 2.1.1 Overall Policy Support ####

data_2.1.1.1 <- data_2 %>%
  filter(!is.na(Q46_1N))%>%
  group_by(Q46_1N, Country)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(Country)%>%
  mutate(sum = sum(number))%>%
  ungroup()%>%
  mutate(share = number/sum)%>%
  group_by(Country)%>%
  mutate(share_sum = cumsum(share))%>%
  ungroup()%>%
  mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
  mutate(Country = factor(Country, levels = c("Spain", "France", "Germany", "Romania")))%>%
  mutate(Q46_1N_label = case_when(Q46_1N == 1 ~ "Strongly\n oppose",
                                      Q46_1N == 2 ~ "Rather\n oppose",
                                      Q46_1N == 3 ~ "Neutral",
                                      Q46_1N == 4 ~ "Rather\n support",
                                      Q46_1N == 5 ~ "Strongly\n support"))%>%
   mutate(Q46_1N_label = factor(Q46_1N_label, levels = c("Strongly\n oppose", "Rather\n oppose", "Neutral", "Rather\n support", "Strongly\n support")))%>%
   mutate(Period = "t=0")

data_2.1.1.2 <- data_2 %>%
  filter(!is.na(Q46_2N))%>%
  group_by(Q46_2N, Country)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(Country)%>%
  mutate(sum = sum(number))%>%
  ungroup()%>%
  mutate(share = number/sum)%>%
  group_by(Country)%>%
  mutate(share_sum = cumsum(share))%>%
  ungroup()%>%
  mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
  mutate(Country = factor(Country, levels = c("Spain", "France", "Germany", "Romania")))%>%
  mutate(Q46_2N_label = case_when(Q46_2N == 1 ~ "Strongly\n oppose",
                                  Q46_2N == 2 ~ "Rather\n oppose",
                                  Q46_2N == 3 ~ "Neutral",
                                  Q46_2N == 4 ~ "Rather\n support",
                                  Q46_2N == 5 ~ "Strongly\n support"))%>%
  mutate(Q46_1N_label = factor(Q46_2N_label, levels = c("Strongly\n oppose", "Rather\n oppose", "Neutral", "Rather\n support", "Strongly\n support")))%>%
  mutate(Period = "t=1")

data_2.1.1 <- bind_rows(data_2.1.1.1, data_2.1.1.2)%>%
  mutate(Period = factor(Period, levels = c("t=1", "t=0")))
 
P_2.1.1 <- ggplot(data_2.1.1, aes(y = Period))+
 facet_grid(Country ~ .)+
 theme_bw()+
 geom_col(position = "stack", aes(x = share, fill = fct_rev(Q46_1N_label)), colour = "black", width = 0.75)+
 scale_fill_viridis_d(direction = -1, guide = guide_legend(reverse = TRUE, title.position = "top"))+
 labs(fill = "Do you support or oppose this policy?")+
 scale_x_continuous(labels = scales::percent_format())+
 xlab("Share of respondents")+
 ggtitle("Overall policy support (Q46_1 and Q46_2)")+
 theme(panel.grid  = element_blank(),
       axis.text.x = element_text(size = 7),
       axis.text.y = element_text(size = 8),
       axis.title  = element_text(size = 8),
       legend.position = "bottom")

P_2.1.2 <- ggplot(data_2.1.1, aes(x = Q46_1N_label, y = Period))+
 facet_grid(Country ~ .)+
 theme_bw()+
 geom_point(aes(fill = share_sum), shape = 22, size = 14)+
 geom_text(aes(label = label_0), size = 4)+
 scale_fill_distiller(limits = c(0,1))+
 xlab("Do you support or oppose this policy?")+
 ggtitle("Overall policy support (Q46_1 and Q46_2)")+
 guides(fill = "none")+
 theme(panel.grid  = element_blank(),
       axis.text.x = element_text(size = 7),
       axis.text.y = element_text(size = 8),
       axis.title  = element_text(size = 8))

P_2.1.3 <- ggplot(filter(data_2.1.1, Period == "t=0"), aes(y = Country))+
  theme_bw()+
  geom_col(position = "stack", aes(x = share, fill = fct_rev(Q46_1N_label)), colour = "black", width = 0.75)+
  scale_fill_viridis_d(direction = -1, guide = guide_legend(reverse = TRUE, title.position = "top"))+
  labs(fill = "Do you support or oppose this policy?")+
  scale_x_continuous(labels = scales::percent_format())+
  xlab("Share of respondents")+
  ggtitle("Overall policy support (Q46_1 and Q46_2)")+
  theme(panel.grid  = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 8),
        legend.position = "bottom")

jpeg("../5_Analysis/1_Descriptive/Figure_D1_%d.jpg", width = 12, height = 12, unit = "cm", res = 600)
print(P_2.1.2)
print(P_2.1.3)
print(P_2.1.1)
dev.off()

rm(data_2.1.1, data_2.1.1.1, data_2.1.1.2, P_2.1.1, P_2.1.2)

# 2.1.2 Effectiveness ####

data_2.1.2.1 <- data_2 %>%
  group_by(Q41_1N, Country)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(Country)%>%
  mutate(sum = sum(number))%>%
  ungroup()%>%
  mutate(share = number/sum)%>%
  group_by(Country)%>%
  mutate(share_sum = cumsum(share))%>%
  ungroup()%>%
  mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
  mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
  #filter(!is.na(Effectiveness_1N))%>%
  mutate(Q41_1N_label = case_when(Q41_1N == 1 ~ "Definetly not",
                                  Q41_1N == 2 ~ "Probably not",
                                  Q41_1N == 3 ~ "Probably yes",
                                  Q41_1N == 4 ~ "Definetly yes",
                                  is.na(Q41_1N) ~ "Don't know"))%>%
  mutate(Q41_1N_label = factor(Q41_1N_label, levels = c("Definetly not", "Probably not", "Probably yes", "Definetly yes", "Don't know")))%>%
  mutate(Period = "t=0")

data_2.1.2.2 <- data_2 %>%
 group_by(Q41_2N, Country)%>%
 summarise(number = n())%>%
 ungroup()%>%
 group_by(Country)%>%
 mutate(sum = sum(number))%>%
 ungroup()%>%
 mutate(share = number/sum)%>%
 group_by(Country)%>%
 mutate(share_sum = cumsum(share))%>%
 ungroup()%>%
 mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
 mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
 # filter(!is.na(Effectiveness_2N))%>%
  mutate(Q41_1N_label = case_when(Q41_2N == 1 ~ "Definetly not",
                                  Q41_2N == 2 ~ "Probably not",
                                  Q41_2N == 3 ~ "Probably yes",
                                  Q41_2N == 4 ~ "Definetly yes",
                                  is.na(Q41_2N) ~ "Don't know"))%>%
  mutate(Q41_1N_label = factor(Q41_1N_label, levels = c("Definetly not", "Probably not", "Probably yes", "Definetly yes", "Don't know")))%>%
  mutate(Period = "t=1")

data_2.1.2 <- bind_rows(data_2.1.2.1, data_2.1.2.2)%>%
 mutate(Period = factor(Period, levels = c("t=1", "t=0")))

P_2.1.2 <- ggplot(data_2.1.2, aes(x = Q41_1N_label, y = Period))+
 facet_grid(Country ~ .)+
 theme_bw()+
 geom_point(aes(fill = share_sum), shape = 22, size = 14)+
 geom_text(aes(label = label_0), size = 4)+
 scale_fill_distiller(limits = c(0,1))+
 xlab("Do you think that this policy will contribute to effectively reducing GHG emissions?")+
 ggtitle("Perception of effectiveness (Q41_1 and Q41_2)")+
 guides(fill = "none")+
 theme(panel.grid  = element_blank(),
       axis.text.x = element_text(size = 7),
       axis.text.y = element_text(size = 8),
       axis.title  = element_text(size = 7))

jpeg("../5_Analysis/1_Descriptive/Figure_D2.jpg", width = 12, height = 12, unit = "cm", res = 600)
print(P_2.1.2)
dev.off()

rm(data_2.1.2, data_2.1.2.1, data_2.1.2.2, P_2.1.2)

# 2.1.3 Expected costs ####

data_2.1.3.1 <- data_2 %>%
  mutate(Cost_estimation = ifelse(is.na(Dif_cost_1), "Don't know", 
                                  ifelse(Dif_cost_1 < -3, "Strongly\n underestimated",
                                         ifelse(Dif_cost_1 < 0, "Under-\nestimated", 
                                                ifelse(Dif_cost_1 == 0, "Estimated\n correctly",
                                                       ifelse(Dif_cost_1 > 0 & Dif_cost_1 < 4, "Over-\nestimated",
                                                              ifelse(Dif_cost_1 >= 4, "Strongly\n overestimated", NA)))))))%>%
  mutate(Cost_estimation = factor(Cost_estimation, levels = c("Strongly\n underestimated", "Under-\nestimated", "Estimated\n correctly", "Over-\nestimated", "Strongly\n overestimated", "Don't know")))%>%
  group_by(Cost_estimation, Country)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(Country)%>%
  mutate(sum = sum(number))%>%
  ungroup()%>%
  mutate(share = number/sum)%>%
  group_by(Country)%>%
  mutate(share_sum = cumsum(share))%>%
  ungroup()%>%
  mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
  mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
  # bind_rows(data.frame(Country = factor("France", levels = levels(.$Country)),
  #                      Cost_estimation = factor("Strongly\n underestimated", levels = levels(.$Cost_estimation)),
  #                      share_sum = 0, label_0 = "0%"))%>%
  # filter(Cost_estimation != "Don't know")%>%
  mutate(Period = "t=0")
  

data_2.1.3.2 <- data_2 %>%
  filter(Treatment_C != "Control")%>%
  mutate(Cost_estimation = ifelse(is.na(Dif_cost_2), "Don't know", 
                                  ifelse(Dif_cost_2 < -3, "Strongly\n underestimated",
                                         ifelse(Dif_cost_2 < 0, "Under-\nestimated", 
                                                ifelse(Dif_cost_2 == 0, "Estimated\n correctly",
                                                       ifelse(Dif_cost_2 > 0 & Dif_cost_2 < 4, "Over-\nestimated",
                                                              ifelse(Dif_cost_2 >= 4, "Strongly\n overestimated", NA)))))))%>%
  mutate(Cost_estimation = factor(Cost_estimation, levels = c("Strongly\n underestimated", "Under-\nestimated", "Estimated\n correctly", "Over-\nestimated", "Strongly\n overestimated", "Don't know")))%>%
  group_by(Cost_estimation, Country)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(Country)%>%
  mutate(sum = sum(number))%>%
  ungroup()%>%
  mutate(share = number/sum)%>%
  group_by(Country)%>%
  mutate(share_sum = cumsum(share))%>%
  ungroup()%>%
  mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
  mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
  # bind_rows(data.frame(Country = factor("Spain", levels = levels(.$Country)),
  #                      Cost_estimation = factor("Strongly\n underestimated", levels = levels(.$Cost_estimation)),
  #                      share_sum = 0, label_0 = "0%"))%>%
  # filter(Cost_estimation != "Don't know")%>%
  mutate(Period = "t=1")

data_2.1.3 <- bind_rows(data_2.1.3.1, data_2.1.3.2)%>%
  mutate(Period = factor(Period, levels = c("t=1", "t=0")))

P_2.1.3 <- ggplot(data_2.1.3, aes(x = Cost_estimation, y = Period))+
  facet_grid(Country ~ .)+
  theme_bw()+
  geom_point(aes(fill = share_sum), shape = 22, size = 14)+
  geom_text(aes(label = label_0), size = 4)+
  scale_fill_distiller(limits = c(0,1))+
  xlab("By how much will this policy increase your costs?")+
  ggtitle("Perception of additional costs (Q42_1 and Q42_2)")+
  guides(fill = "none")+
  theme(panel.grid  = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 7))

jpeg("../5_Analysis/1_Descriptive/Figure_D3.jpg", width = 12, height = 12, unit = "cm", res = 600)
print(P_2.1.3)
dev.off()

rm(data_2.1.3, data_2.1.3.1, data_2.1.3.2, P_2.1.3)

# 2.1.4 Relative loss ####

data_2.1.4.1 <- data_2 %>%
  group_by(Q43_1N, Country)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(Country)%>%
  mutate(sum = sum(number))%>%
  ungroup()%>%
  mutate(share = number/sum)%>%
  group_by(Country)%>%
  mutate(share_sum = cumsum(share))%>%
  ungroup()%>%
  mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
  mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
  # filter(!is.na(Relative_loss_1N))%>%
  mutate(Q43_1N_label = case_when(Q43_1N == 1 ~ "Much higher",
                                  Q43_1N == 2 ~ "Somewhat higher",
                                  Q43_1N == 3 ~ "Similar",
                                  Q43_1N == 4 ~ "Somewhat lower",
                                  Q43_1N == 5 ~ "Much lower",
                                  is.na(Q43_1N) ~ "Don't know"))%>%
  mutate(Q43_1N_label = factor(Q43_1N_label, levels = c("Much higher", "Somewhat higher", "Similar", "Somewhat lower", "Much lower", "Don't know")))%>%
  mutate(Period = "t=0")

data_2.1.4.2 <- data_2 %>%
  filter(Treatment_C != "Control")%>%
  group_by(Q43_2N, Country)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(Country)%>%
  mutate(sum = sum(number))%>%
  ungroup()%>%
  mutate(share = number/sum)%>%
  group_by(Country)%>%
  mutate(share_sum = cumsum(share))%>%
  ungroup()%>%
  mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
  mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
  # filter(!is.na(Relative_loss_2N))%>%
  mutate(Q43_1N_label = case_when(Q43_2N == 1 ~ "Much higher",
                                  Q43_2N == 2 ~ "Somewhat higher",
                                  Q43_2N == 3 ~ "Similar",
                                  Q43_2N == 4 ~ "Somewhat lower",
                                  Q43_2N == 5 ~ "Much lower",
                                  is.na(Q43_2N) ~ "Don't know"))%>%
  mutate(Q43_1N_label = factor(Q43_1N_label, levels = c("Much higher", "Somewhat higher", "Similar", "Somewhat lower", "Much lower", "Don't know")))%>%
  mutate(Period = "t=1")

data_2.1.4 <- bind_rows(data_2.1.4.1, data_2.1.4.2)%>%
  mutate(Period = factor(Period, levels = c("t=1", "t=0")))

P_2.1.4 <- ggplot(data_2.1.4, aes(x = Q43_1N_label, y = Period))+
  facet_grid(Country ~ .)+
  theme_bw()+
  geom_point(aes(fill = share_sum), shape = 22, size = 14)+
  geom_text(aes(label = label_0), size = 4)+
  scale_fill_distiller(limits = c(0,1))+
  xlab("How many costs do you expect\n in comparison to an average household?")+
  ggtitle("Additional relative costs (Q43_1 and Q43_2)")+
  guides(fill = "none")+
  theme(panel.grid  = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 7))

jpeg("../5_Analysis/1_Descriptive/Figure_D4.jpg", width = 12, height = 12, unit = "cm", res = 600)
print(P_2.1.4)
dev.off()

rm(data_2.1.4, data_2.1.4.1, data_2.1.4.2, P_2.1.4)

# 2.1.5 Vulnerable ####

data_2.1.5.1 <- data_2 %>%
  group_by(Q44_1N, Country)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(Country)%>%
  mutate(sum = sum(number))%>%
  ungroup()%>%
  mutate(share = number/sum)%>%
  group_by(Country)%>%
  mutate(share_sum = cumsum(share))%>%
  ungroup()%>%
  mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
  mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
  #filter(!is.na(Vulnerable_1N))%>%
  mutate(Q44_1N_label = case_when(Q44_1N == 1   ~ "Rather hurt",
                                  Q44_1N == 2   ~ "Neither hurt\n nor help",
                                  Q44_1N == 3   ~ "Rather help",
                                  is.na(Q44_1N) ~ "Don't know"))%>%
  mutate(Q44_1N_label = factor(Q44_1N_label, levels = c("Rather hurt", "Neither hurt\n nor help", "Rather help", "Don't know")))%>%
  mutate(Period = "t=0")

data_2.1.5.2 <- data_2 %>%
  group_by(Q44_2N, Country)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(Country)%>%
  mutate(sum = sum(number))%>%
  ungroup()%>%
  mutate(share = number/sum)%>%
  group_by(Country)%>%
  mutate(share_sum = cumsum(share))%>%
  ungroup()%>%
  mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
  mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
  # filter(!is.na(Vulnerable_2N))%>%
  mutate(Q44_1N_label = case_when(Q44_2N == 1 ~ "Rather hurt",
                                  Q44_2N == 2 ~ "Neither hurt\n nor help",
                                  Q44_2N == 3 ~ "Rather help",
                                  is.na(Q44_2N) ~ "Don't know"))%>%
  mutate(Q44_1N_label = factor(Q44_1N_label, levels = c("Rather hurt", "Neither hurt\n nor help", "Rather help", "Don't know")))%>%
  mutate(Period = "t=1")

data_2.1.5 <- bind_rows(data_2.1.5.1, data_2.1.5.2)%>%
  mutate(Period = factor(Period, levels = c("t=1", "t=0")))

P_2.1.5 <- ggplot(data_2.1.5, aes(x = Q44_1N_label, y = Period))+
  facet_grid(Country ~ .)+
  theme_bw()+
  geom_point(aes(fill = share_sum), shape = 22, size = 14)+
  geom_text(aes(label = label_0), size = 4)+
  scale_fill_distiller(limits = c(0,1))+
  xlab("Will this policy help or hurt the most vulnerable households?")+
  ggtitle("Vulnerable households (Q44_1 and Q44_2)")+
  guides(fill = "none")+
  theme(panel.grid  = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 7))

jpeg("../5_Analysis/1_Descriptive/Figure_D5.jpg", width = 12, height = 12, unit = "cm", res = 600)
print(P_2.1.5)
dev.off()

rm(data_2.1.5, data_2.1.5.1, data_2.1.5.2, P_2.1.5)

# 2.1.6 Fairness ####

data_2.1.6.1 <- data_2 %>%
  group_by(Q45_1N, Country)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(Country)%>%
  mutate(sum = sum(number))%>%
  ungroup()%>%
  mutate(share = number/sum)%>%
  group_by(Country)%>%
  mutate(share_sum = cumsum(share))%>%
  ungroup()%>%
  mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
  mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
  #filter(!is.na(Fairness_1N))%>%
  mutate(Q45_1N_label = case_when(Q45_1N == 1      ~ "Unfair",
                                  Q45_1N == 2      ~ "Neither fair\n nor unfair",
                                  Q45_1N == 3      ~ "Fair",
                                  is.na(Q45_1N)    ~ "Don't know"))%>%
  mutate(Q45_1N_label = factor(Q45_1N_label, levels = c("Unfair", "Neither fair\n nor unfair", "Fair", "Don't know")))%>%
  mutate(Period = "t=0")

data_2.1.6.2 <- data_2 %>%
 group_by(Q45_2N, Country)%>%
 summarise(number = n())%>%
 ungroup()%>%
 group_by(Country)%>%
 mutate(sum = sum(number))%>%
 ungroup()%>%
 mutate(share = number/sum)%>%
 group_by(Country)%>%
 mutate(share_sum = cumsum(share))%>%
 ungroup()%>%
 mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
 mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
 # filter(!is.na(Fairness_2N))%>%
 mutate(Q45_1N_label = case_when(Q45_2N == 1   ~ "Unfair",
                                 Q45_2N == 2   ~ "Neither fair\n nor unfair",
                                 Q45_2N == 3   ~ "Fair",
                                 is.na(Q45_2N) ~ "Don't know"))%>%
 mutate(Q45_1N_label = factor(Q45_1N_label, levels = c("Unfair", "Neither fair\n nor unfair", "Fair", "Don't know")))%>%
 mutate(Period = "t=1")

data_2.1.6 <- bind_rows(data_2.1.6.1, data_2.1.6.2)%>%
 mutate(Period = factor(Period, levels = c("t=1", "t=0")))

P_2.1.6 <- ggplot(data_2.1.6, aes(x = Q45_1N_label, y = Period))+
 facet_grid(Country ~ .)+
 theme_bw()+
 geom_point(aes(fill = share_sum), shape = 22, size = 14)+
 geom_text(aes(label = label_0), size = 4)+
 scale_fill_distiller(limits = c(0,1))+
 xlab("Do you find this policy fair or unfair?")+
 ggtitle("Perception of fairness (Q45_1 and Q45_2)")+
 guides(fill = "none")+
 theme(panel.grid  = element_blank(),
       axis.text.x = element_text(size = 7),
       axis.text.y = element_text(size = 8),
       axis.title  = element_text(size = 7))

jpeg("../5_Analysis/1_Descriptive/Figure_D6.jpg", width = 12, height = 12, unit = "cm", res = 600)
print(P_2.1.6)
dev.off()

rm(data_2.1.6, data_2.1.6.1, data_2.1.6.2, P_2.1.6)

# 2.2   Baseline correlation between policy support and institutional trust ####

# data_3.2 <- data_3 %>%
#   mutate(Trust = ifelse(Trust_National_N <= 2, "Low trust",
#                         ifelse(Trust_National_N == 3, "Medium trust",
#                                ifelse(Trust_National_N > 3, "High trust", NA))))%>%
#   group_by(Country, Trust, Support_1N)%>%
#   summarise(number = n())%>%
#   ungroup()%>%
#   group_by(Country, Trust)%>%
#   mutate(sum = sum(number))%>%
#   ungroup()%>%
#   mutate(share = number/sum)%>%
#   group_by(Country, Trust)%>%
#   mutate(share_sum = cumsum(share))%>%
#   ungroup()%>%
#   mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
#   mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
#   mutate(Trust   = factor(Trust,   levels = c("Low trust", "Medium trust", "High trust")))%>%
#   filter(!is.na(Support_1N) & !is.na(Trust))%>%
#   mutate(Support_1N_label = case_when(Support_1N == 1 ~ "Strongly\n oppose",
#                                       Support_1N == 2 ~ "Rather\n oppose",
#                                       Support_1N == 3 ~ "Neutral",
#                                       Support_1N == 4 ~ "Rather\n support",
#                                       Support_1N == 5 ~ "Strongly\n support"))%>%
#   mutate(Support_1N_label = factor(Support_1N_label, levels = c("Strongly\n oppose", "Rather\n oppose", "Neutral", "Rather\n support", "Strongly\n support")))%>%
#   filter(Trust != "Medium trust")
# 
# P_2 <- ggplot(data_3.2, aes(x = Support_1N_label, y = Trust))+
#   facet_grid(Country ~ .)+
#   theme_bw()+
#   geom_point(aes(fill = share_sum), shape = 22, size = 14)+
#   geom_text(aes(label = label_0), size = 4)+
#   scale_fill_distiller(limits = c(0,1))+
#   xlab("Do you support or oppose this policy?")+
#   ggtitle("Overall policy support - by institutional trust")+
#   ylab("Overall trust in national government")+
#   guides(fill = "none")+
#   theme(panel.grid  = element_blank(),
#         axis.text.x = element_text(size = 7),
#         axis.text.y = element_text(size = 8),
#         axis.title  = element_text(size = 8))
# 
# jpeg("../2_Data/Figures/Pilot/Figure_B1.jpg", width = 12, height = 12, unit = "cm", res = 600)
# print(P_2)
# dev.off()
# 
# rm(data_3.2, P_2)


# 2.3   Treatment effects (A,B,C1 to C4) on overall policy support ####

# tex.style <- style.tex(model.title = "", fixef.title = "\\midrule Fixed Effects",
#                        stats.title = "\\midrule", model.format = "",
#                        fontsize = "small", yesNo = c("Yes","No"))
# 
# dict_latex <- c(Support_1N = "Support (1-5)", 
#                 "Treatment_A" = "Treatment A", "B_Post" = "Treatment B*Post", 
#                 "C1_Post" = "Treatment C1*Post", 
#                 "C2_Post" = "Treatment C2*Post",
#                 "C3_Post" = "Treatment C3*Post",
#                 "C4_Post" = "Treatment C4*Post",
#                 treatment = "Treatment")
# 
# data_3.3 <- data_3
# 
# model_A <- feols(Support_1N ~ i(Treatment_A, ref = "nonEU"), split = ~ Country, data = data_3.3)
# 
# etable(model_A, tex = TRUE, dict = dict_latex,
#        file = "../2_Data/Figures/Pilot/Table_HA.tex", fitstat = c("n", "r2"),
#        digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
#        title = "Treatment A on overall policy support",  
#        label = "tab:HA", 
#        # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
#        placement = "htbp!",
#        notes = c("\\medskip \\textit{Note:}",
#                  paste0("This table displays results from an OLS regression on the support for the fit-for-55 policy package over receiving treatment A, which puts emphasis on the role of the EU. 
#                         The dependent variable expresses support on a five-point Lickert-scale."))
# )
# 
# 
# data_3.3.1 <- data_3.3 %>%
#   select(Country, ID, Support_1N, Support_2N)%>%
#   pivot_longer(Support_1N:Support_2N, names_to = "Period_0", values_to = "Support", names_prefix = "Support_")%>%
#   left_join(select(data_3.3, Country, ID, Treatment_B, Treatment_C))%>%
#   mutate(B_Post  = ifelse(Treatment_B == "Treatment" & Period_0 == "2N",1,0),
#          C1_Post = ifelse(Treatment_C == "C1" & Period_0 == "2N",1,0),
#          C2_Post = ifelse(Treatment_C == "C2" & Period_0 == "2N",1,0),
#          C3_Post = ifelse(Treatment_C == "C3" & Period_0 == "2N",1,0),
#          C4_Post = ifelse(Treatment_C == "C4" & Period_0 == "2N",1,0))
# 
# model_BC <- feols(Support ~ B_Post + C1_Post + C2_Post + C3_Post + C4_Post | ID + Period_0, data = data_3.3.1, split = ~ Country)
# # model_BC <- feols(Support ~ B_Post | ID + Period_0, data = data_3.3.1, split = ~ Country)
# 
# etable(model_BC, tex = TRUE, dict = dict_latex,
#        file = "../2_Data/Figures/Pilot/Table_HBC.tex", fitstat = c("n", "r2"),
#        digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
#        title = "Treatment B\\textsubscript{1} and C\\textsubscript{1} to C\\textsubscript{4} on overall policy support",  
#        label = "tab:HBC", 
#        # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
#        placement = "htbp!",
#        notes = c("\\medskip \\textit{Note:}",
#                  paste0("This table displays results from an OLS regression on the support for the fit-for-55 policy package over receiving treatment B\\textsubscript{1}, C\\textsubscript{1}, C\\textsubscript{2}, C\\textsubscript{3} or C\\textsubscript{4}. Treatment B\\textsubscript{1} provides information about the mechanisms of carbon pricing.
#                  Treatments C\\textsubscript{1} to C\\textsubscript{4} provide respondent-level information about the resulting additional costs. The dependent variable expresses support on a five-point Lickert-scale."))
# )



rm(data_2)

# 3     Hypothesis tests ####

data_3_ESP <- data_1.6_ESP
data_3_FRA <- data_1.6_FRA
data_3_GER <- data_1.6_GER
data_3_ROM <- data_1.6_ROM %>%
  rename(Pricelevel = Priceleveleuro)

# 3.1   Hypotheses 1 to 6 ####

# Institutional trust at national level: Q30_2/Q30_2N
# Institutional trust at EU-level: Q30_3/Q30_3N

# Overall policy support: Q46_1N
# Perception of effectiveness: Q41_1N
# Perception of fairness: Q45_1N
# Perception of effects on vulnerable households: Q44_1N
# Estimation of additional costs: Dif_cost_1 (>0 - overestimate costs)
# Estimation of relative additional costs: Dif_Percentile_1 (>0 - overestimate costs)

model_3.1_ESP <- feols(c(Q41_1N, Q46_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1) ~ sw(Q30_2N, Q30_3N), data = data_3_ESP)
model_3.1_FRA <- feols(c(Q41_1N, Q46_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1, Dif_Percentile_1_ABS) ~ sw(Q30_2N, Q30_3N), data = data_3_FRA)
model_3.1_GER <- feols(c(Q41_1N, Q46_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1, Dif_Percentile_1_ABS) ~ sw(Q30_2N, Q30_3N), data = data_3_GER)
model_3.1_ROM <- feols(c(Q41_1N, Q46_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1) ~ sw(Q30_2N, Q30_3N), data = data_3_ROM)

# Correct p-values
correct_p_values_1 <- function(model_0){
  lapply(as.list(model_0), function(model_1) {
    tidy(model_1) %>%
      mutate(
        outcome  = as.character(model_1$fml[[2]]),  # LHS of formula
        variable = as.character(model_1$fml[[3]])   # RHS of formula
      )
  }) %>% bind_rows()%>%
    filter(term %in% c("Q30_2N", "Q30_3N"))%>%
    # Adjust p-values for one-sided t-test.
    mutate(p_value_one_sided = ifelse((outcome %in% c("Q41_1N", "Q44_1N", "Q45_1N", "Q46_1N") & estimate > 0) | (outcome %in% c("Dif_Percentile_1", "Dif_Percentile_1_ABS", "Dif_cost_1") & estimate < 0), p.value/2, 1-p.value/2))%>%
    # Adjust for Benjamini-Hochberg
    group_by(term)%>%
    mutate(p_value_one_sided_bh = p.adjust(p_value_one_sided, method = "BH"))%>%
    ungroup()
}

tidy_3.1_ESP <- correct_p_values_1(model_3.1_ESP)
tidy_3.1_FRA <- correct_p_values_1(model_3.1_FRA)
tidy_3.1_GER <- correct_p_values_1(model_3.1_GER)
tidy_3.1_ROM <- correct_p_values_1(model_3.1_ROM)

# Export tables

# TBA

rm(model_3.1_ESP, model_3.1_FRA, model_3.1_GER, model_3.1_ROM,
   tidy_3.1_ESP, tidy_3.1_FRA, tidy_3.1_GER, tidy_3.1_ROM, correct_p_values_1)

# 3.2   Hypothesis 7 ####

model_3.2_ESP <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1, Dif_Percentile_1_ABS) ~ i(Treatment_A, ref = "nonEU"), data = data_3_ESP)
model_3.2_FRA <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1, Dif_Percentile_1_ABS) ~ i(Treatment_A, ref = "nonEU"), data = data_3_FRA)
model_3.2_GER <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1, Dif_Percentile_1_ABS) ~ i(Treatment_A, ref = "nonEU"), data = data_3_GER)
model_3.2_ROM <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1, Dif_Percentile_1_ABS) ~ i(Treatment_A, ref = "nonEU"), data = data_3_ROM)

correct_p_values_7 <- function(model_0){
  model_1 <- lapply(seq_along(model_0), function(i) {
    tidy(model_0[[i]]) %>%
      mutate(model_index = i,
             outcome  = names(model_3.2_FRA)[i])
  }) %>% bind_rows()%>%
    filter(term == "Treatment_A::EU")%>%
    # Adjust p-values for one-sided t-test.
    mutate(p_value_one_sided = ifelse((outcome %in% c("Q41_1N", "Q44_1N", "Q45_1N", "Q46_1N") & estimate < 0) | (outcome %in% c("Dif_Percentile_1", "Dif_Percentile_1_ABS", "Dif_cost_1") & estimate > 0), p.value/2, 1-p.value/2))%>%
    # Adjust for Benjamini-Hochberg
    group_by(term)%>%
    mutate(p_value_one_sided_bh = p.adjust(p_value_one_sided, method = "BH"))%>%
    ungroup()
}

tidy_3.2_ESP <- correct_p_values_7(model_3.2_ESP)
tidy_3.2_FRA <- correct_p_values_7(model_3.2_FRA)
tidy_3.2_GER <- correct_p_values_7(model_3.2_GER)
tidy_3.2_ROM <- correct_p_values_7(model_3.2_ROM)

# Export tables

# TBA

adjust_hypothesis_7b <- function(data_3_0){
  data_3_1 <- data_3_0 %>%
    mutate(tau = ifelse(Q30_3N < 3,1,0))%>%
    filter(!is.na(tau))%>%
    mutate(tau_Treatment_A = ifelse(tau == 1 & Treatment_A == "EU",1,0))
  
  return(data_3_1)
}

model_3.2.1_ESP <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1, Dif_Percentile_1_ABS) ~ i(Treatment_A, ref = "nonEU") + tau + tau_Treatment_A, data = adjust_hypothesis_7b(data_3_ESP))
model_3.2.1_FRA <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1, Dif_Percentile_1_ABS) ~ i(Treatment_A, ref = "nonEU") + tau + tau_Treatment_A, data = adjust_hypothesis_7b(data_3_FRA))
model_3.2.1_GER <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1, Dif_Percentile_1_ABS) ~ i(Treatment_A, ref = "nonEU") + tau + tau_Treatment_A, data = adjust_hypothesis_7b(data_3_GER))
model_3.2.1_ROM <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1, Dif_Percentile_1_ABS) ~ i(Treatment_A, ref = "nonEU") + tau + tau_Treatment_A, data = adjust_hypothesis_7b(data_3_ROM))

correct_p_values_7b <- function(model_0){
  model_1 <- lapply(seq_along(model_0), function(i) {
    tidy(model_0[[i]]) %>%
      mutate(model_index = i,
             outcome  = names(model_3.2_FRA)[i])
  }) %>% bind_rows()%>%
    filter(term == "tau_Treatment_A")%>%
    # Adjust p-values for one-sided t-test.
    mutate(p_value_one_sided = ifelse((outcome %in% c("Q41_1N", "Q44_1N", "Q45_1N", "Q46_1N") & estimate < 0) | (outcome %in% c("Dif_Percentile_1", "Dif_Percentile_1_ABS", "Dif_cost_1") & estimate > 0), p.value/2, 1-p.value/2))%>%
    # Adjust for Benjamini-Hochberg
    group_by(term)%>%
    mutate(p_value_one_sided_bh = p.adjust(p_value_one_sided, method = "BH"))%>%
    ungroup()
}

tidy_3.2.1_ESP <- correct_p_values_7b(model_3.2.1_ESP)
tidy_3.2.1_FRA <- correct_p_values_7b(model_3.2.1_FRA)
tidy_3.2.1_GER <- correct_p_values_7b(model_3.2.1_GER)
tidy_3.2.1_ROM <- correct_p_values_7b(model_3.2.1_ROM)

# Export tables

# TBA

rm(adjust_hypothesis_7b, correct_p_values_7, correct_p_values_7b,
   model_3.2_ROM, model_3.2_FRA, model_3.2_GER, model_3.2_ESP, tidy_3.2_ROM, tidy_3.2_FRA, tidy_3.2_GER, tidy_3.2_ESP,
   model_3.2.1_ROM, model_3.2.1_FRA, model_3.2.1_GER, model_3.2.1_ESP, tidy_3.2.1_ROM, tidy_3.2.1_FRA, tidy_3.2.1_GER, tidy_3.2.1_ESP)

# 3.3   Hypotheses 8 to 9 ####

adjust_hypothesis_89 <- function(data_3_0, filter_1){
  data_3_3 <- data_3_0 %>%
    select(ID, Treatment_B, Treatment_C, Q30_2N, Q41_1N, Q41_2N, Q45_1N, Q45_2N)%>%
    pivot_longer(Q41_1N:Q45_2N, names_to = "Variable", values_to = "value")%>%
    mutate(Period  = ifelse(Variable %in% c("Q41_1N", "Q45_1N"),1,2),
           Outcome = ifelse(Variable %in% c("Q41_1N", "Q41_2N"), "Effectiveness", "Fairness"))%>%
    mutate(Post_B = ifelse(Period == 2 & Treatment_B == "Treatment",1,0),
           Post_C = ifelse(Period == 1, "Baseline", as.character(Treatment_C)))%>%
    filter(Outcome == filter_1)%>%
    mutate(tau = ifelse(Q30_2N < 3,1,0))%>%
    mutate(tau_Post_B = ifelse(tau == 1 & Post_B == 1,1,0))
  
  return(data_3_3)
}

model_3.3.1_ESP <- feols(value ~ Post_B | ID + Period + Post_C, data = adjust_hypothesis_89(data_3_ESP, "Effectiveness"))
model_3.3.1_FRA <- feols(value ~ Post_B | ID + Period + Post_C, data = adjust_hypothesis_89(data_3_FRA, "Effectiveness"))
model_3.3.1_GER <- feols(value ~ Post_B | ID + Period + Post_C, data = adjust_hypothesis_89(data_3_GER, "Effectiveness"))
model_3.3.1_ROM <- feols(value ~ Post_B | ID + Period + Post_C, data = adjust_hypothesis_89(data_3_ROM, "Effectiveness")) 

model_3.3.2_ESP <- feols(value ~ Post_B | ID + Period + Post_C, data = adjust_hypothesis_89(data_3_ESP, "Fairness"))
model_3.3.2_FRA <- feols(value ~ Post_B | ID + Period + Post_C, data = adjust_hypothesis_89(data_3_FRA, "Fairness"))
model_3.3.2_GER <- feols(value ~ Post_B | ID + Period + Post_C, data = adjust_hypothesis_89(data_3_GER, "Fairness"))
model_3.3.2_ROM <- feols(value ~ Post_B | ID + Period + Post_C, data = adjust_hypothesis_89(data_3_ROM, "Fairness"))

# Export tables

# TBA

# Hypotheses 8a and 9a

model_3.3.3_ESP <- feols(value ~ tau_Post_B | tau + Post_B + ID + Period + Post_C, data = adjust_hypothesis_89(data_3_ESP, "Effectiveness"))
model_3.3.3_FRA <- feols(value ~ tau_Post_B | tau + Post_B + ID + Period + Post_C, data = adjust_hypothesis_89(data_3_FRA, "Effectiveness"))
model_3.3.3_GER <- feols(value ~ tau_Post_B | tau + Post_B + ID + Period + Post_C, data = adjust_hypothesis_89(data_3_GER, "Effectiveness"))
model_3.3.3_ROM <- feols(value ~ tau_Post_B | tau + Post_B + ID + Period + Post_C, data = adjust_hypothesis_89(data_3_ROM, "Effectiveness"))

model_3.3.4_ESP <- feols(value ~ tau_Post_B | tau + Post_B + ID + Period + Post_C, data = adjust_hypothesis_89(data_3_ESP, "Fairness"))
model_3.3.4_FRA <- feols(value ~ tau_Post_B | tau + Post_B + ID + Period + Post_C, data = adjust_hypothesis_89(data_3_FRA, "Fairness"))
model_3.3.4_GER <- feols(value ~ tau_Post_B | tau + Post_B + ID + Period + Post_C, data = adjust_hypothesis_89(data_3_GER, "Fairness"))
model_3.3.4_ROM <- feols(value ~ tau_Post_B | tau + Post_B + ID + Period + Post_C, data = adjust_hypothesis_89(data_3_ROM, "Fairness"))

# Export tables

# TBA

rm(model_3.3.1_ESP, model_3.3.1_FRA, model_3.3.1_GER, model_3.3.1_ROM, model_3.3.2_ESP, model_3.3.2_FRA, model_3.3.2_GER, model_3.3.2_ROM,
   model_3.3.3_ESP, model_3.3.3_FRA, model_3.3.3_GER, model_3.3.3_ROM, model_3.3.4_ESP, model_3.3.4_FRA, model_3.3.4_GER, model_3.3.4_ROM)

# 3.4   Hypotheses 10 to 20 ####

adjust_hypothesis_10f <- function(data_3_0, filter_1){
  data_3_4 <- data_3_0 %>%
    select(ID, Treatment_B, Treatment_C, Dif_cost_1, Dif_cost_2, Dif_Percentile_1, Dif_Percentile_2, Q41_1N, Q41_2N, Q44_1N, Q44_2N, Q45_1N, Q45_2N, 
           Dif_Percentile_1_ABS, Dif_Percentile_2_ABS, Dif_cost_1_ABS, Dif_cost_2_ABS)%>%
    # Overestimated/underestimated
    mutate(Overestimated_Absolute = ifelse(Dif_cost_1 > 0, "Overestimated",
                                           ifelse(Dif_cost_1 < 0, "Underestimated", NA)))%>%
    mutate(Overestimated_Distribution = ifelse(Dif_Percentile_1 > 0, "Overestimated",
                                           ifelse(Dif_Percentile_1 < 0, "Underestimated", NA)))%>%
    pivot_longer(Dif_cost_1:Dif_cost_2_ABS, names_to = "Variable", values_to = "value")%>%
    mutate(Period  = ifelse(Variable %in% c("Q41_1N", "Q44_1N", "Q45_1N", "Dif_cost_1", "Dif_Percentile_1"),1,2),
           Outcome = case_when(Variable %in% c("Q41_1N", "Q41_2N") ~ "Effectiveness",
                               Variable %in% c("Q44_1N", "Q44_2N") ~ "Vulnerable",
                               Variable %in% c("Q45_1N", "Q45_2N") ~ "Fairness",
                               Variable %in% c("Dif_Percentile_1", "Dif_Percentile_2")         ~ "Distribution_costs",
                               Variable %in% c("Dif_cost_1", "Dif_cost_2")                     ~ "Absolute_costs",
                               Variable %in% c("Dif_Percentile_1_ABS", "Dif_Percentile_2_ABS") ~ "Distribution_ABS",
                               Variable %in% c("Dif_cost_1_ABS", "Dif_cost_2_ABS")             ~ "Absolute_ABS"))%>%
    mutate(Post_B   = ifelse(Period == 2 & Treatment_B == "Treatment",1,0),
           Post_C1  = ifelse(Period == 2 & Treatment_C == "C1",1,0),
           Post_C2  = ifelse(Period == 2 & Treatment_C == "C2",1,0),
           Post_C3  = ifelse(Period == 2 & Treatment_C == "C3",1,0),
           Post_C4  = ifelse(Period == 2 & Treatment_C == "C4",1,0),
           Post_C12 = ifelse(Period == 2 & (Treatment_C == "C1" | Treatment_C == "C2"),1,0),
           Post_C34 = ifelse(Period == 2 & (Treatment_C == "C3" | Treatment_C == "C4"),1,0),
           Post_C13 = ifelse(Period == 2 & (Treatment_C == "C1" | Treatment_C == "C3"),1,0),
           Post_C24 = ifelse(Period == 2 & (Treatment_C == "C2" | Treatment_C == "C4"),1,0),
           Post_C234 = ifelse(Period == 2 & (Treatment_C == "C2" | Treatment_C == "C3"| Treatment_C == "C4"),1,0),
           Post_C1234 = ifelse(Period == 2 & Treatment_C != "C5",1,0),
           Post_C5  = ifelse(Period == 2 & Treatment_C == "C5",1,0))%>%
    filter(Outcome == filter_1)%>%
    # Transform into absolute value
    mutate(value_abs = abs(value))
  
  return(data_3_4)
}

# Hypothesis 10:

model_3.4.1_ESP <- feols(value_abs ~ Post_C13 | ID + Period + Post_B + Post_C2, data = adjust_hypothesis_10f(data_3_ESP, "Absolute_costs"))
model_3.4.1_FRA <- feols(value_abs ~ Post_C13 | ID + Period + Post_B + Post_C2, data = adjust_hypothesis_10f(data_3_FRA, "Absolute_costs"))
model_3.4.1_GER <- feols(value_abs ~ Post_C13 | ID + Period + Post_B + Post_C2, data = adjust_hypothesis_10f(data_3_GER, "Absolute_costs"))
model_3.4.1_ROM <- feols(value_abs ~ Post_C13 | ID + Period + Post_B + Post_C2, data = adjust_hypothesis_10f(data_3_ROM, "Absolute_costs"))

# Export tables (TBA)
rm(model_3.4.1_ESP, model_3.4.1_FRA, model_3.4.1_GER, model_3.4.1_ROM)

# Hypothesis 11:

model_3.4.2_ESP <- feols(value_abs ~ Post_C24 | ID + Period + Post_B + Post_C1, data = adjust_hypothesis_10f(data_3_ESP, "Distribution_costs"))
model_3.4.2_FRA <- feols(value_abs ~ Post_C24 | ID + Period + Post_B + Post_C1, data = adjust_hypothesis_10f(data_3_FRA, "Distribution_costs"))
model_3.4.2_GER <- feols(value_abs ~ Post_C24 | ID + Period + Post_B + Post_C1, data = adjust_hypothesis_10f(data_3_GER, "Distribution_costs"))
model_3.4.2_ROM <- feols(value_abs ~ Post_C24 | ID + Period + Post_B + Post_C1, data = adjust_hypothesis_10f(data_3_ROM, "Distribution_costs"))
rm(model_3.4.2_ESP, model_3.4.2_FRA, model_3.4.2_GER, model_3.4.2_ROM)

# Hypothesis 12:

model_3.4.2_ESP_1 <- feols(value ~ Post_C13 | ID + Period + Post_B + Post_C2 + Post_C4, data = filter(adjust_hypothesis_10f(data_3_ESP, "Fairness"), Overestimated_Absolute == "Overestimated"))
model_3.4.2_FRA_1 <- feols(value ~ Post_C13 | ID + Period + Post_B + Post_C2 + Post_C4, data = filter(adjust_hypothesis_10f(data_3_FRA, "Fairness"), Overestimated_Absolute == "Overestimated"))
model_3.4.2_GER_1 <- feols(value ~ Post_C13 | ID + Period + Post_B + Post_C2 + Post_C4, data = filter(adjust_hypothesis_10f(data_3_GER, "Fairness"), Overestimated_Absolute == "Overestimated"))
model_3.4.2_ROM_1 <- feols(value ~ Post_C13 | ID + Period + Post_B + Post_C2 + Post_C4, data = filter(adjust_hypothesis_10f(data_3_ROM, "Fairness"), Overestimated_Absolute == "Overestimated"))

model_3.4.2_ESP_2 <- feols(value ~ Post_C13 | ID + Period + Post_B + Post_C2 + Post_C4, data = filter(adjust_hypothesis_10f(data_3_ESP, "Fairness"), Overestimated_Absolute == "Underestimated"))
model_3.4.2_FRA_2 <- feols(value ~ Post_C13 | ID + Period + Post_B + Post_C2 + Post_C4, data = filter(adjust_hypothesis_10f(data_3_FRA, "Fairness"), Overestimated_Absolute == "Underestimated"))
model_3.4.2_GER_2 <- feols(value ~ Post_C13 | ID + Period + Post_B + Post_C2 + Post_C4, data = filter(adjust_hypothesis_10f(data_3_GER, "Fairness"), Overestimated_Absolute == "Underestimated"))
model_3.4.2_ROM_2 <- feols(value ~ Post_C13 | ID + Period + Post_B + Post_C2 + Post_C4, data = filter(adjust_hypothesis_10f(data_3_ROM, "Fairness"), Overestimated_Absolute == "Underestimated"))
rm(model_3.4.2_ESP_1, model_3.4.2_FRA_1, model_3.4.2_GER_1, model_3.4.2_ROM_1,
   model_3.4.2_ESP_2, model_3.4.2_FRA_2, model_3.4.2_GER_2, model_3.4.2_ROM_2)

# Hypothesis 13:

model_3.4.3_ESP_1 <- feols(value ~ Post_C234 | ID + Period + Post_B + Post_C1, data = filter(adjust_hypothesis_10f(data_3_ESP, "Fairness"), Overestimated_Distribution == "Overestimated"))
model_3.4.3_FRA_1 <- feols(value ~ Post_C234 | ID + Period + Post_B + Post_C1, data = filter(adjust_hypothesis_10f(data_3_FRA, "Fairness"), Overestimated_Distribution == "Overestimated"))
model_3.4.3_GER_1 <- feols(value ~ Post_C234 | ID + Period + Post_B + Post_C1, data = filter(adjust_hypothesis_10f(data_3_GER, "Fairness"), Overestimated_Distribution == "Overestimated"))
model_3.4.3_ROM_1 <- feols(value ~ Post_C234 | ID + Period + Post_B + Post_C1, data = filter(adjust_hypothesis_10f(data_3_ROM, "Fairness"), Overestimated_Distribution == "Overestimated"))

model_3.4.3_ESP_2 <- feols(value ~ Post_C234 | ID + Period + Post_B + Post_C1, data = filter(adjust_hypothesis_10f(data_3_ESP, "Fairness"), Overestimated_Distribution == "Underestimated"))
model_3.4.3_FRA_2 <- feols(value ~ Post_C234 | ID + Period + Post_B + Post_C1, data = filter(adjust_hypothesis_10f(data_3_FRA, "Fairness"), Overestimated_Distribution == "Underestimated"))
model_3.4.3_GER_2 <- feols(value ~ Post_C234 | ID + Period + Post_B + Post_C1, data = filter(adjust_hypothesis_10f(data_3_GER, "Fairness"), Overestimated_Distribution == "Underestimated"))
model_3.4.3_ROM_2 <- feols(value ~ Post_C234 | ID + Period + Post_B + Post_C1, data = filter(adjust_hypothesis_10f(data_3_ROM, "Fairness"), Overestimated_Distribution == "Underestimated"))
rm(model_3.4.3_ESP_1, model_3.4.3_FRA_1, model_3.4.3_GER_1, model_3.4.3_ROM_1,
   model_3.4.3_ESP_2, model_3.4.3_FRA_2, model_3.4.3_GER_2, model_3.4.3_ROM_2)

# Hypothesis 14:
model_3.4.4_ESP <- feols(value ~ Post_C2 | ID + Period + Post_B + Post_C3 + Post_C4 + Post_C5, data = adjust_hypothesis_10f(data_3_ESP, "Fairness"))
model_3.4.4_FRA <- feols(value ~ Post_C2 | ID + Period + Post_B + Post_C3 + Post_C4 + Post_C5, data = adjust_hypothesis_10f(data_3_FRA, "Fairness"))
model_3.4.4_GER <- feols(value ~ Post_C2 | ID + Period + Post_B + Post_C3 + Post_C4 + Post_C5, data = adjust_hypothesis_10f(data_3_GER, "Fairness"))
model_3.4.4_ROM <- feols(value ~ Post_C2 | ID + Period + Post_B + Post_C3 + Post_C4 + Post_C5, data = adjust_hypothesis_10f(data_3_ROM, "Fairness"))
rm(model_3.4.4_ESP, model_3.4.4_FRA, model_3.4.4_GER, model_3.4.4_ROM)

# Hypothesis 15:

model_3.4.5_ESP <- feols(value ~ Post_C3 + Post_C4 | ID + Period + Post_B + Post_C13 + Post_C24, data = adjust_hypothesis_10f(data_3_ESP, "Effectiveness"))
model_3.4.5_FRA <- feols(value ~ Post_C3 + Post_C4 | ID + Period + Post_B + Post_C13 + Post_C24, data = adjust_hypothesis_10f(data_3_FRA, "Effectiveness"))
model_3.4.5_GER <- feols(value ~ Post_C3 + Post_C4 | ID + Period + Post_B + Post_C13 + Post_C24, data = adjust_hypothesis_10f(data_3_GER, "Effectiveness"))
model_3.4.5_ROM <- feols(value ~ Post_C3 + Post_C4 | ID + Period + Post_B + Post_C13 + Post_C24, data = adjust_hypothesis_10f(data_3_ROM, "Effectiveness"))
rm(model_3.4.5_ESP, model_3.4.5_FRA, model_3.4.5_GER, model_3.4.5_ROM)

# Hypothesis 16:

model_3.4.6_ESP <- feols(value ~ Post_C3 + Post_C4 | ID + Period + Post_B + Post_C13 + Post_C24, data = adjust_hypothesis_10f(data_3_ESP, "Fairness"))
model_3.4.6_FRA <- feols(value ~ Post_C3 + Post_C4 | ID + Period + Post_B + Post_C13 + Post_C24, data = adjust_hypothesis_10f(data_3_FRA, "Fairness"))
model_3.4.6_GER <- feols(value ~ Post_C3 + Post_C4 | ID + Period + Post_B + Post_C13 + Post_C24, data = adjust_hypothesis_10f(data_3_GER, "Fairness"))
model_3.4.6_ROM <- feols(value ~ Post_C3 + Post_C4 | ID + Period + Post_B + Post_C13 + Post_C24, data = adjust_hypothesis_10f(data_3_ROM, "Fairness"))
rm(model_3.4.6_ESP, model_3.4.6_FRA, model_3.4.6_GER, model_3.4.6_ROM)

# Hypothesis 17:

model_3.4.7_ESP <- feols(value ~ Post_C4 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = adjust_hypothesis_10f(data_3_ESP, "Vulnerable"))
model_3.4.7_FRA <- feols(value ~ Post_C4 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = adjust_hypothesis_10f(data_3_FRA, "Vulnerable"))
model_3.4.7_GER <- feols(value ~ Post_C4 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = adjust_hypothesis_10f(data_3_GER, "Vulnerable"))
model_3.4.7_ROM <- feols(value ~ Post_C4 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = adjust_hypothesis_10f(data_3_ROM, "Vulnerable"))
rm(model_3.4.7_ESP, model_3.4.7_FRA, model_3.4.7_GER, model_3.4.7_ROM)

# Hypothesis 18:

model_3.4.8_ESP <- feols(value ~ Post_C3 + Post_C4 | ID + Period + Post_B + Post_C1 + Post_C2, data = adjust_hypothesis_10f(data_3_ESP, "Fairness"))
model_3.4.8_FRA <- feols(value ~ Post_C3 + Post_C4 | ID + Period + Post_B + Post_C1 + Post_C2, data = adjust_hypothesis_10f(data_3_FRA, "Fairness"))
model_3.4.8_GER <- feols(value ~ Post_C3 + Post_C4 | ID + Period + Post_B + Post_C1 + Post_C2, data = adjust_hypothesis_10f(data_3_GER, "Fairness"))
model_3.4.8_ROM <- feols(value ~ Post_C3 + Post_C4 | ID + Period + Post_B + Post_C1 + Post_C2, data = adjust_hypothesis_10f(data_3_ROM, "Fairness"))
rm(model_3.4.8_ESP, model_3.4.8_FRA, model_3.4.8_GER, model_3.4.8_ROM)

# Hypothesis 19:
model_3.4.9_ESP <- feols(value ~ Post_C4 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C5, data = adjust_hypothesis_10f(data_3_ESP, "Fairness"))
model_3.4.9_FRA <- feols(value ~ Post_C4 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C5, data = adjust_hypothesis_10f(data_3_FRA, "Fairness"))
model_3.4.9_GER <- feols(value ~ Post_C4 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C5, data = adjust_hypothesis_10f(data_3_GER, "Fairness"))
model_3.4.9_ROM <- feols(value ~ Post_C4 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C5, data = adjust_hypothesis_10f(data_3_ROM, "Fairness"))
rm(model_3.4.9_ESP, model_3.4.9_FRA, model_3.4.9_GER, model_3.4.9_ROM)

# Hypothesis 20:
model_3.4.10_ESP <- feols(value ~ Post_C4 | ID + Period + Post_B + Post_C1 + Post_C3 + Post_C5, data = adjust_hypothesis_10f(data_3_ESP, "Fairness"))
model_3.4.10_FRA <- feols(value ~ Post_C4 | ID + Period + Post_B + Post_C1 + Post_C3 + Post_C5, data = adjust_hypothesis_10f(data_3_FRA, "Fairness"))
model_3.4.10_GER <- feols(value ~ Post_C4 | ID + Period + Post_B + Post_C1 + Post_C3 + Post_C5, data = adjust_hypothesis_10f(data_3_GER, "Fairness"))
model_3.4.10_ROM <- feols(value ~ Post_C4 | ID + Period + Post_B + Post_C1 + Post_C3 + Post_C5, data = adjust_hypothesis_10f(data_3_ROM, "Fairness"))
rm(model_3.4.10_ESP, model_3.4.10_FRA, model_3.4.10_GER, model_3.4.10_ROM)

# 3.5   Hypotheses 21 to 27 ####

adjust_hypothesis_21f <- function(data_3_0, filter_1){
  data_3_4 <- data_3_0 %>%
    select(ID, Treatment_B, Treatment_C, Dif_cost_1, Dif_cost_2, Dif_Percentile_1, Dif_Percentile_2, Q41_1N, Q41_2N, Q44_1N, Q44_2N, Q45_1N, Q45_2N, Q46_1N, Q46_2N)%>%
    # Overestimated/underestimated
    mutate(Overestimated_Absolute = ifelse(Dif_cost_1 > 0, "Overestimated",
                                           ifelse(Dif_cost_1 < 0, "Underestimated", NA)))%>%
    mutate(Overestimated_Distribution = ifelse(Dif_Percentile_1 > 0, "Overestimated",
                                               ifelse(Dif_Percentile_1 < 0, "Underestimated", NA)))%>%
    pivot_longer(Dif_cost_1:Q46_2N, names_to = "Variable", values_to = "value")%>%
    mutate(Period  = ifelse(Variable %in% c("Q41_1N", "Q44_1N", "Q45_1N", "Q46_1N", "Dif_cost_1", "Dif_Percentile_1"),1,2),
           Outcome = case_when(Variable %in% c("Q41_1N", "Q41_2N") ~ "Effectiveness",
                               Variable %in% c("Q44_1N", "Q44_2N") ~ "Vulnerable",
                               Variable %in% c("Q45_1N", "Q45_2N") ~ "Fairness",
                               Variable %in% c("Q46_1N", "Q46_2N") ~ "Support",
                               Variable %in% c("Dif_Percentile_1", "Dif_Percentile_2") ~ "Distribution_costs",
                               Variable %in% c("Dif_cost_1", "Dif_cost_2")             ~ "Absolute_costs"))%>%
    mutate(Post_B   = ifelse(Period == 2 & Treatment_B == "Treatment",1,0),
           Post_C1  = ifelse(Period == 2 & Treatment_C == "C1",1,0),
           Post_C2  = ifelse(Period == 2 & Treatment_C == "C2",1,0),
           Post_C3  = ifelse(Period == 2 & Treatment_C == "C3",1,0),
           Post_C4  = ifelse(Period == 2 & Treatment_C == "C4",1,0),
           Post_C12 = ifelse(Period == 2 & (Treatment_C == "C1" | Treatment_C == "C2"),1,0),
           Post_C34 = ifelse(Period == 2 & (Treatment_C == "C3" | Treatment_C == "C4"),1,0),
           Post_C1234 = ifelse(Period == 2 & Treatment_C != "C5",1,0))
  
  data_3_4.1 <- data_3_4 %>%
    select(ID, Period, Outcome, value)%>%
    filter(Outcome == "Support")%>%
    rename(Support = value)%>%
    select(-Outcome)
  
  data_3_4.2 <- data_3_4 %>%
    left_join(data_3_4.1)%>%
    filter(Outcome == filter_1)
  
  return(data_3_4.2)
}

# Hypothesis 21a:

model_3.5.1_a_ESP <- feols(Support ~ value | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = adjust_hypothesis_21f(data_3_ESP, "Effectiveness"))
model_3.5.1_a_FRA <- feols(Support ~ value | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = adjust_hypothesis_21f(data_3_FRA, "Effectiveness"))
model_3.5.1_a_GER <- feols(Support ~ value | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = adjust_hypothesis_21f(data_3_GER, "Effectiveness"))
model_3.5.1_a_ROM <- feols(Support ~ value | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = adjust_hypothesis_21f(data_3_ROM, "Effectiveness"))
rm(model_3.5.1_a_ESP, model_3.5.1_a_FRA, model_3.5.1_a_GER, model_3.5.1_a_ROM)

# Hypothesis 21b:

model_3.5.1_b_ESP <- feols(Support ~ value | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = adjust_hypothesis_21f(data_3_ESP, "Fairness"))
model_3.5.1_b_FRA <- feols(Support ~ value | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = adjust_hypothesis_21f(data_3_FRA, "Fairness"))
model_3.5.1_b_GER <- feols(Support ~ value | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = adjust_hypothesis_21f(data_3_GER, "Fairness"))
model_3.5.1_b_ROM <- feols(Support ~ value | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = adjust_hypothesis_21f(data_3_ROM, "Fairness"))
rm(model_3.5.1_b_ESP, model_3.5.1_b_FRA, model_3.5.1_b_GER, model_3.5.1_b_ROM)

# Hypothesis 21c:

model_3.5.1_c_ESP <- feols(Support ~ value | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = adjust_hypothesis_21f(data_3_ESP, "Vulnerable"))
model_3.5.1_c_FRA <- feols(Support ~ value | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = adjust_hypothesis_21f(data_3_FRA, "Vulnerable"))
model_3.5.1_c_GER <- feols(Support ~ value | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = adjust_hypothesis_21f(data_3_GER, "Vulnerable"))
model_3.5.1_c_ROM <- feols(Support ~ value | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = adjust_hypothesis_21f(data_3_ROM, "Vulnerable"))
rm(model_3.5.1_c_ESP, model_3.5.1_c_FRA, model_3.5.1_c_GER, model_3.5.1_c_ROM)

# Hypothesis 21d:

model_3.5.1_d_ESP <- feols(Support ~ value | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = adjust_hypothesis_21f(data_3_ESP, "Absolute_costs"))
model_3.5.1_d_FRA <- feols(Support ~ value | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = adjust_hypothesis_21f(data_3_FRA, "Absolute_costs"))
model_3.5.1_d_GER <- feols(Support ~ value | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = adjust_hypothesis_21f(data_3_GER, "Absolute_costs"))
model_3.5.1_d_ROM <- feols(Support ~ value | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = adjust_hypothesis_21f(data_3_ROM, "Absolute_costs"))
rm(model_3.5.1_d_ESP, model_3.5.1_d_FRA, model_3.5.1_d_GER, model_3.5.1_d_ROM)

# Hypothesis 21e:

model_3.5.1_e_ESP <- feols(Support ~ value | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = adjust_hypothesis_21f(data_3_ESP, "Distribution_costs"))
model_3.5.1_e_FRA <- feols(Support ~ value | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = adjust_hypothesis_21f(data_3_FRA, "Distribution_costs"))
model_3.5.1_e_GER <- feols(Support ~ value | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = adjust_hypothesis_21f(data_3_GER, "Distribution_costs"))
model_3.5.1_e_ROM <- feols(Support ~ value | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = adjust_hypothesis_21f(data_3_ROM, "Distribution_costs"))
rm(model_3.5.1_e_ESP, model_3.5.1_e_FRA, model_3.5.1_e_GER, model_3.5.1_e_ROM)

# Hypothesis 22:
adjust_hypothesis_22 <- function(data_3_0){
  data_3_5.1 <- data_3_0 %>%
    select(ID, Q41_1N, Q44_1N, Q45_1N, Dif_cost_1, Dif_Percentile_1, Q46_1N)%>%
    rename(Q41 = Q41_1N,
           Q44 = Q44_1N,
           Q45 = Q45_1N,
           Dif_cost = Dif_cost_1,
           Dif_Percentile = Dif_Percentile_1,
           Q46 = Q46_1N)%>%
    mutate(Period = 1)
  
  data_3_5.2 <- data_3_0 %>%
    select(ID, Q41_2N, Q44_2N, Q45_2N, Dif_cost_2, Dif_Percentile_2, Q46_2N)%>%
    rename(Q41 = Q41_2N,
           Q44 = Q44_2N,
           Q45 = Q45_2N,
           Dif_cost = Dif_cost_2,
           Dif_Percentile = Dif_Percentile_2,
           Q46 = Q46_2N)%>%
    mutate(Period = 2)
  
  data_3_5.3 <- bind_rows(data_3_5.1, data_3_5.2)%>%
    arrange(ID, Period)%>%
    left_join(select(data_3_0, ID, Treatment_B, Treatment_C))%>%
    mutate(Post_B   = ifelse(Period == 2 & Treatment_B == "Treatment",1,0),
           Post_C1  = ifelse(Period == 2 & Treatment_C == "C1",1,0),
           Post_C2  = ifelse(Period == 2 & Treatment_C == "C2",1,0),
           Post_C3  = ifelse(Period == 2 & Treatment_C == "C3",1,0),
           Post_C4  = ifelse(Period == 2 & Treatment_C == "C4",1,0),
           Post_C12 = ifelse(Period == 2 & (Treatment_C == "C1" | Treatment_C == "C2"),1,0),
           Post_C34 = ifelse(Period == 2 & (Treatment_C == "C3" | Treatment_C == "C4"),1,0),
           Post_C1234 = ifelse(Period == 2 & Treatment_C != "C5",1,0))
  
  return(data_3_5.3)
}

model_3.5.2_ESP <- feols(Q46 ~ Q41 + Q44 + Q45 + Dif_cost + Dif_Percentile | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = adjust_hypothesis_22(data_3_ESP))
model_3.5.2_FRA <- feols(Q46 ~ Q41 + Q44 + Q45 + Dif_cost + Dif_Percentile | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = adjust_hypothesis_22(data_3_FRA))
model_3.5.2_GER <- feols(Q46 ~ Q41 + Q44 + Q45 + Dif_cost + Dif_Percentile | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = adjust_hypothesis_22(data_3_GER))
model_3.5.2_ROM <- feols(Q46 ~ Q41 + Q44 + Q45 + Dif_cost + Dif_Percentile | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = adjust_hypothesis_22(data_3_ROM))
rm(model_3.5.2_ESP, model_3.5.2_FRA, model_3.5.2_GER, model_3.5.2_ROM)

# Hypothesis 23:

adjust_hypothesis_23 <- function(data_3_0){
  data_3_5 <- data_3_0 %>%
    select(ID, Treatment_B, Treatment_C, Q46_1N, Q46_2N, Pricelevel)%>%
    pivot_longer(Q46_1N:Q46_2N, names_to = "Variable", values_to = "value")%>%
    mutate(Period  = ifelse(Variable %in% c("Q46_1N"),1,2))%>%
    mutate(Post_B   = ifelse(Period == 2 & Treatment_B == "Treatment",1,0),
           Post_C1  = ifelse(Period == 2 & Treatment_C == "C1",1,0),
           Post_C2  = ifelse(Period == 2 & Treatment_C == "C2",1,0),
           Post_C3  = ifelse(Period == 2 & Treatment_C == "C3",1,0),
           Post_C4  = ifelse(Period == 2 & Treatment_C == "C4",1,0),
           Post_C12 = ifelse(Period == 2 & (Treatment_C == "C1" | Treatment_C == "C2"),1,0),
           Post_C34 = ifelse(Period == 2 & (Treatment_C == "C3" | Treatment_C == "C4"),1,0),
           Post_C1234 = ifelse(Period == 2 & Treatment_C != "C5",1,0))%>%
    mutate(Post_P1 = ifelse(Period == 2 & Pricelevel == "85",1,0),
           Post_P2 = ifelse(Period == 2 & Pricelevel == "125",1,0))
  
  return(data_3_5)
}

model_3.5.3_ESP <- feols(value ~ Post_P1 + Post_P2 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = adjust_hypothesis_23(data_3_ESP))
model_3.5.3_FRA <- feols(value ~ Post_P1 + Post_P2 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = adjust_hypothesis_23(data_3_FRA))
model_3.5.3_GER <- feols(value ~ Post_P1 + Post_P2 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = adjust_hypothesis_23(data_3_GER))
model_3.5.3_ROM <- feols(value ~ Post_P1 + Post_P2 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = adjust_hypothesis_23(data_3_ROM))
rm(model_3.5.3_ESP, model_3.5.3_FRA, model_3.5.3_GER, model_3.5.3_ROM)

# Hypothesis 24:

adjust_hypothesis_24 <- function(data_3_0){
  data_3_5 <- data_3_0 %>%
    filter(Dif_cost_1 > 0)%>%
    select(ID, Q46_1N, Q46_2N, Treatment_B, Treatment_C)%>%
    pivot_longer(Q46_1N:Q46_2N, names_to = "names", values_to = "value")%>%
    mutate(Period = ifelse(names == "Q46_1N",1,2))%>%
    mutate(Post_B = ifelse(Treatment_B == "Treatment" & Period == 2,1,0),
           Post_Gamma = ifelse(Treatment_C != "Control" & Period == 2,1,0))
  
  return(data_3_5)
}

model_3.5.4_ESP <- feols(value ~ Post_Gamma | ID + Period + Post_B, data = adjust_hypothesis_24(data_3_ESP))
model_3.5.4_FRA <- feols(value ~ Post_Gamma | ID + Period + Post_B, data = adjust_hypothesis_24(data_3_FRA))
model_3.5.4_GER <- feols(value ~ Post_Gamma | ID + Period + Post_B, data = adjust_hypothesis_24(data_3_GER))
model_3.5.4_ROM <- feols(value ~ Post_Gamma | ID + Period + Post_B, data = adjust_hypothesis_24(data_3_ROM))
rm(model_3.5.4_ESP, model_3.5.4_FRA, model_3.5.4_GER, model_3.5.4_ROM)

# Hypothesis 25:

adjust_hypothesis_25 <- function(data_3_0){
  data_3_5 <- data_3_0 %>%
    select(ID, Q46_1N, Q46_2N, Treatment_B, Treatment_C)%>%
    pivot_longer(Q46_1N:Q46_2N, names_to = "names", values_to = "value")%>%
    mutate(Period = ifelse(names == "Q46_1N",1,2))%>%
    mutate(Post_B = ifelse(Treatment_B == "Treatment" & Period == 2,1,0),
           Post_C1 = ifelse(Treatment_C == "C1" & Period == 2,1,0),
           Post_C2 = ifelse(Treatment_C == "C2" & Period == 2,1,0),
           Post_C3 = ifelse(Treatment_C == "C3" & Period == 2,1,0),
           Post_C4 = ifelse(Treatment_C == "C4" & Period == 2,1,0),
           Post_C5 = ifelse(Treatment_C == "C5" & Period == 2,1,0))%>%
    mutate(Post_B_C34 = ifelse(Treatment_C %in% c("C3", "C4") & Treatment_B == "Treatment" & Period == 2,1,0))
  
  return(data_3_5)
}

model_3.5.5_ESP <- feols(value ~ Post_B_C34 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = adjust_hypothesis_25(data_3_ESP))
model_3.5.5_FRA <- feols(value ~ Post_B_C34 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = adjust_hypothesis_25(data_3_FRA))
model_3.5.5_GER <- feols(value ~ Post_B_C34 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = adjust_hypothesis_25(data_3_GER))
model_3.5.5_ROM <- feols(value ~ Post_B_C34 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = adjust_hypothesis_25(data_3_ROM))
rm(model_3.5.5_ESP, model_3.5.5_FRA, model_3.5.5_GER, model_3.5.5_ROM)

# Hypothesis 26: 
model_3.5.6_ESP <- feols(value ~ Post_C1 | ID + Period + Post_B + Post_C2 + Post_C3 + Post_C4, data = adjust_hypothesis_25(data_3_ESP))
model_3.5.6_FRA <- feols(value ~ Post_C1 | ID + Period + Post_B + Post_C2 + Post_C3 + Post_C4, data = adjust_hypothesis_25(data_3_FRA))
model_3.5.6_GER <- feols(value ~ Post_C1 | ID + Period + Post_B + Post_C2 + Post_C3 + Post_C4, data = adjust_hypothesis_25(data_3_GER))
model_3.5.6_ROM <- feols(value ~ Post_C1 | ID + Period + Post_B + Post_C2 + Post_C3 + Post_C4, data = adjust_hypothesis_25(data_3_ROM))
rm(model_3.5.6_ESP, model_3.5.6_FRA, model_3.5.6_GER, model_3.5.6_ROM)

# Hypothesis 27
model_3.5.7_ESP <- feols(value ~ Post_C3 | ID + Period + Post_B + Post_C2 + Post_C4 + Post_C5, data = adjust_hypothesis_25(data_3_ESP))
model_3.5.7_FRA <- feols(value ~ Post_C3 | ID + Period + Post_B + Post_C2 + Post_C4 + Post_C5, data = adjust_hypothesis_25(data_3_FRA))
model_3.5.7_GER <- feols(value ~ Post_C3 | ID + Period + Post_B + Post_C2 + Post_C4 + Post_C5, data = adjust_hypothesis_25(data_3_GER))
model_3.5.7_ROM <- feols(value ~ Post_C3 | ID + Period + Post_B + Post_C2 + Post_C4 + Post_C5, data = adjust_hypothesis_25(data_3_ROM))
rm(model_3.5.7_ESP, model_3.5.7_FRA, model_3.5.7_GER, model_3.5.7_ROM)

# 3.6   Hypotheses 28 to 36 (Conjoint) ####

data_3.6_ESP <- data_conjoint_ESP %>%
  left_join(select(data_3_ESP, ID, Q30_1N, Q30_2N, Q31_Gov_nat, Q31_Gov_loc, Q16, Q14, Quintile))%>% # Information about rural households are missing
  mutate(Rank_weighted = ifelse(Preferred == 1,1,
                                ifelse(Preferred_Second == 1, 2/3,
                                       ifelse(Preferred_Least == 1, 1/3,NA))))
data_3.6_FRA <- data_conjoint_FRA %>%
  left_join(select(data_3_FRA, ID, Q30_1N, Q30_2N, Q31_Gov_nat, Q31_Gov_loc, Q16, Q14, Quintile))%>%
  #filter(!is.na(Inclusion))%>%
  mutate(Rank_weighted = ifelse(Preferred == 1,1,
                                ifelse(Preferred_Second == 1, 2/3,
                                       ifelse(Preferred_Least == 1, 1/3,NA))))
data_3.6_GER <- data_conjoint_GER %>%
  left_join(select(data_3_GER, ID, Q30_1N, Q30_2N, Q31_Gov_nat, Q31_Gov_loc, Q16, Q14, Quintile))%>%
  #filter(!is.na(Inclusion))%>%
  mutate(Rank_weighted = ifelse(Preferred == 1,1,
                                ifelse(Preferred_Second == 1, 2/3,
                                       ifelse(Preferred_Least == 1, 1/3,NA))))
data_3.6_ROM <- data_conjoint_ROM %>%
  left_join(select(data_3_ROM, ID, Q30_1N, Q30_2N, Q31_Gov_nat, Q31_Gov_loc, Q16, Q14, Quintile))%>%
  mutate(Rank_weighted = ifelse(Preferred == 1,1,
                                ifelse(Preferred_Second == 1, 2/3,
                                       ifelse(Preferred_Least == 1, 1/3,NA))))

# Hypothesis 28: A is preferred to B and B is preferred to C for all attributes

# Definition of A, B, C TBD

data_3.6.0_ESP <- data_3.6_ESP %>%
  pivot_longer(c("budget_and_funding":"community_mobility_support"), names_to = "Attributes", values_to = "Levels")%>%
  filter(!is.na(Levels))%>%
  group_by(Attributes, Levels)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)

data_3.6.0_FRA <- data_3.6_FRA %>%
  pivot_longer(c("budget_and_funding":"community_mobility_support"), names_to = "Attributes", values_to = "Levels")%>%
  filter(!is.na(Levels))%>%
  filter(!is.na(Choice))%>% # To be revisited - where do NAs come from?
  group_by(Attributes, Levels)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)%>%
  arrange(Attributes, Levels)

# Level müssen hier und da noch synchronisiert werden.

data_3.6.0_GER <- data_3.6_GER %>%
  pivot_longer(c("budget_and_funding":"community_mobility_support"), names_to = "Attributes", values_to = "Levels")%>%
  filter(!is.na(Levels))%>%
  filter(!is.na(Choice_3))%>% # To be revisited - where do NAs come from?
  group_by(Attributes, Levels)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)

data_3.6.0_ROM <- data_3.6_ROM %>%
  pivot_longer(c("budget_and_funding":"community_mobility_support"), names_to = "Attributes", values_to = "Levels")%>%
  filter(!is.na(Levels))%>%
  filter(!is.na(Choice))%>% # To be revisited - where do NAs come from?
  group_by(Attributes, Levels)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)

# Hypothesis 29: Preferences of respondents with lower institutional trust are more strongly influenced by institutional design attributes.

data_3.6.1_ESP <- data_3.6_ESP %>%
  mutate(lower = ifelse(Q30_1N < 3,1,0))%>%
  filter(!is.na(lower))%>%
  group_by(lower, budget_control)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)%>%
  group_by(lower)%>%
  summarise(range = max(ACP) - min(ACP))%>%
  ungroup()
  
data_3.6.1_FRA <- data_3.6_FRA %>%
  mutate(lower = ifelse(Q30_1N < 3,1,0))%>%
  filter(!is.na(lower))%>%
  filter(!is.na(Rank_weighted))%>% # TBD - where do NAs come from?
  group_by(lower, budget_control)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)%>%
  group_by(lower)%>%
  summarise(range = max(ACP) - min(ACP))%>%
  ungroup()

data_3.6.1_GER <- data_3.6_GER %>%
  mutate(lower = ifelse(Q30_1N < 3,1,0))%>%
  filter(!is.na(lower))%>%
  filter(!is.na(Rank_weighted))%>% # TBD - where do NAs come from?
  group_by(lower, budget_control)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)%>%
  group_by(lower)%>%
  summarise(range = max(ACP) - min(ACP))%>%
  ungroup()

data_3.6.1_ROM <- data_3.6_ROM %>%
  mutate(lower = ifelse(Q30_1N < 3,1,0))%>%
  filter(!is.na(lower))%>%
  group_by(lower, budget_control)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)%>%
  group_by(lower)%>%
  summarise(range = max(ACP) - min(ACP))%>%
  ungroup()

# Hypothesis 30: Respondents with lower institutional trust and specifically low trust in the integrity of governments use of funds will prefer
# all other options to "The government, as with any other public revenue"
data_3.6.2_ESP <- data_3.6_ESP %>%
  mutate(lower = ifelse(Q30_1N < 3,1,0))%>%
  filter(Q31_Gov_nat %in% c("En absoluto", "Probablemente no"))%>%
  group_by(budget_control)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)

data_3.6.2_FRA <- data_3.6_FRA %>%
  mutate(lower = ifelse(Q30_1N < 3,1,0))%>%
  filter(!is.na(Rank_weighted))%>% # TBD
  filter(Q31_Gov_nat %in% c("Certainement pas", "Probablement pas"))%>%
  group_by(budget_control)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)

data_3.6.2_GER <- data_3.6_GER %>%
  mutate(lower = ifelse(Q30_1N < 3,1,0))%>%
  filter(!is.na(Rank_weighted))%>% # TBD
  filter(Q31_Gov_nat %in% c("Definitiv nicht", "Vermutlich nicht"))%>%
  group_by(budget_control)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)

data_3.6.2_ROM <- data_3.6_ROM %>%
  mutate(lower = ifelse(Q30_1N < 3,1,0))%>%
  mutate_at(vars(Q31_Gov_nat:Q31_Gov_loc), ~ stri_trans_general(., "Latin-ASCII"))%>%
  filter(Q31_Gov_nat %in% c("In mod sigur nu", "Probabil ca nu"))%>%
  group_by(budget_control)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)

# Hypothesis 31: Respondents with higher trust in their local government than in their national government will prefer 'A local climate center, where an advisor can guide you' to other options for this attribute.
data_3.6.3_ESP <- data_3.6_ESP %>%
  mutate(local = ifelse(Q30_1N > Q30_2N,1,0))%>%
  filter(local == 1)%>%
  filter(!is.na(information))%>%
  group_by(information)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)

data_3.6.3_FRA <- data_3.6_FRA %>%
  mutate(local = ifelse(Q30_1N > Q30_2N,1,0))%>%
  filter(local == 1)%>%
  filter(!is.na(Rank_weighted))%>% # TBD
  filter(!is.na(information))%>%
  group_by(information)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)

data_3.6.3_GER <- data_3.6_GER %>%
  mutate(local = ifelse(Q30_1N > Q30_2N,1,0))%>%
  filter(local == 1)%>%
  filter(!is.na(Rank_weighted))%>% # TBD
  filter(!is.na(information))%>%
  group_by(information)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)

data_3.6.3_ROM <- data_3.6_ROM %>%
  mutate(local = ifelse(Q30_1N > Q30_2N,1,0))%>%
  filter(local == 1)%>%
  filter(!is.na(information))%>%
  group_by(information)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)

# Hypothesis 32: Respondents with higher overall trust in their local government than in their national government will prefer the option 'Preferentially energy project co-owned by local residents' for clean energy subsidy targets. 
data_3.6.4a_ESP <- data_3.6_ESP %>%
  mutate(local = ifelse(Q30_1N > Q30_2N,1,0))%>%
  filter(local == 1)%>%
  filter(!is.na(infrastructure_ownership))%>%
  group_by(infrastructure_ownership)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)

data_3.6.4a_FRA <- data_3.6_FRA %>%
  mutate(local = ifelse(Q30_1N > Q30_2N,1,0))%>%
  filter(local == 1)%>%
  filter(!is.na(Rank_weighted))%>% # TBD
  filter(!is.na(infrastructure_ownership))%>%
  group_by(infrastructure_ownership)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)

data_3.6.4a_GER <- data_3.6_GER %>%
  mutate(local = ifelse(Q30_1N > Q30_2N,1,0))%>%
  filter(local == 1)%>%
  filter(!is.na(Rank_weighted))%>% # TBD
  filter(!is.na(infrastructure_ownership))%>%
  group_by(infrastructure_ownership)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)

data_3.6.4a_ROM <- data_3.6_ROM %>%
  mutate(local = ifelse(Q30_1N > Q30_2N,1,0))%>%
  filter(local == 1)%>%
  filter(!is.na(infrastructure_ownership))%>%
  group_by(infrastructure_ownership)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)

# Hypothesis 33: Respondents with higher overall trust in their national government than in their local government will prefer the option 'Government-owned firms reinvesting profits in the transition'. 
data_3.6.4b_ESP <- data_3.6_ESP %>%
  mutate(national = ifelse(Q30_1N < Q30_2N,1,0))%>%
  filter(national == 1)%>%
  filter(!is.na(infrastructure_ownership))%>%
  group_by(infrastructure_ownership)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)

data_3.6.4b_FRA <- data_3.6_FRA %>%
  mutate(national = ifelse(Q30_1N < Q30_2N,1,0))%>%
  filter(national == 1)%>%
  filter(!is.na(Rank_weighted))%>% # TBD
  filter(!is.na(infrastructure_ownership))%>%
  group_by(infrastructure_ownership)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)

data_3.6.4b_GER <- data_3.6_GER %>%
  mutate(national = ifelse(Q30_1N < Q30_2N,1,0))%>%
  filter(national == 1)%>%
  filter(!is.na(Rank_weighted))%>% # TBD
  filter(!is.na(infrastructure_ownership))%>%
  group_by(infrastructure_ownership)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)

data_3.6.4b_ROM <- data_3.6_ROM %>%
  mutate(national = ifelse(Q30_1N < Q30_2N,1,0))%>%
  filter(national == 1)%>%
  filter(!is.na(infrastructure_ownership))%>%
  group_by(infrastructure_ownership)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)

# Hypothesis 34: Respondents that are unsatisfied with their current access to public transport will prefer all other options to option 'Maintain quality of existing public transport'.

data_3.6.5_ESP <- data_3.6_ESP %>%
  mutate(Bad = ifelse(Q16 %in% c("Muy mala", "Mala"),1,0))%>%
  filter(!is.na(Bad))%>%
  filter(!is.na(community_mobility_support))%>%
  group_by(Bad, community_mobility_support)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)%>%
  group_by(Bad)%>%
  summarise(range = max(ACP) - min(ACP))%>%
  ungroup()

data_3.6.5_FRA <- data_3.6_FRA %>%
  mutate(Bad = ifelse(Q16 %in% c("Très mauvais", "Mauvais"),1,0))%>%
  filter(!is.na(Bad))%>%
  filter(!is.na(community_mobility_support))%>%
  filter(!is.na(Rank_weighted))%>%#TBD
  group_by(Bad, community_mobility_support)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)%>%
  group_by(Bad)%>%
  summarise(range = max(ACP) - min(ACP))%>%
  ungroup()

data_3.6.5_GER <- data_3.6_GER %>%
  mutate(Bad = ifelse(Q16 %in% c("Sehr schlecht", "Schlecht"),1,0))%>%
  filter(!is.na(Bad))%>%
  filter(!is.na(community_mobility_support))%>%
  filter(!is.na(Rank_weighted))%>%#TBD
  group_by(Bad, community_mobility_support)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)%>%
  group_by(Bad)%>%
  summarise(range = max(ACP) - min(ACP))%>%
  ungroup()

data_3.6.5_ROM <- data_3.6_ROM %>%
  mutate(Bad = ifelse(Q16 %in% c("Foarte proasta", "Proasta"),1,0))%>%
  filter(!is.na(Bad))%>%
  filter(!is.na(community_mobility_support))%>%
  group_by(Bad, community_mobility_support)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)%>%
  group_by(Bad)%>%
  summarise(range = max(ACP) - min(ACP))%>%
  ungroup()

# Hypothesis 35: The preferences of respondents that work in more carbon-intensive sectors are more strongly influenced by the attribute 'support for workers' compared to respondents working in less carbon-intensive sectors.
# TBD

# Hypothesis 36: The preferences of respondents having a higher carbon intensity of consumption are more strongly influenced by the attribute 'Support for households' than for other households.
data_3.6.7_ESP <- data_3.6_ESP %>%
  mutate(Higher = ifelse(Quintile > 3,1,0))%>%
  filter(!is.na(Rank_weighted))%>%#TBD
  group_by(Higher, household_support)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)%>%
  group_by(Higher)%>%
  summarise(range = max(ACP) - min(ACP))%>%
  ungroup()

data_3.6.7_FRA <- data_3.6_FRA %>%
  mutate(Higher = ifelse(Quintile > 3,1,0))%>%
  filter(!is.na(Rank_weighted))%>%#TBD
  group_by(Higher, household_support)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)%>%
  group_by(Higher)%>%
  summarise(range = max(ACP) - min(ACP))%>%
  ungroup()

data_3.6.7_GER <- data_3.6_GER %>%
  mutate(Higher = ifelse(Quintile > 3,1,0))%>%
  filter(!is.na(Rank_weighted))%>%#TBD
  group_by(Higher, household_support)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)%>%
  group_by(Higher)%>%
  summarise(range = max(ACP) - min(ACP))%>%
  ungroup()

data_3.6.7_ROM <- data_3.6_ROM %>%
  mutate(Higher = ifelse(Quintile > 3,1,0))%>%
  filter(!is.na(Higher))%>%
  group_by(Higher, household_support)%>%
  summarise(mean = mean(Rank_weighted))%>%
  ungroup()%>%
  mutate(ACP = mean - 0.5)%>%
  group_by(Higher)%>%
  summarise(range = max(ACP) - min(ACP))%>%
  ungroup()

# 3.7   Hypotheses 37 to 41 (Part III) ####

data_3.7_GER <- data_3_GER %>%
  select(ID, Q71_4, Q71_7, Q71_8, Q30_2N, Q30_3N, Q38)%>%
  # TBD
  rename(Q71_1 = Q71_4, Q71_2 = Q71_7, Q71_3 = Q71_8)%>%
  # rename(Q71_1 = Q71_4)%>%
  filter(!is.na(Q71_1)&!is.na(Q71_2)&!is.na(Q71_3))%>%
  mutate_at(vars(Q71_1:Q71_3), ~ as.numeric(.))%>%
  mutate(nA_B = ifelse(Q71_1 < Q71_2,1,0))

data_3.7_FRA <- data_3_FRA %>%
  select(ID, Q71_1, Q71_2, Q71_3, Q30_2N, Q30_3N, Q38)%>%
  filter(!is.na(Q71_1)&!is.na(Q71_2)&!is.na(Q71_3))%>%
  mutate_at(vars(Q71_1:Q71_3), ~ as.numeric(.))%>%
  mutate(nA_B = ifelse(Q71_1 < Q71_2,1,0))

# Hypothesis 37: Respondents prefer option A over option B.
t.test(data_3.7_FRA$Q71_1, data_3.7_FRA$Q71_2, paired = TRUE, alternative = "less")
t.test(data_3.7_GER$Q71_1, data_3.7_GER$Q71_2, paired = TRUE, alternative = "less")

# Hypothesis 38: Respondents that do not trust their national government will prefer option C to both option A and option B.
data_3.7.1_GER <- data_3.7_GER %>%
  filter(Q30_2N < 3)

data_3.7.1_FRA <- data_3.7_FRA %>%
  filter(Q30_2N < 3)

t.test(data_3.7.1_FRA$Q71_3, data_3.7.1_FRA$Q71_1, paired = TRUE, alternative = "less")
t.test(data_3.7.1_GER$Q71_3, data_3.7.1_GER$Q71_1, paired = TRUE, alternative = "less")
t.test(data_3.7.1_FRA$Q71_3, data_3.7.1_FRA$Q71_2, paired = TRUE, alternative = "less")
t.test(data_3.7.1_GER$Q71_3, data_3.7.1_GER$Q71_2, paired = TRUE, alternative = "less")

# Hypothesis 39: Respondents that indicate political leaning towards right-wing extremist parties will prefer option C to both option A and option B.
data_3.7.2_GER <- data_3.7_GER %>%
  filter(Q38 == "AfD")

data_3.7.2_FRA <- data_3.7_FRA %>%
  filter(Q38 == "Extrême droite (RN etc...)")

t.test(data_3.7.2_FRA$Q71_3, data_3.7.2_FRA$Q71_1, paired = TRUE, alternative = "less")
t.test(data_3.7.2_GER$Q71_3, data_3.7.2_GER$Q71_1, paired = TRUE, alternative = "less")
t.test(data_3.7.2_FRA$Q71_3, data_3.7.2_FRA$Q71_2, paired = TRUE, alternative = "less")
t.test(data_3.7.2_GER$Q71_3, data_3.7.2_GER$Q71_2, paired = TRUE, alternative = "less")

# Hypothesis 40: Respondents that indicate political leaning towards right-wing extremist parties will be more likely to prefer A to option B than others
data_3.7.3_GER <- data_3.7_GER %>%
  mutate(A_B = Q71_1 - Q71_2,
         RW  = ifelse(Q38 == "AfD",1,0))

data_3.7.3_FRA <- data_3.7_FRA %>%
  mutate(A_B = Q71_1 - Q71_2,
         RW  = ifelse(Q38 == "Extrême droite (RN etc...)",1,0))

# A_B is more likely to be negative for RW = 1
t.test(A_B ~ RW, data = data_3.7.3_FRA, alternative = "less")
t.test(A_B ~ RW, data = data_3.7.3_GER, alternative = "less")

# Hypothesis 41: Respondents that have a higher trust in their national government thatn in the EU commission will prefer option A to option B.
data_3.7.4_GER <- data_3.7_GER %>%
  mutate(Filter = ifelse(Q30_2N > Q30_3N,1,0))%>%
  filter(Filter == 1)

data_3.7.4_FRA <- data_3.7_FRA %>%
  mutate(Filter = ifelse(Q30_2N > Q30_3N,1,0))%>%
  filter(Filter == 1)

t.test(data_3.7.4_FRA$Q71_1, data_3.7.4_FRA$Q71_2, paired = TRUE, alternative = "less")
t.test(data_3.7.4_GER$Q71_1, data_3.7.4_GER$Q71_2, paired = TRUE, alternative = "less")
