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
data_0_ESP <- read_csv("../2_Data/0_Qualtrics_Output/20250826_Pilot/Spanish/Spanish_28.Oktober2025_06.50.csv")
data_0_FRA <- read_csv("../2_Data/0_Qualtrics_Output/20250826_Pilot/French/French_28.Oktober2025_06.47.csv")
data_0_GER <- read_csv("../2_Data/0_Qualtrics_Output/20250826_Pilot/German/German_28.Oktober2025_06.50.csv")
data_0_ROM <- read_csv("../2_Data/0_Qualtrics_Output/20250826_Pilot/Romanian/Romanian_28.Oktober2025_06.49.csv")

com_0_ESP <- read_parquet("../2_Data/Output/Output data/Combinations_Qualtrics_Spain_250703.parquet")
com_0_FRA <- read_parquet("../2_Data/Output/Output data/Combinations_Qualtrics_France_250703.parquet")
com_0_GER <- read_parquet("../2_Data/Output/Output data/Combinations_Qualtrics_Germany_250703.parquet")
# com_0_ROM <- read_parquet("../2_Data/Output/Output data/Combinations_Qualtrics_Romania_250703.parquet")
com_0_ROM <- read_parquet("../2_Data/Output/Output data/Combinations_Qualtrics_Romania_250916.parquet")

median_costs <- read.xlsx("../2_Data/Supplementary/Median_Costs_Countries.xlsx")

# 1     Data transformation ####
# 1.1   Spain ####

data_1_ESP <- data_0_ESP %>%
  filter(Status == "IP-Adresse")%>%
  # Convert date
  mutate(StartDate = as.POSIXct(StartDate, format = "%Y-%m-%d %H:%M:%S"))%>%
  mutate(Date = format(StartDate, "%Y-%m-%d"))%>%
  filter(as.Date(Date) > as.Date("2025-08-18"))%>%   # TBA
  # Create basic columns
  mutate(Country = "Spain")%>%
  mutate(ID = 1:n())%>%
  select(Country, ID, everything())%>%
  rename(time = "Duration (in seconds)")%>%
  filter(Q02 != "No")%>%
  filter(Finished == "Wahr")%>%
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
  select(Country, ID, Q31_1:Q34_3)%>%
  pivot_longer(Q31_1:Q34_3, names_to = "Variable", values_to = "Value_raw")%>%
  filter(!is.na(Value_raw))%>% # TBA
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
  select(-(Q31_1:Q34_3), -(Institution1:Label5))%>%
  left_join(data_1.1_ESP)%>%
  select(Country:Q30_3, starts_with("Q31"), starts_with("Q32"), starts_with("Q33"), starts_with("Q34"), everything())

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
         Q11 = "gender", Q28 = "hh_expenditures")%>%
  mutate_at(vars(Q20:age_hhh), ~ as.character(.))

data_1.4_ESP <- data_1.3_ESP %>%
  mutate(age_hhh = ifelse(Q10 == "Más de 63 años", "más de 63 años",
                          ifelse(Q10 %in% c("Entre 45 y 54 años", "Entre 55 y 63 años"), "47 a 62 años", "hasta 46 años")),
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

rm(data_1_ESP, data_1.1_ESP, data_1.1.1_ESP, data_1.1.2_ESP, data_1.2_ESP, data_1.2.1_ESP, data_1.3_ESP, data_0_ESP, com_1_ESP, com_0_ESP)

# 1.2   France ####

data_1_FRA <- data_0_FRA %>%
  filter(Status == "IP-Adresse")%>%
  # Convert date
  mutate(StartDate = as.POSIXct(StartDate, format = "%Y-%m-%d %H:%M:%S"))%>%
  mutate(Date = format(StartDate, "%Y-%m-%d"))%>%
  filter(as.Date(Date) > as.Date("2025-08-18"))%>% # TBA
  # Create basic columns
  mutate(Country = "France")%>%
  mutate(ID = 1:n())%>%
  select(Country, ID, everything())%>%
  rename(time = "Duration (in seconds)")%>%
  filter(Q02 != "Non")%>%
  filter(Finished == "Wahr")%>%
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
  select(Country, ID, Q31_1:Q34_3)%>%
  pivot_longer(Q31_1:Q34_3, names_to = "Variable", values_to = "Value_raw")%>%
  filter(!is.na(Value_raw))%>% # TBA
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
  select(-(Q31_1:Q34_3), -(Institution1:Label5))%>%
  left_join(data_1.1_FRA)%>%
  select(Country:Q30_3, starts_with("Q31"), starts_with("Q32"), starts_with("Q33"), starts_with("Q34"), everything())

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
  select(-(FairnessPerception:PolicySupport), -"Create New Field or Choose From Dropdown...")%>%
  # mutate_at(vars(Effectiveness_1, Effectiveness_2, Expected_cost_1_true, Expected_cost_2_true), ~ ifelse(is.na(.), "No lo sé",.))%>%
  # Introducing factors
  mutate(Q41_1 = factor(Q41_1, levels = c("Certainement inefficace", "Probablement inefficace", "Probablement efficace", "Certainement efficace")),
         Q41_2 = factor(Q41_2, levels = c("Certainement inefficace", "Probablement inefficace", "Probablement efficace", "Certainement efficace")),
         Q42_1_true = factor(Q42_1_true, levels = c("Moins de ${e://Field/t1}€", "Entre ${e://Field/t1}€ et ${e://Field/t2}€", "Entre ${e://Field/t2}€ et ${e://Field/t3}€", "Entre ${e://Field/t3}€ et ${e://Field/t4}€", "Entre ${e://Field/t4}€ et ${e://Field/t5}€", "Entre ${e://Field/t5}€ et ${e://Field/t6}€", "Plus de ${e://Field/t6}€")),
         Q42_2_true = factor(Q42_2_true, levels = c("Moins de ${e://Field/t1}€", "Entre ${e://Field/t1}€ et ${e://Field/t2}€", "Entre ${e://Field/t2}€ et ${e://Field/t3}€", "Entre ${e://Field/t3}€ et ${e://Field/t4}€", "Entre ${e://Field/t4}€ et ${e://Field/t5}€", "Entre ${e://Field/t5}€ et ${e://Field/t6}€", "Plus de ${e://Field/t6}€")),
         Q43_1 = factor(Q43_1, levels = c("Beaucoup plus qu'un ménage type","Un peu plus qu'un ménage plus","À peu près autant qu’un ménage type", "Un peu moins qu'un ménage plus","Beaucoup moins qu’un ménage type")),
         Q43_2 = factor(Q43_2, levels = c("Beaucoup plus qu'un ménage type","Un peu plus qu'un ménage plus","À peu près autant qu’un ménage type", "Un peu moins qu'un ménage plus","Beaucoup moins qu’un ménage type")),
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

rm(data_1_FRA, data_1.1_FRA, data_1.1.1_FRA, data_1.1.2_FRA, data_1.2_FRA, data_1.2.1_FRA, data_1.3_FRA, data_0_FRA, com_1_FRA, com_0_FRA)

# 1.3   Germany ####

data_1_GER <- data_0_GER %>%
  filter(Status == "IP-Adresse")%>%
  # Convert date
  mutate(StartDate = as.POSIXct(StartDate, format = "%Y-%m-%d %H:%M:%S"))%>%
  mutate(Date = format(StartDate, "%Y-%m-%d"))%>%
  filter(as.Date(Date) > as.Date("2025-08-18"))%>% # TBA
  # Create basic columns
  mutate(Country = "Germany")%>%
  mutate(ID = 1:n())%>%
  select(Country, ID, everything())%>%
  rename(time = "Duration (in seconds)")%>%
  filter(Q02 != "Nein")%>%
  filter(Q10 != "Unter 18")%>%
  filter(Finished == "Wahr")%>%
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
  select(Country, ID, Q31_1:Q34_3)%>%
  pivot_longer(Q31_1:Q34_3, names_to = "Variable", values_to = "Value_raw")%>%
  filter(!is.na(Value_raw))%>%
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
  select(-(Q31_1:Q34_2), -(Institution1:Label5))%>%
  left_join(data_1.1_GER)%>%
  select(Country:Q30_3, starts_with("Q31"), starts_with("Q32"), starts_with("Q33"), starts_with("Q34"), everything())

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
         Q42_2_true = factor(Q42_2_true, levels = c("um weniger als ${e://Field/t1}€", "um ${e://Field/t1}€ bis ${e://Field/t2}€", "um ${e://Field/t2}€ bis ${e://Field/t3}€", "um ${e://Field/t3}€ bis ${e://Field/t4}€", "um ${e://Field/t4}€ bis ${e://Field/t5}€", "um ${e://Field/t5}€ bis ${e://Field/t6}€", "mehr als ${e://Field/t6}€")),
         Q43_1 = factor(Q43_1, levels = c("Viel höher als bei einem durchschnittlichen Haushalt","Etwas höher als bei einem durchschnittlichen Haushalt", "Ungefähr so hoch wie bei einem durchschnittlichen Haushalt", "Etwas niedriger als bei einem durchschnittlichen Haushalt", "Viel niedriger als bei einem durchschnittlichen Haushalt")),
         Q43_2 = factor(Q43_2, levels = c("Viel höher als bei einem durchschnittlichen Haushalt","Etwas höher als bei einem durchschnittlichen Haushalt", "Ungefähr so hoch wie bei einem durchschnittlichen Haushalt", "Etwas niedriger als bei einem durchschnittlichen Haushalt", "Viel niedriger als bei einem durchschnittlichen Haushalt")),
         Q44_1 = factor(Q44_1, levels = c("Sie wird eher schaden", "Sie wird weder schaden noch helfen", "Sie wird eher helfen")),
         Q44_2 = factor(Q44_2, levels = c("Sie wird eher schaden", "Sie wird weder schaden noch helfen", "Sie wird eher helfen")),
         Q45_1 = factor(Q45_1, levels = c("Ich finde sie ungerecht", "Ich finde sie weder gerecht noch ungerecht", "Ich finde Sie gerecht")),
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

rm(data_1_GER, data_1.1_GER, data_1.1.1_GER, data_1.1.2_GER, data_1.2_GER, data_1.2.1_GER, data_1.3_GER, data_0_GER, com_1_GER, com_0_GER)

# 1.4   Romania ####

data_1_ROM <- data_0_ROM %>%
  filter(Status == "IP-Adresse")%>%
  # Convert date
  mutate(StartDate = as.POSIXct(StartDate, format = "%Y-%m-%d %H:%M:%S"))%>%
  mutate(Date = format(StartDate, "%Y-%m-%d"))%>%
  filter(as.Date(Date) > as.Date("2025-08-18"))%>%
  # Create basic columns
  mutate(Country = "Romania")%>%
  mutate(ID = 1:n())%>%
  select(Country, ID, everything())%>%
  rename(time = "Duration (in seconds)")%>%
  filter(Q02 != "Nu")%>%
  filter(Q10 != "Sub 18")%>%
  filter(Finished == "Wahr")%>%
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
  select(Country, ID, Q31_1:Q34_3)%>%
  pivot_longer(Q31_1:Q34_3, names_to = "Variable", values_to = "Value_raw")%>%
  filter(!is.na(Value_raw))%>%
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
  select(-(Q31_1:Q34_3), -(Institution1:Label5))%>%
  left_join(data_1.1_ROM)%>%
  select(Country:Q30_3, starts_with("Q31"), starts_with("Q32"), starts_with("Q33"), starts_with("Q34"), everything())

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
  mutate(absolute_value = as.numeric(str_replace_all(absolute_value, "[€,]", "")),
         relative_value = as.numeric(str_replace_all(relative_value, "%",""))/100)%>%
  select(-(absolute:relative_165))%>%
  left_join(median_costs)%>%
  mutate(above_median = ifelse((absolute_value > median_45 & Priceleveleuro == "45") | (absolute_value > median_85 & Priceleveleuro == "85") | (absolute_value > median_125 & Priceleveleuro == "125"),1,0))

rm(data_1_ROM, data_1.1_ROM, data_1.1.1_ROM, data_1.1.2_ROM, data_1.2_ROM, data_1.2.1_ROM, data_1.3_ROM, data_0_ROM, com_1_ROM, com_0_ROM)

# 1.5   Editing the conjoint experiment data ####

extract_conjoint <- function(data_1.4.0){
  data_1.5 <- data_1.4.0 %>%
    select(ID, starts_with("profile"))%>%
    pivot_longer(starts_with("profile"), names_to = "names", values_to = "values")%>%
    mutate(Profile = case_when(grepl("profileA", names) ~ "A",
                               grepl("profileB", names) ~ "B"),
           Task    = case_when(grepl("task0", names) ~ "0",
                               grepl("task1", names) ~ "1",
                               grepl("task2", names) ~ "2",
                               grepl("task3", names) ~ "3"))%>%
    select(-names)%>%
    mutate(parsed = map(values, \(x) {
      x <- gsub('^"|"$', '', x)                                  # remove outer quotes
      parts <- strsplit(x, "\\}\\s*\\{")[[1]]                    # split multiple JSONs
      parts <- paste0("{", gsub("(^\\{|\\}$)", "", parts), "}")  # fix braces
      jsons <- map(parts, safely(fromJSON))                      # safely parse each
      bind_rows(map(jsons, "result"))                            # combine parsed pieces
    })) %>%
    unnest(parsed)%>%
    select(-values)%>%
    left_join(select(data_1.4.0, ID, starts_with("Q62"), starts_with("Q63"), starts_with("Q64"), starts_with("Q65")))%>%
    select(ID, starts_with("Q"), everything())
  
  return(data_1.5)
}

data_conjoint_ESP <- extract_conjoint(data_1.4_ESP)
data_conjoint_FRA <- extract_conjoint(data_1.4_FRA)
data_conjoint_GER <- extract_conjoint(data_1.4_GER)
data_conjoint_ROM <- extract_conjoint(data_1.4_ROM)

data_1.5_ESP <- data_1.4_ESP %>%
  select(-starts_with("Q62"), -starts_with("Q63"), -starts_with("Q64"), -starts_with("Q65"), -starts_with("profile"))
data_1.5_FRA <- data_1.4_FRA %>%
  select(-starts_with("Q62"), -starts_with("Q63"), -starts_with("Q64"), -starts_with("Q65"), -starts_with("profile"))
data_1.5_GER <- data_1.4_GER %>%
  select(-starts_with("Q62"), -starts_with("Q63"), -starts_with("Q64"), -starts_with("Q65"), -starts_with("profile"))
data_1.5_ROM <- data_1.4_ROM %>%
  select(-starts_with("Q62"), -starts_with("Q63"), -starts_with("Q64"), -starts_with("Q65"), -starts_with("profile"))

rm(data_1.4_ESP, data_1.4_FRA, data_1.4_GER, data_1.4_ROM, median_costs, extract_conjoint)

# 1.6     Create outcomes ####

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
                                                                        ifelse(absolute_value > t6, 7,absolute_value))))))))%>%
    # \tilde{l}
    mutate(Dif_cost_1 = Q42_1N - absolute_t,
           Dif_cost_2 = Q42_2N - absolute_t)%>%
    # \tilde{lr}
    mutate(Percentile_abs = as.numeric(as.character(Percentile)))%>%
    mutate(Quintile = case_when(Percentile_abs<= 20         ~ 5,
                                Percentile_abs> 20 & Percentile_abs<= 40 ~ 4,
                                Percentile_abs> 40 & Percentile_abs<= 60 ~ 3,
                                Percentile_abs> 60 & Percentile_abs<= 80 ~ 2,
                                Percentile_abs> 80          ~ 1))%>%
    mutate(Dif_Percentile_1 = Q43_1N - Quintile,
           Dif_Percentile_2 = Q43_2N - Quintile)
    
    return(data_1.6.0)
}

data_1.6_ESP <- create_outcomes(data_1.5_ESP)
data_1.6_FRA <- create_outcomes(data_1.5_FRA)
data_1.6_GER <- create_outcomes(data_1.5_GER)
data_1.6_ROM <- create_outcomes(data_1.5_ROM)

rm(data_1.5_ESP, data_1.5_GER, data_1.5_FRA, data_1.5_ROM)

# 2.1   Baseline outcome distribution - TBD ####

data_3 <- bind_rows(data_2_ESP, data_2_FRA)%>%
  bind_rows(data_2_GER)%>%
  bind_rows(data_2_ROM)

rm(data_2_ESP, data_2_FRA, data_2_GER, data_2_ROM)

# 2.1.1 Support ####

data_3.1.1 <- data_3 %>%
  group_by(Support_1N, Country)%>%
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
  filter(!is.na(Support_1N))%>%
  mutate(Support_1N_label = case_when(Support_1N == 1 ~ "Strongly\n oppose",
                                      Support_1N == 2 ~ "Rather\n oppose",
                                      Support_1N == 3 ~ "Neutral",
                                      Support_1N == 4 ~ "Rather\n support",
                                      Support_1N == 5 ~ "Strongly\n support"))%>%
  mutate(Support_1N_label = factor(Support_1N_label, levels = c("Strongly\n oppose", "Rather\n oppose", "Neutral", "Rather\n support", "Strongly\n support")))%>%
  mutate(Period = "t=0")

data_3.1.2 <- data_3 %>%
  group_by(Support_2N, Country)%>%
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
  filter(!is.na(Support_2N))%>%
  mutate(Support_1N_label = case_when(Support_2N == 1 ~ "Strongly\n oppose",
                                      Support_2N == 2 ~ "Rather\n oppose",
                                      Support_2N == 3 ~ "Neutral",
                                      Support_2N == 4 ~ "Rather\n support",
                                      Support_2N == 5 ~ "Strongly\n support"))%>%
  mutate(Support_1N_label = factor(Support_1N_label, levels = c("Strongly\n oppose", "Rather\n oppose", "Neutral", "Rather\n support", "Strongly\n support")))%>%
  mutate(Period = "t=1")

data_3.1 <- bind_rows(data_3.1.1, data_3.1.2)%>%
  mutate(Period = factor(Period, levels = c("t=1", "t=0")))

P_0 <- ggplot(data_3.1, aes(y = Period))+
  facet_grid(Country ~ .)+
  theme_bw()+
  geom_col(position = "stack", aes(x = share, fill = fct_rev(Support_1N_label)), colour = "black", width = 0.75)+
  scale_fill_viridis_d(direction = -1, guide = guide_legend(reverse = TRUE, title.position = "top"))+
  labs(fill = "Do you support or oppose this policy?")+
  scale_x_continuous(labels = scales::percent_format())+
  xlab("Share of respondents")+
  ggtitle("Overall policy support")+
  theme(panel.grid  = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 8),
        legend.position = "bottom"
        )

P_1 <- ggplot(data_3.1, aes(x = Support_1N_label, y = Period))+
  facet_grid(Country ~ .)+
  theme_bw()+
  geom_point(aes(fill = share_sum), shape = 22, size = 14)+
  geom_text(aes(label = label_0), size = 4)+
  scale_fill_distiller(limits = c(0,1))+
  xlab("Do you support or oppose this policy?")+
  ggtitle("Overall policy support")+
  guides(fill = "none")+
  theme(panel.grid  = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 8))

jpeg("../2_Data/Figures/Pilot/Figure_1_%d.jpg", width = 12, height = 12, unit = "cm", res = 600)
print(P_0)
print(P_1)
dev.off()

rm(data_3.1, P_1)

# 2.1.2 Effectiveness ####

data_3.2.1 <- data_3 %>%
  group_by(Effectiveness_1N, Country)%>%
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
  filter(!is.na(Effectiveness_1N))%>%
  mutate(Effectiveness_1N_label = case_when(Effectiveness_1N == 1 ~ "Strongly\n disagree",
                                            Effectiveness_1N == 2 ~ "Rather\n disagree",
                                            Effectiveness_1N == 3 ~ "Rather\n agree",
                                            Effectiveness_1N == 4 ~ "Strongly\n agree"))%>%
  mutate(Effectiveness_1N_label = factor(Effectiveness_1N_label, levels = c("Strongly\n disagree", "Rather\n disagree", "Rather\n agree", "Strongly\n agree")))%>%
  mutate(Period = "t=0")

data_3.2.2 <- data_3 %>%
  group_by(Effectiveness_2N, Country)%>%
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
  filter(!is.na(Effectiveness_2N))%>%
  mutate(Effectiveness_1N_label = case_when(Effectiveness_2N == 1 ~ "Strongly\n disagree",
                                            Effectiveness_2N == 2 ~ "Rather\n disagree",
                                            Effectiveness_2N == 3 ~ "Rather\n agree",
                                            Effectiveness_2N == 4 ~ "Strongly\n agree"))%>%
  mutate(Effectiveness_1N_label = factor(Effectiveness_1N_label, levels = c("Strongly\n disagree", "Rather\n disagree", "Rather\n agree", "Strongly\n agree")))%>%
  mutate(Period = "t=1")

data_3.2 <- bind_rows(data_3.2.1, data_3.2.2)%>%
  mutate(Period = factor(Period, levels = c("t=1", "t=0")))

P_2 <- ggplot(data_3.2, aes(x = Effectiveness_1N_label, y = Period))+
  facet_grid(Country ~ .)+
  theme_bw()+
  geom_point(aes(fill = share_sum), shape = 22, size = 14)+
  geom_text(aes(label = label_0), size = 4)+
  scale_fill_distiller(limits = c(0,1))+
  xlab("Do you think this policy will reduce GHG emissions effectively?")+
  ggtitle("Perception of effectiveness")+
  guides(fill = "none")+
  theme(panel.grid  = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 7))

jpeg("../2_Data/Figures/Pilot/Figure_2.jpg", width = 12, height = 12, unit = "cm", res = 600)
print(P_2)
dev.off()

rm(data_3.2, P_2)

# 2.1.3 Expected costs ####

data_3.3.1 <- data_3 %>%
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
  bind_rows(data.frame(Country = factor("France", levels = levels(.$Country)),
                       Cost_estimation = factor("Strongly\n underestimated", levels = levels(.$Cost_estimation)),
                       share_sum = 0, label_0 = "0%"))%>%
  filter(Cost_estimation != "Don't know")%>%
  mutate(Period = "t=0")
  

data_3.3.2 <- data_3 %>%
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
  bind_rows(data.frame(Country = factor("Spain", levels = levels(.$Country)),
                       Cost_estimation = factor("Strongly\n underestimated", levels = levels(.$Cost_estimation)),
                       share_sum = 0, label_0 = "0%"))%>%
  filter(Cost_estimation != "Don't know")%>%
  mutate(Period = "t=1")

data_3.3 <- bind_rows(data_3.3.1, data_3.3.2)%>%
  mutate(Period = factor(Period, levels = c("t=1", "t=0")))

P_3 <- ggplot(data_3.3, aes(x = Cost_estimation, y = Period))+
  facet_grid(Country ~ .)+
  theme_bw()+
  geom_point(aes(fill = share_sum), shape = 22, size = 14)+
  geom_text(aes(label = label_0), size = 4)+
  scale_fill_distiller(limits = c(0,1))+
  xlab("By how much will this policy increase your costs?")+
  ggtitle("Perception of additional costs")+
  guides(fill = "none")+
  theme(panel.grid  = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 7))

jpeg("../2_Data/Figures/Pilot/Figure_3.jpg", width = 12, height = 12, unit = "cm", res = 600)
print(P_3)
dev.off()

rm(data_3.3, P_3)

# 2.1.4 Relative loss ####

data_3.4.1 <- data_3 %>%
  group_by(Relative_loss_1N, Country)%>%
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
  filter(!is.na(Relative_loss_1N))%>%
  mutate(Relative_loss_1N_label = case_when(Relative_loss_1N == 1 ~ "Less than\n average",
                                            Relative_loss_1N == 2 ~ "Average",
                                            Relative_loss_1N == 3 ~ "More than\n average"))%>%
  mutate(Relative_loss_1N_label = factor(Relative_loss_1N_label, levels = c("Less than\n average", "Average", "More than\n average")))%>%
  mutate(Period = "t=0")

data_3.4.2 <- data_3 %>%
  group_by(Relative_loss_2N, Country)%>%
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
  filter(!is.na(Relative_loss_2N))%>%
  mutate(Relative_loss_1N_label = case_when(Relative_loss_2N == 1 ~ "Less than\n average",
                                            Relative_loss_2N == 2 ~ "Average",
                                            Relative_loss_2N == 3 ~ "More than\n average"))%>%
  mutate(Relative_loss_1N_label = factor(Relative_loss_1N_label, levels = c("Less than\n average", "Average", "More than\n average")))%>%
  mutate(Period = "t=1")

data_3.4 <- bind_rows(data_3.4.1, data_3.4.2)%>%
  mutate(Period = factor(Period, levels = c("t=1", "t=0")))

P_4 <- ggplot(data_3.4, aes(x = Relative_loss_1N_label, y = Period))+
  facet_grid(Country ~ .)+
  theme_bw()+
  geom_point(aes(fill = share_sum), shape = 22, size = 14)+
  geom_text(aes(label = label_0), size = 4)+
  scale_fill_distiller(limits = c(0,1))+
  xlab("Must many costs do you expect\n in comparison to an average household?")+
  ggtitle("Perception of additional relative costs")+
  guides(fill = "none")+
  theme(panel.grid  = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 7))

jpeg("../2_Data/Figures/Pilot/Figure_4.jpg", width = 12, height = 12, unit = "cm", res = 600)
print(P_4)
dev.off()

rm(data_3.4, P_4)

# 2.1.5 Vulnerable ####

data_3.5.1 <- data_3 %>%
  group_by(Vulnerable_1N, Country)%>%
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
  filter(!is.na(Vulnerable_1N))%>%
  mutate(Vulnerable_1N_label = case_when(Vulnerable_1N == 1 ~ "Hurt the\n poorest",
                                         Vulnerable_1N == 2 ~ "Neither hurt\n nor help",
                                         Vulnerable_1N == 3 ~ "Help the\n poorest"))%>%
  mutate(Vulnerable_1N_label = factor(Vulnerable_1N_label, levels = c("Hurt the\n poorest", "Neither hurt\n nor help", "Help the\n poorest")))%>%
  mutate(Period = "t=0")

data_3.5.2 <- data_3 %>%
  group_by(Vulnerable_2N, Country)%>%
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
  filter(!is.na(Vulnerable_2N))%>%
  mutate(Vulnerable_1N_label = case_when(Vulnerable_2N == 1 ~ "Hurt the\n poorest",
                                         Vulnerable_2N == 2 ~ "Neither hurt\n nor help",
                                         Vulnerable_2N == 3 ~ "Help the\n poorest"))%>%
  mutate(Vulnerable_1N_label = factor(Vulnerable_1N_label, levels = c("Hurt the\n poorest", "Neither hurt\n nor help", "Help the\n poorest")))%>%
  mutate(Period = "t=1")

data_3.5 <- bind_rows(data_3.5.1, data_3.5.2)%>%
  mutate(Period = factor(Period, levels = c("t=1", "t=0")))

P_5 <- ggplot(data_3.5, aes(x = Vulnerable_1N_label, y = Period))+
  facet_grid(Country ~ .)+
  theme_bw()+
  geom_point(aes(fill = share_sum), shape = 22, size = 14)+
  geom_text(aes(label = label_0), size = 4)+
  scale_fill_distiller(limits = c(0,1))+
  xlab("Will this policy help or hurt the poorest households?")+
  ggtitle("Effects on poor households")+
  guides(fill = "none")+
  theme(panel.grid  = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 7))

jpeg("../2_Data/Figures/Pilot/Figure_5.jpg", width = 12, height = 12, unit = "cm", res = 600)
print(P_5)
dev.off()

rm(data_3.5, P_5)

# 2.1.6 Fairness ####

data_3.6.1 <- data_3 %>%
  group_by(Fairness_1N, Country)%>%
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
  filter(!is.na(Fairness_1N))%>%
  mutate(Fairness_1N_label = case_when(Fairness_1N == 1 ~ "Unfair",
                                       Fairness_1N == 2 ~ "Neither fair\n nor unfair",
                                       Fairness_1N == 3 ~ "Fair"))%>%
  mutate(Fairness_1N_label = factor(Fairness_1N_label, levels = c("Unfair", "Neither fair\n nor unfair", "Fair")))%>%
  mutate(Period = "t=0")

data_3.6.2 <- data_3 %>%
  group_by(Fairness_2N, Country)%>%
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
  filter(!is.na(Fairness_2N))%>%
  mutate(Fairness_1N_label = case_when(Fairness_2N == 1 ~ "Unfair",
                                       Fairness_2N == 2 ~ "Neither fair\n nor unfair",
                                       Fairness_2N == 3 ~ "Fair"))%>%
  mutate(Fairness_1N_label = factor(Fairness_1N_label, levels = c("Unfair", "Neither fair\n nor unfair", "Fair")))%>%
  mutate(Period = "t=1")

data_3.6 <- bind_rows(data_3.6.1, data_3.6.2)%>%
  mutate(Period = factor(Period, levels = c("t=1", "t=0")))

P_6 <- ggplot(data_3.6, aes(x = Fairness_1N_label, y = Period))+
  facet_grid(Country ~ .)+
  theme_bw()+
  geom_point(aes(fill = share_sum), shape = 22, size = 14)+
  geom_text(aes(label = label_0), size = 4)+
  scale_fill_distiller(limits = c(0,1))+
  xlab("Do you find this policy fair or unfair?")+
  ggtitle("Perception of fairness")+
  guides(fill = "none")+
  theme(panel.grid  = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 7))

jpeg("../2_Data/Figures/Pilot/Figure_6.jpg", width = 12, height = 12, unit = "cm", res = 600)
print(P_6)
dev.off()

rm(data_3.6, P_6)

# 2.2   Baseline correlation between policy support and institutional trust ####

data_3.2 <- data_3 %>%
  mutate(Trust = ifelse(Trust_National_N <= 2, "Low trust",
                        ifelse(Trust_National_N == 3, "Medium trust",
                               ifelse(Trust_National_N > 3, "High trust", NA))))%>%
  group_by(Country, Trust, Support_1N)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(Country, Trust)%>%
  mutate(sum = sum(number))%>%
  ungroup()%>%
  mutate(share = number/sum)%>%
  group_by(Country, Trust)%>%
  mutate(share_sum = cumsum(share))%>%
  ungroup()%>%
  mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
  mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
  mutate(Trust   = factor(Trust,   levels = c("Low trust", "Medium trust", "High trust")))%>%
  filter(!is.na(Support_1N) & !is.na(Trust))%>%
  mutate(Support_1N_label = case_when(Support_1N == 1 ~ "Strongly\n oppose",
                                      Support_1N == 2 ~ "Rather\n oppose",
                                      Support_1N == 3 ~ "Neutral",
                                      Support_1N == 4 ~ "Rather\n support",
                                      Support_1N == 5 ~ "Strongly\n support"))%>%
  mutate(Support_1N_label = factor(Support_1N_label, levels = c("Strongly\n oppose", "Rather\n oppose", "Neutral", "Rather\n support", "Strongly\n support")))%>%
  filter(Trust != "Medium trust")

P_2 <- ggplot(data_3.2, aes(x = Support_1N_label, y = Trust))+
  facet_grid(Country ~ .)+
  theme_bw()+
  geom_point(aes(fill = share_sum), shape = 22, size = 14)+
  geom_text(aes(label = label_0), size = 4)+
  scale_fill_distiller(limits = c(0,1))+
  xlab("Do you support or oppose this policy?")+
  ggtitle("Overall policy support - by institutional trust")+
  ylab("Overall trust in national government")+
  guides(fill = "none")+
  theme(panel.grid  = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 8))

jpeg("../2_Data/Figures/Pilot/Figure_B1.jpg", width = 12, height = 12, unit = "cm", res = 600)
print(P_2)
dev.off()

rm(data_3.2, P_2)


# 2.3   Treatment effects (A,B,C1 to C4) on overall policy support ####

tex.style <- style.tex(model.title = "", fixef.title = "\\midrule Fixed Effects",
                       stats.title = "\\midrule", model.format = "",
                       fontsize = "small", yesNo = c("Yes","No"))

dict_latex <- c(Support_1N = "Support (1-5)", 
                "Treatment_A" = "Treatment A", "B_Post" = "Treatment B*Post", 
                "C1_Post" = "Treatment C1*Post", 
                "C2_Post" = "Treatment C2*Post",
                "C3_Post" = "Treatment C3*Post",
                "C4_Post" = "Treatment C4*Post",
                treatment = "Treatment")

data_3.3 <- data_3

model_A <- feols(Support_1N ~ i(Treatment_A, ref = "nonEU"), split = ~ Country, data = data_3.3)

etable(model_A, tex = TRUE, dict = dict_latex,
       file = "../2_Data/Figures/Pilot/Table_HA.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Treatment A on overall policy support",  
       label = "tab:HA", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!",
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from an OLS regression on the support for the fit-for-55 policy package over receiving treatment A, which puts emphasis on the role of the EU. 
                        The dependent variable expresses support on a five-point Lickert-scale."))
)


data_3.3.1 <- data_3.3 %>%
  select(Country, ID, Support_1N, Support_2N)%>%
  pivot_longer(Support_1N:Support_2N, names_to = "Period_0", values_to = "Support", names_prefix = "Support_")%>%
  left_join(select(data_3.3, Country, ID, Treatment_B, Treatment_C))%>%
  mutate(B_Post  = ifelse(Treatment_B == "Treatment" & Period_0 == "2N",1,0),
         C1_Post = ifelse(Treatment_C == "C1" & Period_0 == "2N",1,0),
         C2_Post = ifelse(Treatment_C == "C2" & Period_0 == "2N",1,0),
         C3_Post = ifelse(Treatment_C == "C3" & Period_0 == "2N",1,0),
         C4_Post = ifelse(Treatment_C == "C4" & Period_0 == "2N",1,0))

model_BC <- feols(Support ~ B_Post + C1_Post + C2_Post + C3_Post + C4_Post | ID + Period_0, data = data_3.3.1, split = ~ Country)
# model_BC <- feols(Support ~ B_Post | ID + Period_0, data = data_3.3.1, split = ~ Country)

etable(model_BC, tex = TRUE, dict = dict_latex,
       file = "../2_Data/Figures/Pilot/Table_HBC.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Treatment B\\textsubscript{1} and C\\textsubscript{1} to C\\textsubscript{4} on overall policy support",  
       label = "tab:HBC", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!",
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from an OLS regression on the support for the fit-for-55 policy package over receiving treatment B\\textsubscript{1}, C\\textsubscript{1}, C\\textsubscript{2}, C\\textsubscript{3} or C\\textsubscript{4}. Treatment B\\textsubscript{1} provides information about the mechanisms of carbon pricing.
                 Treatments C\\textsubscript{1} to C\\textsubscript{4} provide respondent-level information about the resulting additional costs. The dependent variable expresses support on a five-point Lickert-scale."))
)


# 3     Hypothesis tests ####

data_3_ESP <- data_1.6_ESP
data_3_FRA <- data_1.6_FRA
data_3_GER <- data_1.6_GER
data_3_ROM <- data_1.6_ROM

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
model_3.1_FRA <- feols(c(Q41_1N, Q46_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1) ~ sw(Q30_2N, Q30_3N), data = data_3_FRA)
model_3.1_GER <- feols(c(Q41_1N, Q46_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1) ~ sw(Q30_2N, Q30_3N), data = data_3_GER)
model_3.1_ROM <- feols(c(Q41_1N, Q46_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1) ~ sw(Q30_2N, Q30_3N), data = data_3_ROM)

# P-values not yet adjusted for one-sided t-test and they are not yet BH-corrected

rm(model_3.1_ESP, model_3.1_FRA, model_3.1_GER, model_3.1_ROM)

# 3.2   Hypothesis 7 ####

model_3.2_ESP <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1) ~ i(Treatment_A, ref = "nonEU"), data = data_3_ESP)
model_3.2_FRA <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1) ~ i(Treatment_A, ref = "nonEU"), data = data_3_FRA)
model_3.2_GER <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1) ~ i(Treatment_A, ref = "nonEU"), data = data_3_GER)
model_3.2_ROM <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1) ~ i(Treatment_A, ref = "nonEU"), data = data_3_ROM)

# P-values not yet adjusted for one-sided t-test and they are not yet BH-corrected

adjust_hypothesis_7b <- function(data_3_0){
  data_3_1 <- data_3_0 %>%
    mutate(tau = ifelse(Q30_3N < 3,1,0))%>%
    mutate(tau_Treatment_A = ifelse(tau == 1 & Treatment_A == "EU",1,0))
  
  return(data_3_1)
}

model_3.2.1_ESP <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1) ~ i(Treatment_A, ref = "nonEU") + tau + tau_Treatment_A, data = adjust_hypothesis_7b(data_3_ESP))
model_3.2.1_FRA <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1) ~ i(Treatment_A, ref = "nonEU") + tau + tau_Treatment_A, data = adjust_hypothesis_7b(data_3_FRA))
model_3.2.1_GER <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1) ~ i(Treatment_A, ref = "nonEU") + tau + tau_Treatment_A, data = adjust_hypothesis_7b(data_3_GER))
model_3.2.1_ROM <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1) ~ i(Treatment_A, ref = "nonEU") + tau + tau_Treatment_A, data = adjust_hypothesis_7b(data_3_ROM))

rm(adjust_hypothesis_7b)

# 3.3   Hypotheses 8 to 9 ####

adjust_hypothesis_89 <- function(data_3_0, filter_1){
  data_3_3 <- data_3_0 %>%
    select(ID, Treatment_B, Treatment_C, Q30_2N, Q41_1N, Q41_2N, Q45_1N, Q45_2N)%>%
    pivot_longer(Q41_1N:Q45_2N, names_to = "Variable", values_to = "value")%>%
    mutate(Period  = ifelse(Variable %in% c("Q41_1N", "Q45_1N"),1,2),
           Outcome = ifelse(Variable %in% c("Q41_1N", "Q41_2N"), "Effectiveness", "Fairness"))%>%
    mutate(Post_B = ifelse(Period == 2 & Treatment_B == "Treatment",1,0),
           Post_C = ifelse(Period == 2, "Baseline", as.character(Treatment_C)))%>%
    filter(Outcome == filter_1)%>%
    mutate(tau = ifelse(Q30_2N < 3,1,0))%>%
    mutate(tau_Post_B = ifelse(tau == 1 & Post_B == 1,1,0))
  
  return(data_3_3)
}

model_3.3.1_ESP <- feols(value ~ Post_B| ID + Period + Post_C, data = adjust_hypothesis_89(data_3_ESP, "Effectiveness"))
model_3.3.1_FRA <- feols(value ~ Post_B | ID + Period + Post_C, data = adjust_hypothesis_89(data_3_FRA, "Effectiveness"))
model_3.3.1_GER <- feols(value ~ Post_B | ID + Period + Post_C, data = adjust_hypothesis_89(data_3_GER, "Effectiveness"))
model_3.3.1_ROM <- feols(value ~ Post_B | ID + Period + Post_C, data = adjust_hypothesis_89(data_3_ROM, "Effectiveness")) 

model_3.3.2_ESP <- feols(value ~ Post_B | ID + Period + Post_C, data = adjust_hypothesis_89(data_3_ESP, "Fairness"))
model_3.3.2_FRA <- feols(value ~ Post_B | ID + Period + Post_C, data = adjust_hypothesis_89(data_3_FRA, "Fairness"))
model_3.3.2_GER <- feols(value ~ Post_B | ID + Period + Post_C, data = adjust_hypothesis_89(data_3_GER, "Fairness"))
model_3.3.2_ROM <- feols(value ~ Post_B | ID + Period + Post_C, data = adjust_hypothesis_89(data_3_ROM, "Fairness"))

# Hypotheses 8a and 9a

model_3.3.3_ESP <- feols(value ~ tau_Post_B | tau + Post_B + ID + Period + Post_C, data = adjust_hypothesis_89(data_3_ESP, "Effectiveness"))
model_3.3.3_FRA <- feols(value ~ tau_Post_B | tau + Post_B + ID + Period + Post_C, data = adjust_hypothesis_89(data_3_FRA, "Effectiveness"))
model_3.3.3_GER <- feols(value ~ tau_Post_B | tau + Post_B + ID + Period + Post_C, data = adjust_hypothesis_89(data_3_GER, "Effectiveness"))
model_3.3.3_ROM <- feols(value ~ tau_Post_B | tau + Post_B + ID + Period + Post_C, data = adjust_hypothesis_89(data_3_ROM, "Effectiveness"))

model_3.3.4_ESP <- feols(value ~ tau_Post_B | tau + Post_B + ID + Period + Post_C, data = adjust_hypothesis_89(data_3_ESP, "Fairness"))
model_3.3.4_FRA <- feols(value ~ tau_Post_B | tau + Post_B + ID + Period + Post_C, data = adjust_hypothesis_89(data_3_FRA, "Fairness"))
model_3.3.4_GER <- feols(value ~ tau_Post_B | tau + Post_B + ID + Period + Post_C, data = adjust_hypothesis_89(data_3_GER, "Fairness"))
model_3.3.4_ROM <- feols(value ~ tau_Post_B | tau + Post_B + ID + Period + Post_C, data = adjust_hypothesis_89(data_3_ROM, "Fairness"))

rm(model_3.3.1_ESP, model_3.3.1_FRA, model_3.3.1_GER, model_3.3.1_ROM, model_3.3.2_ESP, model_3.3.2_FRA, model_3.3.2_GER, model_3.3.2_ROM,
   model_3.3.3_ESP, model_3.3.3_FRA, model_3.3.3_GER, model_3.3.3_ROM, model_3.3.4_ESP, model_3.3.4_FRA, model_3.3.4_GER, model_3.3.4_ROM)

# 3.4   Hypotheses 10 to 17 ####

adjust_hypothesis_10f <- function(data_3_0, filter_1){
  data_3_4 <- data_3_0 %>%
    select(ID, Treatment_B, Treatment_C, Dif_cost_1, Dif_cost_2, Dif_Percentile_1, Dif_Percentile_2, Q41_1N, Q41_2N, Q44_1N, Q44_2N, Q45_1N, Q45_2N)%>%
    # Overestimated/underestimated
    mutate(Overestimated_Absolute = ifelse(Dif_cost_1 > 0, "Overestimated",
                                           ifelse(Dif_cost_1 < 0, "Underestimated", NA)))%>%
    mutate(Overestimated_Distribution = ifelse(Dif_Percentile_1 > 0, "Overestimated",
                                           ifelse(Dif_Percentile_1 < 0, "Underestimated", NA)))%>%
    pivot_longer(Dif_cost_1:Q45_2N, names_to = "Variable", values_to = "value")%>%
    mutate(Period  = ifelse(Variable %in% c("Q41_1N", "Q44_1N", "Q45_1N", "Dif_cost_1", "Dif_Percentile_1"),1,2),
           Outcome = case_when(Variable %in% c("Q41_1N", "Q41_2N") ~ "Effectiveness",
                               Variable %in% c("Q44_1N", "Q44_2N") ~ "Vulnerable",
                               Variable %in% c("Q45_1N", "Q45_2N") ~ "Fairness",
                               Variable %in% c("Dif_Percentile_1", "Dif_Percentile_2") ~ "Distribution_costs",
                               Variable %in% c("Dif_cost_1", "Dif_cost_2")             ~ "Absolute_costs"))%>%
    mutate(Post_B   = ifelse(Period == 2 & Treatment_B == "Treatment",1,0),
           Post_C1  = ifelse(Period == 2 & Treatment_C == "C1",1,0),
           Post_C2  = ifelse(Period == 2 & Treatment_C == "C2",1,0),
           Post_C3  = ifelse(Period == 2 & Treatment_C == "C3",1,0),
           Post_C4  = ifelse(Period == 2 & Treatment_C == "C4",1,0),
           Post_C12 = ifelse(Period == 2 & (Treatment_C == "C1" | Treatment_C == "C2"),1,0),
           Post_C34 = ifelse(Period == 2 & (Treatment_C == "C3" | Treatment_C == "C4"),1,0),
           Post_C1234 = ifelse(Period == 2 & Treatment_C != "C5",1,0))%>%
    filter(Outcome == filter_1)%>%
    # Transform into absolute value
    mutate(value_abs = abs(value))
    
  
  return(data_3_4)
}

# Hypothesis 10:

model_3.4.1_ESP <- feols(value_abs ~ Post_C12 | ID + Period + Post_B + Post_C3, data = adjust_hypothesis_10f(data_3_ESP, "Absolute_costs"))
model_3.4.1_FRA <- feols(value_abs ~ Post_C12 | ID + Period + Post_B + Post_C3, data = adjust_hypothesis_10f(data_3_FRA, "Absolute_costs"))
model_3.4.1_GER <- feols(value_abs ~ Post_C12 | ID + Period + Post_B + Post_C3, data = adjust_hypothesis_10f(data_3_GER, "Absolute_costs"))
model_3.4.1_ROM <- feols(value_abs ~ Post_C12 | ID + Period + Post_B + Post_C3, data = adjust_hypothesis_10f(data_3_ROM, "Absolute_costs"))

# Hypothesis 11:

model_3.4.2_ESP <- feols(value_abs ~ Post_C34 | ID + Period + Post_B + Post_C1, data = adjust_hypothesis_10f(data_3_ESP, "Distribution_costs"))
model_3.4.2_FRA <- feols(value_abs ~ Post_C34 | ID + Period + Post_B + Post_C1, data = adjust_hypothesis_10f(data_3_FRA, "Distribution_costs"))
model_3.4.2_GER <- feols(value_abs ~ Post_C34 | ID + Period + Post_B + Post_C1, data = adjust_hypothesis_10f(data_3_GER, "Distribution_costs"))
model_3.4.2_ROM <- feols(value_abs ~ Post_C34 | ID + Period + Post_B + Post_C1, data = adjust_hypothesis_10f(data_3_ROM, "Distribution_costs"))

# Hypothesis 12:

model_3.4.2_ESP_1 <- feols(value ~ Post_C12 | ID + Period + Post_B + Post_C3 + Post_C4, data = filter(adjust_hypothesis_10f(data_3_ESP, "Fairness"), Overestimated_Absolute == "Overestimated"))
model_3.4.2_FRA_1 <- feols(value ~ Post_C12 | ID + Period + Post_B + Post_C3 + Post_C4, data = filter(adjust_hypothesis_10f(data_3_FRA, "Fairness"), Overestimated_Absolute == "Overestimated"))
model_3.4.2_GER_1 <- feols(value ~ Post_C12 | ID + Period + Post_B + Post_C3 + Post_C4, data = filter(adjust_hypothesis_10f(data_3_GER, "Fairness"), Overestimated_Absolute == "Overestimated"))
model_3.4.2_ROM_1 <- feols(value ~ Post_C12 | ID + Period + Post_B + Post_C3 + Post_C4, data = filter(adjust_hypothesis_10f(data_3_ROM, "Fairness"), Overestimated_Absolute == "Overestimated"))

model_3.4.2_ESP_2 <- feols(value ~ Post_C12 | ID + Period + Post_B + Post_C3 + Post_C4, data = filter(adjust_hypothesis_10f(data_3_ESP, "Fairness"), Overestimated_Absolute == "Underestimated"))
model_3.4.2_FRA_2 <- feols(value ~ Post_C12 | ID + Period + Post_B + Post_C3 + Post_C4, data = filter(adjust_hypothesis_10f(data_3_FRA, "Fairness"), Overestimated_Absolute == "Underestimated"))
model_3.4.2_GER_2 <- feols(value ~ Post_C12 | ID + Period + Post_B + Post_C3 + Post_C4, data = filter(adjust_hypothesis_10f(data_3_GER, "Fairness"), Overestimated_Absolute == "Underestimated"))
model_3.4.2_ROM_2 <- feols(value ~ Post_C12 | ID + Period + Post_B + Post_C3 + Post_C4, data = filter(adjust_hypothesis_10f(data_3_ROM, "Fairness"), Overestimated_Absolute == "Underestimated"))

# Hypothesis 13:

model_3.4.3_ESP_1 <- feols(value ~ Post_C34 | ID + Period + Post_B + Post_C1 + Post_C2, data = filter(adjust_hypothesis_10f(data_3_ESP, "Fairness"), Overestimated_Distribution == "Overestimated"))
model_3.4.3_FRA_1 <- feols(value ~ Post_C34 | ID + Period + Post_B + Post_C1 + Post_C2, data = filter(adjust_hypothesis_10f(data_3_FRA, "Fairness"), Overestimated_Distribution == "Overestimated"))
model_3.4.3_GER_1 <- feols(value ~ Post_C34 | ID + Period + Post_B + Post_C1 + Post_C2, data = filter(adjust_hypothesis_10f(data_3_GER, "Fairness"), Overestimated_Distribution == "Overestimated"))
model_3.4.3_ROM_1 <- feols(value ~ Post_C34 | ID + Period + Post_B + Post_C1 + Post_C2, data = filter(adjust_hypothesis_10f(data_3_ROM, "Fairness"), Overestimated_Distribution == "Overestimated"))

model_3.4.3_ESP_2 <- feols(value ~ Post_C34 | ID + Period + Post_B + Post_C1 + Post_C2, data = filter(adjust_hypothesis_10f(data_3_ESP, "Fairness"), Overestimated_Distribution == "Underestimated"))
model_3.4.3_FRA_2 <- feols(value ~ Post_C34 | ID + Period + Post_B + Post_C1 + Post_C2, data = filter(adjust_hypothesis_10f(data_3_FRA, "Fairness"), Overestimated_Distribution == "Underestimated"))
model_3.4.3_GER_2 <- feols(value ~ Post_C34 | ID + Period + Post_B + Post_C1 + Post_C2, data = filter(adjust_hypothesis_10f(data_3_GER, "Fairness"), Overestimated_Distribution == "Underestimated"))
model_3.4.3_ROM_2 <- feols(value ~ Post_C34 | ID + Period + Post_B + Post_C1 + Post_C2, data = filter(adjust_hypothesis_10f(data_3_ROM, "Fairness"), Overestimated_Distribution == "Underestimated"))

# Hypothesis 14:

model_3.4.4_ESP <- feols(value ~ Post_C2 + Post_C4 | ID + Period + Post_B + Post_C12 + Post_C34, data = adjust_hypothesis_10f(data_3_ESP, "Effectiveness"))
model_3.4.4_FRA <- feols(value ~ Post_C2 + Post_C4 | ID + Period + Post_B + Post_C12 + Post_C34, data = adjust_hypothesis_10f(data_3_FRA, "Effectiveness"))
model_3.4.4_GER <- feols(value ~ Post_C2 + Post_C4 | ID + Period + Post_B + Post_C12 + Post_C34, data = adjust_hypothesis_10f(data_3_GER, "Effectiveness"))
model_3.4.4_ROM <- feols(value ~ Post_C2 + Post_C4 | ID + Period + Post_B + Post_C12 + Post_C34, data = adjust_hypothesis_10f(data_3_ROM, "Effectiveness"))

# Hypothesis 15:

model_3.4.5_ESP <- feols(value ~ Post_C2 + Post_C4 | ID + Period + Post_B + Post_C12 + Post_C34, data = adjust_hypothesis_10f(data_3_ESP, "Fairness"))
model_3.4.5_FRA <- feols(value ~ Post_C2 + Post_C4 | ID + Period + Post_B + Post_C12 + Post_C34, data = adjust_hypothesis_10f(data_3_FRA, "Fairness"))
model_3.4.5_GER <- feols(value ~ Post_C2 + Post_C4 | ID + Period + Post_B + Post_C12 + Post_C34, data = adjust_hypothesis_10f(data_3_GER, "Fairness"))
model_3.4.5_ROM <- feols(value ~ Post_C2 + Post_C4 | ID + Period + Post_B + Post_C12 + Post_C34, data = adjust_hypothesis_10f(data_3_ROM, "Fairness"))

# Hypothesis 16:

model_3.4.6_ESP <- feols(value ~ Post_C4 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = adjust_hypothesis_10f(data_3_ESP, "Vulnerable"))
model_3.4.6_FRA <- feols(value ~ Post_C4 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = adjust_hypothesis_10f(data_3_FRA, "Vulnerable"))
model_3.4.6_GER <- feols(value ~ Post_C4 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = adjust_hypothesis_10f(data_3_GER, "Vulnerable"))
model_3.4.6_ROM <- feols(value ~ Post_C4 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = adjust_hypothesis_10f(data_3_ROM, "Vulnerable"))

# Hypothesis 17:

model_3.4.7_ESP <- feols(value ~ Post_C2 + Post_C4 | ID + Period + Post_B + Post_C1 + Post_C3, data = adjust_hypothesis_10f(data_3_ESP, "Fairness"))
model_3.4.7_FRA <- feols(value ~ Post_C2 + Post_C4 | ID + Period + Post_B + Post_C1 + Post_C3, data = adjust_hypothesis_10f(data_3_FRA, "Fairness"))
model_3.4.7_GER <- feols(value ~ Post_C2 + Post_C4 | ID + Period + Post_B + Post_C1 + Post_C3, data = adjust_hypothesis_10f(data_3_GER, "Fairness"))
model_3.4.7_ROM <- feols(value ~ Post_C2 + Post_C4 | ID + Period + Post_B + Post_C1 + Post_C3, data = adjust_hypothesis_10f(data_3_ROM, "Fairness"))


# 3.5   Hypotheses 18 to 22 ####
# 3.6   Hypotheses 23 to 29 (Conjoint) ####
# 3.7   Hypotheses 30 to 34 (Part III) ####