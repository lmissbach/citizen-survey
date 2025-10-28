# 0     General ####
# Author: L. Missbach (missbach@mcc-berlin.net)
# Date: 26th of August 2025

# 0.1   Packages ####

if(!require("pacman")) install.packages("pacman")

p_load("arrow", "boot", "broom", "extrafont", "fixest", "ggpubr", "ggrepel",
       "ggsci", "Hmisc", "jsonlite", "knitr", "kableExtra", "openxlsx", "rattle", "readxl", "scales", "showtext", "stringi", "tidymodels", "tidyverse", "xtable")

options(scipen=999)

# 0.2   Load data ####

data_0_ESP <- read_csv("../2_Data/0_Qualtrics_Output/20250826_Pilot/Spanish/Spanish_26. August 2025_04.16.csv")
data_0_FRA <- read_csv("../2_Data/0_Qualtrics_Output/20250826_Pilot/French/French_26. August 2025_04.19.csv")
data_0_GER <- read_csv("../2_Data/0_Qualtrics_Output/20250826_Pilot/German/German_26. August 2025_04.18.csv")
data_0_ROM <- read_csv("../2_Data/0_Qualtrics_Output/20250826_Pilot/Romanian/Romanian_26. August 2025_04.18.csv")

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
  filter(as.Date(Date) > as.Date("2025-08-18"))%>%
  # Create basic columns
  mutate(Country = "Spain")%>%
  mutate(ID = 1:n())%>%
  select(Country, ID, everything())%>%
  rename(time = "Duration (in seconds)")%>%
  filter(consent != "No")%>%
  # Order of columns
  select(Country, ID, time, Age:Date)

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
  select(Country, ID, Integrity_1:Responsiveness_3)%>%
  pivot_longer(Integrity_1:Responsiveness_3, names_to = "Variable", values_to = "Value_raw")%>%
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
  select(-(Integrity_1:Responsiveness_3), -(Institution1:Label5))%>%
  left_join(data_1.1_ESP)%>%
  select(Country:Q41_3, starts_with("Integrity"), starts_with("Competence"), starts_with("Representativeness"), starts_with("Responsiveness"), everything())%>%
  # Renaming of some columns
  rename("Trust_information_1" = "Trust in information_1", "Trust_information_2" = "Trust in information_2", "Trust_information_3" = "Trust in information_3", "Trust_information_4" = "Trust in information_4",
         "CC_concern" = "Climate change conce",
         "Effectiveness_1" = "Effectiveness 1", "Expected_cost_1" = "Expected cost 1", "Relative_loss_1" = "Relative loss 1", "Vulnerable_1" = "Vulnerable 1", "Fairness_1" = "Fairness 1", "Support_1" = "Support 1",
         "Effectiveness_2" = "Q56",             "Expected_cost_2" = "Expected cost 2", "Relative_loss_2" = "Relative loss 2", "Vulnerable_2" = "Vulnerable 2", "Fairness_2" = "Fairness 2", "Support_2" = "Support 2")

# Cost range
data_1.2.1_ESP <- data_1.2_ESP %>%
  select(Country, ID, Expected_cost_1, Expected_cost_2)%>%
  pivot_longer(Expected_cost_1:Expected_cost_2)%>%
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
  rename("Expected_cost_1_true" = "Expected_cost_1", "Expected_cost_2_true" = "Expected_cost_2", policy = "Policy description")%>%
  left_join(data_1.2.1_ESP)%>%
  select(Country:Expected_cost_1_true, Expected_cost_1, Relative_loss_1:Expected_cost_2_true, Expected_cost_2, everything())%>%
  # Remove some columns
  select(-"Message reception_6_TEXT", -(heating:spending), -(FairnessPerception:profileB_task3))%>%
  # mutate_at(vars(Effectiveness_1, Effectiveness_2, Expected_cost_1_true, Expected_cost_2_true), ~ ifelse(is.na(.), "No lo sé",.))%>%
  # Introducing factors
  mutate(Effectiveness_1 = factor(Effectiveness_1, levels = c("En absoluto eficaz", "Probablemente no sea eficaz", "Probablemente eficaz", "Sin duda eficaz")),
         Effectiveness_2 = factor(Effectiveness_2, levels = c("En absoluto eficaz", "Probablemente no sea eficaz", "Probablemente eficaz", "Sin duda eficaz")),
         Expected_cost_1_true = factor(Expected_cost_1_true, levels = c("Menos de ${e://Field/t1}€", "Entre ${e://Field/t1}€ y ${e://Field/t2}€", "Entre ${e://Field/t2}€ y ${e://Field/t3}€", "Entre ${e://Field/t3}€ y ${e://Field/t4}€", "Entre ${e://Field/t4}€ y ${e://Field/t5}€", "Entre ${e://Field/t5}€ y ${e://Field/t6}€", "Más de ${e://Field/t6}€")),
         Expected_cost_2_true = factor(Expected_cost_2_true, levels = c("Menos de ${e://Field/t1}€", "Entre ${e://Field/t1}€ y ${e://Field/t2}€", "Entre ${e://Field/t2}€ y ${e://Field/t3}€", "Entre ${e://Field/t3}€ y ${e://Field/t4}€", "Entre ${e://Field/t4}€ y ${e://Field/t5}€", "Entre ${e://Field/t5}€ y ${e://Field/t6}€", "Más de ${e://Field/t6}€")),
         Relative_loss_1 = factor(Relative_loss_1, levels = c("Menos que a un hogar típico", "Más o menos lo mismo que a un hogar típico", "Más que a un hogar típico")),
         Relative_loss_2 = factor(Relative_loss_2, levels = c("Menos que a un hogar típico", "Más o menos lo mismo que a un hogar típico", "Más que a un hogar típico")),
         Vulnerable_1 = factor(Vulnerable_1, levels = c("Los perjudicará", "Ni los ayudará ni los perjudicará", "Los ayudará")),
         Vulnerable_2 = factor(Vulnerable_2, levels = c("Los perjudicará", "Ni los ayudará ni los perjudicará", "Los ayudará")),
         Fairness_1 = factor(Fairness_1, levels = c("Injusta", "Ni justa ni injusta", "Justa")),
         Fairness_2 = factor(Fairness_2, levels = c("Injusta", "Ni justa ni injusta", "Justa")),
         Support_1 = factor(Support_1, levels = c("Me opongo firmemente", "Me opongo en parte", "Ni la apoyo ni me opongo", "La apoyo en parte", "La apoyo firmemente")),
         Support_2 = factor(Support_2, levels = c("Me opongo firmemente", "Me opongo en parte", "Ni la apoyo ni me opongo", "La apoyo en parte", "La apoyo firmemente")),
         Trust_National = factor(Q41_1, levels = c("En absoluto", "Un poco", "Algo", "Bastante", "Completamente")))%>%
  mutate(PedTreatmentGrp = ifelse(PedTreatmentGrp == "Treat", "Treatment", PedTreatmentGrp))%>%
  mutate(Treatment_A = factor(policy, levels = c("EU", "nonEU")),
         Treatment_B = factor(PedTreatmentGrp,  levels = c("Control", "Treatment")),
         Treatment_C = factor(CostTreatmentGrp, labels = c("C1", "C2", "C3", "C4", "Control")))%>%
  select(-PedTreatmentGrp, -CostTreatmentGrp,-policy)%>%
  arrange(ID)

# Join cost estimates 

com_1_ESP <- com_0_ESP %>%
  rename("Heating" = "heating_fuel", "Ownership" = "tenant", "Heating_water" = "water_energy", "Urbanism" = "urban_identif",
         "Gender" = "gender", "Spending" = "hh_expenditures")%>%
  mutate_at(vars(Heating: age_hhh), ~ as.character(.))

data_1.4_ESP <- data_1.3_ESP %>%
  mutate(age_hhh = ifelse(Age == "Más de 63 años", "más de 63 años",
                          ifelse(Age %in% c("Entre 45 y 54 años", "Entre 55 y 63 años"), "47 a 62 años", "hasta 46 años")),
         occupation = ifelse(Employment %in% c("Inactivo (no en búsqueda de trabajo)"), "Dedicado/a a las labores del hogar",
                             ifelse(Employment %in% c("Jubilado"), "Jubilado/a, retirado/a anticipadamente",
                                    ifelse(Employment %in% c("Desempleado (en búsqueda activa de trabajo", "Estudiante"), "Otra situación",
                                           ifelse(Employment %in% c("Prefiero no revelarlo"), "Prefiero no revelarlo", "Trabajando al menos una hora")))),
         district = case_when(Region == "Comunidad Foral de Navarra" ~ "Navarra, Comunidad Foral de",
                              Region == "Comunidad de Madrid" ~ "Madrid, Comunidad de",
                              Region == "Illes Balears" ~ "Balears, Illes",
                              Region == "Principado de Asturias" ~ "Asturias, Principado d",
                              Region == "Región de Murcia" ~ "Murcia, Región de",
                              Region == "La Rioja" ~ "Rioja, La",
                              TRUE ~ Region))%>%
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
  filter(as.Date(Date) > as.Date("2025-08-18"))%>%
  # Create basic columns
  mutate(Country = "France")%>%
  mutate(ID = 1:n())%>%
  select(Country, ID, everything())%>%
  rename(time = "Duration (in seconds)")%>%
  filter(consent != "Non")%>%
  # Order of columns
  select(Country, ID, time, Age:Date)

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
  select(Country, ID, Integrity_1:Responsiveness_3)%>%
  pivot_longer(Integrity_1:Responsiveness_3, names_to = "Variable", values_to = "Value_raw")%>%
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
  select(-(Integrity_1:Responsiveness_3), -(Institution1:Label5))%>%
  left_join(data_1.1_FRA)%>%
  select(Country:Q41_3, starts_with("Integrity"), starts_with("Competence"), starts_with("Representativeness"), starts_with("Responsiveness"), everything())%>%
  # Renaming of some columns
  rename("Trust_information_1" = "Trust in information_1", "Trust_information_2" = "Trust in information_2", "Trust_information_3" = "Trust in information_3", "Trust_information_4" = "Trust in information_4",
         "CC_concern" = "Climate change conce",
         "Effectiveness_1" = "Effectiveness 1", "Expected_cost_1" = "Expected cost 1", "Relative_loss_1" = "Relative loss 1", "Vulnerable_1" = "Vulnerable 1", "Fairness_1" = "Fairness 1", "Support_1" = "Support 1",
         "Effectiveness_2" = "Effectiveness 2", "Expected_cost_2" = "Expected cost 2", "Relative_loss_2" = "Relative loss 2", "Vulnerable_2" = "Vulnerable 2", "Fairness_2" = "Fairness 2", "Support_2" = "Support 2")

# Cost range
data_1.2.1_FRA <- data_1.2_FRA %>%
  select(Country, ID, Expected_cost_1, Expected_cost_2)%>%
  pivot_longer(Expected_cost_1:Expected_cost_2)%>%
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
  rename("Expected_cost_1_true" = "Expected_cost_1", "Expected_cost_2_true" = "Expected_cost_2", policy = "Policy description")%>%
  left_join(data_1.2.1_FRA)%>%
  select(Country:Expected_cost_1_true, Expected_cost_1, Relative_loss_1:Expected_cost_2_true, Expected_cost_2, everything())%>%
  # Remove some columns
  select(-"Message reception_6_TEXT", -(heating:spending), -(FairnessPerception:profileB_task3))%>%
  # mutate_at(vars(Effectiveness_1, Effectiveness_2, Expected_cost_1_true, Expected_cost_2_true), ~ ifelse(is.na(.), "No lo sé",.))%>%
  # Introducing factors
  mutate(Effectiveness_1 = factor(Effectiveness_1, levels = c("Certainement inefficace", "Probablement inefficace", "Probablement efficace", "Certainement efficace")),
         Effectiveness_2 = factor(Effectiveness_2, levels = c("Certainement inefficace", "Probablement inefficace", "Probablement efficace", "Certainement efficace")),
         Expected_cost_1_true = factor(Expected_cost_1_true, levels = c("Moins de ${e://Field/t1}€", "Entre ${e://Field/t1}€ et ${e://Field/t2}€", "Entre ${e://Field/t2}€ et ${e://Field/t3}€", "Entre ${e://Field/t3}€ et ${e://Field/t4}€", "Entre ${e://Field/t4}€ et ${e://Field/t5}€", "Entre ${e://Field/t5}€ et ${e://Field/t6}€", "Plus de ${e://Field/t6}€")),
         Expected_cost_2_true = factor(Expected_cost_2_true, levels = c("Moins de ${e://Field/t1}€", "Entre ${e://Field/t1}€ et ${e://Field/t2}€", "Entre ${e://Field/t2}€ et ${e://Field/t3}€", "Entre ${e://Field/t3}€ et ${e://Field/t4}€", "Entre ${e://Field/t4}€ et ${e://Field/t5}€", "Entre ${e://Field/t5}€ et ${e://Field/t6}€", "Plus de ${e://Field/t6}€")),
         Relative_loss_1 = factor(Relative_loss_1, levels = c("Moins qu’un ménage type", "À peu près autant qu’un ménage type", "Plus qu’un ménage type")),
         Relative_loss_2 = factor(Relative_loss_2, levels = c("Moins qu’un ménage type", "À peu près autant qu’un ménage type", "Plus qu’un ménage type")),
         Vulnerable_1 = factor(Vulnerable_1, levels = c("Nuisible", "Ni nuisible ni utile", "Utile")),
         Vulnerable_2 = factor(Vulnerable_2, levels = c("Nuisible", "Ni nuisible ni utile", "Utile")),
         Fairness_1 = factor(Fairness_1, levels = c("Injuste", "Ni juste ni injuste", "Juste")),
         Fairness_2 = factor(Fairness_2, levels = c("Injuste", "Ni juste ni injuste", "Juste")),
         Support_1 = factor(Support_1, levels = c("Je suis tout à fait contre", "Je suis plutôt contre", "Je ne suis ni pour ni contre", "Je suis plutôt pour", "Je suis tout à fait pour")),
         Support_2 = factor(Support_2, levels = c("Je suis tout à fait contre", "Je suis plutôt contre", "Je ne suis ni pour ni contre", "Je suis plutôt pour", "Je suis tout à fait pour")),
         Trust_National = factor(Q41_1, levels = c("Pas du tout", "Un peu", "Assez", "Largement", "Totalement")))%>%
  mutate(PedTreatmentGrp = ifelse(PedTreatmentGrp == "Treat", "Treatment", PedTreatmentGrp))%>%
  mutate(Treatment_A = factor(policy, levels = c("EU", "nonEU")),
         Treatment_B = factor(PedTreatmentGrp,  levels = c("Control", "Treatment")),
         Treatment_C = factor(CostTreatmentGrp, labels = c("C1", "C2", "C3", "C4", "Control")))%>%
  select(-PedTreatmentGrp, -CostTreatmentGrp,-policy)%>%
  arrange(ID)

# Join cost estimates 

com_1_FRA <- com_0_FRA %>%
  rename("Heating" = "heating_fuel", "Ownership" = "tenant", "Urbanism" = "urban_type", "Building" = "housing_type", "Province" = "province", "Building_age" = "construction_year",
         "Spending" = "hh_expenditures", "Cars" = "number_of_cars")%>%
  mutate_at(vars(Heating:"Building_age"), ~ as.character(.))

data_1.4_FRA <- data_1.3_FRA %>%
  rename("Building_age" = "Building age")%>%
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
  filter(as.Date(Date) > as.Date("2025-08-18"))%>%
  # Create basic columns
  mutate(Country = "Germany")%>%
  mutate(ID = 1:n())%>%
  select(Country, ID, everything())%>%
  rename(time = "Duration (in seconds)")%>%
  filter(consent != "Nein")%>%
  filter(Age != "Unter 18")%>%
  # Order of columns
  select(Country, ID, time, Age:Date)

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
  select(Country, ID, Integrity_1:Responsiveness_3)%>%
  pivot_longer(Integrity_1:Responsiveness_3, names_to = "Variable", values_to = "Value_raw")%>%
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
  select(-(Integrity_1:Responsiveness_3), -(Institution1:Label5))%>%
  left_join(data_1.1_GER)%>%
  select(Country:Q41_3, starts_with("Integrity"), starts_with("Competence"), starts_with("Representativeness"), starts_with("Responsiveness"), everything())%>%
  # Renaming of some columns
  rename("Trust_information_1" = "Trust in information_1", "Trust_information_2" = "Trust in information_2", "Trust_information_3" = "Trust in information_3", "Trust_information_4" = "Trust in information_4",
         "CC_concern" = "Climate change conce",
         "Effectiveness_1" = "Effectiveness 1", "Expected_cost_1" = "Expected cost 1", "Relative_loss_1" = "Relative loss 1", "Vulnerable_1" = "Vulnerable 1", "Fairness_1" = "Fairness 1", "Support_1" = "Support 1",
         "Effectiveness_2" = "Q56", "Expected_cost_2" = "Expected cost 2", "Relative_loss_2" = "Relative loss 2", "Vulnerable_2" = "Vulnerable 2", "Fairness_2" = "Fairness 2", "Support_2" = "Support 2")

# Cost range
data_1.2.1_GER <- data_1.2_GER %>%
  select(Country, ID, Expected_cost_1, Expected_cost_2)%>%
  pivot_longer(Expected_cost_1:Expected_cost_2)%>%
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
  rename("Expected_cost_1_true" = "Expected_cost_1", "Expected_cost_2_true" = "Expected_cost_2", policy = "Policy description")%>%
  left_join(data_1.2.1_GER)%>%
  select(Country:Expected_cost_1_true, Expected_cost_1, Relative_loss_1:Expected_cost_2_true, Expected_cost_2, everything())%>%
  # Remove some columns
  select(-"Message reception_6_TEXT", -(heating:spending), -(FairnessPerception:profileB_task3))%>%
  # mutate_at(vars(Effectiveness_1, Effectiveness_2, Expected_cost_1_true, Expected_cost_2_true), ~ ifelse(is.na(.), "No lo sé",.))%>%
  # Introducing factors
  mutate(Effectiveness_1 = factor(Effectiveness_1, levels = c("Auf keinen Fall", "Vermutlich nicht", "Vermutlich", "Auf jeden Fall")),
         Effectiveness_2 = factor(Effectiveness_2, levels = c("Auf keinen Fall", "Vermutlich nicht", "Vermutlich", "Auf jeden Fall")),
         Expected_cost_1_true = factor(Expected_cost_1_true, levels = c("um weniger als ${e://Field/t1}€", "um ${e://Field/t1}€ bis ${e://Field/t2}€", "um ${e://Field/t2}€ bis ${e://Field/t3}€", "um ${e://Field/t3}€ bis ${e://Field/t4}€", "um ${e://Field/t4}€ bis ${e://Field/t5}€", "um ${e://Field/t5}€ bis ${e://Field/t6}€", "mehr als ${e://Field/t6}€")),
         Expected_cost_2_true = factor(Expected_cost_2_true, levels = c("um weniger als ${e://Field/t1}€", "um ${e://Field/t1}€ bis ${e://Field/t2}€", "um ${e://Field/t2}€ bis ${e://Field/t3}€", "um ${e://Field/t3}€ bis ${e://Field/t4}€", "um ${e://Field/t4}€ bis ${e://Field/t5}€", "um ${e://Field/t5}€ bis ${e://Field/t6}€", "mehr als ${e://Field/t6}€")),
         Relative_loss_1 = factor(Relative_loss_1, levels = c("Niedriger als bei einem typischen Haushalt", "Ungefähr so hoch wie bei einem typischen Haushalt", "Höher als bei einem durchschnittlichen Haushalt")),
         Relative_loss_2 = factor(Relative_loss_2, levels = c("Niedriger als bei einem typischen Haushal", "Ungefähr so hoch wie bei einem typischen Haushalt", "Höher als bei einem typischen Haushalt")),
         Vulnerable_1 = factor(Vulnerable_1, levels = c("Sie wird eher schaden", "Sie wird weder schaden noch helfen", "Sie wird eher helfen")),
         Vulnerable_2 = factor(Vulnerable_2, levels = c("Sie wird ihnen schaden", "Sie wird ihnen weder schaden noch helfen", "Sie wird ihnen helfen")),
         Fairness_1 = factor(Fairness_1, levels = c("Ich finde sie ungerecht", "Ich finden sie weder gerecht noch ungerecht", "Ich finde Sie gerecht")),
         Fairness_2 = factor(Fairness_2, levels = c("Ich finde sie ungerecht", "Ich finde sie weder gerecht noch ungerecht", "Ich finde sie gerecht")),
         Support_1 = factor(Support_1, levels = c("Ich lehne sie entschieden ab", "Ich bin eher dagegen", "Ich bin weder dafür noch dagegen", "Ich befürworte sie in gewissem Maße", "Ich befürworte sie entschieden")),
         Support_2 = factor(Support_2, levels = c("Ich lehne sie entschieden ab", "Ich bin eher dagegen", "Ich bin weder dafür noch dagegen", "Ich befürworte sie in gewissem Maße", "Ich befürworte sie entschieden")),
         Trust_National = factor(Q41_1, levels = c("Überhaupt nicht", "Ein wenig", "Einigermaßen", "Weitgehend", "Vollständig")))%>%
  mutate(PedTreatmentGrp = ifelse(PedTreatmentGrp == "Treat", "Treatment", PedTreatmentGrp))%>%
  mutate(Treatment_A = factor(policy, levels = c("EU", "nonEU")),
         Treatment_B = factor(PedTreatmentGrp,  levels = c("Control", "Treatment")),
         Treatment_C = factor(CostTreatmentGrp, labels = c("C1", "C2", "C3", "C4", "Control")))%>%
  select(-PedTreatmentGrp, -CostTreatmentGrp,-policy)%>%
  arrange(ID)

# Join cost estimates 

com_1_GER <- com_0_GER %>%
  rename("Heating" = "heating_fuel", "Ownership" = "renting", "Urbanism" = "urban_type", "Building" = "building_type", "Bundesland" = "bundesland", "Building_age" = "building_year",
         "Spending" = "hh_expenditures", "Cars" = "number_of_cars", "House_size" = "space")%>%
  mutate_at(vars(Heating:"House_size"), ~ as.character(.))%>%
  mutate(House_size = case_when(House_size == "59 bis 75 m2"    ~ "59 m2 bis 75 m2",
                                House_size == "76 bis 98 m2"    ~ "76 m2 bis 98 m2",
                                House_size == "99 bis 130 m2"   ~ "99 m2 bis 130 m2",
                                House_size == "mehr als 131 m2" ~ "Mehr als 131 m2",
                                House_size == "bis 58 m2"       ~ "bis 58 m2"))

data_1.4_GER <- data_1.3_GER %>%
  rename("Building_age" = "Building age", "House_size" = "House size")%>%
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
  filter(consent != "Nu")%>%
  filter(Age != "Sub 18")%>%
  # Order of columns
  select(Country, ID, time, Age:Date)

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
  select(Country, ID, Integrity_1:Responsiveness_3)%>%
  pivot_longer(Integrity_1:Responsiveness_3, names_to = "Variable", values_to = "Value_raw")%>%
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
  select(-(Integrity_1:Responsiveness_3), -(Institution1:Label5))%>%
  left_join(data_1.1_ROM)%>%
  select(Country:Q41_3, starts_with("Integrity"), starts_with("Competence"), starts_with("Representativeness"), starts_with("Responsiveness"), everything())%>%
  # Renaming of some columns
  rename("Trust_information_1" = "Trust in information_1", "Trust_information_2" = "Trust in information_2", "Trust_information_3" = "Trust in information_3", "Trust_information_4" = "Trust in information_4",
         "CC_concern" = "Climate change conce",
         "Effectiveness_1" = "Effectiveness 1", "Expected_cost_1" = "Expected cost 1", "Relative_loss_1" = "Relative loss 1", "Vulnerable_1" = "Vulnerable 1", "Fairness_1" = "Fairness 1", "Support_1" = "Support 1",
         "Effectiveness_2" = "Q56", "Expected_cost_2" = "Expected cost 2", "Relative_loss_2" = "Relative loss 2", "Vulnerable_2" = "Vulnerable 2", "Fairness_2" = "Fairness 2", "Support_2" = "Support 2")

# Cost range
data_1.2.1_ROM <- data_1.2_ROM %>%
  select(Country, ID, Expected_cost_1, Expected_cost_2)%>%
  pivot_longer(Expected_cost_1:Expected_cost_2)%>%
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
  rename("Expected_cost_1_true" = "Expected_cost_1", "Expected_cost_2_true" = "Expected_cost_2", policy = "Policy description")%>%
  left_join(data_1.2.1_ROM)%>%
  select(Country:Expected_cost_1_true, Expected_cost_1, Relative_loss_1:Expected_cost_2_true, Expected_cost_2, everything())%>%
  # Remove some columns
  select(-(heating:spending), -(FairnessPerception:profileB_task3))%>%
  mutate_at(vars(Effectiveness_1:Support_1, Effectiveness_2:Support_2, Q41_1), ~ stri_trans_general(., "Latin-ASCII"))%>%
  # mutate_at(vars(Effectiveness_1, Effectiveness_2, Expected_cost_1_true, Expected_cost_2_true), ~ ifelse(is.na(.), "No lo sé",.))%>%
  # Introducing factors
  mutate(Effectiveness_1      = factor(Effectiveness_1,      levels = c("In mod sigur nu va fi eficienta", "Probabil nu va fi eficienta", "Probabil va fi eficienta", "In mod sigur va fi eficienta")),
         Effectiveness_2      = factor(Effectiveness_2,      levels = c("In mod sigur nu va fi eficienta", "Probabil nu va fi eficienta", "Probabil va fi eficienta", "In mod sigur va fi eficienta")),
         Expected_cost_1_true = factor(Expected_cost_1_true, levels = c("Mai putin de ${e://Field/t1} de lei", "Intre ${e://Field/t1} de lei si ${e://Field/t2} de lei", "Intre ${e://Field/t2} de lei si ${e://Field/t3} de lei", "Intre ${e://Field/t3} de lei si ${e://Field/t4} de lei", "Intre ${e://Field/t4} de lei si ${e://Field/t5} de lei", "Intre ${e://Field/t5} de lei si ${e://Field/t6} de lei", "Mai mult de ${e://Field/t6} de lei")),
         Expected_cost_2_true = factor(Expected_cost_2_true, levels = c("Mai putin de ${e://Field/t1} de lei", "Intre ${e://Field/t1} de lei si ${e://Field/t2} de lei", "Intre ${e://Field/t2} de lei si ${e://Field/t3} de lei", "Intre ${e://Field/t3} de lei si ${e://Field/t4} de lei", "Intre ${e://Field/t4} de lei si ${e://Field/t5} de lei", "Intre ${e://Field/t5} de lei si ${e://Field/t6} de lei", "Mai mult de ${e://Field/t6} de lei")),
         Relative_loss_1      = factor(Relative_loss_1,      levels = c("Mai putin decat pe o gospodarie obisnuita", "Cam la fel ca pe o gospodarie obisnuita", "Mai mult decat pe o gospodarie obisnuita")),
         Relative_loss_2      = factor(Relative_loss_2,      levels = c("Mai putin decat pe o gospodarie obisnuita", "Cam la fel ca pe o gospodarie obisnuita", "Mai mult decat pe o gospodarie obisnuita")),
         Vulnerable_1         = factor(Vulnerable_1,         levels = c("Va afecta", "Nici nu va afecta, nici nu va ajuta", "Va ajuta")),
         Vulnerable_2         = factor(Vulnerable_2,         levels = c("Va afecta", "Nici nu va afecta, nici nu va ajuta", "Va ajuta")),
         Fairness_1           = factor(Fairness_1,           levels = c("Incorecta", "Nici corecta nici incorecta", "Corecta")),
         Fairness_2           = factor(Fairness_2,           levels = c("Incorecta", "Nici corecta nici incorecta", "Corecta")),
         Support_1            = factor(Support_1,            levels = c("Ma opun cu tarie", "Ma opun oarecum", "Nici nu o sustin, nici nu ma opun", "O sustin oarecum", "O sustin cu tarie")),
         Support_2            = factor(Support_2,            levels = c("Ma opun cu tarie", "Ma opun oarecum", "Nici nu o sustin, nici nu ma opun", "O sustin oarecum", "O sustin cu tarie")),
         Trust_National       = factor(Q41_1,                levels = c("Deloc", "Putin", "Oarecum", "Mult", "Complet")))%>%
  mutate(PedTreatmentGrp = ifelse(PedTreatmentGrp == "Treat", "Treatment", PedTreatmentGrp))%>%
  mutate(Treatment_A = factor(policy, levels = c("EU", "nonEU")),
         Treatment_B = factor(PedTreatmentGrp,  levels = c("Control", "Treatment")),
         Treatment_C = factor(CostTreatmentGrp, labels = c("C1", "C2", "C3", "C4", "Control")))%>%
  select(-PedTreatmentGrp, -CostTreatmentGrp,-policy)%>%
  arrange(ID)

# Join cost estimates 

com_1_ROM <- com_0_ROM %>%
  rename("Heating"      = "heating_fuel", 
         "Cooking_fuel" = "cooking_fuel",
         "Activity"     = "occupation",
         "Building"     = "housing_type", 
         "Region"       = "province", 
         "Spending"     = "hh_expenditures", 
         "Cars"         = "number_of_cars", 
         "House_size"   = "space")%>%
  mutate_at(vars(Heating:"House_size"), ~ as.character(.))%>%
  mutate_at(vars(Heating:"House_size"), ~ stri_trans_general(., "Latin-ASCII"))%>%
  mutate(Spending = case_when(Spending == "Intre 1,301 de lei si 1,800 de lei"  ~ "intre 1,301 de lei si 1,800 de lei",
                              Spending == "Intre 1,801 de lei si 2,500 de lei"  ~ "intre 1,801 de lei si 2,500 de lei",
                              Spending == "Intre 2,501 de lei si 3,300 de lei"  ~ "intre 2,501 de lei si 3,300 de lei",
                              Spending == "Intre 3,301 de lei si 4,200 de lei"  ~ "intre 3,301 de lei si 4,200 de lei",
                              Spending == "Intre 4,201 de lei si 5,200 de lei"  ~ "intre 4,201 de lei si 5,200 de lei",
                              Spending == "Intre 5,201 de lei si 6,600 de lei"  ~ "intre 5,201 de lei si 6,600 de lei",
                              Spending == "Intre 6,601 de lei si 8,200 de lei"  ~ "intre 6,601 de lei si 8,200 de lei",
                              Spending == "Intre 8,201 de lei si 10,300 de lei" ~ "intre 8,201 de lei si 10,300 de lei",
                              Spending == "Mai putin de 1,300 de lei"           ~ "mai putin de 1,300 de lei",
                              Spending == "Nu stiu asta"                        ~ "Nu stiu asta",
                              Spending == "Peste 10,301 de lei"                 ~ "peste 10,301 de lei"))%>%
  mutate(Cars = case_when(Cars == "Am doua masini."               ~ "Am doua masini",
                          Cars == "Am o masina."                  ~ "Am o masina",
                          Cars == "Am trei sau mai multe masini." ~ "Am trei sau mai multe masini",
                          Cars == "Nu am o masina."               ~ "Nu am o masina"))%>%
  mutate(House_size = ifelse(House_size == "Peste 67 de m2", "peste 67 de m2", House_size),
         Building   = ifelse(Building   == "Casa unifamlilala", "Casa unifamiliala", Building))

data_1.4_ROM <- data_1.3_ROM %>%
  rename("House_size" = "House size", "Cooking_fuel" = "Cooking fuel", "Pricelevel" = "Priceleveleuro")%>%
  mutate_at(vars(Heating, Activity, Cooking_fuel, Building, Region, Spending, Cars, House_size), ~ stri_trans_general(., "Latin-ASCII"))%>%
  # For now %>%
  filter(!is.na(Cooking_fuel))%>%
  left_join(com_1_ROM)%>%
  # For now 
  filter(!is.na(absolute))%>%
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

rm(data_1_ROM, data_1.1_ROM, data_1.1.1_ROM, data_1.1.2_ROM, data_1.2_ROM, data_1.2.1_ROM, data_1.3_ROM, data_0_ROM, com_1_ROM, com_0_ROM)

# 2     Create outcomes ####

data_2_ESP <- data_1.4_ESP %>%
  mutate(Support_1N       = as.numeric(Support_1),
         Effectiveness_1N = as.numeric(Effectiveness_1),
         Vulnerable_1N    = as.numeric(Vulnerable_1),
         Fairness_1N      = as.numeric(Fairness_1),
         Support_2N       = as.numeric(Support_2),
         Effectiveness_2N = as.numeric(Effectiveness_2),
         Vulnerable_2N    = as.numeric(Vulnerable_2),
         Fairness_2N      = as.numeric(Fairness_2),
         Relative_loss_1N = as.numeric(Relative_loss_1),
         Relative_loss_2N = as.numeric(Relative_loss_2),
         Trust_National_N = as.numeric(Trust_National))%>%
  mutate_at(vars(t1:t6), ~ as.numeric(.))%>%
  mutate(absolute_t       = ifelse(absolute_value < t1, 1,
                                   ifelse(absolute_value < t2, 2,
                                          ifelse(absolute_value < t3, 3,
                                                 ifelse(absolute_value < t4, 4,
                                                        ifelse(absolute_value < t5, 5,
                                                               ifelse(absolute_value < t6, 6,
                                                                      ifelse(absolute_value > t6, 7,absolute_value))))))))%>%
  mutate(Expected_cost_1_absolute = as.numeric(Expected_cost_1_true),
         Expected_cost_2_absolute = as.numeric(Expected_cost_2_true))%>%
  mutate(Dif_cost_1 = Expected_cost_1_absolute - absolute_t,
         Dif_cost_2 = Expected_cost_2_absolute - absolute_t)

data_2_FRA <- data_1.4_FRA %>%
  mutate(Support_1N       = as.numeric(Support_1),
         Effectiveness_1N = as.numeric(Effectiveness_1),
         Vulnerable_1N    = as.numeric(Vulnerable_1),
         Fairness_1N      = as.numeric(Fairness_1),
         Support_2N       = as.numeric(Support_2),
         Effectiveness_2N = as.numeric(Effectiveness_2),
         Vulnerable_2N    = as.numeric(Vulnerable_2),
         Fairness_2N      = as.numeric(Fairness_2),
         Relative_loss_1N = as.numeric(Relative_loss_1),
         Relative_loss_2N = as.numeric(Relative_loss_2),
         Trust_National_N = as.numeric(Trust_National))%>%
  mutate_at(vars(t1:t6), ~ as.numeric(.))%>%
  mutate(absolute_t       = ifelse(absolute_value < t1, 1,
                                   ifelse(absolute_value < t2, 2,
                                          ifelse(absolute_value < t3, 3,
                                                 ifelse(absolute_value < t4, 4,
                                                        ifelse(absolute_value < t5, 5,
                                                               ifelse(absolute_value < t6, 6,
                                                                      ifelse(absolute_value > t6, 7,absolute_value))))))))%>%
  mutate(Expected_cost_1_absolute = as.numeric(Expected_cost_1_true),
         Expected_cost_2_absolute = as.numeric(Expected_cost_2_true))%>%
  mutate(Dif_cost_1 = Expected_cost_1_absolute - absolute_t,
         Dif_cost_2 = Expected_cost_2_absolute - absolute_t)

data_2_GER <- data_1.4_GER %>%
  mutate(Support_1N       = as.numeric(Support_1),
         Effectiveness_1N = as.numeric(Effectiveness_1),
         Vulnerable_1N    = as.numeric(Vulnerable_1),
         Fairness_1N      = as.numeric(Fairness_1),
         Support_2N       = as.numeric(Support_2),
         Effectiveness_2N = as.numeric(Effectiveness_2),
         Vulnerable_2N    = as.numeric(Vulnerable_2),
         Fairness_2N      = as.numeric(Fairness_2),
         Relative_loss_1N = as.numeric(Relative_loss_1),
         Relative_loss_2N = as.numeric(Relative_loss_2),
         Trust_National_N = as.numeric(Trust_National))%>%
  mutate_at(vars(t1:t6), ~ as.numeric(.))%>%
  mutate(absolute_t       = ifelse(absolute_value < t1, 1,
                                   ifelse(absolute_value < t2, 2,
                                          ifelse(absolute_value < t3, 3,
                                                 ifelse(absolute_value < t4, 4,
                                                        ifelse(absolute_value < t5, 5,
                                                               ifelse(absolute_value < t6, 6,
                                                                      ifelse(absolute_value > t6, 7,absolute_value))))))))%>%
  mutate(Expected_cost_1_absolute = as.numeric(Expected_cost_1_true),
         Expected_cost_2_absolute = as.numeric(Expected_cost_2_true))%>%
  mutate(Dif_cost_1 = Expected_cost_1_absolute - absolute_t,
         Dif_cost_2 = Expected_cost_2_absolute - absolute_t)

data_2_ROM <- data_1.4_ROM %>%
  mutate(Support_1N       = as.numeric(Support_1),
         Effectiveness_1N = as.numeric(Effectiveness_1),
         Vulnerable_1N    = as.numeric(Vulnerable_1),
         Fairness_1N      = as.numeric(Fairness_1),
         Support_2N       = as.numeric(Support_2),
         Effectiveness_2N = as.numeric(Effectiveness_2),
         Vulnerable_2N    = as.numeric(Vulnerable_2),
         Fairness_2N      = as.numeric(Fairness_2),
         Relative_loss_1N = as.numeric(Relative_loss_1),
         Relative_loss_2N = as.numeric(Relative_loss_2),
         Trust_National_N = as.numeric(Trust_National))%>%
  mutate_at(vars(t1:t6), ~ as.numeric(.))%>%
  mutate(absolute_t       = ifelse(absolute_value < t1, 1,
                                   ifelse(absolute_value < t2, 2,
                                          ifelse(absolute_value < t3, 3,
                                                 ifelse(absolute_value < t4, 4,
                                                        ifelse(absolute_value < t5, 5,
                                                               ifelse(absolute_value < t6, 6,
                                                                      ifelse(absolute_value > t6, 7,absolute_value))))))))%>%
  mutate(Expected_cost_1_absolute = as.numeric(Expected_cost_1_true),
         Expected_cost_2_absolute = as.numeric(Expected_cost_2_true))%>%
  mutate(Dif_cost_1 = Expected_cost_1_absolute - absolute_t,
         Dif_cost_2 = Expected_cost_2_absolute - absolute_t)

rm(data_1.4_ESP, data_1.4_GER, data_1.4_FRA, data_1.4_ROM)

# 3.X   Preliminary pilot tests (Deliverable - to be deleted) ####


# 3.1 Baseline outcome distribution ####

data_3 <- bind_rows(data_2_ESP, data_2_FRA)%>%
  bind_rows(data_2_GER)%>%
  bind_rows(data_2_ROM)

rm(data_2_ESP, data_2_FRA, data_2_GER, data_2_ROM)

# 3.1.1 Support ####

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

# 3.1.2 Effectiveness ####

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

# 3.1.3 Expected costs ####

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

# 3.1.4 Relative loss ####

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

# 3.1.5 Vulnerable ####

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

# 3.1.6 Fairness ####

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

# 3.2   Baseline correlation between policy support and institutional trust ####

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


# 3.3  Treatment effects (A,B,C1 to C4) on overall policy support ####

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


# 3.4   Varia ####

# 3.4.1 Occupation / Industry  ####

data_3.4.1 <- data_3 %>%
  filter(Country != "Romania")%>%
  filter(Employment %in% c("Employé(e) à temps partiel", "Employé(e) à temps plein", "Travailleur(euse) indépendant(e)",
                           "Beschäftigung in Teilzeit", "Beschäftigung in Vollzeit", "Selbstständig",
                           "Empleado a tiempo completo", "Empleado a tiempo parcial", "Trabajador por cuenta propia"))%>%
  group_by(Country)%>%
  summarise(test = sum(is.na(Industry)),
            number = n())%>%
  ungroup()%>%
  mutate(share = test/number)

rm(data_3.4.1)

# 3.4.2 Non-response rate across questions ####

data_3.4.0 <- data_3 %>%
  group_by(Country)%>%
  summarise(observations = n())%>%
  ungroup()

data_3.4.2 <- data_3 %>%
  select(-"Message reception_5_TEXT")%>%
  group_by(Country)%>%
  summarise_all(~ sum(is.na(.)))%>%
  ungroup()%>%
  left_join(data_3.4.0)%>%
  mutate_at(vars(-Country), ~ ./observations)

write.xlsx(data_3.4.2, "../2_Data/Supplementary/Missing Observations.xlsx")

rm(data_3.4.0, data_3.4.2)

# 3.4.3 Concern about climate change and cliamte policy ####

data_3.4.3 <- data_3 %>%
  filter(!is.na(CC_concern))%>%
  group_by(CC_concern, Country)%>%
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
  mutate(CC_concern = stri_trans_general(CC_concern, "Latin-ASCII"))%>%
  mutate(CC_concern_1 = case_when(CC_concern %in% c("Nicht besorgt", "Pas preoccupe(e)", "Nu sunt preocupat(a)", "No me preocupa") ~ "Not\noccupied",
                                  CC_concern %in% c("Ein wenig besorgt", "Un peu preoccupe(e)", "Putin preocupat(a)", "Me preocupa un poco") ~ "Somewhat\noccupied",
                                  CC_concern %in% c("Ziemlich besorgt", "Assez preoccupe(e)", "Oarecum preocupat(a)", "Me preocupa algo") ~ "Rather\noccupied",
                                  CC_concern %in% c("Sehr besorgt", "Tres preoccupe(e)", "Foarte preocupat(a)", "Me preocupa mucho") ~ "Very\noccupied",
                                  CC_concern %in% c("Keine Aussage", "Sans opinion", "Nu am nicio parere", "No tenga una opinión al respecto") ~ "I don't\nknow"))%>%
  mutate(CC_concern_label = factor(CC_concern_1, levels = c("Not\noccupied", "Somewhat\noccupied", "Rather\noccupied", "Very\noccupied", "I don't\nknow")))


P_3.4.3 <- ggplot(data_3.4.3, aes(y = Country))+
  theme_bw()+
  geom_col(position = "stack", aes(x = share, fill = fct_rev(CC_concern_label)), colour = "black", width = 0.75)+
  scale_fill_viridis_d(direction = -1, guide = guide_legend(reverse = TRUE, title.position = "top"))+
  labs(fill = "How concerned are you about climate change?")+
  scale_x_continuous(labels = scales::percent_format())+
  xlab("Share of respondents")+
  ggtitle("Concern about climate change")+
  theme(panel.grid  = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 8),
        legend.position = "bottom"
  )

jpeg("../2_Data/Figures/Pilot/Figure_7.jpg", width = 14, height = 12, unit = "cm", res = 600)
print(P_3.4.3)
dev.off()

data_3.4.3.1 <- data_3 %>%
  select(Country, Q46_1, Q46_2, Q46_3)%>%
  mutate(Q46_1_label = case_when(Q46_1 %in% c("Muy negativo", "Foarte negativ", "Très négatif", "Sehr negativ") ~ "Very\nnegative",
                                 Q46_1 %in% c("Algo negativo", "Oarecum negativ", "Assez négatif", "Eher negativ") ~ "Rather\nnegative",
                                 Q46_1 %in% c("Sin impacto", "Nici un impact", "Aucun impact", "Keine Auswirkungen") ~ "No influence",
                                 Q46_1 %in% c("Algo positivo", "Oarecum pozitiv", "Assez positif", "Eher positiv") ~ "Rather\npositive",
                                 Q46_1 %in% c("Muy positivo", "Foarte pozitiv", "Très positif", "Sehr positiv") ~ "Very\npositive"))%>%
  mutate(Q46_2_label = case_when(Q46_2 %in% c("Muy negativo", "Foarte negativ", "Très négatif", "Sehr negativ") ~ "Very\nnegative",
                                 Q46_2 %in% c("Algo negativo", "Oarecum negativ", "Assez négatif", "Eher negativ") ~ "Rather\nnegative",
                                 Q46_2 %in% c("Sin impacto", "Nici un impact", "Aucun impact", "Keine Auswirkungen") ~ "No influence",
                                 Q46_2 %in% c("Algo positivo", "Oarecum pozitiv", "Assez positif", "Eher positiv") ~ "Rather\npositive",
                                 Q46_2 %in% c("Muy positivo", "Foarte pozitiv", "Très positif", "Sehr positiv") ~ "Very\npositive"))%>%
  mutate(Q46_3_label = case_when(Q46_3 %in% c("Muy negativo", "Foarte negativ", "Très négatif", "Sehr negativ") ~ "Very\nnegative",
                                 Q46_3 %in% c("Algo negativo", "Oarecum negativ", "Assez négatif", "Eher negativ") ~ "Rather\nnegative",
                                 Q46_3 %in% c("Sin impacto", "Nici un impact", "Aucun impact", "Keine Auswirkungen") ~ "No influence",
                                 Q46_3 %in% c("Algo positivo", "Oarecum pozitiv", "Assez positif", "Eher positiv") ~ "Rather\npositive",
                                 Q46_3 %in% c("Muy positivo", "Foarte pozitiv", "Très positif", "Sehr positiv") ~ "Very\npositive"))

data_3.4.3.2 <- data_3.4.3.1 %>%
  select(Country, Q46_1_label)%>%
  filter(!is.na(Q46_1_label))%>%
  group_by(Q46_1_label, Country)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(Country)%>%
  mutate(sum = sum(number))%>%
  ungroup()%>%
  mutate(share = number/sum)%>%
  mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
  mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
  mutate(Q46_1_label = factor(Q46_1_label, levels = c("Very\nnegative", "Rather\nnegative", "No influence", "Rather\npositive", "Very\npositive")))

data_3.4.3.3 <- data_3.4.3.1 %>%
  select(Country, Q46_2_label)%>%
  filter(!is.na(Q46_2_label))%>%
  group_by(Q46_2_label, Country)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(Country)%>%
  mutate(sum = sum(number))%>%
  ungroup()%>%
  mutate(share = number/sum)%>%
  mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
  mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
  mutate(Q46_2_label = factor(Q46_2_label, levels = c("Very\nnegative", "Rather\nnegative", "No influence", "Rather\npositive", "Very\npositive")))

data_3.4.3.4 <- data_3.4.3.1 %>%
  select(Country, Q46_3_label)%>%
  filter(!is.na(Q46_3_label))%>%
  group_by(Q46_3_label, Country)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(Country)%>%
  mutate(sum = sum(number))%>%
  ungroup()%>%
  mutate(share = number/sum)%>%
  mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
  mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
  mutate(Q46_3_label = factor(Q46_3_label, levels = c("Very\nnegative", "Rather\nnegative", "No influence", "Rather\npositive", "Very\npositive")))

P_3.4.3.1 <- ggplot(data_3.4.3.2, aes(y = Country))+
  theme_bw()+
  geom_col(position = "stack", aes(x = share, fill = fct_rev(Q46_1_label)), colour = "black", width = 0.75)+
  scale_fill_viridis_d(direction = -1, guide = guide_legend(reverse = TRUE, title.position = "top"))+
  labs(fill = "What is the impact of climate policy on your life?")+
  scale_x_continuous(labels = scales::percent_format())+
  xlab("Share of respondents")+
  ggtitle("Impact on one's life")+
  theme(panel.grid  = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 8),
        legend.position = "bottom")

P_3.4.3.2 <- ggplot(data_3.4.3.3, aes(y = Country))+
  theme_bw()+
  geom_col(position = "stack", aes(x = share, fill = fct_rev(Q46_2_label)), colour = "black", width = 0.75)+
  scale_fill_viridis_d(direction = -1, guide = guide_legend(reverse = TRUE, title.position = "top"))+
  labs(fill = "What is the impact of climate policy on the economy?")+
  scale_x_continuous(labels = scales::percent_format())+
  xlab("Share of respondents")+
  ggtitle("Impact on the economy")+
  theme(panel.grid  = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 8),
        legend.position = "bottom")

P_3.4.3.3 <- ggplot(data_3.4.3.4, aes(y = Country))+
  theme_bw()+
  geom_col(position = "stack", aes(x = share, fill = fct_rev(Q46_3_label)), colour = "black", width = 0.75)+
  scale_fill_viridis_d(direction = -1, guide = guide_legend(reverse = TRUE, title.position = "top"))+
  labs(fill = "What is the impact of climate policy on the climate?")+
  scale_x_continuous(labels = scales::percent_format())+
  xlab("Share of respondents")+
  ggtitle("Impact on the climate")+
  theme(panel.grid  = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 8),
        legend.position = "bottom")

jpeg("../2_Data/Figures/Pilot/Figure_8_%d.jpg", width = 14, height = 12, unit = "cm", res = 300)
print(P_3.4.3.1)
print(P_3.4.3.2)
print(P_3.4.3.3)
dev.off()

rm(data_3.4.3, data_3.4.3.1, data_3.4.3.2, data_3.4.3.3, data_3.4.3.4, P_3.4.3, P_3.4.3.1, P_3.4.3.2, P_3.4.3.3)

# Correlation between Q46_1 and Q46_2

data_3.4.3.5 <- data_3.4.3.1 %>%
  select(Country, Q46_1_label, Q46_2_label)%>%
  filter(!is.na(Q46_1_label)&!is.na(Q46_2_label))%>%
  group_by(Q46_1_label, Q46_2_label, Country)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(Country)%>%
  mutate(sum = sum(number))%>%
  ungroup()%>%
  mutate(share = number/sum)%>%
  mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
  mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
  mutate(Q46_1_label = factor(Q46_1_label, levels = c("Very\nnegative", "Rather\nnegative", "No influence", "Rather\npositive", "Very\npositive")),
         Q46_2_label = factor(Q46_2_label, levels = c("Very\nnegative", "Rather\nnegative", "No influence", "Rather\npositive", "Very\npositive")))

data_3.4.3.6 <- data_3.4.3.1 %>%
  select(Country, Q46_1_label, Q46_3_label)%>%
  filter(!is.na(Q46_1_label)&!is.na(Q46_3_label))%>%
  group_by(Q46_1_label, Q46_3_label, Country)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(Country)%>%
  mutate(sum = sum(number))%>%
  ungroup()%>%
  mutate(share = number/sum)%>%
  mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
  mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
  mutate(Q46_1_label = factor(Q46_1_label, levels = c("Very\nnegative", "Rather\nnegative", "No influence", "Rather\npositive", "Very\npositive")),
         Q46_3_label = factor(Q46_3_label, levels = c("Very\nnegative", "Rather\nnegative", "No influence", "Rather\npositive", "Very\npositive")))

data_3.4.3.7 <- data_3.4.3.1 %>%
  select(Country, Q46_2_label, Q46_3_label)%>%
  filter(!is.na(Q46_2_label)&!is.na(Q46_3_label))%>%
  group_by(Q46_2_label, Q46_3_label, Country)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(Country)%>%
  mutate(sum = sum(number))%>%
  ungroup()%>%
  mutate(share = number/sum)%>%
  mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
  mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
  mutate(Q46_2_label = factor(Q46_2_label, levels = c("Very\nnegative", "Rather\nnegative", "No influence", "Rather\npositive", "Very\npositive")),
         Q46_3_label = factor(Q46_3_label, levels = c("Very\nnegative", "Rather\nnegative", "No influence", "Rather\npositive", "Very\npositive")))


P_3.4.3.5 <- ggplot(data_3.4.3.5)+
  theme_bw()+
  facet_wrap(. ~ Country)+
  geom_point(aes(x = Q46_2_label, y = Q46_1_label, alpha = share), colour = "black", shape = 22, size = 11, fill = "#0072B5FF")+
  geom_text(aes(x = Q46_2_label, y = Q46_1_label, label = label_0), size = 3.5)+
  scale_alpha_continuous()+
  #scale_fill_continuous(guide = guide_legend(reverse = TRUE, title.position = "top"), type = "viridis")+
  labs(fill = "What is the impact of climate policy on your life?")+
  xlab("Impact on the economy")+
  ylab("Impact on one's life")+
  guides(alpha = "none")+
  theme(panel.grid  = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.title  = element_text(size = 8),
        legend.position = "bottom")

P_3.4.3.6 <- ggplot(data_3.4.3.7)+
  theme_bw()+
  facet_wrap(. ~ Country)+
  geom_point(aes(x = Q46_3_label, y = Q46_2_label, alpha = share), colour = "black", shape = 22, size = 11, fill = "#0072B5FF")+
  geom_text(aes(x = Q46_3_label, y = Q46_2_label, label = label_0), size = 3.5)+
  scale_alpha_continuous()+
  #scale_fill_continuous(guide = guide_legend(reverse = TRUE, title.position = "top"), type = "viridis")+
  labs(fill = "What is the impact of climate policy on your life?")+
  xlab("Impact on the climate")+
  ylab("Impact on the economy")+
  guides(alpha = "none")+
  theme(panel.grid  = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.title  = element_text(size = 8),
        legend.position = "bottom")

P_3.4.3.7 <- ggplot(data_3.4.3.6)+
  theme_bw()+
  facet_wrap(. ~ Country)+
  geom_point(aes(x = Q46_1_label, y = Q46_3_label, alpha = share), colour = "black", shape = 22, size = 11, fill = "#0072B5FF")+
  geom_text(aes(x = Q46_1_label, y = Q46_3_label, label = label_0), size = 3.5)+
  scale_alpha_continuous()+
  #scale_fill_continuous(guide = guide_legend(reverse = TRUE, title.position = "top"), type = "viridis")+
  labs(fill = "What is the impact of climate policy on your life?")+
  xlab("Impact on one's life")+
  ylab("Impact on the climate")+
  guides(alpha = "none")+
  theme(panel.grid  = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.title  = element_text(size = 8),
        legend.position = "bottom")

jpeg("../2_Data/Figures/Pilot/Figure_9_%d.jpg", width = 12, height = 12, unit = "cm", res = 300)
print(P_3.4.3.5)
print(P_3.4.3.6)
print(P_3.4.3.7)
dev.off()

# 3.4.4 Pilot questions ####

data_3.4.4 <- data_3 %>%
  select(Country, "Pilot":"Q58","Q63":"Q68", Treatment_B, Q66)%>%
  rename(understanding_1 = "Understanding questi", reception_1 = "Message reception", clarity_1 = "Pilot question")

data_3.4.4.1 <- data_3.4.4 %>%
  filter(!is.na(Q66))%>%
  group_by(Country, Q66)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(Country)%>%
  mutate(sum = sum(number))%>%
  ungroup()%>%
  mutate(share = round(number/sum,2))

# Check conjoint

data_3.4.5 <- data_3 %>%
  select(Country, "Conjoint task 1_1":"Conjoint task 4_3")%>%
  rename_at(vars(-Country), ~ str_replace(., "Conjoint task ", "Combination_"))

# 4     Hypothesis tests ####
# 4.1   Hypothesis 1 ####
# 4.5   Hypotheses 5 to 8 ####

test_hypothesis_5 <- function(data_2_X){
  model_5 <- feols(Support_1N ~ i(Treatment_A, ref = "nonEU"), data = data_2_X)
}

jpeg("../2_Data/Figures/Pilot/Figure_1_%d.jpg", width = 12, height = 12, unit = "cm", res = 600)
print(P_0)
print(P_1)
dev.off()

rm(data_3.1, P_1)model_5_ESP <- test_hypothesis_5(data_2_ESP)
model_5_FRA <- test_hypothesis_5(data_2_FRA)
model_5_GER <- test_hypothesis_5(data_2_GER)
model_5_ROM <- test_hypothesis_5(data_2_ROM)

test_hypothesis_6 <- function(data_2_X){
  model_6 <- feols(Effectiveness_1N ~ i(Treatment_A, ref = "nonEU"), data = data_2_X)
}

model_6_ESP <- test_hypothesis_6(data_2_ESP)
model_6_FRA <- test_hypothesis_6(data_2_FRA)
model_6_GER <- test_hypothesis_6(data_2_GER)
model_6_ROM <- test_hypothesis_6(data_2_ROM)

test_hypothesis_7 <- function(data_2_X){
  model_7 <- feols(Dif_cost_1 ~ i(Treatment_A, ref = "nonEU"), data = data_2_X)
}

model_7_ESP <- test_hypothesis_7(data_2_ESP)
model_7_FRA <- test_hypothesis_7(data_2_FRA)
model_7_GER <- test_hypothesis_7(data_2_GER)
model_7_ROM <- test_hypothesis_7(data_2_ROM)

test_hypothesis_8 <- function(data_2_X){
  model_8 <- feols(Vulnerable_1N ~ i(Treatment_A, ref = "nonEU"), data = data_2_X)
}

model_8_ESP <- test_hypothesis_8(data_2_ESP)
model_8_FRA <- test_hypothesis_8(data_2_FRA)
model_8_GER <- test_hypothesis_8(data_2_GER)
model_8_ROM <- test_hypothesis_8(data_2_ROM)

rm(test_hypothesis_5, test_hypothesis_6, test_hypothesis_7, test_hypothesis_8)

# 4.6   Hypotheses 9 to 10 ####

data_3.6_ESP <- data_2_ESP %>%
  select(Country, ID, Treatment_B, Treatment_C, Effectiveness_1N, Effectiveness_2N, Fairness_1N, Fairness_2N)%>%
  pivot_longer(Effectiveness_1N:Fairness_2N, names_to = "Name", values_to = "value")%>%
  mutate(time = ifelse(grepl("1", Name),1,2),
         Outcome = ifelse(grepl("Fairness", Name), "Fairness", "Effectiveness"))%>%
  mutate(POST = ifelse(time == 2,1,0))%>%
  mutate(POST_B = ifelse(Treatment_B == "Treatment" & POST == 1,1,0),
         POST_C = ifelse(POST == 0, "Baseline", as.character(Treatment_C)))

test_hypothesis_9 <- function(data_3.6_X){
  model_9 <- feols(value ~ POST_B | ID + time + POST_C, data = filter(data_3.6_ESP, Outcome == "Effectiveness"))
}

model_9_ESP <- test_hypothesis_9(data_3.6_ESP)
model_9_FRA <- test_hypothesis_9(data_3.6_FRA)
model_9_GER <- test_hypothesis_9(data_3.6_GER)
model_9_ROM <- test_hypothesis_9(data_3.6_ROM)

test_hypothesis_10 <- function(data_3.6_X){
  model_10 <- feols(value ~ POST_B | ID + time + POST_C, data = filter(data_3.6_ESP, Outcome == "Fairness"))
}

model_10_ESP <- test_hypothesis_10(data_3.6_ESP)
model_10_FRA <- test_hypothesis_10(data_3.6_FRA)
model_10_GER <- test_hypothesis_10(data_3.6_GER)
model_10_ROM <- test_hypothesis_10(data_3.6_ROM)
