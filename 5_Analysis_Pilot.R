# 0     General ####
# Author: L. Missbach (missbach@mcc-berlin.net)
# Date: 26th of August 2025

# 0.1   Packages ####

if(!require("pacman")) install.packages("pacman")

p_load("arrow", "boot", "broom", "extrafont", "fixest", "ggpubr", "ggrepel",
       "ggsci", "Hmisc", "jsonlite", "knitr", "kableExtra", "openxlsx", "rattle", "readxl", "scales", "showtext", "tidymodels", "tidyverse", "xtable")

options(scipen=999)

# 0.2   Load data ####

data_0_ESP <- read_csv("../2_Data/0_Qualtrics_Output/20250826_Pilot/Spanish/Spanish_26. August 2025_04.16.csv")
data_0_FRA <- read_csv("../2_Data/0_Qualtrics_Output/20250826_Pilot/French/French_26. August 2025_04.19.csv")
data_0_GER <- read_csv("../2_Data/0_Qualtrics_Output/20250826_Pilot/German/German_26. August 2025_04.18.csv")
data_0_ROM <- read_csv("../2_Data/0_Qualtrics_Output/20250826_Pilot/Romanian/Romanian_26. August 2025_04.18.csv")

com_0_ESP <- read_parquet("../2_Data/Output/Output data/Combinations_Qualtrics_Spain_250703.parquet")
com_0_FRA <- read_parquet("../2_Data/Output/Output data/Combinations_Qualtrics_France_250703.parquet")
com_0_GER <- read_parquet("../2_Data/Output/Output data/Combinations_Qualtrics_Germany_250703.parquet")
com_0_ROM <- read_parquet("../2_Data/Output/Output data/Combinations_Qualtrics_Romania_250703.parquet")

median_costs <- read.xlsx("../2_Data/Supplementary/Median_Costs_Countries.xlsx")

# 1     Data transformation ####
# 1.1   Spain ####

data_1_ESP <- data_0_ESP %>%
  filter(Status == "IP-Adresse")%>%
  mutate(StartDate = as.POSIXct(StartDate, format = "%Y-%m-%d %H:%M:%S"))%>%
  mutate(Date = format(StartDate, "%Y-%m-%d"))%>%
  filter(as.Date(Date) > as.Date("2025-08-18"))%>%
  mutate(Country = "Spain")%>%
  mutate(ID = 1:n())%>%
  select(Country, ID, everything())%>%
  rename(time = "Duration (in seconds)")%>%
  filter(consent != "No")%>%
  select(Country, ID, time, Age:Date)

data_1.1.1_ESP <- data_1_ESP %>%
  select(Country, ID, Label1:Label5)%>%
  pivot_longer(Label1:Label5, names_to = "Label", values_to = "Value", names_prefix = "Label")

data_1.1.2_ESP <- data_1_ESP %>%
  select(Country, ID, Institution1:Institution3)%>%
  pivot_longer(Institution1:Institution3, names_to = "Institution", values_to = "Value_Institution", names_prefix = "Institution")

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
  rename("Trust_information_1" = "Trust in information_1", "Trust_information_2" = "Trust in information_2", "Trust_information_3" = "Trust in information_3", "Trust_information_4" = "Trust in information_4",
         "CC_concern" = "Climate change conce",
         "Effectiveness_1" = "Effectiveness 1", "Expected_cost_1" = "Expected cost 1", "Relative_loss_1" = "Relative loss 1", "Vulnerable_1" = "Vulnerable 1", "Fairness_1" = "Fairness 1", "Support_1" = "Support 1",
         "Effectiveness_2" = "Q56",             "Expected_cost_2" = "Expected cost 2", "Relative_loss_2" = "Relative loss 2", "Vulnerable_2" = "Vulnerable 2", "Fairness_2" = "Fairness 2", "Support_2" = "Support 2")

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
         Support_2 = factor(Support_2, levels = c("Me opongo firmemente", "Me opongo en parte", "Ni la apoyo ni me opongo", "La apoyo en parte", "La apoyo firmemente")))%>%
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
# 1.3   Germany ####
# 1.4   Romania ####
# 2     Create outcomes ####

data_2_ESP <- data_1.4_ESP %>%
  mutate(Support_1N       = as.numeric(Support_1),
         Effectiveness_1N = as.numeric(Effectiveness_1),
         Vulnerable_1N    = as.numeric(Vulnerable_1),
         Fairness_1N      = as.numeric(Fairness_1),
         Support_2N       = as.numeric(Support_2),
         Effectiveness_2N = as.numeric(Effectiveness_2),
         Vulnerable_2N    = as.numeric(Vulnerable_2),
         Fairness_2N      = as.numeric(Fairness_2))%>%
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

# 3     Hypothesis tests ####
# 3.1   Hypothesis 1 ####
# 3.5   Hypotheses 5 to 8 ####

test_hypothesis_5 <- function(data_2_X){
  model_5 <- feols(Support_1N ~ i(Treatment_A, ref = "nonEU"), data = data_2_X)
}

model_5_ESP <- test_hypothesis_5(data_2_ESP)
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

# 3.6   Hypotheses 9 to 10 ####

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
