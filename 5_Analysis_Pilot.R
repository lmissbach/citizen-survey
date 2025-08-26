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
  mutate(Effectiveness_1 = factor(Effectiveness_1, levels = c("Sin duda eficaz", "Probablemente eficaz", "Probablemente no sea eficaz", "En absoluto eficaz")),
         Effectiveness_2 = factor(Effectiveness_2, levels = c("Sin duda eficaz", "Probablemente eficaz", "Probablemente no sea eficaz", "En absoluto eficaz")),
         Expected_cost_1_true = factor(Expected_cost_1_true, levels = c("Menos de ${e://Field/t1}€", "Entre ${e://Field/t1}€ y ${e://Field/t2}€", "Entre ${e://Field/t2}€ y ${e://Field/t3}€", "Entre ${e://Field/t3}€ y ${e://Field/t4}€", "Entre ${e://Field/t4}€ y ${e://Field/t5}€", "Entre ${e://Field/t5}€ y ${e://Field/t6}€", "Más de ${e://Field/t6}€")),
         Expected_cost_2_true = factor(Expected_cost_1_true, levels = c("Menos de ${e://Field/t1}€", "Entre ${e://Field/t1}€ y ${e://Field/t2}€", "Entre ${e://Field/t2}€ y ${e://Field/t3}€", "Entre ${e://Field/t3}€ y ${e://Field/t4}€", "Entre ${e://Field/t4}€ y ${e://Field/t5}€", "Entre ${e://Field/t5}€ y ${e://Field/t6}€", "Más de ${e://Field/t6}€")),
         Relative_loss_1 = factor(Relative_loss_1, levels = c("Menos que a un hogar típico", "Más o menos lo mismo que a un hogar típico", "Más que a un hogar típico")),
         Relative_loss_2 = factor(Relative_loss_2, levels = c("Menos que a un hogar típico", "Más o menos lo mismo que a un hogar típico", "Más que a un hogar típico")),
         Vulnerable_1 = factor(Vulnerable_1, levels = c("Los ayudará", "Ni los ayudará ni los perjudicará", "Los perjudicará")),
         Vulnerable_2 = factor(Vulnerable_2, levels = c("Los ayudará", "Ni los ayudará ni los perjudicará", "Los perjudicará")),
         Fairness_1 = factor(Fairness_1, levels = c("Injusta", "Ni justa ni injusta", "Justa")),
         Fairness_2 = factor(Fairness_2, levels = c("Injusta", "Ni justa ni injusta", "Justa")),
         Support_1 = factor(Support_1, levels = c("La apoyo firmemente", "La apoyo en parte", "Ni le apoyo ni me opongo", "Me opongo en parte", "Me opongo firmemente")),
         Support_2 = factor(Support_2, levels = c("La apoyo firmemente", "La apoyo en parte", "Ni le apoyo ni me opongo", "Me opongo en parte", "Me opongo firmemente")))%>%
  mutate(PedTreatmentGrp = ifelse(PedTreatmentGrp == "Treat", "Treatment", PedTreatmentGrp))%>%
  mutate(Treatment_A = factor(policy, levels = c("EU", "nonEU")),
         Treatment_B = factor(PedTreatmentGrp,  levels = c("Control", "Treatment")),
         Treatment_C = factor(CostTreatmentGrp, labels = c("C1", "C2", "C3", "C4", "Control")))%>%
  select(-PedTreatmentGrp, -CostTreatmentGrp,-policy)%>%
  arrange(ID)

         