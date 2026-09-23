# 0      General ####
# Author: L. Missbach (leonard.missbach@pik-potsdam.de)
# Date: 28.10.2025

# 0.1    Packages ####

if(!require("pacman")) install.packages("pacman")

p_load("arrow", "boot", "broom", "extrafont", "fixest", "ggpubr", "ggrepel",
       "ggsci", "Hmisc", "jsonlite", "knitr", "kableExtra", "openxlsx", "pROC", "rattle", "readxl", "scales", "showtext", "stringi", "tidymodels", "tidyverse", "xtable", "xgboost")

options(scipen=999)

# 0.2    Load data ####

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

set.seed(2026)

# 1      Data transformation ####
# 1.1    Spain ####

data_1_ESP <- data_0_ESP %>%
  filter(Status == "IP-Adresse")%>%
  rename(ID_old = ID)%>%
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
  pivot_wider()%>%
  mutate(Q42_1_average = sapply(str_extract_all(Q42_1, "\\d+"), function(nums){
    nums <- as.numeric(nums)
    if(length(nums) == 0) NA_real_ else mean(nums)
  }),
  Q42_2_average = sapply(str_extract_all(Q42_2, "\\d+"), function(nums){
    nums <- as.numeric(nums)
    if(length(nums) == 0) NA_real_ else mean(nums)
  }))

data_1.3_ESP <- data_1.2_ESP %>%
  rename("Q42_1_true" = "Q42_1", "Q42_2_true" = "Q42_2", policy = "Policy description")%>%
  left_join(data_1.2.1_ESP)%>%
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
         Q30_3 = factor(Q30_3, levels = c("En absoluto", "Un poco", "Algo", "Bastante", "Completamente")),
         Q36   = factor(Q36,   levels = c("No me preocupa", "Me preocupa un poco","Me preocupa algo","Me preocupa mucho","No tenga una opinión al respecto")))%>%
  mutate(PedTreatmentGrp = ifelse(PedTreatmentGrp == "Treat", "Treatment", PedTreatmentGrp))%>%
  mutate(Treatment_A = factor(policy, levels = c("EU", "nonEU")),
         Treatment_B = factor(PedTreatmentGrp,  levels = c("Control", "Treatment")),
         Treatment_C = factor(CostTreatmentGrp, labels = c("C1", "C2", "C3", "C4", "Control")))%>%
  select(-PedTreatmentGrp, -CostTreatmentGrp,-policy)%>%
  arrange(ID)%>%
  # Average cost perception
  mutate(Q28_average = sapply(Q28, function(s){
    if (is.na(s)) return(NA_real_)
    
    s_clean <- str_replace_all(s, "(?<=\\d),(?=\\d{3})", "")
    
    nums <- as.numeric(str_extract_all(s_clean, "\\d+")[[1]])
    
    if (length(nums) == 0) NA_real_ else mean(nums)
  }))%>%
  mutate(Q42_1_relative = Q42_1_average/(Q28_average*12),
         Q42_2_relative = Q42_2_average/(Q28_average*12))%>%
  select(Country:Q28, Q28_average, Q30_1:Q42_1_true, Q42_1, Q42_1_average, Q42_1_relative, Q43_1:Q42_2_true, Q42_2, Q42_2_average, Q42_2_relative, everything())

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

# 1.2    France ####

data_1_FRA <- data_0_FRA %>%
  filter(Status == "IP-Adresse")%>%
  rename(ID_old = ID)%>%
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
  pivot_wider()%>%
  # Add average values
  mutate(Q42_1_average = sapply(str_extract_all(Q42_1, "\\d+"), function(nums){
    nums <- as.numeric(nums)
    if(length(nums) == 0) NA_real_ else mean(nums)
  }),
  Q42_2_average = sapply(str_extract_all(Q42_2, "\\d+"), function(nums){
    nums <- as.numeric(nums)
    if(length(nums) == 0) NA_real_ else mean(nums)
  }))


data_1.3_FRA <- data_1.2_FRA %>%
  rename("Q42_1_true" = "Q42_1", "Q42_2_true" = "Q42_2", policy = "Policy description")%>%
  left_join(data_1.2.1_FRA)%>%
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
         Q30_3 = factor(Q30_3, levels = c("Pas du tout", "Un peu", "Assez", "Largement", "Totalement")),
         Q36   = factor(Q36,   levels = c("Pas préoccupé(e)", "Un peu préoccupé(e)", "Assez préoccupé(e)", "Très préoccupé(e)", "Sans opinion")))%>%
  mutate(PedTreatmentGrp = ifelse(PedTreatmentGrp == "Treat", "Treatment", PedTreatmentGrp))%>%
  mutate(Treatment_A = factor(policy, levels = c("EU", "nonEU")),
         Treatment_B = factor(PedTreatmentGrp,  levels = c("Control", "Treatment")),
         Treatment_C = factor(CostTreatmentGrp, labels = c("C1", "C2", "C3", "C4", "Control")))%>%
  select(-PedTreatmentGrp, -CostTreatmentGrp,-policy)%>%
  arrange(ID)%>%
  # Average cost perception
  mutate(Q28_average = sapply(Q28, function(s){
    if (is.na(s)) return(NA_real_)
    
    s_clean <- str_replace_all(s, "(?<=\\d),(?=\\d{3})", "")
    
    nums <- as.numeric(str_extract_all(s_clean, "\\d+")[[1]])
    
    if (length(nums) == 0) NA_real_ else mean(nums)
  }))%>%
  mutate(Q42_1_relative = Q42_1_average/(Q28_average*12),
         Q42_2_relative = Q42_2_average/(Q28_average*12))%>%
  select(Country:Q28, Q28_average, Q30_1:Q42_1_true, Q42_1, Q42_1_average, Q42_1_relative, Q43_1:Q42_2_true, Q42_2, Q42_2_average, Q42_2_relative, everything())

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

# 1.3    Germany ####

data_1_GER <- data_0_GER %>%
  filter(Status == "IP-Adresse")%>%
  rename(ID_old = ID)%>%
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
  pivot_wider()%>%
  mutate(Q42_1_average = sapply(str_extract_all(Q42_1, "\\d+"), function(nums){
    nums <- as.numeric(nums)
    if(length(nums) == 0) NA_real_ else mean(nums)
  }),
  Q42_2_average = sapply(str_extract_all(Q42_2, "\\d+"), function(nums){
    nums <- as.numeric(nums)
    if(length(nums) == 0) NA_real_ else mean(nums)
  }))


data_1.3_GER <- data_1.2_GER %>%
  rename("Q42_1_true" = "Q42_1", "Q42_2_true" = "Q42_2", policy = "Policy description")%>%
  left_join(data_1.2.1_GER)%>%
  # Remove some columns
  select(-(FairnessPerception:PolicySupport))%>%
  # mutate_at(vars(Effectiveness_1, Effectiveness_2, Expected_cost_1_true, Expected_cost_2_true), ~ ifelse(is.na(.), "No lo sé",.))%>%
  mutate(Q36 = ifelse(is.na(Q36), "I don't know", Q36))%>%
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
         Q30_3 = factor(Q30_3, levels = c("Überhaupt nicht", "Ein wenig", "Einigermaßen", "Weitgehend", "Vollständig")),
         Q36   = factor(Q36,   levels = c("Nicht besorgt", "Ein wenig besorgt", "Ziemlich besorgt", "Sehr besorgt", "I don't know")))%>%
  mutate(PedTreatmentGrp = ifelse(PedTreatmentGrp == "Treat", "Treatment", PedTreatmentGrp))%>%
  mutate(Treatment_A = factor(policy, levels = c("EU", "nonEU")),
         Treatment_B = factor(PedTreatmentGrp,  levels = c("Control", "Treatment")),
         Treatment_C = factor(CostTreatmentGrp, labels = c("C1", "C2", "C3", "C4", "Control")))%>%
  select(-PedTreatmentGrp, -CostTreatmentGrp,-policy)%>%
  #select(-all_of(starts_with("QT")), all_of(starts_with("QT")))%>%
  arrange(ID)%>%
  # Average cost perception
  mutate(Q28_average = sapply(Q28, function(s){
    if (is.na(s)) return(NA_real_)
    
    s_clean <- str_replace_all(s, "\\.(?=\\d{3}\\b)", "")
    
    nums <- as.numeric(str_extract_all(s_clean, "\\d+")[[1]])
    
    if (length(nums) == 0) NA_real_ else mean(nums)
  }))%>%
  mutate(Q42_1_relative = Q42_1_average/(Q28_average*12),
         Q42_2_relative = Q42_2_average/(Q28_average*12))%>%
  select(Country:Q28, Q28_average, Q30_1:Q42_1_true, Q42_1, Q42_1_average, Q42_1_relative, Q43_1:Q42_2_true, Q42_2, Q42_2_average, Q42_2_relative, everything())

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

# 1.4    Romania ####

data_1_ROM <- data_0_ROM %>%
  filter(Status == "IP-Adresse")%>%
  rename(ID_old = ID)%>%
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
  pivot_wider()%>%
  mutate(Q42_1_average = sapply(str_extract_all(Q42_1, "\\d+"), function(nums){
    nums <- as.numeric(nums)
    if(length(nums) == 0) NA_real_ else mean(nums)
  }),
  Q42_2_average = sapply(str_extract_all(Q42_2, "\\d+"), function(nums){
    nums <- as.numeric(nums)
    if(length(nums) == 0) NA_real_ else mean(nums)
  }))


data_1.3_ROM <- data_1.2_ROM %>%
  rename("Q42_1_true" = "Q42_1", "Q42_2_true" = "Q42_2", policy = "Policy description")%>%
  left_join(data_1.2.1_ROM)%>%
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
         Q30_3      = factor(Q30_3,      levels = c("Deloc", "Putin", "Oarecum", "Mult", "Complet")),
         Q36        = factor(Q36,        levels = c("Nu sunt preocupat(ă)", "Puțin preocupat(ă)","Oarecum preocupat(ă)","Foarte preocupat(ă)", "Nu am nicio părere")))%>%
  mutate(PedTreatmentGrp = ifelse(PedTreatmentGrp == "Treat", "Treatment", PedTreatmentGrp))%>%
  mutate(Treatment_A = factor(policy, levels = c("EU", "nonEU")),
         Treatment_B = factor(PedTreatmentGrp,  levels = c("Control", "Treatment")),
         Treatment_C = factor(CostTreatmentGrp, labels = c("C1", "C2", "C3", "C4", "Control")))%>%
  select(-PedTreatmentGrp, -CostTreatmentGrp,-policy)%>%
  arrange(ID)%>%
  # Average cost perception
  mutate(Q28_average = sapply(Q28, function(s){
    if (is.na(s)) return(NA_real_)
    
    s_clean <- str_replace_all(s, "(?<=\\d),(?=\\d{3})", "")
    
    nums <- as.numeric(str_extract_all(s_clean, "\\d+")[[1]])
    
    if (length(nums) == 0) NA_real_ else mean(nums)
  }))%>%
  mutate(Q42_1_relative = Q42_1_average/(Q28_average*12),
         Q42_2_relative = Q42_2_average/(Q28_average*12))%>%
  select(Country:Q28, Q28_average, Q30_1:Q42_1_true, Q42_1, Q42_1_average, Q42_1_relative, Q43_1:Q42_2_true, Q42_2, Q42_2_average, Q42_2_relative, everything())

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

# 1.5    Editing the conjoint experiment data ####

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
  select(-starts_with("profile"), - c(Q62:`QT11_Click Count`), -d1, -d2)%>%
  select(-all_of(starts_with("QT")), -all_of(starts_with("Q139")), all_of(starts_with("Q139")), all_of(starts_with("QT")))
data_1.5_FRA <- data_1.4_FRA %>%
  select(-starts_with("profile"), - c(Q62:`QT11_Click Count`), -d1, -d2)%>%
  select(-all_of(starts_with("QT")), -all_of(starts_with("Q139")), all_of(starts_with("Q139")), all_of(starts_with("QT")))
data_1.5_GER <- data_1.4_GER %>%
  select(-starts_with("profile"), - c(Q62:`QT11_Click Count`), -d1, -d2)%>%
  select(-all_of(starts_with("QT")), all_of(starts_with("QT")))
data_1.5_ROM <- data_1.4_ROM %>%
  select(-starts_with("profile"), - c(Q62:`QT11_Click Count`), -d1, d2)%>%
  select(-all_of(starts_with("QT")), -all_of(starts_with("Q139")), all_of(starts_with("Q139")), all_of(starts_with("QT")))

rm(data_1.4_ESP, data_1.4_FRA, data_1.4_GER, data_1.4_ROM, median_costs, extract_conjoint)

# 1.6    Create outcomes ####

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

# 1.7    Filters ####

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


# 1.8    Summary statistics ####

data_1.8 <- data.frame(Country = c("Spain", "France", "Germany", "Romania"),
                       # After relevant time
                       "(1)"   = c(3733, 5531, 4388, 3963),
                       # Finished
                       "(2)" = c(3260, 4612, 3675, 3464),
                       # Attention check 1
                       "(3)" = c(3074, 4253, 3484, 3068),
                       # Attention check 2
                       "(4)" = c(3027, 4143, 3368, 2970),
                       # Too fast or too slow
                       "(5)" = c(2813, 3851, 3128, 2760),
                       # No variation in outcomes
                       "(6)" = c(2789,3784,3096, 2743))

kbl(data_1.8, format = "latex", linesep = "", booktabs = T, caption = "Sample size and data cleaning",
    col.names = c("Country", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
    format.args = list(big.mark = ",", scientific = FALSE), align = "lrrrrrr", label = "Summary_1", digits = 2, na = "")%>%
    kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"), font_size = 8)%>%
    column_spec(1, border_right = TRUE)%>%
    footnote(general = "This table shows our sample size for four countries.
             Column (1) shows the number of all respondents. 
             Column (2) shows the number of all respondents that have completed the survey.
             Column (3) shows the number of all respondents that have answered correctly to the first attention check.
             Column (4) shows the number of all respondents that have answered correctly to the second attention check.
             Column (5) shows the number of all respondents after removing respondents that were too fast (5%) or too slow (2%).
             Column (6) shows the number of all respondents after removing respondents indicating 'I don't know' for all policy perception variables.
             Column (6) shows the final sample size used in this analysis.", threeparttable = T)%>%
    save_kable(., "../6_EUETS2_Citizens_Survey/2_Tables/Table_Summary_1.tex")

data_1.8.1_ESP <- data_1.6_ESP %>%
  select(Country, Q10, Q11, Q12, Q27, Q28)%>%
  pivot_longer(-Country, names_to = "variable", values_to = "value")%>%
  group_by(Country, variable, value)%>%
  summarise(number = n(), .groups = "drop")%>%
  group_by(Country, variable)%>%
  mutate(share = paste0(round(number/sum(number) * 100,1), "%"))%>%
  select(-number)%>%
  ungroup()%>%
  pivot_wider(names_from = Country, values_from = share)%>%
  mutate(variable = ifelse(variable == "Q10", "Age",
                           ifelse(variable == "Q11", "Gender",
                                  ifelse(variable == "Q12", "Education",
                                         ifelse(variable == "Q27", "Location",
                                                ifelse(variable == "Q28", "Expenditures", NA))))))%>%
  mutate(order = 1:n())%>%
  mutate(order = case_when(order == 32 ~ 22.5,
                           order == 31 ~ 22.6,
                           order == 23 ~ 34,
                           order == 19 ~ 20.5,
                           value == "No he completado la enseñanza básica"      ~ 10,
                           value == "Educación primaria"                         ~ 11,
                           value == "Educación secundaria obligatoria (ESO)"     ~ 12,
                           value == "Formación profesional básica (FP)"          ~ 13,
                           value == "Bachillerato"                                ~ 14,
                           value == "Formación profesional de grado medio"       ~ 15,
                           value == "Formación profesional de grado superior"    ~ 16,
                           value == "Grado universitario"                         ~ 17,
                           value == "Máster/doctorado"                            ~ 18,
                           TRUE ~ order))%>%
  arrange(order)%>%
  select(-order)%>%
  rename(Variable = variable, Level = value, Share = Spain)

data_1.8.1_FRA <- data_1.6_FRA %>%
  select(Country, Q10, Q11, Q12, Q27, Q28)%>%
  pivot_longer(-Country, names_to = "variable", values_to = "value")%>%
  filter(!is.na(value))%>%
  group_by(Country, variable, value)%>%
  summarise(number = n(), .groups = "drop")%>%
  group_by(Country, variable)%>%
  mutate(share = paste0(round(number/sum(number) * 100,1), "%"))%>%
  select(-number)%>%
  ungroup()%>%
  pivot_wider(names_from = Country, values_from = share)%>%
  mutate(variable = ifelse(variable == "Q10", "Age",
                           ifelse(variable == "Q11", "Gender",
                                  ifelse(variable == "Q12", "Education",
                                         ifelse(variable == "Q27", "Location",
                                                ifelse(variable == "Q28", "Expenditures", NA))))))%>%
  mutate(order = 1:n())%>%
  mutate(order = case_when(order == 30 ~ 21.5,
                           order == 21 ~ 32,
                           order == 17 ~ 20.5,
                           order == 19 ~ 17.5,
                           value == "Aucun"                                                                          ~ 10,
                           value == "Ecole primaire"                                                                  ~ 11,
                           value == "Brevet"                                                                          ~ 12,
                           value == "CAP ou BEP"                                                                      ~ 13,
                           value == "Baccalauréat"                                                                    ~ 14,
                           value == "Bac +2 ou Bac +3 (license, BTS, DUT, DEUG...)"                                    ~ 15,
                           value == "Bac +5 ou plus (master, école d'ingénieur ou de commerce, doctorat, médecine, maîtrise, DEA, DESS...)" ~ 16,
                           TRUE ~ order))%>%
  arrange(order)%>%
  select(-order)%>%
  rename(Variable = variable, Level = value, Share = France)

data_1.8.1_GER <- data_1.6_GER %>%
  select(Country, Q10, Q11, Q12, Q27, Q28)%>%
  pivot_longer(-Country, names_to = "variable", values_to = "value")%>%
  filter(!is.na(value))%>%
  group_by(Country, variable, value)%>%
  summarise(number = n(), .groups = "drop")%>%
  group_by(Country, variable)%>%
  mutate(share = paste0(round(number/sum(number) * 100,1), "%"))%>%
  select(-number)%>%
  ungroup()%>%
  pivot_wider(names_from = Country, values_from = share)%>%
  mutate(variable = ifelse(variable == "Q10", "Age",
                           ifelse(variable == "Q11", "Gender",
                                  ifelse(variable == "Q12", "Education",
                                         ifelse(variable == "Q27", "Location",
                                                ifelse(variable == "Q28", "Expenditures", NA))))))%>%
  mutate(order = 1:n())%>%
  mutate(order = case_when(order == 21 ~ 30.5,
                           order == 20 ~ 31,
                           order == 19 ~ 17.5,
                           value == "Keine abgeschlossene Schulbildung"                                    ~ 10,
                           value == "Grundschule"                                                           ~ 11,
                           value == "Untere Sekundarstufe (z.B. Haupt- oder Realschulabschluss)"           ~ 12,
                           value == "Abitur"                                                                ~ 13,
                           value == "Beruflicher Abschluss / Ausbildung"                                    ~ 14,
                           value == "Hochschulabschluss (z.B. Bachelor)"                                    ~ 15,
                           value == "Master-Abschluss oder höher"                                           ~ 16,
                           TRUE ~ order))%>%
  arrange(order)%>%
  select(-order)%>%
  rename(Variable = variable, Level = value, Share = Germany)

data_1.8.1_ROM <- data_1.6_ROM %>%
  select(Country, Q10, Q11, Q12, Q28)%>%
  pivot_longer(-Country, names_to = "variable", values_to = "value")%>%
  filter(!is.na(value))%>%
  group_by(Country, variable, value)%>%
  summarise(number = n(), .groups = "drop")%>%
  group_by(Country, variable)%>%
  mutate(share = paste0(round(number/sum(number) * 100,1), "%"))%>%
  select(-number)%>%
  ungroup()%>%
  pivot_wider(names_from = Country, values_from = share)%>%
  mutate(variable = ifelse(variable == "Q10", "Age",
                           ifelse(variable == "Q11", "Gender",
                                  ifelse(variable == "Q12", "Education",
                                         ifelse(variable == "Q27", "Location",
                                                ifelse(variable == "Q28", "Expenditures", NA))))))%>%
  mutate(order = 1:n())%>%
  mutate(order = case_when(order == 26 ~ 17.5,
                           order == 17 ~ 28,
                           value == "Educație primară (Clasele I–IV)"                                  ~ 10,
                           value == "Educație secundară inferioră (Gimnaziu, Clasele V–VIII)"          ~ 11,
                           value == "Școală profesională"                                              ~ 12,
                           value == "Liceu (Liceu – Clasele IX–XII/XIII)"                              ~ 13,
                           value == "Educație postsecundară non-terțiară (Școală postliceală)"         ~ 14,
                           value == "Diplomă de licență"                                               ~ 15,
                           value == "Diplomă de master sau de doctorat"                                ~ 16,
                           TRUE ~ order))%>%
  arrange(order)%>%
  select(-order)%>%
  rename(Variable = variable, Level = value, Share = Romania)


kbl(data_1.8.1_ESP, format = "latex", linesep = "", booktabs = T, caption = "Summary statistics (Spain)",
    format.args = list(big.mark = ",", scientific = FALSE), align = "llr", label = "Summary_1_ESP", digits = 2, na = "")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"), font_size = 8)%>%
  column_spec(1, border_right = TRUE)%>%
  row_spec(6, hline_after = TRUE)%>%
  row_spec(9, hline_after = TRUE)%>%
  row_spec(18, hline_after = TRUE)%>%
  row_spec(22, hline_after = TRUE)%>%
  footnote(general = "This table shows summary statistics for Spain as a share of respondents in the final sample.", threeparttable = T)%>%
  save_kable(., "../6_EUETS2_Citizens_Survey/2_Tables/Table_Summary_2_ESP.tex")

kbl(data_1.8.1_FRA, format = "latex", linesep = "", booktabs = T, caption = "Summary statistics (France)",
    format.args = list(big.mark = ",", scientific = FALSE), align = "llr", label = "Summary_1_FRA", digits = 2, na = "")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"), font_size = 8)%>%
  column_spec(1, border_right = TRUE)%>%
  row_spec(6, hline_after = TRUE)%>%
  row_spec(9, hline_after = TRUE)%>%
  row_spec(16, hline_after = TRUE)%>%
  row_spec(20, hline_after = TRUE)%>%
  footnote(general = "This table shows summary statistics for France as a share of respondents in the final sample.", threeparttable = T)%>%
  save_kable(., "../6_EUETS2_Citizens_Survey/2_Tables/Table_Summary_2_FRA.tex")

kbl(data_1.8.1_GER, format = "latex", linesep = "", booktabs = T, caption = "Summary statistics (Germany)",
    format.args = list(big.mark = ",", scientific = FALSE), align = "llr", label = "Summary_1_GER", digits = 2, na = "")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"), font_size = 8)%>%
  column_spec(1, border_right = TRUE)%>%
  row_spec(6, hline_after = TRUE)%>%
  row_spec(9, hline_after = TRUE)%>%
  row_spec(16, hline_after = TRUE)%>%
  row_spec(19, hline_after = TRUE)%>%
  footnote(general = "This table shows summary statistics for Germany as a share of respondents in the final sample.", threeparttable = T)%>%
  save_kable(., "../6_EUETS2_Citizens_Survey/2_Tables/Table_Summary_2_GER.tex")

kbl(data_1.8.1_ROM, format = "latex", linesep = "", booktabs = T, caption = "Summary statistics (Romania)",
    format.args = list(big.mark = ",", scientific = FALSE), align = "llr", label = "Summary_1_ROM", digits = 2, na = "")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"), font_size = 8)%>%
  column_spec(1, border_right = TRUE)%>%
  row_spec(6, hline_after = TRUE)%>%
  row_spec(9, hline_after = TRUE)%>%
  row_spec(16, hline_after = TRUE)%>%
  footnote(general = "This table shows summary statistics for Romania as a share of respondents in the final sample.", threeparttable = T)%>%
  save_kable(., "../6_EUETS2_Citizens_Survey/2_Tables/Table_Summary_2_ROM.tex")

data_1.8.1_total <- bind_rows(data_1.6_ESP,
                              data_1.6_FRA,
                              data_1.6_GER, data_1.6_ROM)

data_1.8.1.1 <- data_1.8.1_total %>%
  group_by(Country, Treatment_B)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(Country)%>%
  mutate(share = paste0(round(number/sum(number)*100,1), "%"))%>%
  ungroup()%>%
  select(-number)%>%
  pivot_wider(names_from = "Treatment_B", values_from = "share", names_prefix = "B_")

data_1.8.1.2 <- data_1.8.1_total %>%
  group_by(Country, Treatment_C)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(Country)%>%
  mutate(share = paste0(round(number/sum(number)*100,1), "%"))%>%
  ungroup()%>%
  select(-number)%>%
  pivot_wider(names_from = "Treatment_C", values_from = "share")%>%
  rename("C_Control" = "Control")

data_1.8.1.3 <- left_join(data_1.8.1.1, data_1.8.1.2)

kbl(data_1.8.1.3, format = "latex", linesep = "", booktabs = T, caption = "Summary statistics (Treatment and Control)",
    format.args = list(big.mark = ",", scientific = FALSE), align = "lrrrrrr", label = "Summary_3", digits = 2, na = "")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"), font_size = 8)%>%
  column_spec(1, border_right = TRUE)%>%
  column_spec(3, border_right = TRUE)%>%
  footnote(general = "This table shows the share of respondents in the treatment and control group for two information treatments and four countries in the final sample.", threeparttable = T)%>%
  save_kable(., "../6_EUETS2_Citizens_Survey/2_Tables/Table_Summary_3.tex")


# 2      DESCRIPTIVE STATISTICS ####

data_2 <- bind_rows(data_1.6_FRA, data_1.6_GER)%>%
  bind_rows(data_1.6_ROM)%>%
  bind_rows(data_1.6_ESP)%>% 
  mutate(Pricelevel = ifelse(is.na(Pricelevel), Priceleveleuro, Pricelevel))%>%
  mutate(Pricelevel = factor(Pricelevel, levels = c(45,85,125)))%>%
  select(-ID_old)

# 2.1    Baseline outcome distribution ####
# 2.1.1  Overall Policy Support ####

# data_2.1.1.1 <- data_2 %>%
#   filter(!is.na(Q46_1N))%>%
#   group_by(Q46_1N, Country)%>%
#   summarise(number = n())%>%
#   ungroup()%>%
#   group_by(Country)%>%
#   mutate(sum = sum(number))%>%
#   ungroup()%>%
#   mutate(share = number/sum)%>%
#   group_by(Country)%>%
#   mutate(share_sum = cumsum(share))%>%
#   ungroup()%>%
#   mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
#   mutate(Country = factor(Country, levels = c("Spain", "France", "Germany", "Romania")))%>%
#   mutate(Q46_1N_label = case_when(Q46_1N == 1 ~ "Strongly\n oppose",
#                                       Q46_1N == 2 ~ "Rather\n oppose",
#                                       Q46_1N == 3 ~ "Neutral",
#                                       Q46_1N == 4 ~ "Rather\n support",
#                                       Q46_1N == 5 ~ "Strongly\n support"))%>%
#    mutate(Q46_1N_label = factor(Q46_1N_label, levels = c("Strongly\n oppose", "Rather\n oppose", "Neutral", "Rather\n support", "Strongly\n support")))%>%
#    mutate(Period = "t=0")
# 
# data_2.1.1.2 <- data_2 %>%
#   filter(!is.na(Q46_2N))%>%
#   group_by(Q46_2N, Country)%>%
#   summarise(number = n())%>%
#   ungroup()%>%
#   group_by(Country)%>%
#   mutate(sum = sum(number))%>%
#   ungroup()%>%
#   mutate(share = number/sum)%>%
#   group_by(Country)%>%
#   mutate(share_sum = cumsum(share))%>%
#   ungroup()%>%
#   mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
#   mutate(Country = factor(Country, levels = c("Spain", "France", "Germany", "Romania")))%>%
#   mutate(Q46_2N_label = case_when(Q46_2N == 1 ~ "Strongly\n oppose",
#                                   Q46_2N == 2 ~ "Rather\n oppose",
#                                   Q46_2N == 3 ~ "Neutral",
#                                   Q46_2N == 4 ~ "Rather\n support",
#                                   Q46_2N == 5 ~ "Strongly\n support"))%>%
#   mutate(Q46_1N_label = factor(Q46_2N_label, levels = c("Strongly\n oppose", "Rather\n oppose", "Neutral", "Rather\n support", "Strongly\n support")))%>%
#   mutate(Period = "t=1")
# 
# data_2.1.1 <- bind_rows(data_2.1.1.1, data_2.1.1.2)%>%
#   mutate(Period = factor(Period, levels = c("t=1", "t=0")))
#  
# P_2.1.1 <- ggplot(data_2.1.1, aes(y = Period))+
#  facet_grid(Country ~ .)+
#  theme_bw()+
#  geom_col(position = "stack", aes(x = share, fill = fct_rev(Q46_1N_label)), colour = "black", width = 0.75)+
#  scale_fill_viridis_d(direction = -1, guide = guide_legend(reverse = TRUE, title.position = "top"))+
#  labs(fill = "Do you support or oppose this policy?")+
#  scale_x_continuous(labels = scales::percent_format())+
#  xlab("Share of respondents")+
#  ggtitle("Overall policy support (Q46_1 and Q46_2)")+
#  theme(panel.grid  = element_blank(),
#        axis.text.x = element_text(size = 7),
#        axis.text.y = element_text(size = 8),
#        axis.title  = element_text(size = 8),
#        legend.position = "bottom")
# 
# P_2.1.2 <- ggplot(data_2.1.1, aes(x = Q46_1N_label, y = Period))+
#  facet_grid(Country ~ .)+
#  theme_bw()+
#  geom_point(aes(fill = share_sum), shape = 22, size = 14)+
#  geom_text(aes(label = label_0), size = 4)+
#  scale_fill_distiller(limits = c(0,1))+
#  xlab("Do you support or oppose this policy?")+
#  ggtitle("Overall policy support (Q46_1 and Q46_2)")+
#  guides(fill = "none")+
#  theme(panel.grid  = element_blank(),
#        axis.text.x = element_text(size = 7),
#        axis.text.y = element_text(size = 8),
#        axis.title  = element_text(size = 8))
# 
# P_2.1.3 <- ggplot(filter(data_2.1.1, Period == "t=0"), aes(y = Country))+
#   theme_bw()+
#   geom_col(position = "stack", aes(x = share, fill = fct_rev(Q46_1N_label)), colour = "black", width = 0.75)+
#   scale_fill_viridis_d(direction = -1, guide = guide_legend(reverse = TRUE, title.position = "top"))+
#   labs(fill = "Do you support or oppose this policy?")+
#   scale_x_continuous(labels = scales::percent_format())+
#   xlab("Share of respondents")+
#   ggtitle("Overall policy support (Q46_1 and Q46_2)")+
#   theme(panel.grid  = element_blank(),
#         axis.text.x = element_text(size = 7),
#         axis.text.y = element_text(size = 8),
#         axis.title  = element_text(size = 8),
#         legend.position = "bottom")
# 
# jpeg("../5_Analysis/1_Descriptive/Figure_D1_%d.jpg", width = 12, height = 12, unit = "cm", res = 600)
# print(P_2.1.2)
# print(P_2.1.3)
# print(P_2.1.1)
# dev.off()

# Figure 1

data_2.1.1.3 <- data_2 %>%
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
  # mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
  mutate(Country = factor(Country, levels = c("Spain", "France", "Germany", "Romania")))%>%
  mutate(Q46_1N_label = case_when(Q46_1N == 1 ~ "Strongly\n oppose",
                                  Q46_1N == 2 ~ "Rather\n oppose",
                                  Q46_1N == 3 ~ "Neutral",
                                  Q46_1N == 4 ~ "Rather\n support",
                                  Q46_1N == 5 ~ "Strongly\n support"))%>%
  mutate(Q46_1N_label = factor(Q46_1N_label, levels = c("Neutral", "Rather\n oppose", "Strongly\n oppose", "Rather\n support", "Strongly\n support")))%>%
  mutate(Period = "t=0")%>%
  mutate(share = ifelse(Q46_1N < 3, -share, share))%>%
  mutate(share = ifelse(Q46_1N == 3, share/2, share))%>%
  mutate(side = ifelse(Q46_1N == 3, "right", NA))

data_2.1.1.4 <- data_2.1.1.3 %>%
  bind_rows(mutate(mutate(filter(data_2.1.1.3, Q46_1N == 3), share = -share), side = "left"))%>%
  arrange(Country, Q46_1N)

P_2.1.4 <- ggplot(data_2.1.1.4, aes(x = share, y = fct_rev(Country), fill = fct_rev(Q46_1N_label)))+
  geom_vline(aes(xintercept = 0), linewidth = 0.3)+
  geom_col(position = "stack", colour = "black", width = 0.75, linewidth = 0.3)+
  theme_bw()+
  coord_cartesian(xlim = c(-0.76,0.76))+
  scale_fill_manual(guide = guide_legend(title.position = "top"),
                    values = c("#DC0000FF", "#E64B35FF", "#B09C85FF", "#91D1C2FF", "#00A087FF"),
                    breaks = c("Strongly\n oppose", "Rather\n oppose", "Neutral", "Rather\n support", "Strongly\n support"))+
  labs(fill = "Do you support or oppose the EU ETS2?")+
  scale_x_continuous(labels = \(x) scales::percent(abs(x)),
                     breaks = c(-0.75,-0.5,-0.25,0,0.25,0.5,0.75))+
  xlab("Share of respondents")+
  ylab("Country")+
  # ggtitle("Overall policy support (Q46_1 and Q46_2)")+
  theme(panel.grid.minor  = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 9),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(hjust = 0.5, size = 8))

pdf("../6_EUETS2_Citizens_Survey/1_Figures/A_Figure_1.pdf", width = 140/25.4, height = 90/25.4)
print(P_2.1.4)
dev.off()

jpeg("../5_Analysis/2_Presentation/A_Figure_1.jpeg", width = 140 / 25.4, height = 90 / 25.4, units = "in", res = 300, quality = 100)
print(P_2.1.4)
dev.off()

data_2.1.1.table <- data_2 %>%
  mutate(Q46_1N_label = case_when(Q46_1N == 1 ~ "Strongly oppose",
                                  Q46_1N == 2 ~ "Rather oppose",
                                  Q46_1N == 3 ~ "Neutral",
                                  Q46_1N == 4 ~ "Rather support",
                                  Q46_1N == 5 ~ "Strongly support",
                                  is.na(Q46_1N) ~ "I don't know"))%>%
  mutate(Q46_1N_label = factor(Q46_1N_label, levels = c("Strongly oppose", "Rather oppose", "Neutral", "Rather support", "Strongly support", "I don't know")))%>%
  group_by(Q46_1N_label, Country)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(Country)%>%
  mutate(sum = sum(number))%>%
  ungroup()%>%
  mutate(share = number/sum)%>%
  select(-number)%>%
  pivot_wider(names_from = "Q46_1N_label", values_from = "share")%>%
  mutate(Country = factor(Country, levels = c("Spain", "France", "Germany" ,"Romania")))%>%
  arrange(Country)%>%
  mutate_at(vars(-c(Country, sum)), ~ paste(round(.*100,0), "%"))%>%
  rename(Sample = sum)%>%
  select(Country, everything(),-Sample, Sample)

kbl(data_2.1.1.table, format = "latex", linesep = "", booktabs = T, caption = "Distribution of responses: Policy support (Q46\\_1)",
    format.args = list(big.mark = ",", scientific = FALSE), align = "lccccccr", label = "Sum_Q46_1", digits = 2, na = "")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"), font_size = 8)%>%
  footnote(general = "This table shows responses to question Q46_1 (Do you support or oppose this policy?) per country as percentage shares. Total number of valid answers in column Sample.", threeparttable = T)%>%
  save_kable(., "../6_EUETS2_Citizens_Survey/2_Tables/Table_B_1_Q46_1.tex")

rm(data_2.1.1, data_2.1.1.1, data_2.1.1.2, P_2.1.1, P_2.1.2, data_2.1.1.3, data_2.1.1.4, P_2.1.4, data_2.1.1.table)

# 2.1.2  Effectiveness ####

# data_2.1.2.1 <- data_2 %>%
#   group_by(Q41_1N, Country)%>%
#   summarise(number = n())%>%
#   ungroup()%>%
#   group_by(Country)%>%
#   mutate(sum = sum(number))%>%
#   ungroup()%>%
#   mutate(share = number/sum)%>%
#   group_by(Country)%>%
#   mutate(share_sum = cumsum(share))%>%
#   ungroup()%>%
#   mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
#   mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
#   #filter(!is.na(Effectiveness_1N))%>%
#   mutate(Q41_1N_label = case_when(Q41_1N == 1 ~ "Definetly not",
#                                   Q41_1N == 2 ~ "Probably not",
#                                   Q41_1N == 3 ~ "Probably yes",
#                                   Q41_1N == 4 ~ "Definetly yes",
#                                   is.na(Q41_1N) ~ "Don't know"))%>%
#   mutate(Q41_1N_label = factor(Q41_1N_label, levels = c("Definetly not", "Probably not", "Probably yes", "Definetly yes", "Don't know")))%>%
#   mutate(Period = "t=0")
# 
# data_2.1.2.2 <- data_2 %>%
#  group_by(Q41_2N, Country)%>%
#  summarise(number = n())%>%
#  ungroup()%>%
#  group_by(Country)%>%
#  mutate(sum = sum(number))%>%
#  ungroup()%>%
#  mutate(share = number/sum)%>%
#  group_by(Country)%>%
#  mutate(share_sum = cumsum(share))%>%
#  ungroup()%>%
#  mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
#  mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
#  # filter(!is.na(Effectiveness_2N))%>%
#   mutate(Q41_1N_label = case_when(Q41_2N == 1 ~ "Definetly not",
#                                   Q41_2N == 2 ~ "Probably not",
#                                   Q41_2N == 3 ~ "Probably yes",
#                                   Q41_2N == 4 ~ "Definetly yes",
#                                   is.na(Q41_2N) ~ "Don't know"))%>%
#   mutate(Q41_1N_label = factor(Q41_1N_label, levels = c("Definetly not", "Probably not", "Probably yes", "Definetly yes", "Don't know")))%>%
#   mutate(Period = "t=1")
# 
# data_2.1.2 <- bind_rows(data_2.1.2.1, data_2.1.2.2)%>%
#  mutate(Period = factor(Period, levels = c("t=1", "t=0")))
# 
# P_2.1.2 <- ggplot(data_2.1.2, aes(x = Q41_1N_label, y = Period))+
#  facet_grid(Country ~ .)+
#  theme_bw()+
#  geom_point(aes(fill = share_sum), shape = 22, size = 14)+
#  geom_text(aes(label = label_0), size = 4)+
#  scale_fill_distiller(limits = c(0,1))+
#  xlab("Do you think that this policy will contribute to effectively reducing GHG emissions?")+
#  ggtitle("Perception of effectiveness (Q41_1 and Q41_2)")+
#  guides(fill = "none")+
#  theme(panel.grid  = element_blank(),
#        axis.text.x = element_text(size = 7),
#        axis.text.y = element_text(size = 8),
#        axis.title  = element_text(size = 7))
# 
# jpeg("../5_Analysis/1_Descriptive/Figure_D2.jpg", width = 12, height = 12, unit = "cm", res = 600)
# print(P_2.1.2)
# dev.off()

data_2.1.2.3 <- data_2 %>%
  filter(!is.na(Q41_1N))%>%
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
  # mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
  mutate(Country = factor(Country, levels = c("Spain", "France", "Germany", "Romania")))%>%
  mutate(Q41_1N_label = case_when(Q41_1N == 1 ~ "Definetly not",
                                  Q41_1N == 2 ~ "Probably not",
                                  Q41_1N == 3 ~ "Probably yes",
                                  Q41_1N == 4 ~ "Definetly yes",
                                  is.na(Q41_1N) ~ "Don't know"))%>%
  mutate(Q41_1N_label = factor(Q41_1N_label, levels = c("Probably not", "Definetly not", "Probably yes", "Definetly yes")))%>%
  mutate(share = ifelse(Q41_1N < 3, -share, share))

P_2.1.2A <- ggplot(data_2.1.2.3, aes(x = share, y = fct_rev(Country), fill = fct_rev(Q41_1N_label)))+
  geom_vline(aes(xintercept = 0), linewidth = 0.3)+
  geom_col(position = "stack", colour = "black", width = 0.75, linewidth = 0.3)+
  theme_bw()+
  coord_cartesian(xlim = c(-0.76,0.76))+
  scale_fill_manual(guide = guide_legend(title.position = "top"),
                    values = c("#DC0000FF", "#E64B35FF", "#91D1C2FF", "#00A087FF"),
                    breaks = c("Definetly not", "Probably not", "Probably yes", "Definetly yes"))+
  labs(fill = "Do you think that this policy will contribute to \neffectively reducing greenhouse gas emissions?")+
  scale_x_continuous(labels = \(x) scales::percent(abs(x)),
                     breaks = c(-0.75,-0.5,-0.25,0,0.25,0.5,0.75))+
  xlab("Share of respondents")+
  ylab("Country")+
  # ggtitle("Overall policy support (Q46_1 and Q46_2)")+
  theme(panel.grid.minor  = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 9),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(hjust = 0.5, size = 8))

pdf("../6_EUETS2_Citizens_Survey/1_Figures/A_Figure_A1.pdf", width = 140/25.4, height = 90/25.4)
print(P_2.1.2A)
dev.off()

jpeg("../5_Analysis/2_Presentation/A_Figure_A1.jpeg", width = 140 / 25.4, height = 90 / 25.4, units = "in", res = 300, quality = 100)
print(P_2.1.2A)
dev.off()

data_2.1.2.table <- data_2 %>%
  mutate(Q41_1N_label = case_when(Q41_1N == 1 ~ "Definetly not",
                                  Q41_1N == 2 ~ "Probably not",
                                  Q41_1N == 3 ~ "Probably yes",
                                  Q41_1N == 4 ~ "Definetly yes",
                                  is.na(Q41_1N) ~ "I don't know"))%>%
  mutate(Q41_1N_label = factor(Q41_1N_label, levels = c("Definetly not", "Probably not", "Probably yes", "Definetly yes", "I don't know")))%>%
  group_by(Q41_1N_label, Country)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(Country)%>%
  mutate(sum = sum(number))%>%
  ungroup()%>%
  mutate(share = number/sum)%>%
  select(-number)%>%
  pivot_wider(names_from = "Q41_1N_label", values_from = "share")%>%
  mutate(Country = factor(Country, levels = c("Spain", "France", "Germany" ,"Romania")))%>%
  arrange(Country)%>%
  mutate_at(vars(-c(Country, sum)), ~ paste(round(.*100,0), "%"))%>%
  rename(Sample = sum)%>%
  select(Country, everything(),-Sample, Sample)

kbl(data_2.1.2.table, format = "latex", linesep = "", booktabs = T, caption = "Distribution of responses: Perception of effectiveness (Q41\\_1)",
    format.args = list(big.mark = ",", scientific = FALSE), align = "lcccccr", label = "Sum_Q41_1", digits = 2, na = "")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"), font_size = 8)%>%
  footnote(general = "This table shows responses to question Q41_1 (Do you think that this policy will contribute to effectively reducing greenhouse gas emissions?) per country as percentage shares. Total number of valid answers in column Sample.", threeparttable = T)%>%
  save_kable(., "../6_EUETS2_Citizens_Survey/2_Tables/Table_B_2_Q41_1.tex")

rm(data_2.1.2, data_2.1.2.1, data_2.1.2.2, P_2.1.2, P_2.1.2A, data_2.1.2.3, data_2.1.2.table)

# 2.1.3  Expected costs (Over- or underestimated) ####

# data_2.1.3.1 <- data_2 %>%
#   mutate(Cost_estimation = ifelse(is.na(Dif_cost_1), "Don't know", 
#                                   ifelse(Dif_cost_1 < -3, "Strongly\n underestimated",
#                                          ifelse(Dif_cost_1 < 0, "Under-\nestimated", 
#                                                 ifelse(Dif_cost_1 == 0, "Estimated\n correctly",
#                                                        ifelse(Dif_cost_1 > 0 & Dif_cost_1 < 4, "Over-\nestimated",
#                                                               ifelse(Dif_cost_1 >= 4, "Strongly\n overestimated", NA)))))))%>%
#   mutate(Cost_estimation = factor(Cost_estimation, levels = c("Strongly\n underestimated", "Under-\nestimated", "Estimated\n correctly", "Over-\nestimated", "Strongly\n overestimated", "Don't know")))%>%
#   group_by(Cost_estimation, Country)%>%
#   summarise(number = n())%>%
#   ungroup()%>%
#   group_by(Country)%>%
#   mutate(sum = sum(number))%>%
#   ungroup()%>%
#   mutate(share = number/sum)%>%
#   group_by(Country)%>%
#   mutate(share_sum = cumsum(share))%>%
#   ungroup()%>%
#   mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
#   mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
#   # bind_rows(data.frame(Country = factor("France", levels = levels(.$Country)),
#   #                      Cost_estimation = factor("Strongly\n underestimated", levels = levels(.$Cost_estimation)),
#   #                      share_sum = 0, label_0 = "0%"))%>%
#   # filter(Cost_estimation != "Don't know")%>%
#   mutate(Period = "t=0")
#   
# 
# data_2.1.3.2 <- data_2 %>%
#   filter(Treatment_C != "Control")%>%
#   mutate(Cost_estimation = ifelse(is.na(Dif_cost_2), "Don't know", 
#                                   ifelse(Dif_cost_2 < -3, "Strongly\n underestimated",
#                                          ifelse(Dif_cost_2 < 0, "Under-\nestimated", 
#                                                 ifelse(Dif_cost_2 == 0, "Estimated\n correctly",
#                                                        ifelse(Dif_cost_2 > 0 & Dif_cost_2 < 4, "Over-\nestimated",
#                                                               ifelse(Dif_cost_2 >= 4, "Strongly\n overestimated", NA)))))))%>%
#   mutate(Cost_estimation = factor(Cost_estimation, levels = c("Strongly\n underestimated", "Under-\nestimated", "Estimated\n correctly", "Over-\nestimated", "Strongly\n overestimated", "Don't know")))%>%
#   group_by(Cost_estimation, Country)%>%
#   summarise(number = n())%>%
#   ungroup()%>%
#   group_by(Country)%>%
#   mutate(sum = sum(number))%>%
#   ungroup()%>%
#   mutate(share = number/sum)%>%
#   group_by(Country)%>%
#   mutate(share_sum = cumsum(share))%>%
#   ungroup()%>%
#   mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
#   mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
#   # bind_rows(data.frame(Country = factor("Spain", levels = levels(.$Country)),
#   #                      Cost_estimation = factor("Strongly\n underestimated", levels = levels(.$Cost_estimation)),
#   #                      share_sum = 0, label_0 = "0%"))%>%
#   # filter(Cost_estimation != "Don't know")%>%
#   mutate(Period = "t=1")
# 
# data_2.1.3 <- bind_rows(data_2.1.3.1, data_2.1.3.2)%>%
#   mutate(Period = factor(Period, levels = c("t=1", "t=0")))
# 
# P_2.1.3 <- ggplot(data_2.1.3, aes(x = Cost_estimation, y = Period))+
#   facet_grid(Country ~ .)+
#   theme_bw()+
#   geom_point(aes(fill = share_sum), shape = 22, size = 14)+
#   geom_text(aes(label = label_0), size = 4)+
#   scale_fill_distiller(limits = c(0,1))+
#   xlab("By how much will this policy increase your costs?")+
#   ggtitle("Perception of additional costs (Q42_1 and Q42_2)")+
#   guides(fill = "none")+
#   theme(panel.grid  = element_blank(),
#         axis.text.x = element_text(size = 6),
#         axis.text.y = element_text(size = 8),
#         axis.title  = element_text(size = 7))
# 
# jpeg("../5_Analysis/1_Descriptive/Figure_D3.jpg", width = 12, height = 12, unit = "cm", res = 600)
# print(P_2.1.3)
# dev.off()

data_2.1.3.3 <- data_2 %>%
  filter(!is.na(Dif_cost_1))%>%
  mutate(Cost_estimation = ifelse(is.na(Dif_cost_1), "Don't know", 
                                  ifelse(Dif_cost_1 < -3, "Strongly\nunderestimated",
                                         ifelse(Dif_cost_1 < 0, "Under-\nestimated", 
                                                ifelse(Dif_cost_1 == 0, "Estimated\ncorrectly",
                                                       ifelse(Dif_cost_1 > 0 & Dif_cost_1 < 4, "Over-\nestimated",
                                                              ifelse(Dif_cost_1 >= 4, "Strongly\noverestimated", NA)))))))%>%
  mutate(Cost_estimation = factor(Cost_estimation, levels = c("Estimated\ncorrectly", "Under-\nestimated", "Strongly\nunderestimated", "Over-\nestimated", "Strongly\noverestimated", "Don't know")))%>%
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
  mutate(Period = "t=0")%>%
  mutate(share = ifelse(Cost_estimation %in% c("Under-\nestimated", "Strongly\nunderestimated") , -share, share))%>%
  mutate(share = ifelse(Cost_estimation == "Estimated\ncorrectly", share/2, share))%>%
  mutate(side = ifelse(Cost_estimation == "Estimated\ncorrectly", "right", NA))

data_2.1.3.4 <- data_2.1.3.3 %>%
  bind_rows(mutate(mutate(filter(data_2.1.3.3, Cost_estimation == "Estimated\ncorrectly"), share = -share), side = "left"))%>%
  arrange(Country, Cost_estimation)

P_2.1.3A <- ggplot(data_2.1.3.4, aes(x = share, y = fct_rev(Country), fill = fct_rev(Cost_estimation)))+
  geom_vline(aes(xintercept = 0), linewidth = 0.3)+
  geom_col(position = "stack", colour = "black", width = 0.75, linewidth = 0.3)+
  theme_bw()+
  coord_cartesian(xlim = c(-0.8,0.8))+
  scale_fill_manual(guide = guide_legend(title.position = "top"),
                    values = c("#DC0000FF", "#E64B35FF", "#B09C85FF", "#91D1C2FF", "#00A087FF"),
                    breaks = c("Strongly\nunderestimated", "Under-\nestimated", "Estimated\ncorrectly", "Over-\nestimated", "Strongly\noverestimated"))+
  labs(fill = "By how much do you think will your annual expenditures increase because of this policy?")+
  scale_x_continuous(labels = \(x) scales::percent(abs(x)),
                     breaks = c(-0.75,-0.5,-0.25,0,0.25,0.5,0.75))+
  xlab("Share of respondents")+
  ylab("Country")+
  # ggtitle("Overall policy support (Q46_1 and Q46_2)")+
  theme(panel.grid.minor  = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 9),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(hjust = 0.5, size = 8))

pdf("../6_EUETS2_Citizens_Survey/1_Figures/A_Figure_A2.pdf", width = 140/25.4, height = 90/25.4)
print(P_2.1.3A)
dev.off()

jpeg("../5_Analysis/2_Presentation/A_Figure_A2.jpeg", width = 140 / 25.4, height = 90 / 25.4, units = "in", res = 300, quality = 100)
print(P_2.1.3A)
dev.off()

data_2.1.3.5 <- data_2 %>%
  filter(!is.na(Dif_cost_2))%>%
  mutate(Cost_estimation = ifelse(is.na(Dif_cost_2), "Don't know", 
                                  ifelse(Dif_cost_2 < -3, "Strongly\nunderestimated",
                                         ifelse(Dif_cost_2 < 0, "Under-\nestimated", 
                                                ifelse(Dif_cost_2 == 0, "Estimated\ncorrectly",
                                                       ifelse(Dif_cost_2 > 0 & Dif_cost_2 < 4, "Over-\nestimated",
                                                              ifelse(Dif_cost_2 >= 4, "Strongly\noverestimated", NA)))))))%>%
  mutate(Cost_estimation = factor(Cost_estimation, levels = c("Estimated\ncorrectly", "Under-\nestimated", "Strongly\nunderestimated", "Over-\nestimated", "Strongly\noverestimated", "Don't know")))%>%
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
  mutate(Period = "t=1")%>%
  mutate(share = ifelse(Cost_estimation %in% c("Under-\nestimated", "Strongly\nunderestimated") , -share, share))%>%
  mutate(share = ifelse(Cost_estimation == "Estimated\ncorrectly", share/2, share))%>%
  mutate(side = ifelse(Cost_estimation == "Estimated\ncorrectly", "right", NA))

data_2.1.3.6 <- data_2.1.3.5 %>%
  bind_rows(mutate(mutate(filter(data_2.1.3.5, Cost_estimation == "Estimated\ncorrectly"), share = -share), side = "left"))%>%
  arrange(Country, Cost_estimation)

data_2.1.3.7 <- bind_rows(data_2.1.3.4, data_2.1.3.6)%>%
  mutate(Period = factor(Period, levels = c("t=1", "t=0")))

P_2.1.3B <- ggplot(data_2.1.3.7, aes(x = share, y = Period, fill = fct_rev(Cost_estimation)))+
  facet_wrap(. ~ fct_rev(Country))+
  geom_vline(aes(xintercept = 0), linewidth = 0.3)+
  geom_col(position = "stack", colour = "black", width = 0.75, linewidth = 0.3)+
  theme_bw()+
  coord_cartesian(xlim = c(-0.8,0.8))+
  scale_fill_manual(guide = guide_legend(title.position = "top"),
                    values = c("#DC0000FF", "#E64B35FF", "#B09C85FF", "#91D1C2FF", "#00A087FF"),
                    breaks = c("Strongly\nunderestimated", "Under-\nestimated", "Estimated\ncorrectly", "Over-\nestimated", "Strongly\noverestimated"))+
  labs(fill = "By how much do you think will your annual expenditures increase because of this policy?")+
  scale_x_continuous(labels = \(x) scales::percent(abs(x)),
                     breaks = c(-0.75,-0.5,-0.25,0,0.25,0.5,0.75))+
  xlab("Share of respondents")+
  ylab("Country")+
  # ggtitle("Overall policy support (Q46_1 and Q46_2)")+
  theme(panel.grid.minor  = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 9),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(hjust = 0.5, size = 8))

pdf("../6_EUETS2_Citizens_Survey/1_Figures/A_Figure_A2_1.pdf", width = 140/25.4, height = 90/25.4)
print(P_2.1.3B)
dev.off()

data_2.1.3.table <- data_2 %>%
  mutate(Cost_estimation = ifelse(is.na(Dif_cost_1), "I don't know", 
                                  ifelse(Dif_cost_1 < -3, "Strongly underestimated",
                                         ifelse(Dif_cost_1 < 0, "Underestimated", 
                                                ifelse(Dif_cost_1 == 0, "Estimated correctly",
                                                       ifelse(Dif_cost_1 > 0 & Dif_cost_1 < 4, "Overestimated",
                                                              ifelse(Dif_cost_1 >= 4, "Strongly overestimated", NA)))))))%>%
  mutate(Cost_estimation = factor(Cost_estimation, levels = c("Strongly underestimated", "Underestimated", "Estimated correctly", "Overestimated", "Strongly overestimated", "I don't know")))%>%
  group_by(Cost_estimation, Country)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(Country)%>%
  mutate(sum = sum(number))%>%
  ungroup()%>%
  mutate(share = number/sum)%>%
  select(-number)%>%
  pivot_wider(names_from = "Cost_estimation", values_from = "share")%>%
  mutate(Country = factor(Country, levels = c("Spain", "France", "Germany" ,"Romania")))%>%
  arrange(Country)%>%
  mutate_at(vars(-c(Country, sum)), ~ paste(round(.*100,0), "%"))%>%
  rename(Sample = sum)%>%
  select(Country, everything(),-Sample, Sample)

colnames(data_2.1.3.table) <- c("Country", 
                                "Strongly underestimated", 
                                "Underestimated", 
                                "Estimated correctly", 
                                "Overestimated", 
                                "Strongly overestimated", 
                                "I don't know", 
                                "Sample")

kbl(data_2.1.3.table, format = "latex", linesep = "", booktabs = T, caption = "Distribution of responses: Perception of additional costs (Q42\\_1)",
    format.args = list(big.mark = ",", scientific = FALSE), align = "lccccccr", label = "Sum_Q42_1", digits = 2, na = "")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"), font_size = 9)%>%
  column_spec(1:8, width = "2cm")%>%
  footnote(general = "This table shows responses to question Q42_1 (By how much do you think will your annual expenditures increase because of this policy?) per country as percentage shares. Responses are compared against estimated additional costs for each respondent profile. Deviation by more than three brackets is defined as strongly over- or underestimated, respectively. Total number of valid answers in column Sample.", threeparttable = T)%>%
  save_kable(., "../6_EUETS2_Citizens_Survey/2_Tables/Table_B_3_Q42_1.tex")


rm(data_2.1.3, data_2.1.3.1, data_2.1.3.2, P_2.1.3, P_2.1.3A, data_2.1.3.3, data_2.1.3.4, data_2.1.3.5, data_2.1.3.6, data_2.1.3.7, P_2.1.3B, data_2.1.3.table)

# 2.1.4  Relative loss ####

# data_2.1.4.1 <- data_2 %>%
#   group_by(Q43_1N, Country)%>%
#   summarise(number = n())%>%
#   ungroup()%>%
#   group_by(Country)%>%
#   mutate(sum = sum(number))%>%
#   ungroup()%>%
#   mutate(share = number/sum)%>%
#   group_by(Country)%>%
#   mutate(share_sum = cumsum(share))%>%
#   ungroup()%>%
#   mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
#   mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
#   # filter(!is.na(Relative_loss_1N))%>%
#   mutate(Q43_1N_label = case_when(Q43_1N == 1 ~ "Much higher",
#                                   Q43_1N == 2 ~ "Somewhat higher",
#                                   Q43_1N == 3 ~ "Similar",
#                                   Q43_1N == 4 ~ "Somewhat lower",
#                                   Q43_1N == 5 ~ "Much lower",
#                                   is.na(Q43_1N) ~ "Don't know"))%>%
#   mutate(Q43_1N_label = factor(Q43_1N_label, levels = c("Much higher", "Somewhat higher", "Similar", "Somewhat lower", "Much lower", "Don't know")))%>%
#   mutate(Period = "t=0")
# 
# data_2.1.4.2 <- data_2 %>%
#   filter(Treatment_C != "Control")%>%
#   group_by(Q43_2N, Country)%>%
#   summarise(number = n())%>%
#   ungroup()%>%
#   group_by(Country)%>%
#   mutate(sum = sum(number))%>%
#   ungroup()%>%
#   mutate(share = number/sum)%>%
#   group_by(Country)%>%
#   mutate(share_sum = cumsum(share))%>%
#   ungroup()%>%
#   mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
#   mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
#   # filter(!is.na(Relative_loss_2N))%>%
#   mutate(Q43_1N_label = case_when(Q43_2N == 1 ~ "Much higher",
#                                   Q43_2N == 2 ~ "Somewhat higher",
#                                   Q43_2N == 3 ~ "Similar",
#                                   Q43_2N == 4 ~ "Somewhat lower",
#                                   Q43_2N == 5 ~ "Much lower",
#                                   is.na(Q43_2N) ~ "Don't know"))%>%
#   mutate(Q43_1N_label = factor(Q43_1N_label, levels = c("Much higher", "Somewhat higher", "Similar", "Somewhat lower", "Much lower", "Don't know")))%>%
#   mutate(Period = "t=1")
# 
# data_2.1.4 <- bind_rows(data_2.1.4.1, data_2.1.4.2)%>%
#   mutate(Period = factor(Period, levels = c("t=1", "t=0")))
# 
# P_2.1.4 <- ggplot(data_2.1.4, aes(x = Q43_1N_label, y = Period))+
#   facet_grid(Country ~ .)+
#   theme_bw()+
#   geom_point(aes(fill = share_sum), shape = 22, size = 14)+
#   geom_text(aes(label = label_0), size = 4)+
#   scale_fill_distiller(limits = c(0,1))+
#   xlab("How many costs do you expect\n in comparison to an average household?")+
#   ggtitle("Additional relative costs (Q43_1 and Q43_2)")+
#   guides(fill = "none")+
#   theme(panel.grid  = element_blank(),
#         axis.text.x = element_text(size = 7),
#         axis.text.y = element_text(size = 8),
#         axis.title  = element_text(size = 7))
# 
# jpeg("../5_Analysis/1_Descriptive/Figure_D4.jpg", width = 12, height = 12, unit = "cm", res = 600)
# print(P_2.1.4)
# dev.off()

data_2.1.4.3 <- data_2 %>%
  filter(!is.na(Q43_1N))%>%
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
  # mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
  mutate(Country = factor(Country, levels = c("Spain", "France", "Germany", "Romania")))%>%
  mutate(Q43_1N_label = case_when(Q43_1N == 1 ~ "Much\nhigher",
                                  Q43_1N == 2 ~ "Somewhat\nhigher",
                                  Q43_1N == 3 ~ "Similar",
                                  Q43_1N == 4 ~ "Somewhat\nlower",
                                  Q43_1N == 5 ~ "Much\nlower",
                                  is.na(Q43_1N) ~ "Don't know"))%>%
  mutate(Q43_1N_label = factor(Q43_1N_label, levels = c("Similar", "Somewhat\nhigher", "Much\nhigher", "Somewhat\nlower", "Much\nlower")))%>%
  mutate(Period = "t=0")%>%
  mutate(share = ifelse(Q43_1N < 3, -share, share))%>%
  mutate(share = ifelse(Q43_1N == 3, share/2, share))%>%
  mutate(side = ifelse(Q43_1N == 3, "right", NA))

data_2.1.4.4 <- data_2.1.4.3 %>%
  bind_rows(mutate(mutate(filter(data_2.1.4.3, Q43_1N == 3), share = -share), side = "left"))%>%
  arrange(Country, Q43_1N)

P_2.1.4A <- ggplot(data_2.1.4.4, aes(x = share, y = fct_rev(Country), fill = fct_rev(Q43_1N_label)))+
  geom_vline(aes(xintercept = 0), linewidth = 0.3)+
  geom_col(position = "stack", colour = "black", width = 0.75, linewidth = 0.3)+
  theme_bw()+
  coord_cartesian(xlim = c(-0.76,0.76))+
  scale_fill_manual(guide = guide_legend(title.position = "top"),
                    values = c("#DC0000FF", "#E64B35FF", "#B09C85FF", "#91D1C2FF", "#00A087FF"),
                    breaks = c("Much\nhigher", "Somewhat\nhigher", "Similar", "Somewhat\nlower", "Much\nlower"))+
  labs(fill = "How high will the costs of this policy be for you compared to an average household?")+
  scale_x_continuous(labels = \(x) scales::percent(abs(x)),
                     breaks = c(-0.75,-0.5,-0.25,0,0.25,0.5,0.75))+
  xlab("Share of respondents")+
  ylab("Country")+
  # ggtitle("Overall policy support (Q46_1 and Q46_2)")+
  theme(panel.grid.minor  = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 9),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(hjust = 0.5, size = 8))

pdf("../6_EUETS2_Citizens_Survey/1_Figures/A_Figure_A3.pdf", width = 140/25.4, height = 90/25.4)
print(P_2.1.4A)
dev.off()

jpeg("../5_Analysis/2_Presentation/A_Figure_A3.jpeg", width = 140 / 25.4, height = 90 / 25.4, units = "in", res = 300, quality = 100)
print(P_2.1.4A)
dev.off()

data_2.1.4.table <- data_2 %>%
  mutate(Q43_1N_label = case_when(Q43_1N == 1 ~ "Much higher",
                                  Q43_1N == 2 ~ "Somewhat higher",
                                  Q43_1N == 3 ~ "Similar",
                                  Q43_1N == 4 ~ "Somewhat lower",
                                  Q43_1N == 5 ~ "Much lower",
                                  is.na(Q43_1N) ~ "I don't know"))%>%
  mutate(Q43_1N_label = factor(Q43_1N_label, levels = c("Much higher", "Somewhat higher", "Similar", "Somewhat lower", "Much lower", "I don't know")))%>%
  group_by(Q43_1N_label, Country)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(Country)%>%
  mutate(sum = sum(number))%>%
  ungroup()%>%
  mutate(share = number/sum)%>%
  select(-number)%>%
  pivot_wider(names_from = "Q43_1N_label", values_from = "share")%>%
  mutate(Country = factor(Country, levels = c("Spain", "France", "Germany" ,"Romania")))%>%
  arrange(Country)%>%
  mutate_at(vars(-c(Country, sum)), ~ paste(round(.*100,0), "%"))%>%
  rename(Sample = sum)%>%
  select(Country, everything(),-Sample, Sample)

kbl(data_2.1.4.table, format = "latex", linesep = "", booktabs = T, caption = "Distribution of responses: Perception of additional costs relative to others (Q43\\_1)",
    format.args = list(big.mark = ",", scientific = FALSE), align = "lccccccr", label = "Sum_Q43_1", digits = 2, na = "")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"), font_size = 9)%>%
  footnote(general = "This table shows responses to question Q43_1 (How high will the costs of this policy be for you compared to an average household?) per country as percentage shares. Total number of valid answers in column Sample.", threeparttable = T)%>%
  save_kable(., "../6_EUETS2_Citizens_Survey/2_Tables/Table_B_4_Q43_1.tex")

rm(data_2.1.4, data_2.1.4.1, data_2.1.4.2, P_2.1.4, data_2.1.4.3, data_2.1.4.4, P_2.1.4A, data_2.1.4.table)

# 2.1.5  Vulnerable ####

# data_2.1.5.1 <- data_2 %>%
#   group_by(Q44_1N, Country)%>%
#   summarise(number = n())%>%
#   ungroup()%>%
#   group_by(Country)%>%
#   mutate(sum = sum(number))%>%
#   ungroup()%>%
#   mutate(share = number/sum)%>%
#   group_by(Country)%>%
#   mutate(share_sum = cumsum(share))%>%
#   ungroup()%>%
#   mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
#   mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
#   #filter(!is.na(Vulnerable_1N))%>%
#   mutate(Q44_1N_label = case_when(Q44_1N == 1   ~ "Rather hurt",
#                                   Q44_1N == 2   ~ "Neither hurt\n nor help",
#                                   Q44_1N == 3   ~ "Rather help",
#                                   is.na(Q44_1N) ~ "Don't know"))%>%
#   mutate(Q44_1N_label = factor(Q44_1N_label, levels = c("Rather hurt", "Neither hurt\n nor help", "Rather help", "Don't know")))%>%
#   mutate(Period = "t=0")
# 
# data_2.1.5.2 <- data_2 %>%
#   group_by(Q44_2N, Country)%>%
#   summarise(number = n())%>%
#   ungroup()%>%
#   group_by(Country)%>%
#   mutate(sum = sum(number))%>%
#   ungroup()%>%
#   mutate(share = number/sum)%>%
#   group_by(Country)%>%
#   mutate(share_sum = cumsum(share))%>%
#   ungroup()%>%
#   mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
#   mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
#   # filter(!is.na(Vulnerable_2N))%>%
#   mutate(Q44_1N_label = case_when(Q44_2N == 1 ~ "Rather hurt",
#                                   Q44_2N == 2 ~ "Neither hurt\n nor help",
#                                   Q44_2N == 3 ~ "Rather help",
#                                   is.na(Q44_2N) ~ "Don't know"))%>%
#   mutate(Q44_1N_label = factor(Q44_1N_label, levels = c("Rather hurt", "Neither hurt\n nor help", "Rather help", "Don't know")))%>%
#   mutate(Period = "t=1")
# 
# data_2.1.5 <- bind_rows(data_2.1.5.1, data_2.1.5.2)%>%
#   mutate(Period = factor(Period, levels = c("t=1", "t=0")))
# 
# P_2.1.5 <- ggplot(data_2.1.5, aes(x = Q44_1N_label, y = Period))+
#   facet_grid(Country ~ .)+
#   theme_bw()+
#   geom_point(aes(fill = share_sum), shape = 22, size = 14)+
#   geom_text(aes(label = label_0), size = 4)+
#   scale_fill_distiller(limits = c(0,1))+
#   xlab("Will this policy help or hurt the most vulnerable households?")+
#   ggtitle("Vulnerable households (Q44_1 and Q44_2)")+
#   guides(fill = "none")+
#   theme(panel.grid  = element_blank(),
#         axis.text.x = element_text(size = 7),
#         axis.text.y = element_text(size = 8),
#         axis.title  = element_text(size = 7))
# 
# jpeg("../5_Analysis/1_Descriptive/Figure_D5.jpg", width = 12, height = 12, unit = "cm", res = 600)
# print(P_2.1.5)
# dev.off()

data_2.1.5.3 <- data_2 %>%
  filter(!is.na(Q44_1N))%>%
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
  # mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
  mutate(Country = factor(Country, levels = c("Spain", "France", "Germany", "Romania")))%>%
  mutate(Q44_1N_label = case_when(Q44_1N == 1 ~ "Rather hurt",
                                  Q44_1N == 2 ~ "Neither hurt nor help",
                                  Q44_1N == 3 ~ "Rather help",
                                  is.na(Q44_1N) ~ "Don't know"))%>%
  mutate(Q44_1N_label = factor(Q44_1N_label, levels = c("Neither hurt nor help", "Rather hurt", "Rather help")))%>%
  mutate(Period = "t=0")%>%
  mutate(share = ifelse(Q44_1N < 2, -share, share))%>%
  mutate(share = ifelse(Q44_1N == 2, share/2, share))%>%
  mutate(side = ifelse(Q44_1N == 2, "right", NA))

data_2.1.5.4 <- data_2.1.5.3 %>%
  bind_rows(mutate(mutate(filter(data_2.1.5.3, Q44_1N == 2), share = -share), side = "left"))%>%
  arrange(Country, Q44_1N)

P_2.1.5A <- ggplot(data_2.1.5.4, aes(x = share, y = fct_rev(Country), fill = fct_rev(Q44_1N_label)))+
  geom_vline(aes(xintercept = 0), linewidth = 0.3)+
  geom_col(position = "stack", colour = "black", width = 0.75, linewidth = 0.3)+
  theme_bw()+
  coord_cartesian(xlim = c(-0.80,0.80))+
  scale_fill_manual(guide = guide_legend(title.position = "top"),
                    values = c("#DC0000FF", "#B09C85FF", "#00A087FF"),
                    breaks = c("Rather hurt", "Neither hurt nor help", "Rather help"))+
  labs(fill = "Do you think this policy will help or hurt the most vulnerable households?")+
  scale_x_continuous(labels = \(x) scales::percent(abs(x)),
                     breaks = c(-0.75,-0.5,-0.25,0,0.25,0.5,0.75))+
  xlab("Share of respondents")+
  ylab("Country")+
  # ggtitle("Overall policy support (Q46_1 and Q46_2)")+
  theme(panel.grid.minor  = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 9),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(hjust = 0.5, size = 8))

pdf("../6_EUETS2_Citizens_Survey/1_Figures/A_Figure_A4.pdf", width = 140/25.4, height = 90/25.4)
print(P_2.1.5A)
dev.off()

jpeg("../5_Analysis/2_Presentation/A_Figure_A4.jpeg", width = 140 / 25.4, height = 90 / 25.4, units = "in", res = 300, quality = 100)
print(P_2.1.5A)
dev.off()

data_2.1.5.table <- data_2 %>%
  mutate(Q44_1N_label = case_when(Q44_1N == 1 ~ "Rather hurt",
                                  Q44_1N == 2 ~ "Neither hurt nor help",
                                  Q44_1N == 3 ~ "Rather help",
                                  is.na(Q44_1N) ~ "I don't know"))%>%
  mutate(Q44_1N_label = factor(Q44_1N_label, levels = c("Rather hurt", "Neither hurt nor help", "Rather help", "I don't know")))%>%
  group_by(Q44_1N_label, Country)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(Country)%>%
  mutate(sum = sum(number))%>%
  ungroup()%>%
  mutate(share = number/sum)%>%
  select(-number)%>%
  pivot_wider(names_from = "Q44_1N_label", values_from = "share")%>%
  mutate(Country = factor(Country, levels = c("Spain", "France", "Germany" ,"Romania")))%>%
  arrange(Country)%>%
  mutate_at(vars(-c(Country, sum)), ~ paste(round(.*100,0), "%"))%>%
  rename(Sample = sum)%>%
  select(Country, everything(),-Sample, Sample)

kbl(data_2.1.5.table, format = "latex", linesep = "", booktabs = T, caption = "Distribution of responses: Perception of effects on vulnerable households (Q44\\_1)",
    format.args = list(big.mark = ",", scientific = FALSE), align = "lccccr", label = "Sum_Q44_1", digits = 2, na = "")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"), font_size = 9)%>%
  footnote(general = "This table shows responses to question Q44_1 (Do you think this policy will help or hurt the most vulnerable households?) per country as percentage shares. Total number of valid answers in column Sample.", threeparttable = T)%>%
  save_kable(., "../6_EUETS2_Citizens_Survey/2_Tables/Table_B_5_Q44_1.tex")

rm(data_2.1.5, data_2.1.5.1, data_2.1.5.2, P_2.1.5, P_2.1.5A, data_2.1.5.3, data_2.1.5.4, data_2.1.5.table)

# 2.1.6  Fairness ####

# data_2.1.6.1 <- data_2 %>%
#   group_by(Q45_1N, Country)%>%
#   summarise(number = n())%>%
#   ungroup()%>%
#   group_by(Country)%>%
#   mutate(sum = sum(number))%>%
#   ungroup()%>%
#   mutate(share = number/sum)%>%
#   group_by(Country)%>%
#   mutate(share_sum = cumsum(share))%>%
#   ungroup()%>%
#   mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
#   mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
#   #filter(!is.na(Fairness_1N))%>%
#   mutate(Q45_1N_label = case_when(Q45_1N == 1      ~ "Unfair",
#                                   Q45_1N == 2      ~ "Neither fair\n nor unfair",
#                                   Q45_1N == 3      ~ "Fair",
#                                   is.na(Q45_1N)    ~ "Don't know"))%>%
#   mutate(Q45_1N_label = factor(Q45_1N_label, levels = c("Unfair", "Neither fair\n nor unfair", "Fair", "Don't know")))%>%
#   mutate(Period = "t=0")
# 
# data_2.1.6.2 <- data_2 %>%
#  group_by(Q45_2N, Country)%>%
#  summarise(number = n())%>%
#  ungroup()%>%
#  group_by(Country)%>%
#  mutate(sum = sum(number))%>%
#  ungroup()%>%
#  mutate(share = number/sum)%>%
#  group_by(Country)%>%
#  mutate(share_sum = cumsum(share))%>%
#  ungroup()%>%
#  mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
#  mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
#  # filter(!is.na(Fairness_2N))%>%
#  mutate(Q45_1N_label = case_when(Q45_2N == 1   ~ "Unfair",
#                                  Q45_2N == 2   ~ "Neither fair\n nor unfair",
#                                  Q45_2N == 3   ~ "Fair",
#                                  is.na(Q45_2N) ~ "Don't know"))%>%
#  mutate(Q45_1N_label = factor(Q45_1N_label, levels = c("Unfair", "Neither fair\n nor unfair", "Fair", "Don't know")))%>%
#  mutate(Period = "t=1")
# 
# data_2.1.6 <- bind_rows(data_2.1.6.1, data_2.1.6.2)%>%
#  mutate(Period = factor(Period, levels = c("t=1", "t=0")))
# 
# P_2.1.6 <- ggplot(data_2.1.6, aes(x = Q45_1N_label, y = Period))+
#  facet_grid(Country ~ .)+
#  theme_bw()+
#  geom_point(aes(fill = share_sum), shape = 22, size = 14)+
#  geom_text(aes(label = label_0), size = 4)+
#  scale_fill_distiller(limits = c(0,1))+
#  xlab("Do you find this policy fair or unfair?")+
#  ggtitle("Perception of fairness (Q45_1 and Q45_2)")+
#  guides(fill = "none")+
#  theme(panel.grid  = element_blank(),
#        axis.text.x = element_text(size = 7),
#        axis.text.y = element_text(size = 8),
#        axis.title  = element_text(size = 7))
# 
# jpeg("../5_Analysis/1_Descriptive/Figure_D6.jpg", width = 12, height = 12, unit = "cm", res = 600)
# print(P_2.1.6)
# dev.off()

data_2.1.6.3 <- data_2 %>%
  filter(!is.na(Q45_1N))%>%
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
  # mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
  mutate(Country = factor(Country, levels = c("Spain", "France", "Germany", "Romania")))%>%
  mutate(Q45_1N_label = case_when(Q45_1N == 1      ~ "Unfair",
                                  Q45_1N == 2      ~ "Neither fair nor unfair",
                                  Q45_1N == 3      ~ "Fair",
                                  is.na(Q45_1N)    ~ "Don't know"))%>%
  mutate(Q45_1N_label = factor(Q45_1N_label, levels = c("Neither fair nor unfair", "Unfair", "Fair")))%>%
  mutate(Period = "t=0")%>%
  mutate(share = ifelse(Q45_1N < 2, -share, share))%>%
  mutate(share = ifelse(Q45_1N == 2, share/2, share))%>%
  mutate(side = ifelse(Q45_1N == 2, "right", NA))

data_2.1.6.4 <- data_2.1.6.3 %>%
  bind_rows(mutate(mutate(filter(data_2.1.6.3, Q45_1N == 2), share = -share), side = "left"))%>%
  arrange(Country, Q45_1N)

P_2.1.6A <- ggplot(data_2.1.6.4, aes(x = share, y = fct_rev(Country), fill = fct_rev(Q45_1N_label)))+
  geom_vline(aes(xintercept = 0), linewidth = 0.3)+
  geom_col(position = "stack", colour = "black", width = 0.75, linewidth = 0.3)+
  theme_bw()+
  coord_cartesian(xlim = c(-0.75,0.75))+
  scale_fill_manual(guide = guide_legend(title.position = "top"),
                    values = c("#DC0000FF", "#B09C85FF", "#00A087FF"),
                    breaks = c("Unfair", "Neither fair nor unfair", "Fair"))+
  labs(fill = "Do you find this policy fair?")+
  scale_x_continuous(labels = \(x) scales::percent(abs(x)),
                     breaks = c(-0.75,-0.5,-0.25,0,0.25,0.5,0.75))+
  xlab("Share of respondents")+
  ylab("Country")+
  # ggtitle("Overall policy support (Q46_1 and Q46_2)")+
  theme(panel.grid.minor  = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 9),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(hjust = 0.5, size = 8))

pdf("../6_EUETS2_Citizens_Survey/1_Figures/A_Figure_A5.pdf", width = 140/25.4, height = 90/25.4)
print(P_2.1.6A)
dev.off()

jpeg("../5_Analysis/2_Presentation/A_Figure_A5.jpeg", width = 140 / 25.4, height = 90 / 25.4, units = "in", res = 300, quality = 100)
print(P_2.1.6A)
dev.off()

data_2.1.6.table <- data_2 %>%
  mutate(Q45_1N_label = case_when(Q45_1N == 1 ~ "Unfair",
                                  Q45_1N == 2 ~ "Neither fair nor unfair",
                                  Q45_1N == 3 ~ "Fair",
                                  is.na(Q45_1N) ~ "I don't know"))%>%
  mutate(Q45_1N_label = factor(Q45_1N_label, levels = c("Unfair", "Neither fair nor unfair", "Fair", "I don't know")))%>%
  group_by(Q45_1N_label, Country)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(Country)%>%
  mutate(sum = sum(number))%>%
  ungroup()%>%
  mutate(share = number/sum)%>%
  select(-number)%>%
  pivot_wider(names_from = "Q45_1N_label", values_from = "share")%>%
  mutate(Country = factor(Country, levels = c("Spain", "France", "Germany" ,"Romania")))%>%
  arrange(Country)%>%
  mutate_at(vars(-c(Country, sum)), ~ paste(round(.*100,0), "%"))%>%
  rename(Sample = sum)%>%
  select(Country, everything(),-Sample, Sample)

kbl(data_2.1.6.table, format = "latex", linesep = "", booktabs = T, caption = "Distribution of responses: Perception of fairness (Q45\\_1)",
    format.args = list(big.mark = ",", scientific = FALSE), align = "lccccr", label = "Sum_Q45_1", digits = 2, na = "")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"), font_size = 9)%>%
  footnote(general = "This table shows responses to question Q45_1 (Do you find this policy fair?) per country as percentage shares. Total number of valid answers in column Sample.", threeparttable = T)%>%
  save_kable(., "../6_EUETS2_Citizens_Survey/2_Tables/Table_B_6_Q45_1.tex")


rm(data_2.1.6, data_2.1.6.1, data_2.1.6.2, P_2.1.6, data_2.1.6.3, data_2.1.6.4, P_2.1.6A, data_2.1.6.table)

# 2.1.7  Expected costs (in €) ####

data_2.1.7 <- data_2 %>%
  filter(!is.na(Q42_1_average))%>%
  mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
  mutate(Period = "t=0")%>%
  mutate(Q42_1_average = ifelse(Country == "Romania", Q42_1_average*0.223, Q42_1_average))%>%
  group_by(Country)%>%
  summarise(q5   = quantile(Q42_1_average, probs = 0.05),
         q25  = quantile(Q42_1_average, probs = 0.25),
         q50  = quantile(Q42_1_average, probs = 0.5),
         q75  = quantile(Q42_1_average, probs = 0.75),
         q95  = quantile(Q42_1_average, probs = 0.95),
         mean = mean(Q42_1_average))%>%
  ungroup()

P_2.1.7 <- ggplot(data_2.1.7, aes(y = Country))+
  geom_vline(aes(xintercept = 0), linewidth = 0.3)+
  geom_boxplot(aes(xmin = q5, xlower = q25, xmiddle = q50, xupper = q75, xmax = q95), 
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3, fill = "#B09C85FF")+
  geom_point(data = data_2.1.7, aes(x = mean), shape = 23, size = 2, stroke = 0.3, fill = "white")+
  theme_bw()+
  ggtitle("By how much do you think will your annual expenditures increase because of this policy (in €)?")+
  scale_x_continuous(labels = scales::dollar_format(prefix = "€"))+
  #scale_x_continuous(labels = \(x) scales::dollar_format(prefix = "€"))+
  xlab("Increase in annual expenditures")+
  ylab("Country")+
  # ggtitle("Overall policy support (Q46_1 and Q46_2)")+
  theme(panel.grid.minor  = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 9),
        plot.title = element_text(size = 8),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(hjust = 0.5, size = 8))

pdf("../6_EUETS2_Citizens_Survey/1_Figures/A_Figure_A6.pdf", width = 140/25.4, height = 90/25.4)
print(P_2.1.7)
dev.off()

data_2.1.7.1 <- data_2 %>%
  filter(!is.na(Q42_1_average))%>%
  mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
  mutate(Period = "t=0")%>%
  mutate(Q42_1_average = ifelse(Country == "Romania", Q42_1_average*0.223, Q42_1_average))%>%
  group_by(Country, Pricelevel)%>%
  summarise(q5   = quantile(Q42_1_average, probs = 0.05),
            q25  = quantile(Q42_1_average, probs = 0.25),
            q50  = quantile(Q42_1_average, probs = 0.5),
            q75  = quantile(Q42_1_average, probs = 0.75),
            q95  = quantile(Q42_1_average, probs = 0.95),
            mean = mean(Q42_1_average))%>%
  ungroup()

P_2.1.7.1 <- ggplot(data_2.1.7.1, aes(y = Pricelevel))+
  facet_grid(Country ~ .)+
  geom_vline(aes(xintercept = 0), linewidth = 0.3)+
  geom_boxplot(aes(xmin = q5, xlower = q25, xmiddle = q50, xupper = q75, xmax = q95), 
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3, fill = "#B09C85FF")+
  geom_point(data = data_2.1.7.1, aes(x = mean), shape = 23, size = 2, stroke = 0.3, fill = "white")+
  theme_bw()+
  ggtitle("By how much do you think will your annual expenditures increase because of this policy (in €)?")+
  scale_x_continuous(labels = scales::dollar_format(prefix = "€"))+
  #scale_x_continuous(labels = \(x) scales::dollar_format(prefix = "€"))+
  xlab("Increase in annual expenditures")+
  ylab("Country")+
  # ggtitle("Overall policy support (Q46_1 and Q46_2)")+
  theme(panel.grid.minor  = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 9),
        plot.title = element_text(size = 8),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(hjust = 0.5, size = 8))

pdf("../6_EUETS2_Citizens_Survey/1_Figures/A_Figure_A6_1.pdf", width = 140/25.4, height = 90/25.4)
print(P_2.1.7.1)
dev.off()

rm(data_2.1.7.1, P_2.1.7.1)

# 2.1.8  Expected costs (in % of total expenditures) ####

data_2.1.8 <- data_2 %>%
  filter(!is.na(Q42_1_relative))%>%
  mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
  mutate(Period = "t=0")%>%
  # mutate(Q42_1_relative = ifelse(Country == "Romania", Q42_1_average*0.223, Q42_1_average))%>%
  group_by(Country)%>%
  summarise(q5   = quantile(Q42_1_relative, probs = 0.05),
            q25  = quantile(Q42_1_relative, probs = 0.25),
            q50  = quantile(Q42_1_relative, probs = 0.5),
            q75  = quantile(Q42_1_relative, probs = 0.75),
            q95  = quantile(Q42_1_relative, probs = 0.95),
            mean = mean(Q42_1_relative))%>%
  ungroup()

P_2.1.8 <- ggplot(data_2.1.8, aes(y = Country))+
  geom_vline(aes(xintercept = 0), linewidth = 0.3)+
  geom_boxplot(aes(xmin = q5, xlower = q25, xmiddle = q50, xupper = q75, xmax = q95), 
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3, fill = "#B09C85FF")+
  geom_point(data = data_2.1.8, aes(x = mean), shape = 23, size = 2, stroke = 0.3, fill = "white")+
  theme_bw()+
  ggtitle("By how much do you think will your annual expenditures increase because of this policy (in %)?")+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = c(0,0.02,0.04,0.06,0.08,0.1,0.12))+
  #scale_x_continuous(labels = \(x) scales::dollar_format(prefix = "€"))+
  xlab("Increase in annual expenditures (relative to total household expenditures)")+
  ylab("Country")+
  # ggtitle("Overall policy support (Q46_1 and Q46_2)")+
  theme(panel.grid.minor  = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 9),
        plot.title = element_text(size = 8),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(hjust = 0.5, size = 8))

pdf("../6_EUETS2_Citizens_Survey/1_Figures/A_Figure_A7.pdf", width = 140/25.4, height = 90/25.4)
print(P_2.1.8)
dev.off()

rm(data_2.1.8, P_2.1.8)

data_2.1.8.1 <- data_2 %>%
  filter(!is.na(Q42_1_relative))%>%
  mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))%>%
  mutate(Period = "t=0")%>%
  # mutate(Q42_1_relative = ifelse(Country == "Romania", Q42_1_average*0.223, Q42_1_average))%>%
  group_by(Country, Pricelevel)%>%
  summarise(q5   = quantile(Q42_1_relative, probs = 0.05),
            q25  = quantile(Q42_1_relative, probs = 0.25),
            q50  = quantile(Q42_1_relative, probs = 0.5),
            q75  = quantile(Q42_1_relative, probs = 0.75),
            q95  = quantile(Q42_1_relative, probs = 0.95),
            mean = mean(Q42_1_relative))%>%
  ungroup()

P_2.1.8.1 <- ggplot(data_2.1.8.1, aes(y = Pricelevel))+
  facet_wrap(. ~ Country)+
  geom_vline(aes(xintercept = 0), linewidth = 0.3)+
  geom_boxplot(aes(xmin = q5, xlower = q25, xmiddle = q50, xupper = q75, xmax = q95), 
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3, fill = "#B09C85FF")+
  geom_point(data = data_2.1.8.1, aes(x = mean), shape = 23, size = 2, stroke = 0.3, fill = "white")+
  theme_bw()+
  ggtitle("By how much do you think will your annual expenditures increase because of this policy (in %)?")+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = c(0,0.02,0.04,0.06,0.08,0.1,0.12))+
  #scale_x_continuous(labels = \(x) scales::dollar_format(prefix = "€"))+
  xlab("Increase in annual expenditures (relative to total household expenditures)")+
  ylab("Country")+
  # ggtitle("Overall policy support (Q46_1 and Q46_2)")+
  theme(panel.grid.minor  = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 9),
        plot.title = element_text(size = 8),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(hjust = 0.5, size = 8))

pdf("../6_EUETS2_Citizens_Survey/1_Figures/A_Figure_A7_1.pdf", width = 140/25.4, height = 90/25.4)
print(P_2.1.8.1)
dev.off()

rm(data_2.1.8, P_2.1.8)

# 2.1.9  Price levels ####

data_2.1.9 <- data_2 %>%
  filter(!is.na(Q46_1N))%>%
  group_by(Q46_1N, Country, Pricelevel)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(Country, Pricelevel)%>%
  mutate(sum = sum(number))%>%
  ungroup()%>%
  mutate(share = number/sum)%>%
  group_by(Country, Pricelevel)%>%
  mutate(share_sum = cumsum(share))%>%
  ungroup()%>%
  # mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
  mutate(Country = factor(Country, levels = c("Spain", "France", "Germany", "Romania")))%>%
  mutate(Q46_1N_label = case_when(Q46_1N == 1 ~ "Strongly\n oppose",
                                  Q46_1N == 2 ~ "Rather\n oppose",
                                  Q46_1N == 3 ~ "Neutral",
                                  Q46_1N == 4 ~ "Rather\n support",
                                  Q46_1N == 5 ~ "Strongly\n support"))%>%
  mutate(Q46_1N_label = factor(Q46_1N_label, levels = c("Neutral", "Rather\n oppose", "Strongly\n oppose", "Rather\n support", "Strongly\n support")))%>%
  mutate(Period = "t=0")%>%
  mutate(share = ifelse(Q46_1N < 3, -share, share))%>%
  mutate(share = ifelse(Q46_1N == 3, share/2, share))%>%
  mutate(side = ifelse(Q46_1N == 3, "right", NA))

data_2.1.9.1 <- data_2.1.9 %>%
  bind_rows(mutate(mutate(filter(data_2.1.9, Q46_1N == 3), share = -share), side = "left"))%>%
  arrange(Country, Q46_1N)

P_2.1.9 <- ggplot(data_2.1.9.1, aes(x = share, y = fct_rev(Pricelevel), fill = fct_rev(Q46_1N_label)))+
  facet_grid(Country ~ .)+
  geom_vline(aes(xintercept = 0), linewidth = 0.3)+
  geom_col(position = "stack", colour = "black", width = 0.75, linewidth = 0.3)+
  theme_bw()+
  coord_cartesian(xlim = c(-0.76,0.76))+
  scale_fill_manual(guide = guide_legend(title.position = "top"),
                    values = c("#DC0000FF", "#E64B35FF", "#B09C85FF", "#91D1C2FF", "#00A087FF"),
                    breaks = c("Strongly\n oppose", "Rather\n oppose", "Neutral", "Rather\n support", "Strongly\n support"))+
  labs(fill = "Do you support or oppose the EU ETS2?")+
  scale_x_continuous(labels = \(x) scales::percent(abs(x)),
                     breaks = c(-0.75,-0.5,-0.25,0,0.25,0.5,0.75))+
  xlab("Share of respondents")+
  ylab("Carbon price level [€]")+
  # ggtitle("Overall policy support (Q46_1 and Q46_2)")+
  theme(panel.grid.minor  = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 9),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(hjust = 0.5, size = 8))

pdf("../6_EUETS2_Citizens_Survey/1_Figures/A_Figure_A8.pdf", width = 140/25.4, height = 110/25.4)
print(P_2.1.9)
dev.off()

rm(data_2.1.9, data_2.1.9.1, P_2.1.9)

# 2.1.10 Understanding and agreement with information treatments ####

data_2.1.10.1 <- data_2 %>%
  mutate(Country = paste0(Country, " (", Treatment_B, ")"))%>%
  group_by(Country)%>%
  count(Q51)%>%
  mutate(share = n/sum(n))%>%
  filter(Q51 %in% c("Wahr", "Vrai", "Adevărat", "Verdadero"))%>%
  rename("Agreement (B)" = share)%>%
  select(Country, "Agreement (B)")

data_2.1.10.2 <- data_2 %>%
  filter(!is.na(Q52A))%>%
  mutate(Country = paste0(Country, " (", Treatment_B, ")"))%>%
  group_by(Country)%>%
  count(Q52A)%>%
  mutate(share = n/sum(n))%>%
  filter(Q52A %in% c("Ja", "Oui", "Da", "Sí"))%>%
  rename("Clarity (B)" = share)%>%
  select(Country, "Clarity (B)")

data_2.1.10.3 <- data_2 %>%
  filter(!is.na(Q52B))%>%
  mutate(Country = paste0(Country, " (", Treatment_B, ")"))%>%
  group_by(Country)%>%
  count(Q52B)%>%
  mutate(share = n/sum(n))%>%
  filter(Q52B %in% c("Ja", "Oui", "Da", "Sí"))%>%
  rename("Credibility (B)" = share)%>%
  select(Country, "Credibility (B)")

data_2.1.10.4 <- data_2 %>%
  filter(!is.na(Q57))%>%
  mutate(Country = paste0(Country, " (Treatment)"))%>%
  group_by(Country)%>%
  count(Q57)%>%
  mutate(share = n/sum(n))%>%
  filter(Q57 %in% c("Ja", "Oui", "Da", "Sí"))%>%
  rename("Clarity (C)" = share)%>%
  select(Country, "Clarity (C)")

data_2.1.10.5 <- data_2 %>%
  filter(!is.na(Q58))%>%
  mutate(Country = paste0(Country, " (Treatment)"))%>%
  group_by(Country)%>%
  count(Q58)%>%
  mutate(share = n/sum(n))%>%
  filter(Q58 %in% c("Ja", "Oui", "Da", "Sí"))%>%
  rename("Credibility (C)" = share)%>%
  select(Country, "Credibility (C)")

data_2.1.10 <- data_2.1.10.1 %>%
  left_join(data_2.1.10.2)%>%
  left_join(data_2.1.10.3)%>%
  left_join(data_2.1.10.4)%>%
  left_join(data_2.1.10.5)%>%
  mutate(Country = factor(Country, levels = c("Spain (Control)",   "Spain (Treatment)",   "France (Control)",  "France (Treatment)",
                                              "Germany (Control)", "Germany (Treatment)", "Romania (Control)", "Romania (Treatment)")))%>%
  kbl(digits = 2)

options(knitr.kable.NA = "")

kbl(data_2.1.10, format = "latex", linesep = "", booktabs = T, caption = "Perception of information treatments",
    format.args = list(big.mark = ",", scientific = FALSE), align = "lccccc", label = "tab:A1", digits = 2, na = "")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position"), font_size = 9)%>%
  footnote(general = "This table shows TBD.", threeparttable = T)%>%
  save_kable(., "../6_EUETS2_Citizens_Survey/2_Tables/Table_A_Treatment_Perception.tex")

rm(data_2.1.10, data_2.1.10.1, data_2.1.10.2, data_2.1.10.3, data_2.1.10.4, data_2.1.10.5)

# 2.1.11 Expectation about costs and actual costs ####

data_2.1.11 <- data_2 %>%
  filter(!is.na(Q42_1_average))%>%
  mutate(Q42_1_average = ifelse(Country == "Romania", Q42_1_average*0.223, Q42_1_average))%>%
  mutate(absolute_value = ifelse(Country == "Romania", absolute_value*0.223,absolute_value))%>%
  mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))

P_2.1.11 <- ggplot(data_2.1.11, aes(y = Q42_1_average, x = absolute_value))+
  geom_abline(slope = 1, intercept = 0, linewidth = 0.3)+
  facet_wrap(. ~ Country)+
  geom_point(alpha = 0.1, shape = 16, fill = "lightgrey")+
  theme_bw()+
  coord_cartesian(xlim = c(0,5000), ylim = c(0,5000))+
  # ggtitle("By how much do you think will your annual expenditures increase because of this policy (in %)?")+
  scale_x_continuous(labels = scales::dollar_format(prefix = "€"))+
  scale_y_continuous(labels = scales::dollar_format(prefix = "€"))+
  ylab("Expected additional costs [in €]")+
  xlab("Estimated additional costs for households [in €]")+
  # ggtitle("Overall policy support (Q46_1 and Q46_2)")+
  theme(panel.grid.minor  = element_blank(),
        panel.grid.major = element_line(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 9),
        plot.title = element_text(size = 8),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(hjust = 0.5, size = 8))

data_2.1.11.1 <- data_2 %>%
  filter(!is.na(Q42_1_relative))%>%
  mutate(Country = factor(Country, levels = c("Romania", "Germany", "France", "Spain")))

P_2.1.11.1 <- ggplot(data_2.1.11.1, aes(y = Q42_1_relative, x = relative_value))+
  geom_abline(slope = 1, intercept = 0, linewidth = 0.3)+
  facet_wrap(. ~ Country)+
  geom_point(alpha = 0.1, shape = 16, fill = "lightgrey")+
  theme_bw()+
  coord_cartesian(xlim = c(0,0.2), ylim = c(0,0.2))+
  # ggtitle("By how much do you think will your annual expenditures increase because of this policy (in %)?")+
  scale_x_continuous(labels = scales::percent_format())+
  scale_y_continuous(labels = scales::percent_format())+
  ylab("Expected additional costs [in % of total expenditures]")+
  xlab("Estimated additional costs for households [in % of total expenditures]")+
  # ggtitle("Overall policy support (Q46_1 and Q46_2)")+
  theme(panel.grid.minor  = element_blank(),
        panel.grid.major = element_line(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title  = element_text(size = 9),
        plot.title = element_text(size = 8),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(hjust = 0.5, size = 8))

jpeg("../6_EUETS2_Citizens_Survey/1_Figures/A_Figure_A9_%d.jpeg", width = 140/25.4, height = 150/25.4, unit = "in", res = 600)
print(P_2.1.11)
print(P_2.1.11.1)
dev.off()

# 2.2    Boosted regression trees - predicting support or opposition in t=0 ####

data_2.2 <- data_2 %>%
  # Excluding "I don't know" answers
  filter(!is.na(Q46_1N))%>%
  # Recode policy support
  mutate(support_2 = ifelse(Q46_1N < 2,0,1),
         support_3 = ifelse(Q46_1N < 3,0,1),
         support_4 = ifelse(Q46_1N < 4,0,1),
         support_5 = ifelse(Q46_1N < 5,0,1))%>%
  mutate_at(vars(starts_with("support_")), ~ factor(.))%>%
  mutate(Pricelevel = ifelse(is.na(Pricelevel), Priceleveleuro, Pricelevel))%>%
  mutate(Pricelevel = factor(Pricelevel, levels = c(45,85,125)))%>%
  select(Country, Q10:Q23,Q24,Q25:Q28,Q29A,Q29B,Q30_1:Q38,Q41_1:Q45_1, support_2:support_5, Pricelevel)%>%
  # TBD: Homogenize industry variable. Leave out for now.
  select(-Q42_1, -Q14, -Q42_1_average)

track_tuning <- data.frame()
track_tuning <- read.xlsx("../2_Data/1_Support_Datasets/1_SHAP_1/Tuning_SHAP_1.xlsx")

track_performance <- data.frame()
track_performance <- read.xlsx("../2_Data/1_Support_Datasets/1_SHAP_1/Performance_SHAP_1.xlsx")

for(i in c("Spain", "France", "Germany", "Romania")){
  
  data_2.2.1 <- data_2.2 %>%
    filter(Country == i)
  
  if(i == "Spain"){
    data_2.2.1 <- data_2.2.1 %>%
      mutate(Q11 = ifelse(Q11 == "Otros", "Mujer", Q11))
  }
  
  if(i == "France"){
    data_2.2.1 <- data_2.2.1 %>%
      mutate(Q11 = ifelse(Q11 == "Autre", "Féminin", Q11))
  }
  
  if(i == "Romania"){
    data_2.2.1 <- data_2.2.1 %>%
      mutate(Q11 = ifelse(Q11 == "Altul", "Feminin", Q11))%>%
      mutate_at(vars(Q31_Gov_nat:Q31_EU_Comm), ~ ifelse(is.na(.), "Nu știu / Nu pot să spun",.))
  }
  
  for(j in c("support_2", "support_3", "support_4", "support_5")){
    
    track_0 <- data.frame(Country = i,
                          Outcome = j,
                          date    = date())
    run_number <- 1
    run_ID <- paste0(i,"_",j,"_",1)
    # run_number <- if(i %in% track_tuning$Country & j %in% track_tuning$Outcome) max(track_tuning$number[track_tuning$Country == i & track_tuning$Outcome == j])+1 else 1
    # run_ID     <- if(i %in% track_tuning$Country & j %in% track_tuning$Outcome) paste0(i,"_",j,"_",max(track_tuning$number[track_tuning$Country == i & track_tuning$Outcome == j])+1) else (paste0(i,"_",j,"_",1))
    
    print(paste0("Start ", i, ": ", run_ID, ": ", Sys.time()))
    
    data_2.2.2 <- data_2.2.1 %>%
      # Remove all other support columns and only keep relevant outcome variable
      rename(outcome = all_of(j))%>%
      select(-Country, -starts_with("support"))%>%
      # Removes unused factor levels
      mutate(across(where(is.factor), ~ fct_drop(.)))%>%
      # Convert to factor variable
      mutate(across(where(~ is.character(.)), ~ as.factor(.)))%>%
      # Replace NAs where not applicable or for "I don't know"
      # mutate_at(vars(Q14),   ~ fct_na_value_to_level(., level = "Not applicable"))%>%
      mutate_at(vars(Q30_1:Q30_3, Q41_1:Q42_1_true,Q43_1:Q45_1), ~ fct_na_value_to_level(., level = "I don't know"))%>%
      mutate(Missing_Q42_1_relative = ifelse(is.na(Q42_1_relative),1,0))%>%
      # Remove columns with just NA
      select(where(~ !all(is.na(.))))%>%
      # Create noise parameter
      mutate(noise = rnorm(nrow(.),0,1))
    
    if(i == "France"){
      data_2.2.2 <- data_2.2.2 %>%
        filter(!is.na(Q10) & !is.na(Q12) & !is.na(Q35_1))%>%
        mutate_at(vars(Q11, Q13, Q15, Q16, Q35_2:Q35_4), ~ fct_na_value_to_level(., level = "I don't know"))
    }
    
    if(i == "Germany"){
      data_2.2.2 <- data_2.2.2 %>%
        mutate_at(vars(Q36:Q37_c), ~ fct_na_value_to_level(., level = "I don't know"))
    }
    
  if(i == "Romania"){
    data_2.2.2 <- data_2.2.2 %>%
      filter(!is.na(Q11) & !is.na(Q12) & !is.na(Q35_1))%>%
      mutate_at(vars(Q15,Q16,Q35_2:Q35_4), ~ fct_na_value_to_level(., level = "I don't know"))
  }
    
    # Training and testing dataset
    
    data_split_2.2.2 <- initial_split(data_2.2.2, prop = 0.8, strata = outcome)
    
    train_2.2.2 <- training(data_split_2.2.2) 
    test_2.2.2  <- testing(data_split_2.2.2)
    
    # Recipe
    
    recipe_2.2.2 <- recipe(outcome ~ ., 
                           data = data_2.2.2)%>%
      # Delete columns with NA (should be redundant)
      step_filter_missing(all_nominal_predictors(), threshold = 0)%>%
      step_zv(all_predictors())%>%
      step_other(Q12,Q13,Q26, threshold = 0.05)%>%
      step_dummy(all_nominal_predictors(), sparse = "no")
    
    mtry_max <- recipe_2.2.2 %>%
      prep(training = train_2.2.2)%>%
      bake(new_data = NULL)%>%
      select(-outcome)%>%
      ncol()
      
    # Five-fold cross-validation
    
    folds_2.2.2 <- vfold_cv(train_2.2.2, v = 5, strata = outcome)
    
    model_2.2.2 <- boost_tree(
      trees      = 1000,
      tree_depth = tune(),
      learn_rate = tune(),
      mtry       = tune(),
      stop_iter  = 15)%>%
      set_mode("classification")%>%
      set_engine("xgboost")
    
    workflow_2.2.2 <- workflow()%>%
      add_recipe(recipe_2.2.2)%>%
      add_model(model_2.2.2)
      
    # Create tuning grid
    
    grid_2.2.2 <- grid_space_filling(
      tree_depth(c(3,15)),
      learn_rate(c(-3,-1)),
      mtry(c(round((mtry_max/2),0),mtry_max)),
      size = 99)%>%
      # default parameters
      bind_rows(data.frame(tree_depth = 6, learn_rate = 0.3, mtry = mtry_max))
    
    # Tune the model
    
    doParallel::registerDoParallel()
    
    time_1 <- Sys.time()
    
    model_2.2.2 <- tune_grid(workflow_2.2.2,
                           resamples = folds_2.2.2,
                           grid      = grid_2.2.2,
                           metrics   = metric_set(accuracy, mn_log_loss, f_meas))
    
    time_2 <- Sys.time()
    
    doParallel::stopImplicitCluster()
    
    print("End computing")
    
    tuning_time <- as.integer(difftime(time_2, time_1, units = "min"))
    
    # Collect metrics of tuned model
    
    metrics_2.2.2 <- collect_metrics(model_2.2.2)
    
    model_2.2.2.1 <- select_best(model_2.2.2, metric = "mn_log_loss")
    
    metrics_2.2.2.1 <- metrics_2.2.2 %>%
      filter(.config == model_2.2.2.1$.config[1])
    
    track_1 <- track_0 %>%
      mutate(number      = run_number,
            run_ID      = run_ID,
            tuning_time = tuning_time)%>%
      bind_cols(model_2.2.2.1)%>%
      rename(tree_depth_best = tree_depth, learn_rate_best = learn_rate, mtry_best = mtry)%>%
      select(-.config)%>%
      mutate(accuracy    = metrics_2.2.2.1$mean[metrics_2.2.2.1$.metric == "accuracy"],
            f_meas      = metrics_2.2.2.1$mean[metrics_2.2.2.1$.metric == "f_meas"],
            mn_log_loss = metrics_2.2.2.1$mean[metrics_2.2.2.1$.metric == "mn_log_loss"])
    
    # First outcome: Table with all tuning details.
    track_tuning <- track_tuning %>%
      bind_rows(track_1)
    
    parameters <- track_tuning %>%
      filter(Country == i & Outcome == j)%>%
      mutate(number = 1:n())%>%
      dplyr::slice(which.max(number))%>%
      rename(tree_depth = tree_depth_best,
             learn_rate = learn_rate_best,
             mtry       = mtry_best)%>%
      select(tree_depth, learn_rate, mtry)%>%
      as.list()
    
    # Fit best model
    
    workflow_2.2.3 <- finalize_workflow(workflow_2.2.2, parameters)
    
    # Fit model
    
    model_2.2.3 <- fit(workflow_2.2.3, data = train_2.2.2)
    
    evaluation_2.2.3 <- predict(model_2.2.3, test_2.2.2, type = "class")%>%
      bind_cols(predict(model_2.2.3, test_2.2.2, type = "prob"))%>%
      bind_cols(test_2.2.2)
    
    metrics_2.2.3 <- metric_set(
      accuracy,
      kap,
      sens,
      yardstick::spec,
      f_meas,
      roc_auc
    )
    
    # Observations test set
    obs_test <- nrow(test_2.2.2)
    obs_class <- count(test_2.2.2, outcome)%>%
      filter(outcome == 1)%>%
      pull(n)
    
    metrics_2.2.3.1 <- metrics_2.2.3(evaluation_2.2.3,
                             truth = outcome,
                             estimate = .pred_class,
                             .pred_1,
                             event_level = "second")%>%
      select(-.estimator)%>%
      pivot_wider(names_from = ".metric", values_from = ".estimate")
    
    CI_AUC <- pROC::roc(
      response = evaluation_2.2.3$outcome,
      predictor = evaluation_2.2.3$.pred_1,
      levels = c("0","1"),
      direction = "<"
    )
    
    CI_AUC_2 <- as.numeric(pROC::ci.auc(CI_AUC, method = "bootstrap", boot.n = 1000, conf.level = 0.95))
    
    metrics_2.2.3.1 <- metrics_2.2.3.1 %>%
      mutate(ROC_AUC = as.numeric(pROC::auc(CI_AUC)),
             ROC_AUC_CI_1 = CI_AUC_2[1],
             ROC_AUC_CI_2 = CI_AUC_2[3],
             test_sample = obs_test,
             test_class  = obs_class)
    
    track_2 <- track_0 %>%
      mutate(number      = run_number,
             run_ID      = run_ID)%>%
      bind_cols(metrics_2.2.3.1)
    
    track_performance <- track_performance %>%
      bind_rows(track_2)
    
    # Extract SHAP values (full dataset)
    
    model_2.2.4 <- fit(workflow_2.2.3, data = data_2.2.2)
    engine_2.2.4 <- extract_fit_engine(model_2.2.4)
    data_2.2.4 <- bake(prep(recipe_2.2.2, training = data_2.2.2), new_data = data_2.2.2)%>%
      select(-outcome)%>%
      as.matrix()

    time_3 <- Sys.time()
    
    shap_2.2.4 <- predict(engine_2.2.4,
                          data_2.2.4,
                          predcontrib = TRUE,
                          approxcontrib = FALSE)
    
    time_4 <- Sys.time()
    
    shaping_time <- as.integer(difftime(time_4, time_3, units = "min"))
    
    # shap_2.2.4.1 <- shap_2.2.4 %>%
    #   as_tibble()%>%
    #   summarise_all(~ mean(abs(.)))%>%
    #   select(-"(Intercept)")%>%
    #   pivot_longer(everything(), names_to = "variable", values_to = "SHAP_contribution")%>%
    #   arrange(desc(SHAP_contribution))%>%
    #   mutate(tot_contribution = sum(SHAP_contribution))%>%
    #   mutate(share_SHAP = SHAP_contribution/tot_contribution)%>%
    #   select(-tot_contribution)
    
    shap_2.2.4.1 <- shap_2.2.4 %>%
      as_tibble()
    
    write_parquet(shap_2.2.4.1, sprintf("../2_Data/1_Support_Datasets/1_SHAP_1/SHAP_%s_%s.parquet", i, j))
    write_parquet(data_2.2.2,   sprintf("../2_Data/1_Support_Datasets/1_SHAP_1/Data_%s_%s.parquet", i, j))
    
    rm(data_2.2.2, data_2.2.4, data_split_2.2.2, engine_2.2.4, evaluation_2.2.3, folds_2.2.2, grid_2.2.2,
       metrics_2.2.2, metrics_2.2.2.1, metrics_2.2.3.1, model_2.2.2, model_2.2.2.1, model_2.2.3, model_2.2.4,
       parameters, recipe_2.2.2, shap_2.2.4, shap_2.2.4.1, test_2.2.2, train_2.2.2, workflow_2.2.2, workflow_2.2.3,
       time_1, time_2, time_3, time_4)
  }
  
}

write.xlsx(track_tuning, "../2_Data/1_Support_Datasets/1_SHAP_1/Tuning_SHAP_1.xlsx")
write.xlsx(track_performance, "../2_Data/1_Support_Datasets/1_SHAP_1/Performance_SHAP_1.xlsx")

# 2.2.1  Analysing SHAP values for each combination ####

shap_a <- data.frame()
shap_b <- data.frame()

for(i in c("Spain", "France", "Germany", "Romania")){
  for(j in c("support_2", "support_3", "support_4", "support_5")){
    
    shap_2.2.1.1 <- read_parquet(sprintf("../2_Data/1_Support_Datasets/1_SHAP_1/SHAP_%s_%s.parquet", i, j))
    data_2.2.1.1 <- read_parquet(sprintf("../2_Data/1_Support_Datasets/1_SHAP_1/Data_%s_%s.parquet", i, j))
    
    shap_2.2.1.1.0 <- shap_2.2.1.1 %>%
      summarise_all(~ mean(abs(.)))%>%
      select(-"(Intercept)")%>%
      pivot_longer(everything(), names_to = "variable", values_to = "SHAP_contribution")%>%
      arrange(desc(SHAP_contribution))%>%
      mutate(tot_contribution = sum(SHAP_contribution))%>%
      mutate(share_SHAP       = SHAP_contribution/tot_contribution)%>%
      select(-tot_contribution)%>%
      mutate(Country = i,
             Outcome = j)
    
    shap_2.2.1.1.1 <- shap_2.2.1.1 %>%
      summarise_all(~ mean(.))%>%
      select(-"(Intercept)")%>%
      pivot_longer(everything(), names_to = "variable", values_to = "SHAP_contribution")%>%
      mutate(Variable   = ifelse(variable != "Q42_1_relative", str_replace(variable, "_[^_]+$", ""), variable))%>%
      group_by(Variable)%>%
      summarise(direction = sign(SHAP_contribution[which.max(abs(SHAP_contribution))]))%>%
      ungroup()%>%
      mutate(Country = i,
             Outcome = j)
                
    shap_2.2.1.2.0 <- shap_2.2.1.1.0 %>%
      mutate(Variable   = ifelse(variable != "Q42_1_relative", str_replace(variable, "_[^_]+$", ""), variable))%>%
      group_by(Variable)%>%
      summarise(share_SHAP = sum(share_SHAP))%>%
      ungroup()%>%
      arrange(desc(share_SHAP))%>%
      mutate(Country = i,
             Outcome = j)%>%
      left_join(shap_2.2.1.1.1)
    
    shap_a <- shap_a %>%
      bind_rows(shap_2.2.1.1.0)
    
    shap_b <- shap_b %>%
      bind_rows(shap_2.2.1.2.0)
    
  }
}

# What are the ten most important features (on average) for each level of support

shap_b_1 <- shap_b %>%
  group_by(Country, Variable)%>%
  summarise(mean_SHAP = mean(share_SHAP))%>%
  arrange(desc(mean_SHAP))%>%
  mutate(rank = 1:n())%>%
  ungroup()%>%
  filter(rank < 7)

# Dataset for each country

for(i in c("Spain", "France", "Germany", "Romania")){
  shap_b_1.1 <- shap_b_1 %>%
    filter(Country == i)
  
  shap_b_2 <- shap_b %>%
    filter(Country == i)%>%
    left_join(shap_b_1.1)%>%
    filter(!is.na(rank))%>%
    # Data transformation
    mutate(Outcome = case_when(Outcome == "support_2" ~ "> Rather oppose",
                               Outcome == "support_3" ~ "> Neutral",
                               Outcome == "support_4" ~ "> Rather support",
                               Outcome == "support_5" ~ "> Strongly support"))%>%
    mutate(Outcome = factor(Outcome, levels = c("> Rather oppose", "> Neutral", "> Rather support", "> Strongly support")))%>%
    mutate(direction = factor(direction))%>%
    mutate(Variable = case_when(Variable == "Q45_1"          ~ "Fairness perception (Q45)",
                                Variable == "Q44_1"          ~ "Effects on vulnerable (Q44)",
                                Variable == "Q43_1"          ~ "Relative costs (Q43)",
                                Variable == "Q42_1_relative" ~ "Individual costs [%] (Q42)",
                                Variable == "Q42_1_true"     ~ "Individual costs (Q42)",
                                Variable == "Q41_1"          ~ "Effectiveness perception (Q41)",
                                Variable == "Q38"            ~ "Political Party (Q38)",
                                Variable == "Q37_c"          ~ "Impact on emissions (Q37)",
                                Variable == "Q37_b"          ~ "Impact on life (Q37)",
                                Variable == "Q37_a"          ~ "Impact on economy (Q37)",
                                Variable == "Q36"            ~ "Climate change concern (Q36)",
                                Variable == "Q35_1"          ~ "Communication: Public (Q35)",
                                Variable == "Q35_4"          ~ "Communcation: Scientists (Q35)",
                                Variable == "Q31_Gov_nat"    ~ "Integrity national gov. (Q31)",
                                Variable == "Q31_EU_Comm"    ~ "Integrity EU commission (Q31)",
                                Variable == "Q30_1"          ~ "Trust in local gov. (Q30)",
                                Variable == "Q30_2"          ~ "Trust in national gov. (Q30)",
                                Variable == "Q30_3"          ~ "Trust in EU (Q30)",
                                Variable == "Q28"            ~ "Expenditures (Q28)",
                                Variable == "noise"          ~ "Random term",
                                Variable == "Q10"            ~ "Age (Q10)",
                                TRUE ~ Variable))%>%
    mutate(Variable = fct_reorder(Variable, rank, .desc = TRUE))%>%
    mutate(label_0 = paste0(round(share_SHAP*100,0),"%"))
    
  P_2.2.1 <- ggplot(data = shap_b_2, aes(x = Outcome, y = Variable))+
    # geom_point(shape = 22,fill = NA,colour = "black",stroke = 0.3,size = 9)+
    geom_point(aes(alpha = share_SHAP), shape = 22, size = 9, fill = "#3C5488FF", colour = "black", stroke = 0.3)+
    geom_text(aes(label = label_0), size = 2)+
    scale_alpha_continuous(range = c(0,0.7))+
    scale_x_discrete(position = "top")+
    # geom_point(alpha = 0.85, shape = 21, fill = "#4DBBD5FF", colour = "black")+
    # scale_size_continuous(range = c(2,8),
    #                       breaks = c(0.01, 0.05, 0.1, 0.2, 0.5),
    #                       name    = "Average SHAP Contribution", 
    #                       labels = percent)+l
    theme_bw()+
    ylab("Feature")+
    xlab("Average SHAP Contribution: Do you support or oppose the EU ETS2?")+
    ggtitle(i)+
    guides(alpha = "none")+
    theme(legend.position = "bottom",
          panel.grid.major = element_blank(),
          axis.ticks = element_line(linewidth = 0.3),
          axis.text  = element_text(size = 7),
          axis.title = element_text(size = 8),
          title = element_text(size = 8),
          legend.text = element_text(size = 8),
          legend.title = element_text(hjust = 0.5, size = 8))
  
  pdf(sprintf("../6_EUETS2_Citizens_Survey/1_Figures/A_Figure_B_%s.pdf", i), width = 130/25.4, height = 70/25.4)
  print(P_2.2.1)
  dev.off()
  
}

# Dataset for each outcome

shap_b_3 <- shap_b %>%
  select(-direction)%>%
  pivot_wider(names_from = "Variable", values_from = "share_SHAP", values_fill = 0)%>%
  pivot_longer(-c(Country, Outcome), names_to = "Variable", values_to = "share_SHAP")%>%
  group_by(Outcome, Variable)%>%
  summarise(mean_SHAP = mean(share_SHAP))%>%
  arrange(desc(mean_SHAP))%>%
  mutate(rank = 1:n())%>%
  ungroup()%>%
  filter(rank < 7)

for(j in c("support_2", "support_3", "support_4", "support_5")){
  shap_b_3.1 <- shap_b_3 %>%
    filter(Outcome == j)
  
  shap_b_4 <- shap_b %>%
    filter(Outcome == j)%>%
    left_join(shap_b_3.1)%>%
    filter(!is.na(rank))%>%
    # Data transformation
    mutate(Country = factor(Country, levels = c("Spain", "France", "Germany", "Romania")))%>%
    mutate(direction = factor(direction))%>%
    mutate(Variable = case_when(Variable == "Q45_1"          ~ "Fairness perception (Q45)",
                                Variable == "Q44_1"          ~ "Effects on vulnerable (Q44)",
                                Variable == "Q43_1"          ~ "Relative costs (Q43)",
                                Variable == "Q42_1_relative" ~ "Individual costs [%] (Q42)",
                                Variable == "Q42_1_true"     ~ "Individual costs (Q42)",
                                Variable == "Q41_1"          ~ "Effectiveness perception (Q41)",
                                Variable == "Q38"            ~ "Political Party (Q38)",
                                Variable == "Q37_c"          ~ "Impact on emissions (Q37)",
                                Variable == "Q37_b"          ~ "Impact on life (Q37)",
                                Variable == "Q37_a"          ~ "Impact on economy (Q37)",
                                Variable == "Q36"            ~ "Climate change concern (Q36)",
                                Variable == "Q35_1"          ~ "Communication: Public (Q35)",
                                Variable == "Q35_4"          ~ "Communcation: Scientists (Q35)",
                                Variable == "Q31_Gov_nat"    ~ "Integrity national gov. (Q31)",
                                Variable == "Q31_EU_Comm"    ~ "Integrity EU commission (Q31)",
                                Variable == "Q30_1"          ~ "Trust in local gov. (Q30)",
                                Variable == "Q30_2"          ~ "Trust in national gov. (Q30)",
                                Variable == "Q30_3"          ~ "Trust in EU (Q30)",
                                Variable == "Q28"            ~ "Expenditures (Q28)",
                                Variable == "noise"          ~ "Random term",
                                Variable == "Q10"            ~ "Age (Q10)",
                                TRUE ~ Variable))%>%
    mutate(Variable = fct_reorder(Variable, rank, .desc = TRUE))%>%
    mutate(label_0 = paste0(round(share_SHAP*100,0),"%"))
  
  if(j == "support_2") title_0 <- "Important predictors: Strongly oppose"
  if(j == "support_3") title_0 <- "Important predictors: Strongly or rather oppose"
  if(j == "support_4") title_0 <- "Important predictors: Strongly or rather support"
  if(j == "support_5") title_0 <- "Important predictors: Strongly support"
  
  P_2.2.2 <- ggplot(data = shap_b_4, aes(x = Country, y = Variable))+
    # geom_point(shape = 22,fill = NA,colour = "black",stroke = 0.3,size = 9)+
    geom_point(aes(alpha = share_SHAP), shape = 22, size = 9, fill = "#3C5488FF", colour = "black", stroke = 0.3)+
    geom_text(aes(label = label_0), size = 2)+
    scale_alpha_continuous(range = c(0,0.7))+
    scale_x_discrete(position = "top")+
    # geom_point(alpha = 0.85, shape = 21, fill = "#4DBBD5FF", colour = "black")+
    # scale_size_continuous(range = c(2,8),
    #                       breaks = c(0.01, 0.05, 0.1, 0.2, 0.5),
    #                       name    = "Average SHAP Contribution", 
    #                       labels = percent)+l
    theme_bw()+
    ylab("Feature")+
    xlab("Average SHAP Contribution: Do you support or oppose the EU ETS2?")+
    ggtitle(title_0)+
    guides(alpha = "none")+
    theme(legend.position = "bottom",
          panel.grid.major = element_blank(),
          axis.ticks = element_line(linewidth = 0.3),
          axis.text  = element_text(size = 7),
          axis.title = element_text(size = 8),
          title = element_text(size = 8),
          legend.text = element_text(size = 8),
          legend.title = element_text(hjust = 0.5, size = 8))
  
  pdf(sprintf("../6_EUETS2_Citizens_Survey/1_Figures/A_Figure_B_%s.pdf", j), width = 130/25.4, height = 70/25.4)
  print(P_2.2.2)
  dev.off()
  
}

rm(shap_2.2.1.1, shap_2.2.1.1.0, shap_2.2.1.1.1, shap_2.2.1.2.0, shap_2.2.1.1,
   shap_a, shap_b, shap_b_1, shap_b_1.1, shap_b_2, P_2.2.1, i, j)

# 2.2.2  Individual SHAP-values (Figure 2 and Appendix) ####

data_2.2.2 <- data.frame()

for(i in c("Spain", "France", "Germany", "Romania")){
  shap_2.2.2   <- read_parquet(sprintf("../2_Data/1_Support_Datasets/1_SHAP_1/SHAP_%s_support_3.parquet", i))
  data_2.2.2.0 <- read_parquet(sprintf("../2_Data/1_Support_Datasets/1_SHAP_1/Data_%s_support_3.parquet", i))

  # Select relevant variables
  shap_2.2.2.1 <- shap_2.2.2 %>%
    mutate(id = 1:n())%>%
    select(id, everything())%>%
    pivot_longer(-id, names_to = "variable", values_to = "SHAP")%>%
    mutate(VAR_0 = case_when(grepl("Q45_", variable) ~ "Fairness perception (Q45)",
                             grepl("Q41_", variable) ~ "Effectiveness perception (Q41)",
                             grepl("Q36_", variable) ~ "Climate change concern (Q36)",
                             TRUE ~ NA))%>%
    filter(!is.na(VAR_0))
  
  for(j in c("Q36", "Q41_1", "Q45_1")){
    # Select relevant SHAP values
    shap_2.2.2.2 <- shap_2.2.2.1 %>%
      filter(grepl(j, variable))
    
    data_2.2.2.1 <- data_2.2.2.0 %>%
      mutate(id = 1:n())%>%
      select(id, all_of(j))%>%
      left_join(shap_2.2.2.2, by = "id")%>%
      rename(var_interest = j)%>%
      mutate(var_interest = as.character(var_interest))%>%
      mutate(variable = str_remove(variable, paste0(j,"_")))%>%
      mutate(variable = str_replace_all(variable, "\\."," "))%>%
      mutate(variable = ifelse(variable == "I don t know", "I don't know", variable))%>%
      group_by(id)%>%
      summarise(SHAP = sum(SHAP))%>%
      ungroup()%>%
      select(id, SHAP)
    
    # Dataframe with relevant values and corresponding SHAP values
    data_2.2.2.2 <- data_2.2.2.0 %>%
      mutate(id = 1:n())%>%
      select(id, all_of(j))%>%
      left_join(data_2.2.2.1, by = "id")%>%
      mutate(SHAP = ifelse(is.na(SHAP),0,SHAP))%>%
      rename(level = j)%>%
      mutate(VAR_0 = j)%>%
      mutate(Country = i)
    
    data_2.2.2 <- data_2.2.2 %>%
      bind_rows(data_2.2.2.2)
  }
}

data_2.2.2.1 <- data_2.2.2 %>%
  mutate(LEVEL = case_when(VAR_0 == "Q45_1" & level %in% c("Injusta", "Injuste", "Ich finde sie ungerecht", "Incorecta") ~ "Unfair",
                           VAR_0 == "Q45_1" & level %in% c("Ni justa ni injusta","Ni juste ni injuste","Ich finde sie weder gerecht noch ungerecht", "Nici corecta nici incorecta") ~ "Neither fair nor unfair",
                           VAR_0 == "Q45_1" & level %in% c("Justa", "Juste", "Ich finde sie gerecht", "Corecta") ~  "Fair",
                           VAR_0 == "Q45_1" & level == "I don't know" ~ "I don't know",
                           VAR_0 == "Q41_1" & level %in% c("Probablemente eficaz", "Probablement efficace", "Vermutlich", "Probabil va fi eficienta")                   ~ "Probably effective",
                           VAR_0 == "Q41_1" & level %in% c("Sin duda eficaz", "Certainement efficace", "Auf jeden Fall", "In mod sigur va fi eficienta")                ~ "Definitely effective",
                           VAR_0 == "Q41_1" & level %in% c("Probablemente no sea eficaz", "Probablement inefficace", "Vermutlich nicht", "Probabil nu va fi eficienta") ~ "Probably ineffective",
                           VAR_0 == "Q41_1" & level %in% c("En absoluto eficaz", "Certainement inefficace", "Auf keinen Fall", "In mod sigur nu va fi eficienta")       ~ "Definitely ineffective",
                           VAR_0 == "Q41_1" & level == "I don't know" ~ "I don't know",
                           VAR_0 == "Q36" & level %in% c("No me preocupa", "Pas préoccupé(e)", "Nicht besorgt", "Nu sunt preocupat(ă)")           ~ "Not concerned",
                           VAR_0 == "Q36" & level %in% c("Me preocupa un poco","Un peu préoccupé(e)", "Ein wenig besorgt", "Puțin preocupat(ă)")  ~ "Somewhat concerned",
                           VAR_0 == "Q36" & level %in% c("Me preocupa algo", "Assez préoccupé(e)", "Ziemlich besorgt", "Oarecum preocupat(ă)")    ~ "Quite concerned",
                           VAR_0 == "Q36" & level %in% c("Me preocupa mucho", "Très préoccupé(e)", "Sehr besorgt", "Foarte preocupat(ă)")         ~ "Very concerned",
                           VAR_0 == "Q36" & level %in% c("No tenga una opinión al respecto","Sans opinion", "Nu am nicio părere", "I don't know") ~ "No opinion"))%>%
  mutate(VAR_0 = factor(VAR_0, levels = c("Q45_1", "Q41_1","Q36")))%>%
  mutate(VAR_1 = case_when(VAR_0 == "Q45_1" ~ "Fairness perception",
                           VAR_0 == "Q41_1" ~ "Effectiveness perception",
                           VAR_0 == "Q36"   ~ "Climate change concern"))%>%
  mutate(VAR_1 = factor(VAR_1, levels = c("Fairness perception",
                                          "Effectiveness perception",
                                          "Climate change concern")))%>%
  # Remove I donÄt know and no opinions
  filter(!LEVEL %in% c("I don't know", "No opinion"))%>%
  # Remove baselines
  # filter(!LEVEL %in% c("Unfair", "Definitely ineffective", "Not concerned"))%>% # TBD for Q38
  mutate(LEVEL = factor(LEVEL, levels = c("Fair", "Neither fair nor unfair", "Unfair",
                                          "Definitely effective", "Probably effective","Probably ineffective", "Definitely ineffective",
                                           "Very concerned", "Quite concerned", "Somewhat concerned", "Not concerned")))%>%
  mutate(Country = case_when(Country == "Spain"   ~ "Spain (AUC: 0.9)",
                             Country == "France"  ~ "France (AUC: 0.91)",
                             Country == "Germany" ~ "Germany (AUC: 0.9)",
                             Country == "Romania" ~ "Romania (AUC: 0.92)"))%>%
  mutate(Country = factor(Country, levels = c("Spain (AUC: 0.9)",
                                              "France (AUC: 0.91)",
                                              "Germany (AUC: 0.9)",
                                              "Romania (AUC: 0.92)")))%>%
  mutate(SHAP = SHAP*-1)

P_2.2.2 <- ggplot(data_2.2.2.1)+
  geom_vline(aes(xintercept = 0))+
  # geom_boxplot(aes(x = SHAP, y = LEVEL))+
  geom_jitter(aes(x = SHAP, y = LEVEL), size = 0.4, height = 0.25, width = 0, shape = 21, fill = "#3C5488FF", alpha = 0.4, stroke = 0)+
  facet_grid(VAR_1 ~ Country, scales = "free_y", switch = "y", space = "free")+
  theme_bw()+
  xlab("SHAP values")+
  theme(strip.placement    = "outside",
        axis.title.y       = element_blank(),
        strip.background   = element_blank(),
        axis.text.y        = element_text(size = 6), 
        axis.text.x        = element_text(size = 6),
        axis.title.x       = element_text(size = 7),
        strip.text         = element_text(size = 7),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks         = element_line(size = 0.2))

jpeg("../6_EUETS2_Citizens_Survey/1_Figures/A_Figure_2.jpeg", width = 160/25.4, height = 120/25.4, unit = "in", res = 600)
print(P_2.2.2)
dev.off()

rm(data_2.2.2, data_2.2.2.0, data_2.2.2.1, data_2.2.2.2, shap_2.2.2, shap_2.2.2.1, shap_2.2.2.2, P_2.2.2, i, j)

# 2.3    Boosted regression trees - predicting support or opposition in t=0 without policy variables ####

data_2.3 <- data_2 %>%
  # Excluding "I don't know" answers
  filter(!is.na(Q46_1N))%>%
  # Recode policy support
  mutate(support_2 = ifelse(Q46_1N < 2,0,1),
         support_3 = ifelse(Q46_1N < 3,0,1),
         support_4 = ifelse(Q46_1N < 4,0,1),
         support_5 = ifelse(Q46_1N < 5,0,1))%>%
  mutate_at(vars(starts_with("support_")), ~ factor(.))%>%
  select(Country, Q10:Q23,Q24,Q25:Q28,Q29A,Q29B,Q30_1:Q38, support_2:support_5)%>%
  # TBD: Homogenize industry variable. Leave out for now.
  select(-Q14)

track_tuning <- data.frame()
track_tuning <- read.xlsx("../2_Data/1_Support_Datasets/1_SHAP_1/Tuning_SHAP_1.xlsx")

track_performance <- data.frame()
track_performance <- read.xlsx("../2_Data/1_Support_Datasets/1_SHAP_1/Performance_SHAP_1.xlsx")

for(i in c("Spain", "France", "Germany", "Romania")){
  
  data_2.3.1 <- data_2.3 %>%
    filter(Country == i)
  
  if(i == "Spain"){
    data_2.3.1 <- data_2.3.1 %>%
      mutate(Q11 = ifelse(Q11 == "Otros", "Mujer", Q11))
  }
  
  if(i == "France"){
    data_2.3.1 <- data_2.3.1 %>%
      mutate(Q11 = ifelse(Q11 == "Autre", "Féminin", Q11))
  }
  
  if(i == "Romania"){
    data_2.3.1 <- data_2.3.1 %>%
      mutate(Q11 = ifelse(Q11 == "Altul", "Feminin", Q11))%>%
      mutate_at(vars(Q31_Gov_nat:Q31_EU_Comm), ~ ifelse(is.na(.), "Nu știu / Nu pot să spun",.))
  }
  
  for(j in c("support_2", "support_3", "support_4", "support_5")){
    
    track_0 <- data.frame(Country = i,
                          Outcome = j,
                          date    = date())
    run_number <- 1
    run_ID <- paste0(i,"_",j,"_",1)
    # run_number <- if(i %in% track_tuning$Country & j %in% track_tuning$Outcome) max(track_tuning$number[track_tuning$Country == i & track_tuning$Outcome == j])+1 else 1
    # run_ID     <- if(i %in% track_tuning$Country & j %in% track_tuning$Outcome) paste0(i,"_",j,"_",max(track_tuning$number[track_tuning$Country == i & track_tuning$Outcome == j])+1) else (paste0(i,"_",j,"_",1))
    
    print(paste0("Start ", i, ": ", run_ID, ": ", Sys.time()))
    
    data_2.3.2 <- data_2.3.1 %>%
      # Remove all other support columns and only keep relevant outcome variable
      rename(outcome = all_of(j))%>%
      select(-Country, -starts_with("support"))%>%
      # Removes unused factor levels
      mutate(across(where(is.factor), ~ fct_drop(.)))%>%
      # Convert to factor variable
      mutate(across(where(~ is.character(.)), ~ as.factor(.)))%>%
      # Replace NAs where not applicable or for "I don't know"
      # mutate_at(vars(Q14),   ~ fct_na_value_to_level(., level = "Not applicable"))%>%
      mutate_at(vars(Q30_1:Q30_3), ~ fct_na_value_to_level(., level = "I don't know"))%>%
      # Remove columns with just NA
      select(where(~ !all(is.na(.))))%>%
      # Create noise parameter
      mutate(noise = rnorm(nrow(.),0,1))
    
    if(i == "France"){
      data_2.3.2 <- data_2.3.2 %>%
        filter(!is.na(Q10) & !is.na(Q12) & !is.na(Q35_1))%>%
        mutate_at(vars(Q11, Q13, Q15, Q16, Q35_2:Q35_4), ~ fct_na_value_to_level(., level = "I don't know"))
    }
    
    if(i == "Germany"){
      data_2.3.2 <- data_2.3.2 %>%
        mutate_at(vars(Q36:Q37_c), ~ fct_na_value_to_level(., level = "I don't know"))
    }
    
    if(i == "Romania"){
      data_2.3.2 <- data_2.3.2 %>%
        filter(!is.na(Q11) & !is.na(Q12) & !is.na(Q35_1))%>%
        mutate_at(vars(Q15,Q16,Q35_2:Q35_4), ~ fct_na_value_to_level(., level = "I don't know"))
    }
    
    # Training and testing dataset
    
    data_split_2.3.2 <- initial_split(data_2.3.2, prop = 0.8, strata = outcome)
    
    train_2.3.2 <- training(data_split_2.3.2) 
    test_2.3.2  <- testing(data_split_2.3.2)
    
    # Recipe
    
    recipe_2.3.2 <- recipe(outcome ~ ., 
                           data = data_2.3.2)%>%
      # Delete columns with NA (should be redundant)
      step_filter_missing(all_predictors(), threshold = 0)%>%
      step_zv(all_predictors())%>%
      step_other(Q12,Q13,Q26, threshold = 0.05)%>%
      step_dummy(all_nominal_predictors(), sparse = "no")
    
    mtry_max <- recipe_2.3.2 %>%
      prep(training = train_2.3.2)%>%
      bake(new_data = NULL)%>%
      select(-outcome)%>%
      ncol()
    
    # Five-fold cross-validation
    
    folds_2.3.2 <- vfold_cv(train_2.3.2, v = 5, strata = outcome)
    
    model_2.3.2 <- boost_tree(
      trees      = 1000,
      tree_depth = tune(),
      learn_rate = tune(),
      mtry       = tune(),
      stop_iter  = 15)%>%
      set_mode("classification")%>%
      set_engine("xgboost")
    
    workflow_2.3.2 <- workflow()%>%
      add_recipe(recipe_2.3.2)%>%
      add_model(model_2.3.2)
    
    # Create tuning grid
    
    grid_2.3.2 <- grid_space_filling(
      tree_depth(c(3,15)),
      learn_rate(c(-3,-1)),
      mtry(c(round((mtry_max/2),0),mtry_max)),
      size = 99)%>%
      # default parameters
      bind_rows(data.frame(tree_depth = 6, learn_rate = 0.3, mtry = mtry_max))
    
    # Tune the model
    
    doParallel::registerDoParallel()
    
    time_1 <- Sys.time()
    
    model_2.3.2 <- tune_grid(workflow_2.3.2,
                           resamples = folds_2.3.2,
                           grid      = grid_2.3.2,
                           metrics   = metric_set(accuracy, mn_log_loss, f_meas))
    
    time_2 <- Sys.time()
    
    doParallel::stopImplicitCluster()
    
    print("End computing")
    
    tuning_time <- as.integer(difftime(time_2, time_1, units = "min"))
    
    # Collect metrics of tuned model
    
    metrics_2.3.2 <- collect_metrics(model_2.3.2)
    
    model_2.3.2.1 <- select_best(model_2.3.2, metric = "mn_log_loss")
    
    metrics_2.3.2.1 <- metrics_2.3.2 %>%
    filter(.config == model_2.3.2.1$.config[1])
    
    track_1 <- track_0 %>%
    mutate(number      = run_number,
           run_ID      = run_ID,
           tuning_time = tuning_time)%>%
    bind_cols(model_2.3.2.1)%>%
    rename(tree_depth_best = tree_depth, learn_rate_best = learn_rate, mtry_best = mtry)%>%
    select(-.config)%>%
    mutate(accuracy    = metrics_2.3.2.1$mean[metrics_2.3.2.1$.metric == "accuracy"],
           f_meas      = metrics_2.3.2.1$mean[metrics_2.3.2.1$.metric == "f_meas"],
           mn_log_loss = metrics_2.3.2.1$mean[metrics_2.3.2.1$.metric == "mn_log_loss"])
    
    # First outcome: Table with all tuning details.
    track_tuning <- track_tuning %>%
    bind_rows(track_1)
    
    parameters <- track_tuning %>%
      filter(Country == i & Outcome == j)%>%
      mutate(number = 1:n())%>%
      dplyr::slice(which.max(number))%>%
      rename(tree_depth = tree_depth_best,
             learn_rate = learn_rate_best,
             mtry       = mtry_best)%>%
      select(tree_depth, learn_rate, mtry)%>%
      as.list()
    
    # Fit best model
    
    workflow_2.3.3 <- finalize_workflow(workflow_2.3.2, parameters)
    
    # Fit model
    
    model_2.3.3 <- fit(workflow_2.3.3, data = train_2.3.2)
    
    evaluation_2.3.3 <- predict(model_2.3.3, test_2.3.2, type = "class")%>%
      bind_cols(predict(model_2.3.3, test_2.3.2, type = "prob"))%>%
      bind_cols(test_2.3.2)
    
    metrics_2.3.3 <- metric_set(
      accuracy,
      kap,
      sens,
      yardstick::spec,
      f_meas,
      roc_auc
    )
    
    # Observations test set
    obs_test <- nrow(test_2.3.2)
    obs_class <- count(test_2.3.2, outcome)%>%
      filter(outcome == 1)%>%
      pull(n)
    
    metrics_2.3.3.1 <- metrics_2.3.3(evaluation_2.3.3,
                                     truth = outcome,
                                     estimate = .pred_class,
                                     .pred_1,
                                     event_level = "second")%>%
      select(-.estimator)%>%
      pivot_wider(names_from = ".metric", values_from = ".estimate")
    
    CI_AUC <- pROC::roc(
      response = evaluation_2.3.3$outcome,
      predictor = evaluation_2.3.3$.pred_1,
      levels = c("0","1"),
      direction = "<"
    )
    
    CI_AUC_2 <- as.numeric(pROC::ci.auc(CI_AUC, method = "bootstrap", boot.n = 1000, conf.level = 0.95))
    
    metrics_2.3.3.1 <- metrics_2.3.3.1 %>%
      mutate(ROC_AUC = as.numeric(pROC::auc(CI_AUC)),
             ROC_AUC_CI_1 = CI_AUC_2[1],
             ROC_AUC_CI_2 = CI_AUC_2[3],
             test_sample = obs_test,
             test_class  = obs_class)
    
    track_2 <- track_0 %>%
      mutate(number      = run_number,
             run_ID      = run_ID)%>%
      bind_cols(metrics_2.3.3.1)
    
    track_performance <- track_performance %>%
      bind_rows(track_2)
    
    # Extract SHAP values (full dataset)
    
    model_2.3.4 <- fit(workflow_2.3.3, data = data_2.3.2)
    engine_2.3.4 <- extract_fit_engine(model_2.3.4)
    data_2.3.4 <- bake(prep(recipe_2.3.2, training = data_2.3.2), new_data = data_2.3.2)%>%
      select(-outcome)%>%
      as.matrix()
    
    time_3 <- Sys.time()
    
    shap_2.3.4 <- predict(engine_2.3.4,
                          data_2.3.4,
                          predcontrib = TRUE,
                          approxcontrib = FALSE)
    
    time_4 <- Sys.time()
    
    shaping_time <- as.integer(difftime(time_4, time_3, units = "min"))
    
    # shap_2.2.4.1 <- shap_2.2.4 %>%
    #   as_tibble()%>%
    #   summarise_all(~ mean(abs(.)))%>%
    #   select(-"(Intercept)")%>%
    #   pivot_longer(everything(), names_to = "variable", values_to = "SHAP_contribution")%>%
    #   arrange(desc(SHAP_contribution))%>%
    #   mutate(tot_contribution = sum(SHAP_contribution))%>%
    #   mutate(share_SHAP = SHAP_contribution/tot_contribution)%>%
    #   select(-tot_contribution)
    
    shap_2.3.4.1 <- shap_2.3.4 %>%
      as_tibble()
    
    write_parquet(shap_2.3.4.1, sprintf("../2_Data/1_Support_Datasets/1_SHAP_1/SHAP_wo_%s_%s.parquet", i, j))
    write_parquet(data_2.3.2,   sprintf("../2_Data/1_Support_Datasets/1_SHAP_1/Data_wo_%s_%s.parquet", i, j))
    
    rm(data_2.3.2, data_2.3.4, data_split_2.3.2, engine_2.3.4, evaluation_2.3.3, folds_2.3.2, grid_2.3.2,
       metrics_2.3.2, metrics_2.3.2.1, metrics_2.3.3.1, model_2.3.2, model_2.3.2.1, model_2.3.3, model_2.3.4,
       parameters, recipe_2.3.2, shap_2.3.4, shap_2.3.4.1, test_2.3.2, train_2.3.2, workflow_2.3.2, workflow_2.3.3,
       time_1, time_2, time_3, time_4)
  }
  
}

write.xlsx(track_tuning, "../2_Data/1_Support_Datasets/1_SHAP_1/Tuning_SHAP_wo_1.xlsx")
write.xlsx(track_performance, "../2_Data/1_Support_Datasets/1_SHAP_1/Performance_SHAP_wo_1.xlsx")

# 2.3.1  Analysing SHAP values for each combination ####

shap_a <- data.frame()
shap_b <- data.frame()

for(i in c("Spain", "France", "Germany", "Romania")){
  for(j in c("support_2", "support_3", "support_4", "support_5")){
    
    shap_2.3.1.1 <- read_parquet(sprintf("../2_Data/1_Support_Datasets/1_SHAP_1/SHAP_wo_%s_%s.parquet", i, j))
    data_2.3.1.1 <- read_parquet(sprintf("../2_Data/1_Support_Datasets/1_SHAP_1/Data_wo_%s_%s.parquet", i, j))
    
    shap_2.3.1.1.0 <- shap_2.3.1.1 %>%
      summarise_all(~ mean(abs(.)))%>%
      select(-"(Intercept)")%>%
      pivot_longer(everything(), names_to = "variable", values_to = "SHAP_contribution")%>%
      arrange(desc(SHAP_contribution))%>%
      mutate(tot_contribution = sum(SHAP_contribution))%>%
      mutate(share_SHAP       = SHAP_contribution/tot_contribution)%>%
      select(-tot_contribution)%>%
      mutate(Country = i,
             Outcome = j)
    
    shap_2.3.1.1.1 <- shap_2.3.1.1 %>%
      summarise_all(~ mean(.))%>%
      select(-"(Intercept)")%>%
      pivot_longer(everything(), names_to = "variable", values_to = "SHAP_contribution")%>%
      mutate(Variable   = str_replace(variable, "_[^_]+$", ""))%>%
      group_by(Variable)%>%
      summarise(direction = sign(SHAP_contribution[which.max(abs(SHAP_contribution))]))%>%
      ungroup()%>%
      mutate(Country = i,
             Outcome = j)
    
    shap_2.3.1.2.0 <- shap_2.3.1.1.0 %>%
      mutate(Variable   = str_replace(variable, "_[^_]+$", ""))%>%
      group_by(Variable)%>%
      summarise(share_SHAP = sum(share_SHAP))%>%
      ungroup()%>%
      arrange(desc(share_SHAP))%>%
      mutate(Country = i,
             Outcome = j)%>%
      left_join(shap_2.3.1.1.1)
    
    shap_a <- shap_a %>%
      bind_rows(shap_2.3.1.1.0)
    
    shap_b <- shap_b %>%
      bind_rows(shap_2.3.1.2.0)
    
  }
}

# What are the ten most important features (on average) for each level of support

shap_b_1 <- shap_b %>%
  group_by(Country, Variable)%>%
  summarise(mean_SHAP = mean(share_SHAP))%>%
  arrange(desc(mean_SHAP))%>%
  mutate(rank = 1:n())%>%
  ungroup()%>%
  filter(rank < 7)

# Dataset for each country

for(i in c("Spain", "France", "Germany", "Romania")){
  shap_b_1.1 <- shap_b_1 %>%
    filter(Country == i)
  
  shap_b_2 <- shap_b %>%
    filter(Country == i)%>%
    left_join(shap_b_1.1)%>%
    filter(!is.na(rank))%>%
    # Data transformation
    mutate(Outcome = case_when(Outcome == "support_2" ~ "> Rather oppose",
                               Outcome == "support_3" ~ "> Neutral",
                               Outcome == "support_4" ~ "> Rather support",
                               Outcome == "support_5" ~ "> Strongly support"))%>%
    mutate(Outcome = factor(Outcome, levels = c("> Rather oppose", "> Neutral", "> Rather support", "> Strongly support")))%>%
    mutate(direction = factor(direction))%>%
    mutate(Variable = case_when(Variable == "Q45_1"       ~ "Fairness perception (Q45)",
                                Variable == "Q44_1"       ~ "Effects on vulnerable (Q44)",
                                Variable == "Q43_1"       ~ "Relative costs (Q43)",
                                Variable == "Q42_1_true"  ~ "Individual costs (Q42)",
                                Variable == "Q41_1"       ~ "Effectiveness perception (Q41)",
                                Variable == "Q38"         ~ "Political Party (Q38)",
                                Variable == "Q37_c"       ~ "Impact on emissions (Q37)",
                                Variable == "Q37_b"       ~ "Impact on life (Q37)",
                                Variable == "Q37_a"       ~ "Impact on economy (Q37)",
                                Variable == "Q36"         ~ "Climate change concern (Q36)",
                                Variable == "Q35_1"       ~ "Communication: Public (Q35)",
                                Variable == "Q35_4"       ~ "Communcation: Scientists (Q35)",
                                Variable == "Q31_Gov_nat" ~ "Integrity national gov. (Q31)",
                                Variable == "Q31_EU_Comm" ~ "Integrity EU commission (Q31)",
                                Variable == "Q30_1"       ~ "Trust in local gov. (Q30)",
                                Variable == "Q30_2"       ~ "Trust in national gov. (Q30)",
                                Variable == "Q30_3"       ~ "Trust in EU (Q30)",
                                Variable == "Q28"         ~ "Expenditures (Q28)",
                                Variable == "noise"       ~ "Random term",
                                Variable == "Q10"         ~ "Age (Q10)",
                                TRUE ~ Variable))%>%
    mutate(Variable = fct_reorder(Variable, rank, .desc = TRUE))%>%
    mutate(label_0 = paste0(round(share_SHAP*100,0),"%"))
  
  P_2.3.1 <- ggplot(data = shap_b_2, aes(x = Outcome, y = Variable))+
    # geom_point(shape = 22,fill = NA,colour = "black",stroke = 0.3,size = 9)+
    geom_point(aes(alpha = share_SHAP), shape = 22, size = 9, fill = "#3C5488FF", colour = "black", stroke = 0.3)+
    geom_text(aes(label = label_0), size = 2)+
    scale_alpha_continuous(range = c(0,0.7))+
    scale_x_discrete(position = "top")+
    # geom_point(alpha = 0.85, shape = 21, fill = "#4DBBD5FF", colour = "black")+
    # scale_size_continuous(range = c(2,8),
    #                       breaks = c(0.01, 0.05, 0.1, 0.2, 0.5),
    #                       name    = "Average SHAP Contribution", 
    #                       labels = percent)+l
    theme_bw()+
    ylab("Feature")+
    xlab("Average SHAP Contribution: Do you support or oppose the EU ETS2?")+
    ggtitle(i)+
    guides(alpha = "none")+
    theme(legend.position = "bottom",
          panel.grid.major = element_blank(),
          axis.ticks = element_line(linewidth = 0.3),
          axis.text  = element_text(size = 7),
          axis.title = element_text(size = 8),
          title = element_text(size = 8),
          legend.text = element_text(size = 8),
          legend.title = element_text(hjust = 0.5, size = 8))
  
  pdf(sprintf("../6_EUETS2_Citizens_Survey/1_Figures/A_Figure_B_wo_%s.pdf", i), width = 130/25.4, height = 70/25.4)
  print(P_2.3.1)
  dev.off()
  
}

rm(shap_2.3.1.1, shap_2.3.1.1.0, shap_2.3.1.1.1, shap_2.3.1.2.0, shap_2.3.1.1,
   shap_a, shap_b, shap_b_1, shap_b_1.1, shap_b_2, P_2.3.1, i, j)

# 2.3.2  Individual SHAP-values (Figure 2 and Appendix) ####

data_2.3.2 <- data.frame()

for(i in c("Spain", "France", "Germany", "Romania")){
  shap_2.3.2   <- read_parquet(sprintf("../2_Data/1_Support_Datasets/1_SHAP_1/SHAP_wo_%s_support_3.parquet", i))
  data_2.3.2.0 <- read_parquet(sprintf("../2_Data/1_Support_Datasets/1_SHAP_1/Data_wo_%s_support_3.parquet", i))
  
  # Select relevant variables
  shap_2.3.2.1 <- shap_2.3.2 %>%
    mutate(id = 1:n())%>%
    select(id, everything())%>%
    pivot_longer(-id, names_to = "variable", values_to = "SHAP")%>%
    mutate(VAR_0 = case_when(grepl("Q45_", variable) ~ "Fairness perception (Q45)",
                             grepl("Q41_", variable) ~ "Effectiveness perception (Q41)",
                             grepl("Q36_", variable) ~ "Climate change concern (Q36)",
                             TRUE ~ NA))%>%
    filter(!is.na(VAR_0))
  
  for(j in c("Q36", "Q41_1", "Q45_1")){
    # Select relevant SHAP values
    shap_2.3.2.2 <- shap_2.3.2.1 %>%
      filter(grepl(j, variable))
    
    data_2.3.2.1 <- data_2.3.2.0 %>%
      mutate(id = 1:n())%>%
      select(id, all_of(j))%>%
      left_join(shap_2.3.2.2, by = "id")%>%
      rename(var_interest = j)%>%
      mutate(var_interest = as.character(var_interest))%>%
      mutate(variable = str_remove(variable, paste0(j,"_")))%>%
      mutate(variable = str_replace_all(variable, "\\."," "))%>%
      mutate(variable = ifelse(variable == "I don t know", "I don't know", variable))%>%
      mutate(Yes = ifelse(var_interest == variable,1,0))%>%
      group_by(id)%>%
      mutate(Sum_Yes = sum(Yes))%>%
      ungroup()%>%
      filter(Yes == 1)%>%
      select(id, SHAP)
    
    # Dataframe with relevant values and corresponding SHAP values
    data_2.3.2.2 <- data_2.3.2.0 %>%
      mutate(id = 1:n())%>%
      select(id, all_of(j))%>%
      left_join(data_2.3.2.1, by = "id")%>%
      mutate(SHAP = ifelse(is.na(SHAP),0,SHAP))%>%
      rename(level = j)%>%
      mutate(VAR_0 = j)%>%
      mutate(Country = i)
    
    data_2.3.2 <- data_2.3.2 %>%
      bind_rows(data_2.3.2.2)
  }
}

data_2.3.2.1 <- data_2.3.2 %>%
  mutate(LEVEL = case_when(VAR_0 == "Q45_1" & level %in% c("Injusta", "Injuste", "Ich finde sie ungerecht", "Incorecta") ~ "Unfair",
                           VAR_0 == "Q45_1" & level %in% c("Ni justa ni injusta","Ni juste ni injuste","Ich finde sie weder gerecht noch ungerecht", "Nici corecta nici incorecta") ~ "Neither fair nor unfair",
                           VAR_0 == "Q45_1" & level %in% c("Justa", "Juste", "Ich finde sie gerecht", "Corecta") ~  "Fair",
                           VAR_0 == "Q45_1" & level == "I don't know" ~ "I don't know",
                           VAR_0 == "Q41_1" & level %in% c("Probablemente eficaz", "Probablement efficace", "Vermutlich", "Probabil va fi eficienta")                   ~ "Probably effective",
                           VAR_0 == "Q41_1" & level %in% c("Sin duda eficaz", "Certainement efficace", "Auf jeden Fall", "In mod sigur va fi eficienta")                ~ "Definitely effective",
                           VAR_0 == "Q41_1" & level %in% c("Probablemente no sea eficaz", "Probablement inefficace", "Vermutlich nicht", "Probabil nu va fi eficienta") ~ "Probably ineffective",
                           VAR_0 == "Q41_1" & level %in% c("En absoluto eficaz", "Certainement inefficace", "Auf keinen Fall", "In mod sigur nu va fi eficienta")       ~ "Definitely ineffective",
                           VAR_0 == "Q41_1" & level == "I don't know" ~ "I don't know",
                           VAR_0 == "Q36" & level %in% c("No me preocupa", "Pas préoccupé(e)", "Nicht besorgt", "Nu sunt preocupat(ă)")           ~ "Not concerned",
                           VAR_0 == "Q36" & level %in% c("Me preocupa un poco","Un peu préoccupé(e)", "Ein wenig besorgt", "Puțin preocupat(ă)")  ~ "Somewhat concerned",
                           VAR_0 == "Q36" & level %in% c("Me preocupa algo", "Assez préoccupé(e)", "Ziemlich besorgt", "Oarecum preocupat(ă)")    ~ "Quite concerned",
                           VAR_0 == "Q36" & level %in% c("Me preocupa mucho", "Très préoccupé(e)", "Sehr besorgt", "Foarte preocupat(ă)")         ~ "Very concerned",
                           VAR_0 == "Q36" & level %in% c("No tenga una opinión al respecto","Sans opinion", "Nu am nicio părere", "I don't know") ~ "No opinion"))%>%
  mutate(VAR_0 = factor(VAR_0, levels = c("Q45_1", "Q41_1","Q36")))%>%
  mutate(VAR_1 = case_when(VAR_0 == "Q45_1" ~ "Fairness perception\n(Ref.: Unfair)",
                           VAR_0 == "Q41_1" ~ "Effectiveness perception\n(Ref.: Definitely ineffective)",
                           VAR_0 == "Q36"   ~ "Climate change concern\n(Ref.: Not concerned)"))%>%
  mutate(VAR_1 = factor(VAR_1, levels = c("Fairness perception\n(Ref.: Unfair)",
                                          "Effectiveness perception\n(Ref.: Definitely ineffective)",
                                          "Climate change concern\n(Ref.: Not concerned)")))%>%
  # Remove I donÄt know and no opinions
  filter(!LEVEL %in% c("I don't know", "No opinion"))%>%
  # Remove baselines
  filter(!LEVEL %in% c("Unfair", "Definitely ineffective", "Not concerned"))%>% # TBD for Q38
  mutate(LEVEL = factor(LEVEL, levels = c("Fair", "Neither fair nor unfair", 
                                          "Definitely effective", "Probably effective","Probably ineffective",
                                          "Very concerned", "Quite concerned", "Somewhat concerned")))%>%
  mutate(Country = case_when(Country == "Spain"   ~ "Spain (ACC: 0.83)",
                             Country == "France"  ~ "France (ACC: 0.85)",
                             Country == "Germany" ~ "Germany (ACC: 0.82)",
                             Country == "Romania" ~ "Romania (ACC: 0.88)"))%>%
  mutate(Country = factor(Country, levels = c("Spain (ACC: 0.83)",
                                              "France (ACC: 0.85)",
                                              "Germany (ACC: 0.82)",
                                              "Romania (ACC: 0.88)")))

P_2.3.2 <- ggplot(data_2.3.2.1)+
  geom_vline(aes(xintercept = 0))+
  # geom_boxplot(aes(x = SHAP, y = LEVEL))+
  geom_jitter(aes(x = SHAP, y = LEVEL), size = 0.4, height = 0.25, width = 0, shape = 21, fill = "#3C5488FF", alpha = 0.4, stroke = 0)+
  facet_grid(VAR_1 ~ Country, scales = "free_y", switch = "y", space = "free")+
  theme_bw()+
  xlab("SHAP values")+
  theme(strip.placement    = "outside",
        axis.title.y       = element_blank(),
        strip.background   = element_blank(),
        axis.text.y        = element_text(size = 6), 
        axis.text.x        = element_text(size = 6),
        axis.title.x       = element_text(size = 7),
        strip.text         = element_text(size = 7),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks         = element_line(size = 0.2))

jpeg("../6_EUETS2_Citizens_Survey/1_Figures/A_Figure_B2.jpeg", width = 160/25.4, height = 120/25.4, unit = "in", res = 600)
print(P_2.3.2)
dev.off()

rm(data_2.3.2, data_2.3.2.0, data_2.3.2.1, data_2.3.2.2, shap_2.3.2, shap_2.3.2.1, shap_2.3.2.2, P_2.3.2, i, j)

# 2.3    Baseline correlation between policy support and institutional trust ####

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


# 2.4    Treatment effects (A,B,C1 to C4) on overall policy support ####

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

# 2.5    Boosted regression trees - predicting delta in support (t=1 vs. t=0) ####

data_2.5 <- data_2 %>%
  # Create outcome variables - negative values: reduces support - positive values: increases support
  mutate(Q46_delta = Q46_2N - Q46_1N)%>%
  # Excluding "I don't know" answers
  filter(!is.na(Q46_delta))%>%
  # Recode policy support - reduces support
  mutate(support_1 = ifelse(Q46_delta < 0,1,0),
  # Recode policy support - sustain support
         support_2 = ifelse(Q46_delta == 0,1,0),
  # Recode policy support - increase support
         support_3 = ifelse(Q46_delta > 0,1,0))%>%
  mutate_at(vars(starts_with("support_")), ~ factor(.))%>%
  mutate(Pricelevel = ifelse(is.na(Pricelevel), Priceleveleuro, Pricelevel))%>%
  mutate(Pricelevel = factor(Pricelevel, levels = c(45,85,125)))%>%
  # Overestimated/underestimated
  mutate(Overestimated_Absolute = ifelse(Dif_cost_1 > 0, "Overestimated",
                                         ifelse(Dif_cost_1 < 0, "Underestimated", NA)))%>%
  mutate(Overestimated_Distribution = ifelse(Dif_Percentile_1 > 0, "Overestimated",
                                             ifelse(Dif_Percentile_1 < 0, "Underestimated", NA)))%>%
  # C1 - overestimated/underestimated absolute
  mutate(C1_overestimated  = ifelse(Treatment_C == "C1" & Overestimated_Absolute == "Overestimated" & !is.na(Overestimated_Absolute),1,0),
         C1_underestimated = ifelse(Treatment_C == "C1" & Overestimated_Absolute == "Underestimated" & !is.na(Overestimated_Absolute),1,0))%>%
  # C2 - overestimated/underestimated relative
  mutate(C2_overestimated  = ifelse(Treatment_C == "C2" & Overestimated_Distribution == "Overestimated" & !is.na(Overestimated_Distribution),1,0),
         C2_underestimated = ifelse(Treatment_C == "C2" & Overestimated_Distribution == "Underestimated" & !is.na(Overestimated_Distribution),1,0))%>%
  # C3 - overestimated/underestimated absolute and relative
  mutate(C3_overestimated  = ifelse(Treatment_C == "C3" & Overestimated_Absolute == "Overestimated" & !is.na(Overestimated_Absolute),1,0),
         C3_underestimated = ifelse(Treatment_C == "C3" & Overestimated_Absolute == "Underestimated" & !is.na(Overestimated_Absolute),1,0))%>%
  mutate(C3_overestimated_dist  = ifelse(Treatment_C == "C3" & Overestimated_Distribution == "Overestimated" & !is.na(Overestimated_Distribution),1,0),
         C3_underestimated_dist = ifelse(Treatment_C == "C3" & Overestimated_Distribution == "Underestimated" & !is.na(Overestimated_Distribution),1,0))%>%
  # C4 - overestimated/underestimated relative
  mutate(C4_overestimated  = ifelse(Treatment_C == "C4" & Overestimated_Distribution == "Overestimated" & !is.na(Overestimated_Distribution),1,0),
         C4_underestimated = ifelse(Treatment_C == "C4" & Overestimated_Distribution == "Underestimated" & !is.na(Overestimated_Distribution),1,0))%>%
  # Clear / credible
  mutate(B_clear = ifelse(Treatment_B == "Treatment", Q52A, NA),
         C1_clear = ifelse(Treatment_C == "C1", Q57, NA),
         C2_clear = ifelse(Treatment_C == "C2", Q57, NA),
         C3_clear = ifelse(Treatment_C == "C3", Q57, NA),
         C4_clear = ifelse(Treatment_C == "C4", Q57, NA))%>%
  mutate(B_credible = ifelse(Treatment_B == "Treatment", Q52B, NA),
         C1_credible = ifelse(Treatment_C == "C1", Q58, NA),
         C2_credible = ifelse(Treatment_C == "C2", Q58, NA),
         C3_credible = ifelse(Treatment_C == "C3", Q58, NA),
         C4_credible = ifelse(Treatment_C == "C4", Q58, NA))%>%
  # Include support in first period
  select(Country, Q10:Q23,Q24,Q25:Q28,Q29A,Q29B,Q30_1:Q38,Q41_1:Q46_1, support_1:support_3, Pricelevel,
         Treatment_A, Treatment_B, Treatment_C, Q51, 
         starts_with("C1_"), starts_with("C2_"), starts_with("C3_"), starts_with("C4_"), starts_with("B_"))%>%
  # TBD: Homogenize industry variable. Leave out for now.
  select(-Q42_1, -Q14, -Q42_1_average)

track_tuning <- read.xlsx("../2_Data/1_Support_Datasets/1_SHAP_1/Tuning_SHAP_delta.xlsx")

track_performance <- read.xlsx("../2_Data/1_Support_Datasets/1_SHAP_1/Performance_SHAP_delta.xlsx")

for(i in c("Spain", "France", "Germany", "Romania")){
  
  data_2.5.1 <- data_2.5 %>%
    filter(Country == i)
  
  if(i == "Spain"){
    data_2.5.1 <- data_2.5.1 %>%
      mutate(Q11 = ifelse(Q11 == "Otros", "Mujer", Q11))
  }
  
  if(i == "France"){
    data_2.5.1 <- data_2.5.1 %>%
      mutate(Q11 = ifelse(Q11 == "Autre", "Féminin", Q11))
  }
  
  if(i == "Romania"){
    data_2.5.1 <- data_2.5.1 %>%
      mutate(Q11 = ifelse(Q11 == "Altul", "Feminin", Q11))%>%
      mutate_at(vars(Q31_Gov_nat:Q31_EU_Comm), ~ ifelse(is.na(.), "Nu știu / Nu pot să spun",.))
  }
  
  for(j in c("support_1", "support_2", "support_3")){
    
    track_0 <- data.frame(Country = i,
                          Outcome = j,
                          date    = date())
    run_number <- 1
    run_ID <- paste0(i,"_",j,"_",1)
    # run_number <- if(i %in% track_tuning$Country & j %in% track_tuning$Outcome) max(track_tuning$number[track_tuning$Country == i & track_tuning$Outcome == j])+1 else 1
    # run_ID     <- if(i %in% track_tuning$Country & j %in% track_tuning$Outcome) paste0(i,"_",j,"_",max(track_tuning$number[track_tuning$Country == i & track_tuning$Outcome == j])+1) else (paste0(i,"_",j,"_",1))
    
    print(paste0("Start ", i, ": ", run_ID, ": ", Sys.time()))
    
    data_2.5.2 <- data_2.5.1 %>%
      # Remove all other support columns and only keep relevant outcome variable
      rename(outcome = all_of(j))%>%
      select(-Country, -starts_with("support"))%>%
      # Removes unused factor levels
      mutate(across(where(is.factor), ~ fct_drop(.)))%>%
      # Convert to factor variable
      mutate(across(where(~ is.character(.)), ~ as.factor(.)))%>%
      # Replace NAs where not applicable or for "I don't know"
      # mutate_at(vars(Q14),   ~ fct_na_value_to_level(., level = "Not applicable"))%>%
      mutate_at(vars(Q30_1:Q30_3, Q41_1:Q42_1_true,Q43_1:Q45_1), ~ fct_na_value_to_level(., level = "I don't know"))%>%
      mutate(Missing_Q42_1_relative = ifelse(is.na(Q42_1_relative),1,0))%>%
      # Remove columns with just NA
      select(where(~ !all(is.na(.))))%>%
      # Create noise parameter
      mutate(noise = rnorm(nrow(.),0,1))
    
    if(i == "France"){
      data_2.5.2 <- data_2.5.2 %>%
        filter(!is.na(Q10) & !is.na(Q12) & !is.na(Q35_1))%>%
        mutate_at(vars(Q11, Q13, Q15, Q16, Q35_2:Q35_4), ~ fct_na_value_to_level(., level = "I don't know"))
    }
    
    if(i == "Germany"){
      data_2.5.2 <- data_2.5.2 %>%
        mutate_at(vars(Q36:Q37_c), ~ fct_na_value_to_level(., level = "I don't know"))
    }
    
    if(i == "Romania"){
      data_2.5.2 <- data_2.5.2 %>%
        filter(!is.na(Q11) & !is.na(Q12) & !is.na(Q35_1))%>%
        mutate_at(vars(Q15,Q16,Q35_2:Q35_4), ~ fct_na_value_to_level(., level = "I don't know"))
    }
    
    # Training and testing dataset
    
    data_split_2.5.2 <- initial_split(data_2.5.2, prop = 0.8, strata = outcome)
    
    train_2.5.2 <- training(data_split_2.5.2) 
    test_2.5.2  <- testing(data_split_2.5.2)
    
    # Recipe
    
    recipe_2.5.2 <- recipe(outcome ~ ., 
                           data = data_2.5.2)%>%
      # Delete columns with NA (should be redundant)
      step_unknown(all_nominal_predictors(), new_level = "MISSING")%>%
      # step_filter_missing(all_nominal_predictors(), threshold = 0)%>%
      step_other(Q12,Q13,Q26, threshold = 0.05)%>%
      step_dummy(all_nominal_predictors(), sparse = "no")%>%
      step_zv(all_predictors())
    
    mtry_max <- recipe_2.5.2 %>%
      prep(training = train_2.5.2)%>%
      bake(new_data = NULL)%>%
      select(-outcome)%>%
      ncol()
    
    # Five-fold cross-validation
    
    folds_2.5.2 <- vfold_cv(train_2.5.2, v = 5)
    
    model_2.5.2 <- boost_tree(
      trees      = 1000,
      tree_depth = tune(),
      learn_rate = tune(),
      mtry       = tune(),
      stop_iter  = 15)%>%
      set_mode("classification")%>%
      set_engine("xgboost")
    
    workflow_2.5.2 <- workflow()%>%
      add_recipe(recipe_2.5.2)%>%
      add_model(model_2.5.2)
    
    # Create tuning grid
    
    grid_2.5.2 <- grid_space_filling(
      tree_depth(c(3,15)),
      learn_rate(c(-3,-1)),
      mtry(c(round((mtry_max/2),0),mtry_max)),
      size = 99)%>%
      # default parameters
      bind_rows(data.frame(tree_depth = 6, learn_rate = 0.3, mtry = mtry_max))
    
    # Tune the model
    
    doParallel::registerDoParallel()
    
    time_1 <- Sys.time()
    
    model_2.5.2 <- tune_grid(workflow_2.5.2,
                             resamples = folds_2.5.2,
                             grid      = grid_2.5.2,
                             metrics   = metric_set(accuracy, mn_log_loss, f_meas))
    
    time_2 <- Sys.time()
    
    doParallel::stopImplicitCluster()
    
    print("End computing")
    
    tuning_time <- as.integer(difftime(time_2, time_1, units = "min"))
    
    # Collect metrics of tuned model
    
    metrics_2.5.2 <- collect_metrics(model_2.5.2)
    
    model_2.5.2.1 <- select_best(model_2.5.2, metric = "f_meas")
    
    metrics_2.5.2.1 <- metrics_2.5.2 %>%
      filter(.config == model_2.5.2.1$.config[1])
    
    track_1 <- track_0 %>%
      mutate(number      = run_number,
             run_ID      = run_ID,
             tuning_time = tuning_time)%>%
      bind_cols(model_2.5.2.1)%>%
      rename(tree_depth_best = tree_depth, learn_rate_best = learn_rate, mtry_best = mtry)%>%
      select(-.config)%>%
      mutate(accuracy    = metrics_2.5.2.1$mean[metrics_2.5.2.1$.metric == "accuracy"],
             f_meas      = metrics_2.5.2.1$mean[metrics_2.5.2.1$.metric == "f_meas"],
             mn_log_loss = metrics_2.5.2.1$mean[metrics_2.5.2.1$.metric == "mn_log_loss"])
    
    # First outcome: Table with all tuning details.
    track_tuning <- track_tuning %>%
      bind_rows(track_1)
    
    parameters <- track_tuning %>%
      filter(Country == i & Outcome == j)%>%
      dplyr::slice(which.max(number))%>%
      rename(tree_depth = tree_depth_best,
             learn_rate = learn_rate_best,
             mtry       = mtry_best)%>%
      select(tree_depth, learn_rate, mtry)%>%
      as.list()
    
    # Fit best model
    
    workflow_2.5.3 <- finalize_workflow(workflow_2.5.2, parameters)
    
    # Fit model
    
    model_2.5.3 <- fit(workflow_2.5.3, data = train_2.5.2)
    
    evaluation_2.5.3 <- predict(model_2.5.3, test_2.5.2, type = "class")%>%
      bind_cols(predict(model_2.5.3, test_2.5.2, type = "prob"))%>%
      bind_cols(test_2.5.2)
    
    metrics_2.5.3 <- metric_set(
      accuracy,
      kap,
      sens,
      yardstick::spec,
      f_meas,
      roc_auc
    )
    
    metrics_2.5.3.1 <- metrics_2.5.3(evaluation_2.5.3,
                                     truth = outcome,
                                     estimate = .pred_class,
                                     .pred_1)%>%
      select(-.estimator)%>%
      pivot_wider(names_from = ".metric", values_from = ".estimate")
    
    track_2 <- track_0 %>%
      mutate(number      = run_number,
             run_ID      = run_ID)%>%
      bind_cols(metrics_2.5.3.1)
    
    track_performance <- track_performance %>%
      bind_rows(track_2)
    
    # Extract SHAP values (full dataset)
    
    model_2.5.4 <- fit(workflow_2.5.3, data = data_2.5.2)
    engine_2.5.4 <- extract_fit_engine(model_2.5.4)
    data_2.5.4 <- bake(prep(recipe_2.5.2, training = data_2.5.2), new_data = data_2.5.2)%>%
      select(-outcome)%>%
      as.matrix()
    
    time_3 <- Sys.time()
    
    shap_2.5.4 <- predict(engine_2.5.4,
                          data_2.5.4,
                          predcontrib = TRUE,
                          approxcontrib = FALSE)
    
    time_4 <- Sys.time()
    
    shaping_time <- as.integer(difftime(time_4, time_3, units = "min"))
    
    # shap_2.2.4.1 <- shap_2.2.4 %>%
    #   as_tibble()%>%
    #   summarise_all(~ mean(abs(.)))%>%
    #   select(-"(Intercept)")%>%
    #   pivot_longer(everything(), names_to = "variable", values_to = "SHAP_contribution")%>%
    #   arrange(desc(SHAP_contribution))%>%
    #   mutate(tot_contribution = sum(SHAP_contribution))%>%
    #   mutate(share_SHAP = SHAP_contribution/tot_contribution)%>%
    #   select(-tot_contribution)
    
    shap_2.5.4.1 <- shap_2.5.4 %>%
      as_tibble()
    
    write_parquet(shap_2.5.4.1, sprintf("../2_Data/1_Support_Datasets/1_SHAP_1/SHAP_%s_%s_delta.parquet", i, j))
    write_parquet(data_2.5.2,   sprintf("../2_Data/1_Support_Datasets/1_SHAP_1/Data_%s_%s_delta.parquet", i, j))
    
    rm(data_2.5.2, data_2.5.4, data_split_2.5.2, engine_2.5.4, evaluation_2.5.3, folds_2.5.2, grid_2.5.2,
       metrics_2.5.2, metrics_2.5.2.1, metrics_2.5.3.1, model_2.5.2, model_2.5.2.1, model_2.5.3, model_2.5.4,
       parameters, recipe_2.5.2, shap_2.5.4, shap_2.5.4.1, test_2.5.2, train_2.5.2, workflow_2.5.2, workflow_2.5.3,
       time_1, time_2, time_3, time_4)
  }
  
}

write.xlsx(track_tuning, "../2_Data/1_Support_Datasets/1_SHAP_1/Tuning_SHAP_delta.xlsx")
write.xlsx(track_performance, "../2_Data/1_Support_Datasets/1_SHAP_1/Performance_SHAP_delta.xlsx")


# 2.5.1  Analysing SHAP values for each combination ####

shap_a <- data.frame()
shap_b <- data.frame()

for(i in c("Spain", "France", "Germany", "Romania")){
  for(j in c("support_1", "support_2", "support_3")){
    
    shap_2.5.1.1 <- read_parquet(sprintf("../2_Data/1_Support_Datasets/1_SHAP_1/SHAP_%s_%s_delta.parquet", i, j))
    data_2.5.1.1 <- read_parquet(sprintf("../2_Data/1_Support_Datasets/1_SHAP_1/Data_%s_%s_delta.parquet", i, j))
    
    shap_2.5.1.1.0 <- shap_2.5.1.1 %>%
      summarise_all(~ mean(abs(.)))%>%
      select(-"(Intercept)")%>%
      pivot_longer(everything(), names_to = "variable", values_to = "SHAP_contribution")%>%
      arrange(desc(SHAP_contribution))%>%
      mutate(tot_contribution = sum(SHAP_contribution))%>%
      mutate(share_SHAP       = SHAP_contribution/tot_contribution)%>%
      select(-tot_contribution)%>%
      mutate(Country = i,
             Outcome = j)
    
    shap_2.5.1.1.1 <- shap_2.5.1.1 %>%
      summarise_all(~ mean(.))%>%
      select(-"(Intercept)")%>%
      pivot_longer(everything(), names_to = "variable", values_to = "SHAP_contribution")%>%
      mutate(Variable   = str_replace(variable, "_[^_]+$", ""))%>%
      group_by(Variable)%>%
      summarise(direction = sign(SHAP_contribution[which.max(abs(SHAP_contribution))]))%>%
      ungroup()%>%
      mutate(Country = i,
             Outcome = j)
    
    shap_2.5.1.2.0 <- shap_2.5.1.1.0 %>%
      mutate(Variable   = str_replace(variable, "_[^_]+$", ""))%>%
      group_by(Variable)%>%
      summarise(share_SHAP = sum(share_SHAP))%>%
      ungroup()%>%
      arrange(desc(share_SHAP))%>%
      mutate(Country = i,
             Outcome = j)%>%
      left_join(shap_2.5.1.1.1)
    
    shap_a <- shap_a %>%
      bind_rows(shap_2.5.1.1.0)
    
    shap_b <- shap_b %>%
      bind_rows(shap_2.5.1.2.0)
    
  }
}

# What are the ten most important features (on average) for each level of support

shap_b_1 <- shap_b %>%
  group_by(Country, Variable)%>%
  summarise(mean_SHAP = mean(share_SHAP))%>%
  arrange(desc(mean_SHAP))%>%
  mutate(rank = 1:n())%>%
  ungroup()%>%
  filter(rank < 7)

# Dataset for each country

for(i in c("Spain", "France", "Germany", "Romania")){
  shap_b_1.1 <- shap_b_1 %>%
    filter(Country == i)
  
  shap_b_2 <- shap_b %>%
    filter(Country == i)%>%
    left_join(shap_b_1.1)%>%
    #filter(!is.na(rank))%>%
    # Data transformation
    mutate(Outcome = case_when(Outcome == "support_1" ~ "Reduce support",
                               Outcome == "support_2" ~ "Not change support",
                               Outcome == "support_3" ~ "Increase support"))%>%
    mutate(Outcome = factor(Outcome, levels = c("Reduce support", "Not change support", "Increase support")))%>%
    mutate(direction = factor(direction))%>%
    mutate(Variable = case_when(Variable == "Q45_1"       ~ "Fairness perception (Q45)",
                                Variable == "Q44_1"       ~ "Effects on vulnerable (Q44)",
                                Variable == "Q43_1"       ~ "Relative costs (Q43)",
                                Variable == "Q42_1_true"  ~ "Individual costs (Q42)",
                                Variable == "Q41_1"       ~ "Effectiveness perception (Q41)",
                                Variable == "Q38"         ~ "Political Party (Q38)",
                                Variable == "Q37_c"       ~ "Impact on emissions (Q37)",
                                Variable == "Q37_b"       ~ "Impact on life (Q37)",
                                Variable == "Q37_a"       ~ "Impact on economy (Q37)",
                                Variable == "Q36"         ~ "Climate change concern (Q36)",
                                Variable == "Q35_1"       ~ "Communication: Public (Q35)",
                                Variable == "Q35_4"       ~ "Communcation: Scientists (Q35)",
                                Variable == "Q31_Gov_nat" ~ "Integrity national gov. (Q31)",
                                Variable == "Q31_EU_Comm" ~ "Integrity EU commission (Q31)",
                                Variable == "Q30_1"       ~ "Trust in local gov. (Q30)",
                                Variable == "Q30_2"       ~ "Trust in national gov. (Q30)",
                                Variable == "Q30_3"       ~ "Trust in EU (Q30)",
                                Variable == "Q28"         ~ "Expenditures (Q28)",
                                Variable == "noise"       ~ "Random term",
                                Variable == "Q10"         ~ "Age (Q10)",
                                Variable %in% c("Treatment_B", "B_clear", "B_credible") ~ "B",
                                Variable %in% c("C1", "C1_clear", "C1_credible") ~ "C1",
                                Variable %in% c("C2", "C2_clear", "C2_credible") ~ "C2",
                                Variable %in% c("C3", "C3_clear", "C3_credible") ~ "C3",
                                Variable %in% c("C4", "C4_clear", "C4_credible") ~ "C4",
                                TRUE ~ Variable))%>%
    mutate(Variable_2 = ifelse(Variable %in% c("C1", "C2", "C3", "C4"), "Treatment_C", Variable))%>%
    group_by(Variable_2, Outcome, Country)%>%
    summarise(share_SHAP = sum(share_SHAP))%>%
    ungroup()%>%
    filter(!is.na(Variable_2))%>%
    group_by(Variable_2)%>%
    mutate(mean_shap = mean(share_SHAP))%>%
    ungroup()%>%
    arrange(desc(mean_shap))%>%
    mutate(rank = 1:n())%>%
    filter(rank < 25)%>%
    mutate(Variable = fct_reorder(Variable_2, mean_shap))%>%
    mutate(label_0 = paste0(round(share_SHAP*100,0),"%"))
  
  P_2.3.1 <- ggplot(data = shap_b_2, aes(x = Outcome, y = Variable))+
    # geom_point(shape = 22,fill = NA,colour = "black",stroke = 0.3,size = 9)+
    geom_point(aes(alpha = share_SHAP), shape = 22, size = 9, fill = "#3C5488FF", colour = "black", stroke = 0.3)+
    geom_text(aes(label = label_0), size = 2)+
    scale_alpha_continuous(range = c(0,0.7))+
    scale_x_discrete(position = "top")+
    # geom_point(alpha = 0.85, shape = 21, fill = "#4DBBD5FF", colour = "black")+
    # scale_size_continuous(range = c(2,8),
    #                       breaks = c(0.01, 0.05, 0.1, 0.2, 0.5),
    #                       name    = "Average SHAP Contribution", 
    #                       labels = percent)+l
    theme_bw()+
    ylab("Feature")+
    xlab("Average SHAP Contribution: Change in support for EU ETS 2")+
    ggtitle(i)+
    guides(alpha = "none")+
    theme(legend.position = "bottom",
          panel.grid.major = element_blank(),
          axis.ticks = element_line(linewidth = 0.3),
          axis.text  = element_text(size = 7),
          axis.title = element_text(size = 8),
          title = element_text(size = 8),
          legend.text = element_text(size = 8),
          legend.title = element_text(hjust = 0.5, size = 8))
  
  pdf(sprintf("../6_EUETS2_Citizens_Survey/1_Figures/A_Figure_B_Delta_%s.pdf", i), width = 130/25.4, height = 100/25.4)
  print(P_2.3.1)
  dev.off()
  
}

rm(shap_2.3.1.1, shap_2.3.1.1.0, shap_2.3.1.1.1, shap_2.3.1.2.0, shap_2.3.1.1,
   shap_a, shap_b, shap_b_1, shap_b_1.1, shap_b_2, P_2.3.1, i, j)


# 3      Hypothesis tests ####

data_3_ESP <- data_1.6_ESP
data_3_FRA <- data_1.6_FRA
data_3_GER <- data_1.6_GER
data_3_ROM <- data_1.6_ROM %>%
  rename(Pricelevel = Priceleveleuro)

tex.style <- style.tex(model.title = "", fixef.title = "\\midrule \\textit{Fixed Effects/Control Variables:}",
                       depvar.title = "",
                       stats.title = "\\midrule", model.format = "",
                       fontsize = "small", yesNo = c("Yes","No"),
                       tablefoot.value	= "",
                       tablefoot = FALSE)

dict_latex <- c("Post_B" = "Information Treatment B $\\times$ Post",
                "Post_C" = "Information Treatment C $\\times$ Post",
                "Post_C13" = "Information Treatment C1 or C3 $\\times$ Post",
                "Post_C24" = "Information Treatment C2 or C4 $\\times$ Post",
                "Post_C234" = "Information Treatment C2 or C3 or C4 $\\times$ Post",
                "Post_C1"  = "Information Treatment C1 $\\times$ Post",
                "Post_C2"  = "Information Treatment C2 $\\times$ Post",
                "Post_C3"  = "Information Treatment C3 $\\times$ Post",
                "Post_C4"  = "Information Treatment C4 $\\times$ Post",
                "Post_B_ONLY" = "Information Treatment B $\\times$ Post",
                "Post_C_ONLY" = "Information Treatment C $\\times$ Post",
                "Post_C_ONLY_85" = "Information Treatment C $\\times$ Post $\\times$ 85€",
                "Post_C_ONLY_125" = "Information Treatment C $\\times$ Post $\\times$ 125€",
                "Post_B_C" = "Information Treatment B and C $\\times$ Post",
                "Post_B_C_85" = "Information Treatment B and C $\\times$ Post $\\times$ 85€",
                "Post_B_C_125" = "Information Treatment B and C $\\times$ Post $\\times$ 125€",
                "Post_B_ONLY_Credible" = "Information Treatment B $\\times$ Post $\\times$ Credible",
                "Post_C_ONLY_Credible" = "Information Treatment C $\\times$ Post $\\times$ Credible",
                "Post_B_C_Credible" = "Information Treatment B and C $\\times$ Post $\\times$ Credible",
                "Post_B_ONLY_Opposer" = "Information Treatment B $\\times$ Post $\\times$ Opposer",
                "Post_C_ONLY_Opposer" = "Information Treatment C $\\times$ Post $\\times$ Opposer",
                "Post_B_C_Opposer" = "Information Treatment B and C $\\times$ Post $\\times$ Opposer",
                "Post_C_ONLY_Over" = "Information Treatment C $\\times$ Post $\\times$ Overestimation",
                "Post_B_C_Over" = "Information Treatment B and C $\\times$ Post $\\times$ Overestimation",
                "Post_B_C1" = "Information Treatment B $\\times$ C1 $\\times$ Post",
                "Post_B_C2" = "Information Treatment B $\\times$ C2 $\\times$ Post",
                "Post_B_C3" = "Information Treatment B $\\times$ C3 $\\times$ Post",
                "Post_B_C4" = "Information Treatment B $\\times$ C4 $\\times$ Post",
                "C1_O_Post" = "Information Treatment C1 $\\times$ Overestimation $\\times$ Post",
                "C2_O_Post" = "Information Treatment C2 $\\times$ Overestimation $\\times$ Post",
                "C3_O_Post" = "Information Treatment C3 $\\times$ Overestimation $\\times$ Post",
                "C3_OR_Post" = "Information Treatment C3 $\\times$ Overestimation (R) $\\times$ Post",
                "C4_O_Post" = "Information Treatment C4 $\\times$ Overestimation $\\times$ Post",
                "Post_C5"  = "Control Group * Post",
                "Group_BC" = "Group",
                "B_C1" = "B $\\times$ C1",
                "B_C2" = "B $\\times$ C2",
                "B_C3" = "B $\\times$ C3",
                "B_C4" = "B $\\times$ C4",
                "C1_Only" = "C1",
                "C2_Only" = "C2",
                "C3_Only" = "C3",
                "C4_Only" = "C4",
                "Post_C1_Credible" = "Information Treatment C1 $\\times$ Post $\\times$ Credible",
                "Post_C2_Credible" = "Information Treatment C2 $\\times$ Post $\\times$ Credible",
                "Post_C3_Credible" = "Information Treatment C3 $\\times$ Post $\\times$ Credible",
                "Post_C4_Credible" = "Information Treatment C4 $\\times$ Post $\\times$ Credible",
                "Post_B_Credible" = "Information Treatment B $\\times$ Post $\\times$ Credible",
                "Post_B_Opposer"  = "Information Treatment B $\\times$ Post $\\times$ Opposer",
                "Post"                           = "Period",
                "ID"                             = "Respondent-ID",
                "Q41_1N"           = "$E_{i,t=1}$",
                "Q46_1N"           = "$\\Omega_{i,t=1}$",
                "Q46"              = "$\\Omega_{i,t}$",
                "Q45_1N"           = "$F_{i,t=1}$",
                "Q44_1N"           = "$V_{i,t=1}$",
                "Dif_cost_1"       = "$\\tilde{L}_{i,t=1}$",
                "Dif_Percentile_1" = "$\\tilde{L}^r_{i,t=1}$",
                "Q30_2N"           = "Institutional trust (national)",
                "Q30_3N"           = "Institutional trust (EU)",
                "Treatment_A"      = "Treatment A",
                "tau"              = "$\\gamma_{a,k}$",
                "tau_Treatment_A"  = "Trust * Treatment A",
                "tau_Post_B"       = "Trust * Treatment B * Post",
                "model_3.3.1_ESP"  = "Spain",
                "model_3.3.1_FRA"  = "France",
                "model_3.3.1_GER"  = "Germany",
                "model_3.3.1_ROM"  = "Romania",
                "value_abs_1"      = "$\\big|\\tilde{L}_{i,t}\\big|$",
                "value_abs_2"      = "$\\big|\\tilde{L}^r_{i,t}\\big|$",
                "Q41"              = "Effectiveness",
                "Q42"              = "Expected additional costs",
                "Q44"              = "Effects on vulnerable",
                "Q45"              = "Fairness",
                "Dif_cost"         = "Additional absolute cost",
                "Dif_Percentile"   = "Additional relative cost",
                "Post_P1"          = "Price-level 85€ $\\times$ Post",
                "Post_P2"          = "Price-level 125€ $\\times$ Post",
                "Period_Opposer"   = "Period $\\times$ Opposer",
                "Post_C_Opposer"   = "Information Treatment C $\\times$ Post $\\times$ Opposer",
                "Fairness"         = "Perception of fairness (Q45)",
                "Effectiveness"    = "Perception of effectiveness (Q41)",
                "Support"          = "Policy support (Q46)",
                "Dif_cost_ABS"     = "Perception of additional costs (error, Q42)")


# 3.1    Hypotheses 1 to 6 ####

# Institutional trust at national level: Q30_2/Q30_2N
# Institutional trust at EU-level: Q30_3/Q30_3N

# Overall policy support: Q46_1N
# Perception of effectiveness: Q41_1N
# Perception of fairness: Q45_1N
# Perception of effects on vulnerable households: Q44_1N
# Estimation of additional costs: Dif_cost_1 (>0 - overestimate costs)
# Estimation of relative additional costs: Dif_Percentile_1 (>0 - overestimate costs)

model_3.1_ESP <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1) ~ sw(Q30_2N, Q30_3N), data = data_3_ESP)
model_3.1_FRA <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1) ~ sw(Q30_2N, Q30_3N), data = data_3_FRA)
model_3.1_GER <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1) ~ sw(Q30_2N, Q30_3N), data = data_3_GER)
model_3.1_ROM <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1) ~ sw(Q30_2N, Q30_3N), data = data_3_ROM)

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
model_3.1_ESP_n <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1) ~ Q30_2N, data = data_3_ESP)
model_3.1_FRA_n <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1) ~ Q30_2N, data = data_3_FRA)
model_3.1_GER_n <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1) ~ Q30_2N, data = data_3_GER)
model_3.1_ROM_n <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1) ~ Q30_2N, data = data_3_ROM)

# Correction for BH does not change anything

etable(model_3.1_ESP_n, se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Correlation between institutional trust and policy perception in Spain",
       label = "tab_H_1_ESP",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H1_ESP.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD.")))

etable(model_3.1_FRA_n, se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Correlation between institutional trust and policy perception in France",
       label = "tab_H_1_FRA",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H1_FRA.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD.")))

etable(model_3.1_GER_n, se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Correlation between institutional trust and policy perception in Germany",
       label = "tab_H_1_GER",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H1_GER.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD.")))

etable(model_3.1_ESP_n, se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Correlation between institutional trust and policy perception in Romania",
       label = "tab_H_1_ROM",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H1_ROM.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD.")))

# TBA

rm(model_3.1_ESP, model_3.1_FRA, model_3.1_GER, model_3.1_ROM,
   model_3.1_ESP_n, model_3.1_FRA_n, model_3.1_GER_n, model_3.1_ROM_n,
   tidy_3.1_ESP, tidy_3.1_FRA, tidy_3.1_GER, tidy_3.1_ROM, correct_p_values_1)

# 3.2    Hypothesis 7 ####

model_3.2_ESP <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1) ~ i(Treatment_A, ref = "nonEU"), data = data_3_ESP)
model_3.2_FRA <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1) ~ i(Treatment_A, ref = "nonEU"), data = data_3_FRA)
model_3.2_GER <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1) ~ i(Treatment_A, ref = "nonEU"), data = data_3_GER)
model_3.2_ROM <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1) ~ i(Treatment_A, ref = "nonEU"), data = data_3_ROM)

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
# Correction for BH does not change anything

etable(model_3.2_ESP, se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment A in Spain",
       label = "tab_H_7_ESP",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H7_ESP.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD.")))

etable(model_3.2_FRA, se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment A in France",
       label = "tab_H_7_FRA",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H7_FRA.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD.")))

etable(model_3.2_GER, se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment A in Germany",
       label = "tab_H_7_GER",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H7_GER.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD.")))

etable(model_3.2_ROM, se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment A in Romania",
       label = "tab_H_7_ROM",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H7_ROM.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD.")))

adjust_hypothesis_7b <- function(data_3_0){
  data_3_1 <- data_3_0 %>%
    mutate(tau = ifelse(Q30_3N < 3,1,0))%>%
    filter(!is.na(tau))%>%
    mutate(tau_Treatment_A = ifelse(tau == 1 & Treatment_A == "EU",1,0))
  
  return(data_3_1)
}

model_3.2.1_ESP <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1) ~ i(Treatment_A, ref = "nonEU") + tau + tau_Treatment_A, data = adjust_hypothesis_7b(data_3_ESP))
model_3.2.1_FRA <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1) ~ i(Treatment_A, ref = "nonEU") + tau + tau_Treatment_A, data = adjust_hypothesis_7b(data_3_FRA))
model_3.2.1_GER <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1) ~ i(Treatment_A, ref = "nonEU") + tau + tau_Treatment_A, data = adjust_hypothesis_7b(data_3_GER))
model_3.2.1_ROM <- feols(c(Q46_1N, Q41_1N, Q45_1N, Q44_1N, Dif_cost_1, Dif_Percentile_1) ~ i(Treatment_A, ref = "nonEU") + tau + tau_Treatment_A, data = adjust_hypothesis_7b(data_3_ROM))

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

etable(model_3.2.1_ESP, se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment A on citizens with low trust in EU in Spain",
       label = "tab_H_7.1_ESP",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H7.1_ESP.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD.")))

etable(model_3.2.1_FRA, se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment A on citizens with low trust in EU in France",
       label = "tab_H_7.1_FRA",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H7.1_FRA.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD.")))

etable(model_3.2.1_GER, se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment A on citizens with low trust in EU in Germany",
       label = "tab_H_7.1_GER",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H7.1_GER.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD.")))

etable(model_3.2.1_ESP, se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment A on citizens with low trust in EU in Romania",
       label = "tab_H_7.1_ROM",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H7.1_ROM.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD.")))

# TBA

rm(adjust_hypothesis_7b, correct_p_values_7, correct_p_values_7b,
   model_3.2_ROM, model_3.2_FRA, model_3.2_GER, model_3.2_ESP, tidy_3.2_ROM, tidy_3.2_FRA, tidy_3.2_GER, tidy_3.2_ESP,
   model_3.2.1_ROM, model_3.2.1_FRA, model_3.2.1_GER, model_3.2.1_ESP, tidy_3.2.1_ROM, tidy_3.2.1_FRA, tidy_3.2.1_GER, tidy_3.2.1_ESP)

# 3.3    Hypotheses 8 to 9 ####

adjust_hypothesis_89 <- function(data_3_0, filter_1){
  data_3_3 <- data_3_0 %>%
    select(ID, Treatment_B, Treatment_C, Q30_2N, Q41_1N, Q41_2N, Q45_1N, Q45_2N, Q46_1N, Q46_2N, Pricelevel, Q52B)%>%
    # Opposer Y/N
    mutate(Opposer = ifelse(Q46_1N < 3,1,0))%>%
    pivot_longer(Q41_1N:Q46_2N, names_to = "Variable", values_to = "value")%>%
    mutate(Period  = ifelse(Variable %in% c("Q41_1N", "Q45_1N", "Q46_1N"),1,2),
           Outcome = ifelse(Variable %in% c("Q41_1N", "Q41_2N"), "Effectiveness", 
                            ifelse(Variable %in% c("Q45_1N", "Q45_2N"), "Fairness", "Support")))%>%
    mutate(Post_B = ifelse(Period == 2 & Treatment_B == "Treatment",1,0),
           Post_C = ifelse(Period == 1, "Baseline", as.character(Treatment_C)))%>%
    filter(Outcome == filter_1)%>%
    mutate(tau = ifelse(Q30_2N < 3,1,0))%>%
    mutate(tau_Post_B = ifelse(tau == 1 & Post_B == 1,1,0))%>%
    group_by(ID, Outcome)%>%
    mutate(NAS = sum(is.na(value)))%>%
    ungroup()%>%
    mutate(Pricelevel = factor(Pricelevel, levels = c("45", "85", "125")))%>%
    # Credibile
    mutate(Credible_B   = ifelse(Treatment_B == "Control" | Q52B %in% c("Ja", "Sí", "Oui", "Da"),1,0))%>%
    mutate(Post_B_Credible  = ifelse(Post_B == 1 & Credible_B == 1,1,0))%>%
    # Opposer Y/N
    mutate(Post_B_Opposer = ifelse(Post_B == 1 & Opposer == 1,1,0))%>%
    # FE
    mutate(Period_Opposer = paste0(Period,"_",Opposer),
           Post_C_Opposer = paste0(Post_C,"_",Opposer))
    
  # Correction for z-values
  z_values <- data_3_3 %>%
    filter(Period == 1)%>%
    summarise(mean_Pre = mean(value, na.rm = TRUE),
              sd_Pre   = sd(value, na.rm = TRUE))
  
  data_3_3 <- data_3_3 %>%
    mutate(value_z = (value - z_values$mean_Pre)/z_values$sd_Pre)
  
  return(data_3_3)
}

# Main estimation: Drop respondents with "I don't know" 
model_3.3.0_ESP <- feols(Support ~ Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_ESP, "Support"), NAS == 0), Support = value), cluster = ~ ID)
model_3.3.0_FRA <- feols(Support ~ Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_FRA, "Support"), NAS == 0), Support = value), cluster = ~ ID)
model_3.3.0_GER <- feols(Support ~ Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_GER, "Support"), NAS == 0), Support = value), cluster = ~ ID)
model_3.3.0_ROM <- feols(Support ~ Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_ROM, "Support"), NAS == 0), Support = value), cluster = ~ ID) 

model_3.3.1_ESP <- feols(Effectiveness ~ Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_ESP, "Effectiveness"), NAS == 0), Effectiveness = value), cluster = ~ ID)
model_3.3.1_FRA <- feols(Effectiveness ~ Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_FRA, "Effectiveness"), NAS == 0), Effectiveness = value), cluster = ~ ID)
model_3.3.1_GER <- feols(Effectiveness ~ Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_GER, "Effectiveness"), NAS == 0), Effectiveness = value), cluster = ~ ID)
model_3.3.1_ROM <- feols(Effectiveness ~ Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_ROM, "Effectiveness"), NAS == 0), Effectiveness = value), cluster = ~ ID) 

model_3.3.2_ESP <- feols(Fairness ~ Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_ESP, "Fairness"), NAS == 0), Fairness = value), cluster = ~ ID)
model_3.3.2_FRA <- feols(Fairness ~ Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_FRA, "Fairness"), NAS == 0), Fairness = value), cluster = ~ ID)
model_3.3.2_GER <- feols(Fairness ~ Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_GER, "Fairness"), NAS == 0), Fairness = value), cluster = ~ ID)
model_3.3.2_ROM <- feols(Fairness ~ Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_ROM, "Fairness"), NAS == 0), Fairness = value), cluster = ~ ID)

# Only those that find information credible
model_3.3.0_ESP_c <- feols(Support ~ Post_B | ID + Period + Post_C, data = filter(rename(filter(adjust_hypothesis_89(data_3_ESP, "Support"), NAS == 0), Support = value), Credible == 1), cluster = ~ ID)
model_3.3.0_FRA_c <- feols(Support ~ Post_B | ID + Period + Post_C, data = filter(rename(filter(adjust_hypothesis_89(data_3_FRA, "Support"), NAS == 0), Support = value), Credible == 1), cluster = ~ ID)
model_3.3.0_GER_c <- feols(Support ~ Post_B | ID + Period + Post_C, data = filter(rename(filter(adjust_hypothesis_89(data_3_GER, "Support"), NAS == 0), Support = value), Credible == 1), cluster = ~ ID)
model_3.3.0_ROM_c <- feols(Support ~ Post_B | ID + Period + Post_C, data = filter(rename(filter(adjust_hypothesis_89(data_3_ROM, "Support"), NAS == 0), Support = value), Credible == 1), cluster = ~ ID) 

model_3.3.1_ESP_c <- feols(Effectiveness ~ Post_B | ID + Period + Post_C, data = filter(rename(filter(adjust_hypothesis_89(data_3_ESP, "Effectiveness"), NAS == 0), Effectiveness = value), Credible == 1), cluster = ~ ID)
model_3.3.1_FRA_c <- feols(Effectiveness ~ Post_B | ID + Period + Post_C, data = filter(rename(filter(adjust_hypothesis_89(data_3_FRA, "Effectiveness"), NAS == 0), Effectiveness = value), Credible == 1), cluster = ~ ID)
model_3.3.1_GER_c <- feols(Effectiveness ~ Post_B | ID + Period + Post_C, data = filter(rename(filter(adjust_hypothesis_89(data_3_GER, "Effectiveness"), NAS == 0), Effectiveness = value), Credible == 1), cluster = ~ ID)
model_3.3.1_ROM_c <- feols(Effectiveness ~ Post_B | ID + Period + Post_C, data = filter(rename(filter(adjust_hypothesis_89(data_3_ROM, "Effectiveness"), NAS == 0), Effectiveness = value), Credible == 1), cluster = ~ ID)

model_3.3.2_ESP_c <- feols(Fairness ~ Post_B | ID + Period + Post_C, data = filter(rename(filter(adjust_hypothesis_89(data_3_ESP, "Fairness"), NAS == 0), Fairness = value), Credible == 1), cluster = ~ ID)
model_3.3.2_FRA_c <- feols(Fairness ~ Post_B | ID + Period + Post_C, data = filter(rename(filter(adjust_hypothesis_89(data_3_FRA, "Fairness"), NAS == 0), Fairness = value), Credible == 1), cluster = ~ ID)
model_3.3.2_GER_c <- feols(Fairness ~ Post_B | ID + Period + Post_C, data = filter(rename(filter(adjust_hypothesis_89(data_3_GER, "Fairness"), NAS == 0), Fairness = value), Credible == 1), cluster = ~ ID)
model_3.3.2_ROM_c <- feols(Fairness ~ Post_B | ID + Period + Post_C, data = filter(rename(filter(adjust_hypothesis_89(data_3_ROM, "Fairness"), NAS == 0), Fairness = value), Credible == 1), cluster = ~ ID)

# Export tables

etable(model_3.3.1_ESP, model_3.3.1_ESP_c,
       model_3.3.2_ESP, model_3.3.2_ESP_c,
       model_3.3.0_ESP, model_3.3.0_ESP_c,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment B on policy perception in Spain",
       label = "tab_A_1_ESP",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_Treament_B_ESP.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

etable(model_3.3.1_FRA, model_3.3.1_FRA_c,
       model_3.3.2_FRA, model_3.3.2_FRA_c,
       model_3.3.0_FRA, model_3.3.0_FRA_c,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment B on policy perception in France",
       label = "tab_A_1_FRA",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_Treament_B_FRA.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

etable(model_3.3.1_GER, model_3.3.1_GER_c,
       model_3.3.2_GER, model_3.3.2_GER_c,
       model_3.3.0_GER, model_3.3.0_GER_c,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment B on policy perception in Germany",
       label = "tab_A_1_GER",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_Treament_B_GER.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

etable(model_3.3.1_ROM, model_3.3.1_ROM_c,
       model_3.3.2_ROM, model_3.3.2_ROM_c,
       model_3.3.0_ROM, model_3.3.0_ROM_c,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment B on policy perception in Romania",
       label = "tab_A_1_ROM",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_Treament_B_ROM.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

# TBA

etable(model_3.3.1_ESP, model_3.3.1_FRA, model_3.3.1_GER, model_3.3.1_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment B on perception of effectiveness",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_H8",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H8.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

etable(model_3.3.2_ESP, model_3.3.2_FRA, model_3.3.2_GER, model_3.3.2_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment B on perception of fairness",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_H9",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H9.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

# Hypotheses 8a and 9a

model_3.3.3_ESP <- feols(Effectiveness ~ tau_Post_B | tau + Post_B + ID + Period + Post_C, data = filter(rename(filter(adjust_hypothesis_89(data_3_ESP, "Effectiveness"), NAS == 0), Effectiveness = value), !is.na(tau)))
model_3.3.3_FRA <- feols(Effectiveness ~ tau_Post_B | tau + Post_B + ID + Period + Post_C, data = filter(rename(filter(adjust_hypothesis_89(data_3_FRA, "Effectiveness"), NAS == 0), Effectiveness = value), !is.na(tau)))
model_3.3.3_GER <- feols(Effectiveness ~ tau_Post_B | tau + Post_B + ID + Period + Post_C, data = filter(rename(filter(adjust_hypothesis_89(data_3_GER, "Effectiveness"), NAS == 0), Effectiveness = value), !is.na(tau)))
model_3.3.3_ROM <- feols(Effectiveness ~ tau_Post_B | tau + Post_B + ID + Period + Post_C, data = filter(rename(filter(adjust_hypothesis_89(data_3_ROM, "Effectiveness"), NAS == 0), Effectiveness = value), !is.na(tau)))

model_3.3.4_ESP <- feols(Fairness ~ tau_Post_B | tau + Post_B + ID + Period + Post_C, data = filter(rename(filter(adjust_hypothesis_89(data_3_ESP, "Fairness"), NAS == 0), Fairness = value), !is.na(tau)))
model_3.3.4_FRA <- feols(Fairness ~ tau_Post_B | tau + Post_B + ID + Period + Post_C, data = filter(rename(filter(adjust_hypothesis_89(data_3_FRA, "Fairness"), NAS == 0), Fairness = value), !is.na(tau)))
model_3.3.4_GER <- feols(Fairness ~ tau_Post_B | tau + Post_B + ID + Period + Post_C, data = filter(rename(filter(adjust_hypothesis_89(data_3_GER, "Fairness"), NAS == 0), Fairness = value), !is.na(tau)))
model_3.3.4_ROM <- feols(Fairness ~ tau_Post_B | tau + Post_B + ID + Period + Post_C, data = filter(rename(filter(adjust_hypothesis_89(data_3_ROM, "Fairness"), NAS == 0), Fairness = value), !is.na(tau)))

# Export tables

etable(model_3.3.3_ESP, model_3.3.3_FRA, model_3.3.3_GER, model_3.3.3_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment B on perception of effectiveness on citizens with low trust in national government",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_H8A",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H8A.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

etable("Spain" = model_3.3.4_ESP, "France" = model_3.3.4_FRA, "Germany" = model_3.3.4_GER, "Romania" = model_3.3.4_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment B on perception of fairness on citizens with low trust national government",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_H9A",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H9A.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

# Treatment effects on knowing/not knowing
adjust_hypothesis_89_a <- function(data_3_0){
  data_3_3 <- data_3_0 %>%
    mutate(know_t_0_E = ifelse(is.na(Q41_1N),0,1),
           know_t_1_E = ifelse(is.na(Q41_2N),0,1),
           know_t_0_F = ifelse(is.na(Q45_1N),0,1),
           know_t_1_F = ifelse(is.na(Q45_2N),0,1),
           Treatment  = ifelse(Treatment_B == "Treatment",1,0))
}

model_3.3.5_ESP <- feglm(know_t_1_E ~ Treatment + know_t_0_E, family = binomial(link="logit"), data = adjust_hypothesis_89_a(data_3_ESP))
model_3.3.6_ESP <- feglm(know_t_1_F ~ Treatment + know_t_0_F, family = binomial(link="logit"), data = adjust_hypothesis_89_a(data_3_ESP))
model_3.3.5_FRA <- feglm(know_t_1_E ~ Treatment + know_t_0_E, family = binomial(link="logit"), data = adjust_hypothesis_89_a(data_3_FRA))
model_3.3.6_FRA <- feglm(know_t_1_F ~ Treatment + know_t_0_F, family = binomial(link="logit"), data = adjust_hypothesis_89_a(data_3_FRA))
model_3.3.5_GER <- feglm(know_t_1_E ~ Treatment + know_t_0_E, family = binomial(link="logit"), data = adjust_hypothesis_89_a(data_3_GER))
model_3.3.6_GER <- feglm(know_t_1_F ~ Treatment + know_t_0_F, family = binomial(link="logit"), data = adjust_hypothesis_89_a(data_3_GER))
model_3.3.5_ROM <- feglm(know_t_1_E ~ Treatment + know_t_0_E, family = binomial(link="logit"), data = adjust_hypothesis_89_a(data_3_ROM))
model_3.3.6_ROM <- feglm(know_t_1_F ~ Treatment + know_t_0_F, family = binomial(link="logit"), data = adjust_hypothesis_89_a(data_3_ROM))

# Tidy and figure
model_3.3.0_ESP_z <- feols(Support ~ Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_ESP, "Support"), NAS == 0), Support = value_z), cluster = ~ ID)
model_3.3.0_FRA_z <- feols(Support ~ Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_FRA, "Support"), NAS == 0), Support = value_z), cluster = ~ ID)
model_3.3.0_GER_z <- feols(Support ~ Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_GER, "Support"), NAS == 0), Support = value_z), cluster = ~ ID)
model_3.3.0_ROM_z <- feols(Support ~ Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_ROM, "Support"), NAS == 0), Support = value_z), cluster = ~ ID) 

model_3.3.1_ESP_z <- feols(Effectiveness ~ Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_ESP, "Effectiveness"), NAS == 0), Effectiveness = value_z), cluster = ~ ID)
model_3.3.1_FRA_z <- feols(Effectiveness ~ Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_FRA, "Effectiveness"), NAS == 0), Effectiveness = value_z), cluster = ~ ID)
model_3.3.1_GER_z <- feols(Effectiveness ~ Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_GER, "Effectiveness"), NAS == 0), Effectiveness = value_z), cluster = ~ ID)
model_3.3.1_ROM_z <- feols(Effectiveness ~ Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_ROM, "Effectiveness"), NAS == 0), Effectiveness = value_z), cluster = ~ ID) 

model_3.3.2_ESP_z <- feols(Fairness ~ Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_ESP, "Fairness"), NAS == 0), Fairness = value_z), cluster = ~ ID)
model_3.3.2_FRA_z <- feols(Fairness ~ Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_FRA, "Fairness"), NAS == 0), Fairness = value_z), cluster = ~ ID)
model_3.3.2_GER_z <- feols(Fairness ~ Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_GER, "Fairness"), NAS == 0), Fairness = value_z), cluster = ~ ID)
model_3.3.2_ROM_z <- feols(Fairness ~ Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_ROM, "Fairness"), NAS == 0), Fairness = value_z), cluster = ~ ID)

tidy_3.3.0_ESP <- tidy(model_3.3.0_ESP_z)%>% mutate(Country = "Spain", Outcome = "Support")
tidy_3.3.1_ESP <- tidy(model_3.3.1_ESP_z)%>% mutate(Country = "Spain", Outcome = "Effectiveness")
tidy_3.3.2_ESP <- tidy(model_3.3.2_ESP_z)%>% mutate(Country = "Spain", Outcome = "Fairness")
tidy_3.3.0_FRA <- tidy(model_3.3.0_FRA_z)%>% mutate(Country = "France", Outcome = "Support")
tidy_3.3.1_FRA <- tidy(model_3.3.1_FRA_z)%>% mutate(Country = "France", Outcome = "Effectiveness")
tidy_3.3.2_FRA <- tidy(model_3.3.2_FRA_z)%>% mutate(Country = "France", Outcome = "Fairness")
tidy_3.3.0_GER <- tidy(model_3.3.0_GER_z)%>% mutate(Country = "Germany", Outcome = "Support")
tidy_3.3.1_GER <- tidy(model_3.3.1_GER_z)%>% mutate(Country = "Germany", Outcome = "Effectiveness")
tidy_3.3.2_GER <- tidy(model_3.3.2_GER_z)%>% mutate(Country = "Germany", Outcome = "Fairness")
tidy_3.3.0_ROM <- tidy(model_3.3.0_ROM_z)%>% mutate(Country = "Romania", Outcome = "Support")
tidy_3.3.1_ROM <- tidy(model_3.3.1_ROM_z)%>% mutate(Country = "Romania", Outcome = "Effectiveness")
tidy_3.3.2_ROM <- tidy(model_3.3.2_ROM_z)%>% mutate(Country = "Romania", Outcome = "Fairness")

tidy_3.3 <- bind_rows(tidy_3.3.0_ESP, tidy_3.3.1_ESP, tidy_3.3.2_ESP,
                      tidy_3.3.0_FRA, tidy_3.3.1_FRA, tidy_3.3.2_FRA,
                      tidy_3.3.0_GER, tidy_3.3.1_GER, tidy_3.3.2_GER,
                      tidy_3.3.0_ROM, tidy_3.3.1_ROM, tidy_3.3.2_ROM)%>%
  mutate(ci_high = estimate + 1.96*std.error,
         ci_low  = estimate - 1.96*std.error)%>%
  mutate(VAR = case_when(Outcome == "Support" ~ "Policy support (Q46)",
                         Outcome == "Fairness" ~ "Perception of fairness (Q45)",
                         Outcome == "Effectiveness" ~ "Perception of effectiveness (Q41)"))%>%
  mutate(Country = factor(Country, levels = c("Romania","Germany", "France", "Spain")))

P_3.3 <- ggplot(tidy_3.3, aes(x = estimate, y = Country))+
  geom_vline(aes(xintercept = 0), linewidth = 0.25)+
  facet_wrap(. ~ VAR)+
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), linewidth = 0.25, width = 0.3)+
  geom_point(shape = 22, stroke = 0.3, size = 3, fill = "#3C5488FF")+
  expand_limits(x = c(-0.05,0.25))+
  theme_bw()+
  xlab("Effect of video information treatment")+
  theme(panel.grid.minor = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 7),
        panel.border = element_rect(color = "black", fill = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.2),
        axis.ticks = element_line(linewidth = 0.2),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.title  = element_text(size = 7))

pdf("../6_EUETS2_Citizens_Survey/1_Figures/Figure_2.pdf", width = 160/25.4, height = 50/25.4)
print(P_3.3)
dev.off()

# Split by price level
model_3.3.0_ESP_s <- feols(Support ~ Post_B | ID + Period + Post_C, split = ~ Pricelevel, data = rename(filter(adjust_hypothesis_89(data_3_ESP, "Support"), NAS == 0), Support = value), cluster = ~ ID)
model_3.3.0_FRA_s <- feols(Support ~ Post_B | ID + Period + Post_C, split = ~ Pricelevel, data = rename(filter(adjust_hypothesis_89(data_3_FRA, "Support"), NAS == 0), Support = value), cluster = ~ ID)
model_3.3.0_GER_s <- feols(Support ~ Post_B | ID + Period + Post_C, split = ~ Pricelevel, data = rename(filter(adjust_hypothesis_89(data_3_GER, "Support"), NAS == 0), Support = value), cluster = ~ ID)
model_3.3.0_ROM_s <- feols(Support ~ Post_B | ID + Period + Post_C, split = ~ Pricelevel, data = rename(filter(adjust_hypothesis_89(data_3_ROM, "Support"), NAS == 0), Support = value), cluster = ~ ID) 

model_3.3.1_ESP_s <- feols(Effectiveness ~ Post_B | ID + Period + Post_C, split = ~ Pricelevel, data = rename(filter(adjust_hypothesis_89(data_3_ESP, "Effectiveness"), NAS == 0), Effectiveness = value), cluster = ~ ID)
model_3.3.1_FRA_s <- feols(Effectiveness ~ Post_B | ID + Period + Post_C, split = ~ Pricelevel, data = rename(filter(adjust_hypothesis_89(data_3_FRA, "Effectiveness"), NAS == 0), Effectiveness = value), cluster = ~ ID)
model_3.3.1_GER_s <- feols(Effectiveness ~ Post_B | ID + Period + Post_C, split = ~ Pricelevel, data = rename(filter(adjust_hypothesis_89(data_3_GER, "Effectiveness"), NAS == 0), Effectiveness = value), cluster = ~ ID)
model_3.3.1_ROM_s <- feols(Effectiveness ~ Post_B | ID + Period + Post_C, split = ~ Pricelevel, data = rename(filter(adjust_hypothesis_89(data_3_ROM, "Effectiveness"), NAS == 0), Effectiveness = value), cluster = ~ ID)

model_3.3.2_ESP_s <- feols(Fairness ~ Post_B | ID + Period + Post_C, split = ~ Pricelevel, data = rename(filter(adjust_hypothesis_89(data_3_ESP, "Fairness"), NAS == 0), Fairness = value), cluster = ~ ID)
model_3.3.2_FRA_s <- feols(Fairness ~ Post_B | ID + Period + Post_C, split = ~ Pricelevel, data = rename(filter(adjust_hypothesis_89(data_3_FRA, "Fairness"), NAS == 0), Fairness = value), cluster = ~ ID)
model_3.3.2_GER_s <- feols(Fairness ~ Post_B | ID + Period + Post_C, split = ~ Pricelevel, data = rename(filter(adjust_hypothesis_89(data_3_GER, "Fairness"), NAS == 0), Fairness = value), cluster = ~ ID)
model_3.3.2_ROM_s <- feols(Fairness ~ Post_B | ID + Period + Post_C, split = ~ Pricelevel, data = rename(filter(adjust_hypothesis_89(data_3_ROM, "Fairness"), NAS == 0), Fairness = value), cluster = ~ ID)

# Effects on those finding treatment credible

model_3.3.0_ESP_c <- feols(Support ~ Post_B_Credible + Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_ESP, "Support"), NAS == 0), Support = value), cluster = ~ ID)
model_3.3.0_FRA_c <- feols(Support ~ Post_B_Credible + Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_FRA, "Support"), NAS == 0), Support = value), cluster = ~ ID)
model_3.3.0_GER_c <- feols(Support ~ Post_B_Credible + Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_GER, "Support"), NAS == 0), Support = value), cluster = ~ ID)
model_3.3.0_ROM_c <- feols(Support ~ Post_B_Credible + Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_ROM, "Support"), NAS == 0), Support = value), cluster = ~ ID)

model_3.3.1_ESP_c <- feols(Effectiveness ~ Post_B_Credible + Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_ESP, "Effectiveness"), NAS == 0), Effectiveness = value), cluster = ~ ID)
model_3.3.1_FRA_c <- feols(Effectiveness ~ Post_B_Credible + Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_FRA, "Effectiveness"), NAS == 0), Effectiveness = value), cluster = ~ ID)
model_3.3.1_GER_c <- feols(Effectiveness ~ Post_B_Credible + Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_GER, "Effectiveness"), NAS == 0), Effectiveness = value), cluster = ~ ID)
model_3.3.1_ROM_c <- feols(Effectiveness ~ Post_B_Credible + Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_ROM, "Effectiveness"), NAS == 0), Effectiveness = value), cluster = ~ ID)

model_3.3.2_ESP_c <- feols(Fairness ~ Post_B_Credible + Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_ESP, "Fairness"), NAS == 0), Fairness = value), cluster = ~ ID)
model_3.3.2_FRA_c <- feols(Fairness ~ Post_B_Credible + Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_FRA, "Fairness"), NAS == 0), Fairness = value), cluster = ~ ID)
model_3.3.2_GER_c <- feols(Fairness ~ Post_B_Credible + Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_GER, "Fairness"), NAS == 0), Fairness = value), cluster = ~ ID)
model_3.3.2_ROM_c <- feols(Fairness ~ Post_B_Credible + Post_B | ID + Period + Post_C, data = rename(filter(adjust_hypothesis_89(data_3_ROM, "Fairness"), NAS == 0), Fairness = value), cluster = ~ ID)

# Effects on intial opposers (with interaction)
model_3.3.0_ESP_o <- feols(Support ~ Post_B_Opposer + Post_B | ID + Period_Opposer + Post_C_Opposer, data = rename(filter(adjust_hypothesis_89(data_3_ESP, "Support"), NAS == 0), Support = value), cluster = ~ ID)
model_3.3.0_FRA_o <- feols(Support ~ Post_B_Opposer + Post_B | ID + Period_Opposer + Post_C_Opposer, data = rename(filter(adjust_hypothesis_89(data_3_FRA, "Support"), NAS == 0), Support = value), cluster = ~ ID)
model_3.3.0_GER_o <- feols(Support ~ Post_B_Opposer + Post_B | ID + Period_Opposer + Post_C_Opposer, data = rename(filter(adjust_hypothesis_89(data_3_GER, "Support"), NAS == 0), Support = value), cluster = ~ ID)
model_3.3.0_ROM_o <- feols(Support ~ Post_B_Opposer + Post_B | ID + Period_Opposer + Post_C_Opposer, data = rename(filter(adjust_hypothesis_89(data_3_ROM, "Support"), NAS == 0), Support = value), cluster = ~ ID)

model_3.3.1_ESP_o <- feols(Effectiveness ~ Post_B_Opposer + Post_B | ID + Period^Opposer + Post_C^Opposer, data = rename(filter(adjust_hypothesis_89(data_3_ESP, "Effectiveness"), NAS == 0), Effectiveness = value), cluster = ~ ID)
model_3.3.1_FRA_o <- feols(Effectiveness ~ Post_B_Opposer + Post_B | ID + Period^Opposer + Post_C^Opposer, data = rename(filter(adjust_hypothesis_89(data_3_FRA, "Effectiveness"), NAS == 0), Effectiveness = value), cluster = ~ ID)
model_3.3.1_GER_o <- feols(Effectiveness ~ Post_B_Opposer + Post_B | ID + Period^Opposer + Post_C^Opposer, data = rename(filter(adjust_hypothesis_89(data_3_GER, "Effectiveness"), NAS == 0), Effectiveness = value), cluster = ~ ID)
model_3.3.1_ROM_o <- feols(Effectiveness ~ Post_B_Opposer + Post_B | ID + Period^Opposer + Post_C^Opposer, data = rename(filter(adjust_hypothesis_89(data_3_ROM, "Effectiveness"), NAS == 0), Effectiveness = value), cluster = ~ ID)

model_3.3.2_ESP_o <- feols(Fairness ~ Post_B_Opposer + Post_B | ID + Period^Opposer + Post_C^Opposer, data = rename(filter(adjust_hypothesis_89(data_3_ESP, "Fairness"), NAS == 0), Fairness = value), cluster = ~ ID)
model_3.3.2_FRA_o <- feols(Fairness ~ Post_B_Opposer + Post_B | ID + Period^Opposer + Post_C^Opposer, data = rename(filter(adjust_hypothesis_89(data_3_FRA, "Fairness"), NAS == 0), Fairness = value), cluster = ~ ID)
model_3.3.2_GER_o <- feols(Fairness ~ Post_B_Opposer + Post_B | ID + Period^Opposer + Post_C^Opposer, data = rename(filter(adjust_hypothesis_89(data_3_GER, "Fairness"), NAS == 0), Fairness = value), cluster = ~ ID)
model_3.3.2_ROM_o <- feols(Fairness ~ Post_B_Opposer + Post_B | ID + Period^Opposer + Post_C^Opposer, data = rename(filter(adjust_hypothesis_89(data_3_ROM, "Fairness"), NAS == 0), Fairness = value), cluster = ~ ID)

# Exporting tables

etable(model_3.3.0_ESP_z, model_3.3.0_ESP, model_3.3.0_ESP_c, model_3.3.0_ESP_o,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE, 
       title = "Effects of video information treatment on overall policy support in Spain (Alternative)",
       headers = c("(I)", "(II)", "(III)", "(IV)"),
       label = "tab_B_ESP_1",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       interaction.combine = "\\times",
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_Treatment_B_ESP_Support.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from four OLS regressions of our video information treatment (B) on overall policy support among respondents from Spain.
                 Column (I) shows such results with the outcome standardized using z-scores based on average and SD of overall policy support in the first period. 
                 Column (II) shows such results with the non-standardized outcome (five-point Likert scale).
                 Column (III) shows effects on those that find the information not credible and on those that find it credible.
                 Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                 All estimations include respondent- and period-level fixed effects and controls for the cost information treatment C. Column (IV) includes additional interactions of fixed effects with whether respondents are opposers in the initial period. Therefore, these coefficients can be understood as a split-sample estimation.
                 Sample excludes respondents that answered 'I don't know' in either period.
                 Standard errors clustered at the level of respondents in parentheses.")))

etable(model_3.3.0_FRA_z, model_3.3.0_FRA, model_3.3.0_FRA_c, model_3.3.0_FRA_o,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE, 
       title = "Effects of video information treatment on overall policy support in France",
       headers = c("(I)", "(II)", "(III)", "(IV)"),
       label = "tab_B_FRA_1",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       interaction.combine = "\\times",
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_Treatment_B_FRA_Support.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from four OLS regressions of our video information treatment (B) on overall policy support among respondents from France.
                 Column (I) shows such results with the outcome standardized using z-scores based on average and SD of overall policy support in the first period. These values are shown in Figure \\ref{fig:2}.
                 Column (II) shows such results with the non-standardized outcome (five-point Likert scale).
                 Column (III) shows effects on those that find the information not credible and on those that find it credible.
                 Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                 All estimations include respondent- and period-level fixed effects and controls for the cost information treatment C. Column (IV) includes additional interactions of fixed effects with whether respondents are opposers in the initial period. Therefore, these coefficients can be understood as a split-sample estimation.
                 Sample excludes respondents that answered 'I don't know' in either period.
                 Standard errors clustered at the level of respondents in parentheses.")))

etable(model_3.3.0_GER_z, model_3.3.0_GER, model_3.3.0_GER_c, model_3.3.0_GER_o,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE, 
       title = "Effects of video information treatment on overall policy support in Germany",
       headers = c("(I)", "(II)", "(III)", "(IV)"),
       label = "tab_B_GER_1",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       interaction.combine = "\\times",
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_Treatment_B_GER_Support.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from four OLS regressions of our video information treatment (B) on overall policy support among respondents from Germany.
                 Column (I) shows such results with the outcome standardized using z-scores based on average and SD of overall policy support in the first period. These values are shown in Figure \\ref{fig:2}.
                 Column (II) shows such results with the non-standardized outcome (five-point Likert scale).
                 Column (III) shows effects on those that find the information not credible and on those that find it credible.
                 Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                 All estimations include respondent- and period-level fixed effects and controls for the cost information treatment C. Column (IV) includes additional interactions of fixed effects with whether respondents are opposers in the initial period. Therefore, these coefficients can be understood as a split-sample estimation.
                 Sample excludes respondents that answered 'I don't know' in either period.
                 Standard errors clustered at the level of respondents in parentheses.")))

etable(model_3.3.0_ROM_z, model_3.3.0_ROM, model_3.3.0_ROM_c, model_3.3.0_ROM_o,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE, 
       title = "Effects of video information treatment on overall policy support in Romania",
       headers = c("(I)", "(II)", "(III)", "(IV)"),
       label = "tab_B_ROM_1",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       interaction.combine = "\\times",
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_Treatment_B_ROM_Support.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from four OLS regressions of our video information treatment (B) on overall policy support among respondents from Romania.
                 Column (I) shows such results with the outcome standardized using z-scores based on average and SD of overall policy support in the first period. These values are shown in Figure \\ref{fig:2}.
                 Column (II) shows such results with the non-standardized outcome (five-point Likert scale).
                 Column (III) shows effects on those that find the information not credible and on those that find it credible.
                 Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                 All estimations include respondent- and period-level fixed effects and controls for the cost information treatment C. Column (IV) includes additional interactions of fixed effects with whether respondents are opposers in the initial period. Therefore, these coefficients can be understood as a split-sample estimation.
                 Sample excludes respondents that answered 'I don't know' in either period.
                 Standard errors clustered at the level of respondents in parentheses.")))

etable(model_3.3.1_ESP_z, model_3.3.1_ESP, model_3.3.1_ESP_c, model_3.3.1_ESP_o,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE, 
       title = "Effects of video information treatment on perception of effectiveness in Spain",
       headers = c("(I)", "(II)", "(III)", "(IV)"),
       label = "tab_B_ESP_2",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       interaction.combine = "\\times",
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_Treatment_B_ESP_Effectiveness.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from four OLS regressions of our video information treatment (B) on the perception of effectiveness among respondents from Spain.
                 Column (I) shows such results with the outcome standardized using z-scores based on average and SD of the perception of effectiveness in the first period. These values are shown in Figure \\ref{fig:2}.
                 Column (II) shows such results with the non-standardized outcome (four-point Likert scale).
                 Column (III) shows effects on those that find the information not credible and on those that find it credible.
                 Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                 All estimations include respondent- and period-level fixed effects and controls for the cost information treatment C. Column (IV) includes additional interactions of fixed effects with whether respondents are opposers in the initial period. Therefore, these coefficients can be understood as a split-sample estimation.
                 Sample excludes respondents that answered 'I don't know' in either period.
                 Standard errors clustered at the level of respondents in parentheses.")))

etable(model_3.3.1_FRA_z, model_3.3.1_FRA, model_3.3.1_FRA_c, model_3.3.1_FRA_o,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE, 
       title = "Effects of video information treatment on perception of effectiveness in France",
       headers = c("(I)", "(II)", "(III)", "(IV)"),
       label = "tab_B_FRA_2",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       interaction.combine = "\\times",
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_Treatment_B_FRA_Effectiveness.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from four OLS regressions of our video information treatment (B) on the perception of effectiveness among respondents from France.
                 Column (I) shows such results with the outcome standardized using z-scores based on average and SD of the perception of effectiveness in the first period. These values are shown in Figure \\ref{fig:2}.
                 Column (II) shows such results with the non-standardized outcome (four-point Likert scale).
                 Column (III) shows effects on those that find the information not credible and on those that find it credible.
                 Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                 All estimations include respondent- and period-level fixed effects and controls for the cost information treatment C. Column (IV) includes additional interactions of fixed effects with whether respondents are opposers in the initial period. Therefore, these coefficients can be understood as a split-sample estimation.
                 Sample excludes respondents that answered 'I don't know' in either period.
                 Standard errors clustered at the level of respondents in parentheses.")))

etable(model_3.3.1_GER_z, model_3.3.1_GER, model_3.3.1_GER_c, model_3.3.1_GER_o,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE, 
       title = "Effects of video information treatment on perception of effectiveness in Germany",
       headers = c("(I)", "(II)", "(III)", "(IV)"),
       label = "tab_B_GER_2",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       interaction.combine = "\\times",
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_Treatment_B_GER_Effectiveness.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from four OLS regressions of our video information treatment (B) on the perception of effectiveness among respondents from Germany.
                 Column (I) shows such results with the outcome standardized using z-scores based on average and SD of the perception of effectiveness in the first period. These values are shown in Figure \\ref{fig:2}.
                 Column (II) shows such results with the non-standardized outcome (four-point Likert scale).
                 Column (III) shows effects on those that find the information not credible and on those that find it credible.
                 Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                 All estimations include respondent- and period-level fixed effects and controls for the cost information treatment C. Column (IV) includes additional interactions of fixed effects with whether respondents are opposers in the initial period. Therefore, these coefficients can be understood as a split-sample estimation.
                 Sample excludes respondents that answered 'I don't know' in either period.
                 Standard errors clustered at the level of respondents in parentheses.")))

etable(model_3.3.1_ROM_z, model_3.3.1_ROM, model_3.3.1_ROM_c, model_3.3.1_ROM_o,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE, 
       title = "Effects of video information treatment on perception of effectiveness in Romania",
       headers = c("(I)", "(II)", "(III)", "(IV)"),
       label = "tab_B_ROM_2",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       interaction.combine = "\\times",
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_Treatment_B_ROM_Effectiveness.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from four OLS regressions of our video information treatment (B) on the perception of effectiveness among respondents from Romania.
                 Column (I) shows such results with the outcome standardized using z-scores based on average and SD of the perception of effectiveness in the first period. These values are shown in Figure \\ref{fig:2}.
                 Column (II) shows such results with the non-standardized outcome (four-point Likert scale).
                 Column (III) shows effects on those that find the information not credible and on those that find it credible.
                 Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                 All estimations include respondent- and period-level fixed effects and controls for the cost information treatment C. Column (IV) includes additional interactions of fixed effects with whether respondents are opposers in the initial period. Therefore, these coefficients can be understood as a split-sample estimation.
                 Sample excludes respondents that answered 'I don't know' in either period.
                 Standard errors clustered at the level of respondents in parentheses.")))

etable(model_3.3.2_ESP_z, model_3.3.2_ESP, model_3.3.2_ESP_c, model_3.3.2_ESP_o,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE, 
       title = "Effects of video information treatment on perception of fairness in Spain",
       headers = c("(I)", "(II)", "(III)", "(IV)"),
       label = "tab_B_ESP_3",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       interaction.combine = "\\times",
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_Treatment_B_ESP_Fairness.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from four OLS regressions of our video information treatment (B) on the perception of fairness among respondents from Spain.
                 Column (I) shows such results with the outcome standardized using z-scores based on average and SD of the perception of fairness in the first period. These values are shown in Figure \\ref{fig:2}.
                 Column (II) shows such results with the non-standardized outcome (three-point Likert scale).
                 Column (III) shows effects on those that find the information not credible and on those that find it credible.
                 Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                 All estimations include respondent- and period-level fixed effects and controls for the cost information treatment C. Column (IV) includes additional interactions of fixed effects with whether respondents are opposers in the initial period. Therefore, these coefficients can be understood as a split-sample estimation.
                 Sample excludes respondents that answered 'I don't know' in either period.
                 Standard errors clustered at the level of respondents in parentheses.")))

etable(model_3.3.2_FRA_z, model_3.3.2_FRA, model_3.3.2_FRA_c, model_3.3.2_FRA_o,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE, 
       title = "Effects of video information treatment on perception of fairness in France",
       headers = c("(I)", "(II)", "(III)", "(IV)"),
       label = "tab_B_FRA_3",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       interaction.combine = "\\times",
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_Treatment_B_FRA_Fairness.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from four OLS regressions of our video information treatment (B) on the perception of fairness among respondents from France.
                 Column (I) shows such results with the outcome standardized using z-scores based on average and SD of the perception of fairness in the first period. These values are shown in Figure \\ref{fig:2}.
                 Column (II) shows such results with the non-standardized outcome (three-point Likert scale).
                 Column (III) shows effects on those that find the information not credible and on those that find it credible.
                 Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                 All estimations include respondent- and period-level fixed effects and controls for the cost information treatment C. Column (IV) includes additional interactions of fixed effects with whether respondents are opposers in the initial period. Therefore, these coefficients can be understood as a split-sample estimation.
                 Sample excludes respondents that answered 'I don't know' in either period.
                 Standard errors clustered at the level of respondents in parentheses.")))

etable(model_3.3.2_GER_z, model_3.3.2_GER, model_3.3.2_GER_c, model_3.3.2_GER_o,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE, 
       title = "Effects of video information treatment on perception of fairness in Germany",
       headers = c("(I)", "(II)", "(III)", "(IV)"),
       label = "tab_B_GER_3",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       interaction.combine = "\\times",
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_Treatment_B_GER_Fairness.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from four OLS regressions of our video information treatment (B) on the perception of fairness among respondents from Germany.
                 Column (I) shows such results with the outcome standardized using z-scores based on average and SD of the perception of fairness in the first period. These values are shown in Figure \\ref{fig:2}.
                 Column (II) shows such results with the non-standardized outcome (three-point Likert scale).
                 Column (III) shows effects on those that find the information not credible and on those that find it credible.
                 Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                 All estimations include respondent- and period-level fixed effects and controls for the cost information treatment C. Column (IV) includes additional interactions of fixed effects with whether respondents are opposers in the initial period. Therefore, these coefficients can be understood as a split-sample estimation.
                 Sample excludes respondents that answered 'I don't know' in either period.
                 Standard errors clustered at the level of respondents in parentheses.")))

etable(model_3.3.2_ROM_z, model_3.3.2_ROM, model_3.3.2_ROM_c, model_3.3.2_ROM_o,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE, 
       title = "Effects of video information treatment on perception of fairness in Romania",
       headers = c("(I)", "(II)", "(III)", "(IV)"),
       label = "tab_B_ROM_3",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       interaction.combine = "\\times",
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_Treatment_B_ROM_Fairness.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from four OLS regressions of our video information treatment (B) on the perception of fairness among respondents from Romania.
                 Column (I) shows such results with the outcome standardized using z-scores based on average and SD of the perception of fairness in the first period. These values are shown in Figure \\ref{fig:2}.
                 Column (II) shows such results with the non-standardized outcome (three-point Likert scale).
                 Column (III) shows effects on those that find the information not credible and on those that find it credible.
                 Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                 All estimations include respondent- and period-level fixed effects and controls for the cost information treatment C. Column (IV) includes additional interactions of fixed effects with whether respondents are opposers in the initial period. Therefore, these coefficients can be understood as a split-sample estimation.
                 Sample excludes respondents that answered 'I don't know' in either period.
                 Standard errors clustered at the level of respondents in parentheses.")))

rm(model_3.3.1_ESP, model_3.3.1_FRA, model_3.3.1_GER, model_3.3.1_ROM, model_3.3.2_ESP, model_3.3.2_FRA, model_3.3.2_GER, model_3.3.2_ROM,
   model_3.3.3_ESP, model_3.3.3_FRA, model_3.3.3_GER, model_3.3.3_ROM, model_3.3.4_ESP, model_3.3.4_FRA, model_3.3.4_GER, model_3.3.4_ROM,
   model_3.3.1_ES_z, model_3.3.1_FRA_z, model_3.3.1_GER_z, model_3.3.1_ROM_z, model_3.3.2_ESP_z, model_3.3.2_FRA_z, model_3.3.2_GER_z, model_3.3.2_ROM_z,
   model_3.3.3_ES_z, model_3.3.3_FRA_z, model_3.3.3_GER_z, model_3.3.3_ROM_z, model_3.3.4_ESP_z, model_3.3.4_FRA_z, model_3.3.4_GER_z, model_3.3.4_ROM_z,
   model_3.3.1_ES_s, model_3.3.1_FRA_s, model_3.3.1_GER_s, model_3.3.1_ROM_s, model_3.3.2_ESP_s, model_3.3.2_FRA_s, model_3.3.2_GER_s, model_3.3.2_ROM_s,
   model_3.3.3_ES_s, model_3.3.3_FRA_s, model_3.3.3_GER_s, model_3.3.3_ROM_s, model_3.3.4_ESP_s, model_3.3.4_FRA_s, model_3.3.4_GER_s, model_3.3.4_ROM_s,
   model_3.3.1_ES_o, model_3.3.1_FRA_o, model_3.3.1_GER_o, model_3.3.1_ROM_o, model_3.3.2_ESP_o, model_3.3.2_FRA_o, model_3.3.2_GER_o, model_3.3.2_ROM_o,
   model_3.3.3_ES_o, model_3.3.3_FRA_o, model_3.3.3_GER_o, model_3.3.3_ROM_o,,
   tidy_3.3.0_ESP, tidy_3.3.1_ESP, tidy_3.3.2_ESP, tidy_3.3.0_FRA, tidy_3.3.1_FRA, tidy_3.3.2_FRA,
   tidy_3.3.0_GER, tidy_3.3.1_GER, tidy_3.3.2_GER, tidy_3.3.0_ROM, tidy_3.3.1_ROM, tidy_3.3.2_ROM)

# 3.4    Hypotheses 10 to 20 ####

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
    mutate(Post_C1_Over = ifelse(Post_C1 == 1 & Overestimated_Absolute == "Overestimated",1,0),
           Post_C2_Over = ifelse(Post_C2 == 1 & Overestimated_Absolute == "Overestimated",1,0),
           Post_C3_Over = ifelse(Post_C3 == 1 & Overestimated_Absolute == "Overestimated",1,0),
           Post_C4_Over = ifelse(Post_C4 == 1 & Overestimated_Absolute == "Overestimated",1,0))%>%
    filter(Outcome == filter_1)%>%
    # Transform into absolute value
    mutate(value_abs = abs(value))
  
  return(data_3_4)
}

# Hypothesis 10:

model_3.4.1_ESP <- feols(value_abs_1 ~ Post_C13 | ID + Period + Post_B + Post_C2, data = rename(adjust_hypothesis_10f(data_3_ESP, "Absolute_costs"), value_abs_1 = value_abs))
model_3.4.1_FRA <- feols(value_abs_1 ~ Post_C13 | ID + Period + Post_B + Post_C2, data = rename(adjust_hypothesis_10f(data_3_FRA, "Absolute_costs"), value_abs_1 = value_abs))
model_3.4.1_GER <- feols(value_abs_1 ~ Post_C13 | ID + Period + Post_B + Post_C2, data = rename(adjust_hypothesis_10f(data_3_GER, "Absolute_costs"), value_abs_1 = value_abs))
model_3.4.1_ROM <- feols(value_abs_1 ~ Post_C13 | ID + Period + Post_B + Post_C2, data = rename(adjust_hypothesis_10f(data_3_ROM, "Absolute_costs"), value_abs_1 = value_abs))

# Export tables (TBA)
etable(model_3.4.1_ESP, model_3.4.1_FRA, model_3.4.1_GER, model_3.4.1_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment C on perception of own additional costs",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_H10",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H10.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

model_3.4.1_ESP <- feols(value_abs_1 ~ Post_C1 + Post_C3 | ID + Period + Post_B + Post_C2, data = rename(adjust_hypothesis_10f(data_3_ESP, "Absolute_costs"), value_abs_1 = value_abs))
model_3.4.1_FRA <- feols(value_abs_1 ~ Post_C1 + Post_C3 | ID + Period + Post_B + Post_C2, data = rename(adjust_hypothesis_10f(data_3_FRA, "Absolute_costs"), value_abs_1 = value_abs))
model_3.4.1_GER <- feols(value_abs_1 ~ Post_C1 + Post_C3 | ID + Period + Post_B + Post_C2, data = rename(adjust_hypothesis_10f(data_3_GER, "Absolute_costs"), value_abs_1 = value_abs))
model_3.4.1_ROM <- feols(value_abs_1 ~ Post_C1 + Post_C3 | ID + Period + Post_B + Post_C2, data = rename(adjust_hypothesis_10f(data_3_ROM, "Absolute_costs"), value_abs_1 = value_abs))

etable(model_3.4.1_ESP, model_3.4.1_FRA, model_3.4.1_GER, model_3.4.1_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment C on perception of own additional costs",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_H10",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H10_dif.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

rm(model_3.4.1_ESP, model_3.4.1_FRA, model_3.4.1_GER, model_3.4.1_ROM)

# Hypothesis 11:

model_3.4.2_ESP <- feols(value_abs_2 ~ Post_C24 | ID + Period + Post_B + Post_C1, data = rename(adjust_hypothesis_10f(data_3_ESP, "Distribution_costs"), value_abs_2 = value_abs))
model_3.4.2_FRA <- feols(value_abs_2 ~ Post_C24 | ID + Period + Post_B + Post_C1, data = rename(adjust_hypothesis_10f(data_3_FRA, "Distribution_costs"), value_abs_2 = value_abs))
model_3.4.2_GER <- feols(value_abs_2 ~ Post_C24 | ID + Period + Post_B + Post_C1, data = rename(adjust_hypothesis_10f(data_3_GER, "Distribution_costs"), value_abs_2 = value_abs))
model_3.4.2_ROM <- feols(value_abs_2 ~ Post_C24 | ID + Period + Post_B + Post_C1, data = rename(adjust_hypothesis_10f(data_3_ROM, "Distribution_costs"), value_abs_2 = value_abs))

# Export tables (TBA)
etable(model_3.4.2_ESP, model_3.4.2_FRA, model_3.4.2_GER, model_3.4.2_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment C on perception of costs relative to others",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_H11",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H11.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

rm(model_3.4.2_ESP, model_3.4.2_FRA, model_3.4.2_GER, model_3.4.2_ROM)

# Hypothesis 12:

model_3.4.2_ESP_0 <- feols(Fairness ~ Post_C13 | ID + Period + Post_B + Post_C2 + Post_C4, data = rename(adjust_hypothesis_10f(data_3_ESP, "Fairness"), Fairness = value))
model_3.4.2_FRA_0 <- feols(Fairness ~ Post_C13 | ID + Period + Post_B + Post_C2 + Post_C4, data = rename(adjust_hypothesis_10f(data_3_FRA, "Fairness"), Fairness = value))
model_3.4.2_GER_0 <- feols(Fairness ~ Post_C13 | ID + Period + Post_B + Post_C2 + Post_C4, data = rename(adjust_hypothesis_10f(data_3_GER, "Fairness"), Fairness = value))
model_3.4.2_ROM_0 <- feols(Fairness ~ Post_C13 | ID + Period + Post_B + Post_C2 + Post_C4, data = rename(adjust_hypothesis_10f(data_3_ROM, "Fairness"), Fairness = value))

model_3.4.2_ESP_1 <- feols(Fairness ~ Post_C13 | ID + Period + Post_B + Post_C2 + Post_C4, data = rename(filter(adjust_hypothesis_10f(data_3_ESP, "Fairness"), Overestimated_Absolute == "Overestimated"), Fairness = value))
model_3.4.2_FRA_1 <- feols(Fairness ~ Post_C13 | ID + Period + Post_B + Post_C2 + Post_C4, data = rename(filter(adjust_hypothesis_10f(data_3_FRA, "Fairness"), Overestimated_Absolute == "Overestimated"), Fairness = value))
model_3.4.2_GER_1 <- feols(Fairness ~ Post_C13 | ID + Period + Post_B + Post_C2 + Post_C4, data = rename(filter(adjust_hypothesis_10f(data_3_GER, "Fairness"), Overestimated_Absolute == "Overestimated"), Fairness = value))
model_3.4.2_ROM_1 <- feols(Fairness ~ Post_C13 | ID + Period + Post_B + Post_C2 + Post_C4, data = rename(filter(adjust_hypothesis_10f(data_3_ROM, "Fairness"), Overestimated_Absolute == "Overestimated"), Fairness = value))

model_3.4.2_ESP_2 <- feols(Fairness ~ Post_C13 | ID + Period + Post_B + Post_C2 + Post_C4, data = rename(filter(adjust_hypothesis_10f(data_3_ESP, "Fairness"), Overestimated_Absolute == "Underestimated"), Fairness = value))
model_3.4.2_FRA_2 <- feols(Fairness ~ Post_C13 | ID + Period + Post_B + Post_C2 + Post_C4, data = rename(filter(adjust_hypothesis_10f(data_3_FRA, "Fairness"), Overestimated_Absolute == "Underestimated"), Fairness = value))
model_3.4.2_GER_2 <- feols(Fairness ~ Post_C13 | ID + Period + Post_B + Post_C2 + Post_C4, data = rename(filter(adjust_hypothesis_10f(data_3_GER, "Fairness"), Overestimated_Absolute == "Underestimated"), Fairness = value))
model_3.4.2_ROM_2 <- feols(Fairness ~ Post_C13 | ID + Period + Post_B + Post_C2 + Post_C4, data = rename(filter(adjust_hypothesis_10f(data_3_ROM, "Fairness"), Overestimated_Absolute == "Underestimated"), Fairness = value))

# Export tables (TBA)
etable(model_3.4.2_ESP_1, model_3.4.2_FRA_1, model_3.4.2_GER_1, model_3.4.2_ROM_1,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment C on perception of fairness among respondents that overestimated their additional costs",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_H12A",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H12A.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

etable(model_3.4.2_ESP_2, model_3.4.2_FRA_2, model_3.4.2_GER_2, model_3.4.2_ROM_2,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment C on perception of fairness among respondents that underrestimated their additional costs",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_H12B",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H12B.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

rm(model_3.4.2_ESP_1, model_3.4.2_FRA_1, model_3.4.2_GER_1, model_3.4.2_ROM_1,
   model_3.4.2_ESP_2, model_3.4.2_FRA_2, model_3.4.2_GER_2, model_3.4.2_ROM_2)

# Hypothesis 13:

model_3.4.3_ESP_1 <- feols(Fairness ~ Post_C234 | ID + Period + Post_B + Post_C1, data = rename(filter(adjust_hypothesis_10f(data_3_ESP, "Fairness"), Overestimated_Distribution == "Overestimated"), Fairness = value))
model_3.4.3_FRA_1 <- feols(Fairness ~ Post_C234 | ID + Period + Post_B + Post_C1, data = rename(filter(adjust_hypothesis_10f(data_3_FRA, "Fairness"), Overestimated_Distribution == "Overestimated"), Fairness = value))
model_3.4.3_GER_1 <- feols(Fairness ~ Post_C234 | ID + Period + Post_B + Post_C1, data = rename(filter(adjust_hypothesis_10f(data_3_GER, "Fairness"), Overestimated_Distribution == "Overestimated"), Fairness = value))
model_3.4.3_ROM_1 <- feols(Fairness ~ Post_C234 | ID + Period + Post_B + Post_C1, data = rename(filter(adjust_hypothesis_10f(data_3_ROM, "Fairness"), Overestimated_Distribution == "Overestimated"), Fairness = value))

model_3.4.3_ESP_2 <- feols(Fairness ~ Post_C234 | ID + Period + Post_B + Post_C1, data = rename(filter(adjust_hypothesis_10f(data_3_ESP, "Fairness"), Overestimated_Distribution == "Underestimated"), Fairness = value))
model_3.4.3_FRA_2 <- feols(Fairness ~ Post_C234 | ID + Period + Post_B + Post_C1, data = rename(filter(adjust_hypothesis_10f(data_3_FRA, "Fairness"), Overestimated_Distribution == "Underestimated"), Fairness = value))
model_3.4.3_GER_2 <- feols(Fairness ~ Post_C234 | ID + Period + Post_B + Post_C1, data = rename(filter(adjust_hypothesis_10f(data_3_GER, "Fairness"), Overestimated_Distribution == "Underestimated"), Fairness = value))
model_3.4.3_ROM_2 <- feols(Fairness ~ Post_C234 | ID + Period + Post_B + Post_C1, data = rename(filter(adjust_hypothesis_10f(data_3_ROM, "Fairness"), Overestimated_Distribution == "Underestimated"), Fairness = value))

# Export tables (TBA)
etable(model_3.4.3_ESP_1, model_3.4.3_FRA_1, model_3.4.3_GER_1, model_3.4.3_ROM_1,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment C on perception of fairness among respondents that overestimated their relative additional costs",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_H13A",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H13A.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

etable(model_3.4.3_ESP_2, model_3.4.3_FRA_2, model_3.4.3_GER_2, model_3.4.3_ROM_2,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment C on perception of fairness among respondents that underrestimated their relative additional costs",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_H13B",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H13B.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

rm(model_3.4.3_ESP_1, model_3.4.3_FRA_1, model_3.4.3_GER_1, model_3.4.3_ROM_1,
   model_3.4.3_ESP_2, model_3.4.3_FRA_2, model_3.4.3_GER_2, model_3.4.3_ROM_2)

# Hypothesis 14:
model_3.4.4_ESP <- feols(value ~ Post_C2 | ID + Period + Post_B + Post_C3 + Post_C4 + Post_C5, data = adjust_hypothesis_10f(data_3_ESP, "Fairness"))
model_3.4.4_FRA <- feols(value ~ Post_C2 | ID + Period + Post_B + Post_C3 + Post_C4 + Post_C5, data = adjust_hypothesis_10f(data_3_FRA, "Fairness"))
model_3.4.4_GER <- feols(value ~ Post_C2 | ID + Period + Post_B + Post_C3 + Post_C4 + Post_C5, data = adjust_hypothesis_10f(data_3_GER, "Fairness"))
model_3.4.4_ROM <- feols(value ~ Post_C2 | ID + Period + Post_B + Post_C3 + Post_C4 + Post_C5, data = adjust_hypothesis_10f(data_3_ROM, "Fairness"))

etable(model_3.4.4_ESP, model_3.4.4_FRA, model_3.4.4_GER, model_3.4.4_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment C2 in comparison to C1 on perception of fairness",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_H14",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H14.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

rm(model_3.4.4_ESP, model_3.4.4_FRA, model_3.4.4_GER, model_3.4.4_ROM)

# Hypothesis 15:

model_3.4.5_ESP <- feols(Effectiveness ~ Post_C3 + Post_C4 | ID + Period + Post_B + Post_C13 + Post_C24, data = rename(adjust_hypothesis_10f(data_3_ESP, "Effectiveness"), Effectiveness = value))
model_3.4.5_FRA <- feols(Effectiveness ~ Post_C3 + Post_C4 | ID + Period + Post_B + Post_C13 + Post_C24, data = rename(adjust_hypothesis_10f(data_3_FRA, "Effectiveness"), Effectiveness = value))
model_3.4.5_GER <- feols(Effectiveness ~ Post_C3 + Post_C4 | ID + Period + Post_B + Post_C13 + Post_C24, data = rename(adjust_hypothesis_10f(data_3_GER, "Effectiveness"), Effectiveness = value))
model_3.4.5_ROM <- feols(Effectiveness ~ Post_C3 + Post_C4 | ID + Period + Post_B + Post_C13 + Post_C24, data = rename(adjust_hypothesis_10f(data_3_ROM, "Effectiveness"), Effectiveness = value))

etable(model_3.4.5_ESP, model_3.4.5_FRA, model_3.4.5_GER, model_3.4.5_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment about drivers of additional costs on perception of effectiveness",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_H15",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H15.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

rm(model_3.4.5_ESP, model_3.4.5_FRA, model_3.4.5_GER, model_3.4.5_ROM)

# Hypothesis 16:

model_3.4.6_ESP <- feols(Fairness ~ Post_C3 + Post_C4 | ID + Period + Post_B + Post_C13 + Post_C24, data = rename(adjust_hypothesis_10f(data_3_ESP, "Fairness"), Fairness = value))
model_3.4.6_FRA <- feols(Fairness ~ Post_C3 + Post_C4 | ID + Period + Post_B + Post_C13 + Post_C24, data = rename(adjust_hypothesis_10f(data_3_FRA, "Fairness"), Fairness = value))
model_3.4.6_GER <- feols(Fairness ~ Post_C3 + Post_C4 | ID + Period + Post_B + Post_C13 + Post_C24, data = rename(adjust_hypothesis_10f(data_3_GER, "Fairness"), Fairness = value))
model_3.4.6_ROM <- feols(Fairness ~ Post_C3 + Post_C4 | ID + Period + Post_B + Post_C13 + Post_C24, data = rename(adjust_hypothesis_10f(data_3_ROM, "Fairness"), Fairness = value))

etable(model_3.4.6_ESP, model_3.4.6_FRA, model_3.4.6_GER, model_3.4.6_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment about drivers of additional costs on perception of fairness",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_H16",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H16.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

rm(model_3.4.6_ESP, model_3.4.6_FRA, model_3.4.6_GER, model_3.4.6_ROM)

# Hypothesis 17:

model_3.4.7_ESP <- feols(Vulnerable ~ Post_C4 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = rename(adjust_hypothesis_10f(data_3_ESP, "Vulnerable"), Vulnerable = value))
model_3.4.7_FRA <- feols(Vulnerable ~ Post_C4 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = rename(adjust_hypothesis_10f(data_3_FRA, "Vulnerable"), Vulnerable = value))
model_3.4.7_GER <- feols(Vulnerable ~ Post_C4 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = rename(adjust_hypothesis_10f(data_3_GER, "Vulnerable"), Vulnerable = value))
model_3.4.7_ROM <- feols(Vulnerable ~ Post_C4 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = rename(adjust_hypothesis_10f(data_3_ROM, "Vulnerable"), Vulnerable = value))

etable(model_3.4.7_ESP, model_3.4.7_FRA, model_3.4.7_GER, model_3.4.7_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment C4 on perception of policy hurting vulnerable households",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_H17",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H17.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

rm(model_3.4.7_ESP, model_3.4.7_FRA, model_3.4.7_GER, model_3.4.7_ROM)

# Hypothesis 18:

model_3.4.8_ESP <- feols(Fairness ~ Post_C3 + Post_C4 | ID + Period + Post_B + Post_C1 + Post_C2, data = rename(adjust_hypothesis_10f(data_3_ESP, "Fairness"), Fairness = value))
model_3.4.8_FRA <- feols(Fairness ~ Post_C3 + Post_C4 | ID + Period + Post_B + Post_C1 + Post_C2, data = rename(adjust_hypothesis_10f(data_3_FRA, "Fairness"), Fairness = value))
model_3.4.8_GER <- feols(Fairness ~ Post_C3 + Post_C4 | ID + Period + Post_B + Post_C1 + Post_C2, data = rename(adjust_hypothesis_10f(data_3_GER, "Fairness"), Fairness = value))
model_3.4.8_ROM <- feols(Fairness ~ Post_C3 + Post_C4 | ID + Period + Post_B + Post_C1 + Post_C2, data = rename(adjust_hypothesis_10f(data_3_ROM, "Fairness"), Fairness = value))

etable(model_3.4.8_ESP, model_3.4.8_FRA, model_3.4.8_GER, model_3.4.8_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments C3 and C4 on perception of fairness",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_H18",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H18.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

rm(model_3.4.8_ESP, model_3.4.8_FRA, model_3.4.8_GER, model_3.4.8_ROM)

# Hypothesis 19:
model_3.4.9_ESP <- feols(Fairness ~ Post_C4 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C5, data = rename(adjust_hypothesis_10f(data_3_ESP, "Fairness"), Fairness = value))
model_3.4.9_FRA <- feols(Fairness ~ Post_C4 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C5, data = rename(adjust_hypothesis_10f(data_3_FRA, "Fairness"), Fairness = value))
model_3.4.9_GER <- feols(Fairness ~ Post_C4 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C5, data = rename(adjust_hypothesis_10f(data_3_GER, "Fairness"), Fairness = value))
model_3.4.9_ROM <- feols(Fairness ~ Post_C4 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C5, data = rename(adjust_hypothesis_10f(data_3_ROM, "Fairness"), Fairness = value))

etable(model_3.4.9_ESP, model_3.4.9_FRA, model_3.4.9_GER, model_3.4.9_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment C4 in comparison to C3 on perception of fairness",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_H19",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H19.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

rm(model_3.4.9_ESP, model_3.4.9_FRA, model_3.4.9_GER, model_3.4.9_ROM)

# Hypothesis 20:
model_3.4.10_ESP <- feols(Fairness ~ Post_C4 | ID + Period + Post_B + Post_C1 + Post_C3 + Post_C5, data = rename(adjust_hypothesis_10f(data_3_ESP, "Fairness"), Fairness = value))
model_3.4.10_FRA <- feols(Fairness ~ Post_C4 | ID + Period + Post_B + Post_C1 + Post_C3 + Post_C5, data = rename(adjust_hypothesis_10f(data_3_FRA, "Fairness"), Fairness = value))
model_3.4.10_GER <- feols(Fairness ~ Post_C4 | ID + Period + Post_B + Post_C1 + Post_C3 + Post_C5, data = rename(adjust_hypothesis_10f(data_3_GER, "Fairness"), Fairness = value))
model_3.4.10_ROM <- feols(Fairness ~ Post_C4 | ID + Period + Post_B + Post_C1 + Post_C3 + Post_C5, data = rename(adjust_hypothesis_10f(data_3_ROM, "Fairness"), Fairness = value))

etable(model_3.4.10_ESP, model_3.4.10_FRA, model_3.4.10_GER, model_3.4.10_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment C4 in comparison to C2 on perception of fairness",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_H20",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H20.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

rm(model_3.4.10_ESP, model_3.4.10_FRA, model_3.4.10_GER, model_3.4.10_ROM)

# 3.5    Hypotheses 21 to 27 ####

adjust_hypothesis_21f <- function(data_3_0, filter_1){
  data_3_4 <- data_3_0 %>%
    select(ID, Treatment_B, Treatment_C, Dif_cost_1, Dif_cost_2, Dif_cost_1_ABS, Dif_cost_2_ABS, Dif_Percentile_1, Dif_Percentile_2, Q41_1N, Q41_2N, Q44_1N, Q44_2N, Q45_1N, Q45_2N, Q46_1N, Q46_2N)%>%
    # Overestimated/underestimated
    mutate(Overestimated_Absolute = ifelse(Dif_cost_1 > 0, "Overestimated",
                                           ifelse(Dif_cost_1 < 0, "Underestimated", NA)))%>%
    mutate(Overestimated_Distribution = ifelse(Dif_Percentile_1 > 0, "Overestimated",
                                               ifelse(Dif_Percentile_1 < 0, "Underestimated", NA)))%>%
    pivot_longer(Dif_cost_1:Q46_2N, names_to = "Variable", values_to = "value")%>%
    mutate(Period  = ifelse(Variable %in% c("Q41_1N", "Q44_1N", "Q45_1N", "Q46_1N", "Dif_cost_1", "Dif_Percentile_1", "Dif_cost_1_ABS"),1,2),
           Outcome = case_when(Variable %in% c("Q41_1N", "Q41_2N") ~ "Effectiveness",
                               Variable %in% c("Q44_1N", "Q44_2N") ~ "Vulnerable",
                               Variable %in% c("Q45_1N", "Q45_2N") ~ "Fairness",
                               Variable %in% c("Q46_1N", "Q46_2N") ~ "Support",
                               Variable %in% c("Dif_Percentile_1", "Dif_Percentile_2") ~ "Distribution_costs",
                               Variable %in% c("Dif_cost_1", "Dif_cost_2")             ~ "Absolute_costs",
                               Variable %in% c("Dif_cost_1_ABS", "Dif_cost_2_ABS") ~ "Absolute_costs_ABS"))%>%
    mutate(Post_B   = ifelse(Period == 2 & Treatment_B == "Treatment",1,0),
           Post_C1  = ifelse(Period == 2 & Treatment_C == "C1",1,0),
           Post_C2  = ifelse(Period == 2 & Treatment_C == "C2",1,0),
           Post_C3  = ifelse(Period == 2 & Treatment_C == "C3",1,0),
           Post_C4  = ifelse(Period == 2 & Treatment_C == "C4",1,0),
           Post_C12 = ifelse(Period == 2 & (Treatment_C == "C1" | Treatment_C == "C2"),1,0),
           Post_C34 = ifelse(Period == 2 & (Treatment_C == "C3" | Treatment_C == "C4"),1,0),
           Post_C = ifelse(Period == 2 & Treatment_C != "Control",1,0))
  
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

model_3.5.1_a_ESP <- feols(Support ~ Effectiveness | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = rename(adjust_hypothesis_21f(data_3_ESP, "Effectiveness"), Effectiveness = value))
model_3.5.1_a_FRA <- feols(Support ~ Effectiveness | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = rename(adjust_hypothesis_21f(data_3_FRA, "Effectiveness"), Effectiveness = value))
model_3.5.1_a_GER <- feols(Support ~ Effectiveness | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = rename(adjust_hypothesis_21f(data_3_GER, "Effectiveness"), Effectiveness = value))
model_3.5.1_a_ROM <- feols(Support ~ Effectiveness | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = rename(adjust_hypothesis_21f(data_3_ROM, "Effectiveness"), Effectiveness = value))

model_3.5.1_a1_ESP <- feols(Support ~ Effectiveness | ID + Period + Post_B + Post_C, data = rename(adjust_hypothesis_21f(data_3_ESP, "Effectiveness"), Effectiveness = value))
model_3.5.1_a1_FRA <- feols(Support ~ Effectiveness | ID + Period + Post_B + Post_C, data = rename(adjust_hypothesis_21f(data_3_FRA, "Effectiveness"), Effectiveness = value))
model_3.5.1_a1_GER <- feols(Support ~ Effectiveness | ID + Period + Post_B + Post_C, data = rename(adjust_hypothesis_21f(data_3_GER, "Effectiveness"), Effectiveness = value))
model_3.5.1_a1_ROM <- feols(Support ~ Effectiveness | ID + Period + Post_B + Post_C, data = rename(adjust_hypothesis_21f(data_3_ROM, "Effectiveness"), Effectiveness = value))

etable(model_3.5.1_a1_ESP, model_3.5.1_a1_FRA, model_3.5.1_a1_GER, model_3.5.1_a1_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of changing the perception of effectiveness on policy support",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_H21a",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H21a.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from four OLS regressions of perceptions of effectiveness on overall policy support among respondents in four countries.
                 Respondents indicate overall policy support on a five-point Likert scale and their perception of effectiveness on a four-point Likert scale.
                 All estimations include respondent- and period-level fixed effects and control variables for the video information treatment B and the cost information treatment C.
                 Samples excludes respondents that answered 'I don't know' in either period on either policy support (Q_46) or perception of effectiveness (Q_41).
                        Standard errors clustered at the level of respondents in parentheses.")))

rm(model_3.5.1_a_ESP, model_3.5.1_a_FRA, model_3.5.1_a_GER, model_3.5.1_a_ROM,
   model_3.5.1_a1_ESP, model_3.5.1_a1_FRA, model_3.5.1_a1_GER, model_3.5.1_a1_ROM)

# Hypothesis 21b:

model_3.5.1_b_ESP <- feols(Support ~ Fairness | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = rename(adjust_hypothesis_21f(data_3_ESP, "Fairness"), Fairness = value))
model_3.5.1_b_FRA <- feols(Support ~ Fairness | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = rename(adjust_hypothesis_21f(data_3_FRA, "Fairness"), Fairness = value))
model_3.5.1_b_GER <- feols(Support ~ Fairness | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = rename(adjust_hypothesis_21f(data_3_GER, "Fairness"), Fairness = value))
model_3.5.1_b_ROM <- feols(Support ~ Fairness | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = rename(adjust_hypothesis_21f(data_3_ROM, "Fairness"), Fairness = value))

model_3.5.1_b1_ESP <- feols(Support ~ Fairness | ID + Period + Post_B + Post_C, data = rename(adjust_hypothesis_21f(data_3_ESP, "Fairness"), Fairness = value))
model_3.5.1_b1_FRA <- feols(Support ~ Fairness | ID + Period + Post_B + Post_C, data = rename(adjust_hypothesis_21f(data_3_FRA, "Fairness"), Fairness = value))
model_3.5.1_b1_GER <- feols(Support ~ Fairness | ID + Period + Post_B + Post_C, data = rename(adjust_hypothesis_21f(data_3_GER, "Fairness"), Fairness = value))
model_3.5.1_b1_ROM <- feols(Support ~ Fairness | ID + Period + Post_B + Post_C, data = rename(adjust_hypothesis_21f(data_3_ROM, "Fairness"), Fairness = value))

etable(model_3.5.1_b1_ESP, model_3.5.1_b1_FRA, model_3.5.1_b1_GER, model_3.5.1_b1_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of changing the perception of fairness on policy support",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_H21b",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H21b.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from four OLS regressions of perceptions of fairness on overall policy support among respondents in four countries.
                 Respondents indicate overall policy support on a five-point Likert scale and their perception of fairness on a three-point Likert scale.
                 All estimations include respondent- and period-level fixed effects and control variables for the video information treatment B and the cost information treatment C.
                 Samples excludes respondents that answered 'I don't know' in either period on either policy support (Q_46) or perception of fairness (Q_45).
                        Standard errors clustered at the level of respondents in parentheses.")))

rm(model_3.5.1_b_ESP, model_3.5.1_b_FRA, model_3.5.1_b_GER, model_3.5.1_b_ROM)

# Hypothesis 21c:

model_3.5.1_c_ESP <- feols(Support ~ Vulnerable | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = rename(adjust_hypothesis_21f(data_3_ESP, "Vulnerable"), Vulnerable = value))
model_3.5.1_c_FRA <- feols(Support ~ Vulnerable | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = rename(adjust_hypothesis_21f(data_3_FRA, "Vulnerable"), Vulnerable = value))
model_3.5.1_c_GER <- feols(Support ~ Vulnerable | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = rename(adjust_hypothesis_21f(data_3_GER, "Vulnerable"), Vulnerable = value))
model_3.5.1_c_ROM <- feols(Support ~ Vulnerable | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = rename(adjust_hypothesis_21f(data_3_ROM, "Vulnerable"), Vulnerable = value))

etable(model_3.5.1_c_ESP, model_3.5.1_c_FRA, model_3.5.1_c_GER, model_3.5.1_c_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of changing the perception of effects on vulnerable households on policy support",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_H21c",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H21c.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

rm(model_3.5.1_c_ESP, model_3.5.1_c_FRA, model_3.5.1_c_GER, model_3.5.1_c_ROM)

# Hypothesis 21d:

model_3.5.1_d_ESP <- feols(Support ~ Absolute_costs | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = rename(adjust_hypothesis_21f(data_3_ESP, "Absolute_costs"), Absolute_costs = value))
model_3.5.1_d_FRA <- feols(Support ~ Absolute_costs | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = rename(adjust_hypothesis_21f(data_3_FRA, "Absolute_costs"), Absolute_costs = value))
model_3.5.1_d_GER <- feols(Support ~ Absolute_costs | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = rename(adjust_hypothesis_21f(data_3_GER, "Absolute_costs"), Absolute_costs = value))
model_3.5.1_d_ROM <- feols(Support ~ Absolute_costs | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = rename(adjust_hypothesis_21f(data_3_ROM, "Absolute_costs"), Absolute_costs = value))

etable(model_3.5.1_d_ESP, model_3.5.1_d_FRA, model_3.5.1_d_GER, model_3.5.1_d_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of changing the perception of absolute additional costs on policy support",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_H21d",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H21d.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

rm(model_3.5.1_d_ESP, model_3.5.1_d_FRA, model_3.5.1_d_GER, model_3.5.1_d_ROM)

# Hypothesis 21e:

model_3.5.1_e_ESP <- feols(Support ~ Distribution_costs | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = rename(adjust_hypothesis_21f(data_3_ESP, "Distribution_costs"), Distribution_costs = value))
model_3.5.1_e_FRA <- feols(Support ~ Distribution_costs | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = rename(adjust_hypothesis_21f(data_3_FRA, "Distribution_costs"), Distribution_costs = value))
model_3.5.1_e_GER <- feols(Support ~ Distribution_costs | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = rename(adjust_hypothesis_21f(data_3_GER, "Distribution_costs"), Distribution_costs = value))
model_3.5.1_e_ROM <- feols(Support ~ Distribution_costs | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3, data = rename(adjust_hypothesis_21f(data_3_ROM, "Distribution_costs"), Distribution_costs = value))

etable(model_3.5.1_e_ESP, model_3.5.1_e_FRA, model_3.5.1_e_GER, model_3.5.1_e_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of changing the perception of relative additional costs on policy support",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_H21e",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H21e.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

rm(model_3.5.1_e_ESP, model_3.5.1_e_FRA, model_3.5.1_e_GER, model_3.5.1_e_ROM)

# Non-registered Hypothesis 21f:

model_3.5.1_f_ESP <- feols(Support ~ Absolute_costs_ABS | ID + Period + Post_B + Post_C, data = rename(adjust_hypothesis_21f(data_3_ESP, "Absolute_costs_ABS"), Absolute_costs_ABS = value))
model_3.5.1_f_FRA <- feols(Support ~ Absolute_costs_ABS | ID + Period + Post_B + Post_C, data = rename(adjust_hypothesis_21f(data_3_FRA, "Absolute_costs_ABS"), Absolute_costs_ABS = value))
model_3.5.1_f_GER <- feols(Support ~ Absolute_costs_ABS | ID + Period + Post_B + Post_C, data = rename(adjust_hypothesis_21f(data_3_GER, "Absolute_costs_ABS"), Absolute_costs_ABS = value))
model_3.5.1_f_ROM <- feols(Support ~ Absolute_costs_ABS | ID + Period + Post_B + Post_C, data = rename(adjust_hypothesis_21f(data_3_ROM, "Absolute_costs_ABS"), Absolute_costs_ABS = value))

etable(model_3.5.1_f_ESP, model_3.5.1_f_FRA, model_3.5.1_f_GER, model_3.5.1_f_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of changing the perception of absolute additional costs (error) on policy support",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_H21f",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H21f.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from four OLS regressions of error in the perception of additional costs on overall policy support among respondents in four countries.
                 Respondents indicate overall policy support on a five-point Likert scale and their perception of additional costs in seven brackets (Q_42). 
                 The error is the absolute difference between respondents' perception of additional costs and estimated additional costs for each respondent profile. Lower values indicate more accurate perceptions of additional costs.
                 All estimations include respondent- and period-level fixed effects and control variables for the video information treatment B and the cost information treatment C.
                 Samples excludes respondents that answered 'I don't know' in either period on either policy support (Q_46) or perception of additional costs (Q_42).
                        Standard errors clustered at the level of respondents in parentheses.")))

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

etable(model_3.5.2_ESP, model_3.5.2_FRA, model_3.5.2_GER, model_3.5.2_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of changing the perception of policy in different dimensions on policy support",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_H22",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H22.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

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

model_3.5.3_ESP <- feols(Support ~ Post_P1 + Post_P2 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = rename(adjust_hypothesis_23(data_3_ESP), Support = value))
model_3.5.3_FRA <- feols(Support ~ Post_P1 + Post_P2 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = rename(adjust_hypothesis_23(data_3_FRA), Support = value))
model_3.5.3_GER <- feols(Support ~ Post_P1 + Post_P2 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = rename(adjust_hypothesis_23(data_3_GER), Support = value))
model_3.5.3_ROM <- feols(Support ~ Post_P1 + Post_P2 | ID + Period + Post_B + Post_C1 + Post_C2 + Post_C3 + Post_C4, data = rename(adjust_hypothesis_23(data_3_ROM), Support = value))

etable(model_3.5.3_ESP, model_3.5.3_FRA, model_3.5.3_GER, model_3.5.3_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatment and higher price levels on policy support",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_H23",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_H23.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

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

# 3.6    Hypotheses 28 to 36 (Conjoint) ####

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

# 3.7    Hypotheses 37 to 41 (Part III) ####

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

# 3.8    Add-On Analyses ####

# 3.8.1  Clean effects of Treatment C ####

adjust_3.8.1 <- function(data_3_0, filter_1){
  data_3_8.1 <- data_3_0 %>%
    select(ID, Treatment_B, Treatment_C, Q30_2N, Q41_1N, Q41_2N, Q45_1N, Q45_2N, Q46_1N, Q46_2N, Pricelevel, Q52B, Q58,
           Dif_cost_1, Dif_Percentile_1, Country)%>%
    mutate(Opposer = ifelse(Q46_1N < 3,1,0))%>%
    pivot_longer(Q41_1N:Q46_2N, names_to = "Variable", values_to = "value")%>%
    mutate(Period  = ifelse(Variable %in% c("Q41_1N", "Q45_1N", "Q46_1N"),1,2),
           Outcome = ifelse(Variable %in% c("Q41_1N", "Q41_2N"), "Effectiveness", 
                            ifelse(Variable %in% c("Q45_1N", "Q45_2N"), "Fairness", "Support")))%>%
    mutate(Post_B     = ifelse(Period == 2 & Treatment_B == "Treatment",1,0),
           Post_B_ONLY = ifelse(Period == 2 & Treatment_B == "Treatment" & Treatment_C == "Control",1,0),
           Post_C_ONLY = ifelse(Period == 2 & Treatment_B == "Control" & Treatment_C != "Control",1,0),
           Post_C1    = ifelse(Period == 2 & Treatment_C == "C1",1,0),
           Post_C2    = ifelse(Period == 2 & Treatment_C == "C2",1,0),
           Post_C3    = ifelse(Period == 2 & Treatment_C == "C3",1,0),
           Post_C4    = ifelse(Period == 2 & Treatment_C == "C4",1,0),
           Post_C12   = ifelse(Period == 2 & (Treatment_C == "C1" | Treatment_C == "C2"),1,0),
           Post_C34   = ifelse(Period == 2 & (Treatment_C == "C3" | Treatment_C == "C4"),1,0),
           Post_C1234 = ifelse(Period == 2 & Treatment_C != "Control",1,0))%>%
    # Interactions
    mutate(Post_B_C1 = ifelse(Period == 2 & Treatment_B == "Treatment" & Treatment_C == "C1",1,0),
           Post_B_C2 = ifelse(Period == 2 & Treatment_B == "Treatment" & Treatment_C == "C2",1,0),
           Post_B_C3 = ifelse(Period == 2 & Treatment_B == "Treatment" & Treatment_C == "C3",1,0),
           Post_B_C4 = ifelse(Period == 2 & Treatment_B == "Treatment" & Treatment_C == "C4",1,0),
           Post_B_C  = ifelse(Period == 2 & Treatment_B == "Treatment" & Treatment_C != "Control",1,0))%>%
    mutate(Group_BC = case_when(Treatment_B == "Control" & Treatment_C == "Control" ~ "None",
                             Treatment_B == "Treatment" & Treatment_C == "Control" ~ "B_Only",
                             Treatment_B == "Control" & Treatment_C == "C1" ~ "C1_Only",
                             Treatment_B == "Treatment" & Treatment_C == "C1" ~ "B_C1",
                             Treatment_B == "Control" & Treatment_C == "C2" ~ "C2_Only",
                             Treatment_B == "Treatment" & Treatment_C == "C2" ~ "B_C2",
                             Treatment_B == "Control" & Treatment_C == "C3" ~ "C3_Only",
                             Treatment_B == "Treatment" & Treatment_C == "C3" ~ "B_C3",
                             Treatment_B == "Control" & Treatment_C == "C4" ~ "C4_Only",
                             Treatment_B == "Treatment" & Treatment_C == "C4" ~ "B_C4",
                             TRUE ~ NA))%>%
    mutate(Overestimated_Absolute = ifelse(Dif_cost_1 > 0, "Overestimated",
                                           ifelse(Dif_cost_1 < 0, "Underestimated", NA)))%>%
    mutate(Overestimated_Distribution = ifelse(Dif_Percentile_1 > 0, "Overestimated",
                                               ifelse(Dif_Percentile_1 < 0, "Underestimated", NA)))%>%
    # C1 - overestimated/underestimated absolute
    mutate(C1_overestimated  = ifelse(Treatment_C == "C1" & Overestimated_Absolute == "Overestimated" & !is.na(Overestimated_Absolute),1,0),
           C1_underestimated = ifelse(Treatment_C == "C1" & Overestimated_Absolute == "Underestimated" & !is.na(Overestimated_Absolute),1,0))%>%
    # C2 - overestimated/underestimated relative
    mutate(C2_overestimated  = ifelse(Treatment_C == "C2" & Overestimated_Distribution == "Overestimated" & !is.na(Overestimated_Distribution),1,0),
           C2_underestimated = ifelse(Treatment_C == "C2" & Overestimated_Distribution == "Underestimated" & !is.na(Overestimated_Distribution),1,0))%>%
    # C3 - overestimated/underestimated absolute and relative
    mutate(C3_overestimated  = ifelse(Treatment_C == "C3" & Overestimated_Absolute == "Overestimated" & !is.na(Overestimated_Absolute),1,0),
           C3_underestimated = ifelse(Treatment_C == "C3" & Overestimated_Absolute == "Underestimated" & !is.na(Overestimated_Absolute),1,0))%>%
    mutate(C3_overestimated_dist  = ifelse(Treatment_C == "C3" & Overestimated_Distribution == "Overestimated" & !is.na(Overestimated_Distribution),1,0),
           C3_underestimated_dist = ifelse(Treatment_C == "C3" & Overestimated_Distribution == "Underestimated" & !is.na(Overestimated_Distribution),1,0))%>%
    # C4 - overestimated/underestimated relative
    mutate(C4_overestimated  = ifelse(Treatment_C == "C4" & Overestimated_Distribution == "Overestimated" & !is.na(Overestimated_Distribution),1,0),
           C4_underestimated = ifelse(Treatment_C == "C4" & Overestimated_Distribution == "Underestimated" & !is.na(Overestimated_Distribution),1,0))%>%
    mutate(C_overestimated  = ifelse(C1_overestimated == 1 | C2_overestimated == 1 | C3_overestimated == 1 | C4_overestimated == 1,1,0))%>%
    mutate(C1_O_Post = ifelse(C1_overestimated == 1  & Period == 2,1,0),
           C1_U_Post = ifelse(C1_underestimated == 1 & Period == 2,1,0),
           C2_O_Post = ifelse(C2_overestimated == 1  & Period == 2,1,0),
           C2_U_Post = ifelse(C2_underestimated == 1 & Period == 2,1,0),
           C3_O_Post = ifelse(C3_overestimated == 1  & Period == 2,1,0),
           C3_U_Post = ifelse(C3_underestimated == 1 & Period == 2,1,0),
           C3_OR_Post = ifelse(C3_overestimated_dist == 1  & Period == 2,1,0),
           C3_UR_Post = ifelse(C3_underestimated_dist == 1 & Period == 2,1,0),
           C4_O_Post = ifelse(C4_overestimated == 1  & Period == 2,1,0),
           C4_U_Post = ifelse(C4_underestimated == 1 & Period == 2,1,0))%>%
    mutate(Post_C_ONLY_Over = ifelse(Period == 2 & Treatment_B == "Control" & Treatment_C != "Control" & C_overestimated == 1,1,0),
           Post_B_C_Over    = ifelse(Period == 2 & Treatment_B == "Treatment" & Treatment_C != "Control" & C_overestimated == 1,1,0))%>%
    # Clear / credible
    mutate(Post_0 = ifelse(Period == 2,1,0))%>%
    filter(Outcome == filter_1)%>%
    # mutate(tau = ifelse(Q30_2N < 3,1,0))%>%
    # mutate(tau_Post_B = ifelse(tau == 1 & Post_B == 1,1,0))%>%
    group_by(ID, Outcome)%>%
    mutate(NAS = sum(is.na(value)))%>%
    ungroup()%>%
    mutate(Pricelevel = factor(Pricelevel, levels = c("45", "85", "125")))%>%
    mutate(Credible_B = ifelse(Treatment_B == "Control" | Q52B %in% c("Ja", "Sí", "Oui", "Da"),1,0))%>%
    mutate(Credible_C = ifelse(Treatment_C == "Control" | Q58  %in% c("Ja", "Sí", "Oui", "Da"),1,0))%>%
    mutate(Post_B_Credible  = ifelse(Post_B == 1 & Credible_B == 1,1,0),
           Post_C1_Credible = ifelse(Post_C1 == 1 & Credible_C == 1,1,0),
           Post_C2_Credible = ifelse(Post_C2 == 1 & Credible_C == 1,1,0),
           Post_C3_Credible = ifelse(Post_C3 == 1 & Credible_C == 1,1,0),
           Post_C4_Credible = ifelse(Post_C4 == 1 & Credible_C == 1,1,0))%>%
    # Interaction Credible B/C
    mutate(Post_B_ONLY_Credible = ifelse(Post_B_ONLY == 1 & Credible_B == 1,1,0),
           Post_C_ONLY_Credible = ifelse(Post_C_ONLY == 1 & Credible_C == 1,1,0),
           Post_B_C_Credible    = ifelse(Post_B_C == 1 & Credible_B == 1 & Credible_C == 1,1,0))%>%
    # Interaction Opposer B/C
    mutate(Post_B_ONLY_Opposer = ifelse(Post_B_ONLY == 1 & Opposer == 1,1,0),
           Post_C_ONLY_Opposer = ifelse(Post_C_ONLY == 1 & Opposer == 1,1,0),
           Post_B_C_Opposer    = ifelse(Post_B_C == 1 & Opposer == 1,1,0))%>%
    # Price level
    mutate(Post_C_ONLY_45  = ifelse(Post_C_ONLY == 1 & Pricelevel == "45",1,0),
           Post_C_ONLY_85  = ifelse(Post_C_ONLY == 1 & Pricelevel == "85",1,0),
           Post_C_ONLY_125 = ifelse(Post_C_ONLY == 1 & Pricelevel == "125",1,0),
           Post_B_C_45     = ifelse(Post_B_C == 1 & Pricelevel == "45",1,0),
           Post_B_C_85     = ifelse(Post_B_C == 1 & Pricelevel == "85",1,0),
           Post_B_C_125    = ifelse(Post_B_C == 1 & Pricelevel == "125",1,0))
  
  # Correction for z-values
  z_values <- data_3_8.1 %>%
    filter(Period == 1)%>%
    summarise(mean_Pre = mean(value, na.rm = TRUE),
              sd_Pre   = sd(value, na.rm = TRUE))
  
  data_3_8.1 <- data_3_8.1 %>%
    mutate(value_z = (value - z_values$mean_Pre)/z_values$sd_Pre)
  
  return(data_3_8.1)
}

model_3.8.1.1_ESP <- feols(Support ~ Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B | ID + Period, data = rename(adjust_3.8.1(data_3_ESP, "Support"), Support = value), cluster = ~ ID)
model_3.8.1.1_FRA <- feols(Support ~ Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B | ID + Period, data = rename(adjust_3.8.1(data_3_FRA, "Support"), Support = value), cluster = ~ ID)
model_3.8.1.1_GER <- feols(Support ~ Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B | ID + Period, data = rename(adjust_3.8.1(data_3_GER, "Support"), Support = value), cluster = ~ ID)
model_3.8.1.1_ROM <- feols(Support ~ Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B | ID + Period, data = rename(adjust_3.8.1(data_3_ROM, "Support"), Support = value), cluster = ~ ID)

model_3.8.1.2_ESP <- feols(Fairness ~ Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B | ID + Period, data = rename(adjust_3.8.1(data_3_ESP, "Fairness"), Fairness = value), cluster = ~ ID)
model_3.8.1.2_FRA <- feols(Fairness ~ Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B | ID + Period, data = rename(adjust_3.8.1(data_3_FRA, "Fairness"), Fairness = value), cluster = ~ ID)
model_3.8.1.2_GER <- feols(Fairness ~ Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B | ID + Period, data = rename(adjust_3.8.1(data_3_GER, "Fairness"), Fairness = value), cluster = ~ ID)
model_3.8.1.2_ROM <- feols(Fairness ~ Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B | ID + Period, data = rename(adjust_3.8.1(data_3_ROM, "Fairness"), Fairness = value), cluster = ~ ID)

model_3.8.1.3_ESP <- feols(Effectiveness ~ Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B | ID + Period, data = rename(adjust_3.8.1(data_3_ESP, "Effectiveness"), Effectiveness = value), cluster = ~ ID)
model_3.8.1.3_FRA <- feols(Effectiveness ~ Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B | ID + Period, data = rename(adjust_3.8.1(data_3_FRA, "Effectiveness"), Effectiveness = value), cluster = ~ ID)
model_3.8.1.3_GER <- feols(Effectiveness ~ Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B | ID + Period, data = rename(adjust_3.8.1(data_3_GER, "Effectiveness"), Effectiveness = value), cluster = ~ ID)
model_3.8.1.3_ROM <- feols(Effectiveness ~ Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B | ID + Period, data = rename(adjust_3.8.1(data_3_ROM, "Effectiveness"), Effectiveness = value), cluster = ~ ID)

etable(model_3.8.1.1_ESP, model_3.8.1.1_FRA, model_3.8.1.1_GER, model_3.8.1.1_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments on policy support",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_Add_1",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_Add_1.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

etable(model_3.8.1.2_ESP, model_3.8.1.2_FRA, model_3.8.1.2_GER, model_3.8.1.2_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments on perception of fairness",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_Add_2",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_Add_2.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

etable(model_3.8.1.3_ESP, model_3.8.1.3_FRA, model_3.8.1.3_GER, model_3.8.1.3_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments on perception of effectiveness",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_Add_3",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_Add_3.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

# Interactions for better interpretation

# Comparison against seeing B - comparison to those seeing B! When seeing B and C, B outweighs C substantially!
# Adding C to B does not change support.
model_3.8.1.4_ESP <- feols(Support ~ i(Group_BC, Post_0, ref = "B_only") | ID + Period, data = rename(adjust_3.8.1(data_3_ESP, "Support"), Support = value), cluster = ~ ID)
model_3.8.1.4_FRA <- feols(Support ~ i(Group_BC, Post_0, ref = "B_only") | ID + Period, data = rename(adjust_3.8.1(data_3_FRA, "Support"), Support = value), cluster = ~ ID)
model_3.8.1.4_GER <- feols(Support ~ i(Group_BC, Post_0, ref = "B_only") | ID + Period, data = rename(adjust_3.8.1(data_3_GER, "Support"), Support = value), cluster = ~ ID)
model_3.8.1.4_ROM <- feols(Support ~ i(Group_BC, Post_0, ref = "B_only") | ID + Period, data = rename(adjust_3.8.1(data_3_ROM, "Support"), Support = value), cluster = ~ ID)

etable(model_3.8.1.4_ESP, model_3.8.1.4_FRA, model_3.8.1.4_GER, model_3.8.1.4_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments on policy support (B/C - Part 1)",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_Add_1_BC_1",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_Add_1_BC_1.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

# Comparison against seeing nothing
model_3.8.1.5_ESP <- feols(Support ~ Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B_C1 + Post_B_C2 + Post_B_C3 + Post_B_C4 | ID + Period, data = rename(adjust_3.8.1(data_3_ESP, "Support"), Support = value), cluster = ~ ID)
model_3.8.1.5_FRA <- feols(Support ~ Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B_C1 + Post_B_C2 + Post_B_C3 + Post_B_C4 | ID + Period, data = rename(adjust_3.8.1(data_3_FRA, "Support"), Support = value), cluster = ~ ID)
model_3.8.1.5_GER <- feols(Support ~ Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B_C1 + Post_B_C2 + Post_B_C3 + Post_B_C4 | ID + Period, data = rename(adjust_3.8.1(data_3_GER, "Support"), Support = value), cluster = ~ ID)
model_3.8.1.5_ROM <- feols(Support ~ Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B_C1 + Post_B_C2 + Post_B_C3 + Post_B_C4 | ID + Period, data = rename(adjust_3.8.1(data_3_ROM, "Support"), Support = value), cluster = ~ ID)

etable(model_3.8.1.5_ESP, model_3.8.1.5_FRA, model_3.8.1.5_GER, model_3.8.1.5_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments on policy support (B/C - Part 2)",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_Add_1_BC_2",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_Add_1_BC_2.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

# Comparing against first period - no period-FE

model_3.8.1.7_ESP   <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   ID, data = rename(adjust_3.8.1(data_3_ESP, "Support"), Support = value),   cluster = ~ ID)
model_3.8.1.7_ESP_z <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   ID, data = rename(adjust_3.8.1(data_3_ESP, "Support"), Support = value_z), cluster = ~ ID)
model_3.8.1.7_ESP_c <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Credible + Post_C_ONLY_Credible + Post_B_C_Credible | ID, data = rename(adjust_3.8.1(data_3_ESP, "Support"), Support = value), cluster = ~ ID)
model_3.8.1.7_ESP_o <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Opposer + Post_C_ONLY_Opposer + Post_B_C_Opposer |    ID, data = rename(adjust_3.8.1(data_3_ESP, "Support"), Support = value), cluster = ~ ID)
model_3.8.1.7_ESP_e <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_C_ONLY_Over + Post_B_C_Over |                                ID, data = rename(adjust_3.8.1(data_3_ESP, "Support"), Support = value), cluster = ~ ID)
model_3.8.1.7_ESP_p <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_C_ONLY_85 + Post_C_ONLY_125 + Post_B_C_85 + Post_B_C_125 |   ID, data = rename(adjust_3.8.1(data_3_ESP, "Support"), Support = value), cluster = ~ ID)

etable(model_3.8.1.7_ESP_z, model_3.8.1.7_ESP, model_3.8.1.7_ESP_c, model_3.8.1.7_ESP_e, model_3.8.1.7_ESP_p,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments B and C on policy support in Spain",
       headers = c("(I)", "(II)", "(III)", "(IV)", "(V)"),
       label = "tab_BC_ESP",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_BC_ESP.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from five OLS regressions of our video information treatment (B) and our cost information treatment (C) on overall policy support among respondents from Spain.
                        Column (I) shows such results with the outcome standardized using z-scores based on average and SD of overall policy support in the first period. These values are shown in Figures \\ref{fig:2}, \\ref{fig:3} and \\ref{fig:2:3}.
                        Column (II) shows such results with the non-standardized outcome (five-point Likert scale).
                        Column (III) shows effects on those that find the information not credible and on those that find it credible.
                        Column (IV) shows effects on those that have received cost information lower than expected and on those that have received cost information higher than expected.
                        Column (V) shows effects on those that have received cost information for different price levels (Baseline: 45€/tCO\\textsubscript{2}, 85€/tCO\\textsubscript{2}, 125€/tCO\\textsubscript{2}).
                        All estimations include a respondent-level fixed effect. Coefficients in column (IV) can be understood as a split-sample estimation between opposers and non-opposers.
                        Sample excludes respondents that answered 'I don't know' in either period.
                        Standard errors clustered at the level of respondents in parentheses.")))

model_3.8.1.7_FRA   <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   ID, data = rename(adjust_3.8.1(data_3_FRA, "Support"), Support = value),   cluster = ~ ID)
model_3.8.1.7_FRA_z <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   ID, data = rename(adjust_3.8.1(data_3_FRA, "Support"), Support = value_z), cluster = ~ ID)
model_3.8.1.7_FRA_c <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Credible + Post_C_ONLY_Credible + Post_B_C_Credible | ID, data = rename(adjust_3.8.1(data_3_FRA, "Support"), Support = value), cluster = ~ ID)
model_3.8.1.7_FRA_o <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Opposer + Post_C_ONLY_Opposer + Post_B_C_Opposer |    ID, data = rename(adjust_3.8.1(data_3_FRA, "Support"), Support = value), cluster = ~ ID)
model_3.8.1.7_FRA_e <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_C_ONLY_Over + Post_B_C_Over |                                ID, data = rename(adjust_3.8.1(data_3_FRA, "Support"), Support = value), cluster = ~ ID)
model_3.8.1.7_FRA_p <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_C_ONLY_85 + Post_C_ONLY_125 + Post_B_C_85 + Post_B_C_125 |   ID, data = rename(adjust_3.8.1(data_3_FRA, "Support"), Support = value), cluster = ~ ID)

etable(model_3.8.1.7_FRA_z, model_3.8.1.7_FRA, model_3.8.1.7_FRA_c, model_3.8.1.7_FRA_e, model_3.8.1.7_FRA_p,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments B and C on policy support in France",
       headers = c("(I)", "(II)", "(III)", "(IV)", "(V)"),
       label = "tab_BC_FRA",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_BC_FRA.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from five OLS regressions of our video information treatment (B) and our cost information treatment (C) on overall policy support among respondents from France.
                        Column (I) shows such results with the outcome standardized using z-scores based on average and SD of overall policy support in the first period. These values are shown in Figures \\ref{fig:2}, \\ref{fig:3} and \\ref{fig:2:3}.
                        Column (II) shows such results with the non-standardized outcome (five-point Likert scale).
                        Column (III) shows effects on those that find the information not credible and on those that find it credible.
                        Column (IV) shows effects on those that have received cost information lower than expected and on those that have received cost information higher than expected.
                        Column (V) shows effects on those that have received cost information for different price levels (Baseline: 45€/tCO\\textsubscript{2}, 85€/tCO\\textsubscript{2}, 125€/tCO\\textsubscript{2}).
                        All estimations include a respondent-level fixed effect. Coefficients in column (IV) can be understood as a split-sample estimation between opposers and non-opposers.
                        Sample excludes respondents that answered 'I don't know' in either period.
                        Standard errors clustered at the level of respondents in parentheses.")))

model_3.8.1.7_GER   <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   ID, data = rename(adjust_3.8.1(data_3_GER, "Support"), Support = value),   cluster = ~ ID)
model_3.8.1.7_GER_z <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   ID, data = rename(adjust_3.8.1(data_3_GER, "Support"), Support = value_z), cluster = ~ ID)
model_3.8.1.7_GER_c <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Credible + Post_C_ONLY_Credible + Post_B_C_Credible | ID, data = rename(adjust_3.8.1(data_3_GER, "Support"), Support = value), cluster = ~ ID)
model_3.8.1.7_GER_o <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Opposer + Post_C_ONLY_Opposer + Post_B_C_Opposer |    ID, data = rename(adjust_3.8.1(data_3_GER, "Support"), Support = value), cluster = ~ ID)
model_3.8.1.7_GER_e <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_C_ONLY_Over + Post_B_C_Over |                                ID, data = rename(adjust_3.8.1(data_3_GER, "Support"), Support = value), cluster = ~ ID)
model_3.8.1.7_GER_p <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_C_ONLY_85 + Post_C_ONLY_125 + Post_B_C_85 + Post_B_C_125 |   ID, data = rename(adjust_3.8.1(data_3_GER, "Support"), Support = value), cluster = ~ ID)

etable(model_3.8.1.7_GER_z, model_3.8.1.7_GER, model_3.8.1.7_GER_c, model_3.8.1.7_GER_e, model_3.8.1.7_GER_p,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments B and C on policy support in Germany",
       headers = c("(I)", "(II)", "(III)", "(IV)", "(V)"),
       label = "tab_BC_GER",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_BC_GER.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from five OLS regressions of our video information treatment (B) and our cost information treatment (C) on overall policy support among respondents from Germany.
                        Column (I) shows such results with the outcome standardized using z-scores based on average and SD of overall policy support in the first period. These values are shown in Figures \\ref{fig:2}, \\ref{fig:3} and \\ref{fig:2:3}.
                        Column (II) shows such results with the non-standardized outcome (five-point Likert scale).
                        Column (III) shows effects on those that find the information not credible and on those that find it credible.
                        Column (IV) shows effects on those that have received cost information lower than expected and on those that have received cost information higher than expected.
                        Column (V) shows effects on those that have received cost information for different price levels (Baseline: 45€/tCO\\textsubscript{2}, 85€/tCO\\textsubscript{2}, 125€/tCO\\textsubscript{2}).
                        All estimations include a respondent-level fixed effect. Coefficients in column (IV) can be understood as a split-sample estimation between opposers and non-opposers.
                        Sample excludes respondents that answered 'I don't know' in either period.
                        Standard errors clustered at the level of respondents in parentheses.")))

model_3.8.1.7_ROM   <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   ID, data = rename(adjust_3.8.1(data_3_ROM, "Support"), Support = value),   cluster = ~ ID)
model_3.8.1.7_ROM_z <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   ID, data = rename(adjust_3.8.1(data_3_ROM, "Support"), Support = value_z), cluster = ~ ID)
model_3.8.1.7_ROM_c <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Credible + Post_C_ONLY_Credible + Post_B_C_Credible | ID, data = rename(adjust_3.8.1(data_3_ROM, "Support"), Support = value), cluster = ~ ID)
model_3.8.1.7_ROM_o <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Opposer + Post_C_ONLY_Opposer + Post_B_C_Opposer |    ID, data = rename(adjust_3.8.1(data_3_ROM, "Support"), Support = value), cluster = ~ ID)
model_3.8.1.7_ROM_e <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_C_ONLY_Over + Post_B_C_Over |                                ID, data = rename(adjust_3.8.1(data_3_ROM, "Support"), Support = value), cluster = ~ ID)
model_3.8.1.7_ROM_p <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_C_ONLY_85 + Post_C_ONLY_125 + Post_B_C_85 + Post_B_C_125 |   ID, data = rename(adjust_3.8.1(data_3_ROM, "Support"), Support = value), cluster = ~ ID)

etable(model_3.8.1.7_ROM_z, model_3.8.1.7_ROM, model_3.8.1.7_ROM_c, model_3.8.1.7_ROM_e, model_3.8.1.7_ROM_p,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments B and C on policy support in Romania",
       headers = c("(I)", "(II)", "(III)", "(IV)", "(V)"),
       label = "tab_BC_ROM",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_BC_ROM.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from five OLS regressions of our video information treatment (B) and our cost information treatment (C) on overall policy support among respondents from Romania.
                        Column (I) shows such results with the outcome standardized using z-scores based on average and SD of overall policy support in the first period. These values are shown in Figures \\ref{fig:2}, \\ref{fig:3} and \\ref{fig:2:3}.
                        Column (II) shows such results with the non-standardized outcome (five-point Likert scale).
                        Column (III) shows effects on those that find the information not credible and on those that find it credible.
                        Column (IV) shows effects on those that have received cost information lower than expected and on those that have received cost information higher than expected.
                        Column (V) shows effects on those that have received cost information for different price levels (Baseline: 45€/tCO\\textsubscript{2}, 85€/tCO\\textsubscript{2}, 125€/tCO\\textsubscript{2}).
                        All estimations include a respondent-level fixed effect. Coefficients in column (IV) can be understood as a split-sample estimation between opposers and non-opposers.
                        Sample excludes respondents that answered 'I don't know' in either period.
                        Standard errors clustered at the level of respondents in parentheses.")))

# Fairness
model_3.8.1.8_ESP   <- feols(Fairness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   ID, data = rename(adjust_3.8.1(data_3_ESP, "Fairness"), Fairness = value),   cluster = ~ ID)
model_3.8.1.8_ESP_z <- feols(Fairness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   ID, data = rename(adjust_3.8.1(data_3_ESP, "Fairness"), Fairness = value_z), cluster = ~ ID)
model_3.8.1.8_ESP_c <- feols(Fairness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Credible + Post_C_ONLY_Credible + Post_B_C_Credible | ID, data = rename(adjust_3.8.1(data_3_ESP, "Fairness"), Fairness = value), cluster = ~ ID)
model_3.8.1.8_ESP_o <- feols(Fairness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Opposer + Post_C_ONLY_Opposer + Post_B_C_Opposer |    ID, data = rename(adjust_3.8.1(data_3_ESP, "Fairness"), Fairness = value), cluster = ~ ID)
model_3.8.1.8_ESP_e <- feols(Fairness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_C_ONLY_Over + Post_B_C_Over |                                ID, data = rename(adjust_3.8.1(data_3_ESP, "Fairness"), Fairness = value), cluster = ~ ID)

etable(model_3.8.1.8_ESP_z, model_3.8.1.8_ESP, model_3.8.1.8_ESP_c, model_3.8.1.8_ESP_o, model_3.8.1.8_ESP_e,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments B and C on perception of fairness in Spain",
       headers = c("(I)", "(II)", "(III)", "(IV)", "(V)"),
       label = "tab_BC_ESP_1",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_BC_ESP_1.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from five OLS regressions of our video information treatment (B) and our cost information treatment (C) on overall perception of fairness among respondents from Spain.
                        Column (I) shows such results with the outcome standardized using z-scores based on average and SD of overall perception of fairness in the first period. These values are shown in Figures \\ref{fig:2}, \\ref{fig:3} and \\ref{fig:2:3}.
                        Column (II) shows such results with the non-standardized outcome (three-point Likert scale).
                        Column (III) shows effects on those that find the information not credible and on those that find it credible.
                        Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                        Column (V) shows effects on those that have received cost information lower than expected and on those that have received cost information higher than expected.
                        All estimations include a respondent-level fixed effect. Coefficients in column (IV) can be understood as a split-sample estimation between opposers and non-opposers.
                        Sample excludes respondents that answered 'I don't know' in either period.
                        Standard errors clustered at the level of respondents in parentheses.")))

# Fairness
model_3.8.1.8_FRA   <- feols(Fairness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   ID, data = rename(adjust_3.8.1(data_3_FRA, "Fairness"), Fairness = value),   cluster = ~ ID)
model_3.8.1.8_FRA_z <- feols(Fairness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   ID, data = rename(adjust_3.8.1(data_3_FRA, "Fairness"), Fairness = value_z), cluster = ~ ID)
model_3.8.1.8_FRA_c <- feols(Fairness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Credible + Post_C_ONLY_Credible + Post_B_C_Credible | ID, data = rename(adjust_3.8.1(data_3_FRA, "Fairness"), Fairness = value), cluster = ~ ID)
model_3.8.1.8_FRA_o <- feols(Fairness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Opposer + Post_C_ONLY_Opposer + Post_B_C_Opposer |    ID, data = rename(adjust_3.8.1(data_3_FRA, "Fairness"), Fairness = value), cluster = ~ ID)
model_3.8.1.8_FRA_e <- feols(Fairness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_C_ONLY_Over + Post_B_C_Over |                                ID, data = rename(adjust_3.8.1(data_3_FRA, "Fairness"), Fairness = value), cluster = ~ ID)

etable(model_3.8.1.8_FRA_z, model_3.8.1.8_FRA, model_3.8.1.8_FRA_c, model_3.8.1.8_FRA_o, model_3.8.1.8_FRA_e,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments B and C on perception of fairness in France",
       headers = c("(I)", "(II)", "(III)", "(IV)", "(V)"),
       label = "tab_BC_FRA_1",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_BC_FRA_1.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from five OLS regressions of our video information treatment (B) and our cost information treatment (C) on overall perception of fairness among respondents from France.
                        Column (I) shows such results with the outcome standardized using z-scores based on average and SD of overall perception of fairness in the first period. These values are shown in Figures \\ref{fig:2}, \\ref{fig:3} and \\ref{fig:2:3}.
                        Column (II) shows such results with the non-standardized outcome (three-point Likert scale).
                        Column (III) shows effects on those that find the information not credible and on those that find it credible.
                        Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                        Column (V) shows effects on those that have received cost information lower than expected and on those that have received cost information higher than expected.
                        All estimations include a respondent-level fixed effect. Coefficients in column (IV) can be understood as a split-sample estimation between opposers and non-opposers.
                        Sample excludes respondents that answered 'I don't know' in either period.
                        Standard errors clustered at the level of respondents in parentheses.")))

# Fairness
model_3.8.1.8_GER   <- feols(Fairness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   ID, data = rename(adjust_3.8.1(data_3_GER, "Fairness"), Fairness = value),   cluster = ~ ID)
model_3.8.1.8_GER_z <- feols(Fairness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   ID, data = rename(adjust_3.8.1(data_3_GER, "Fairness"), Fairness = value_z), cluster = ~ ID)
model_3.8.1.8_GER_c <- feols(Fairness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Credible + Post_C_ONLY_Credible + Post_B_C_Credible | ID, data = rename(adjust_3.8.1(data_3_GER, "Fairness"), Fairness = value), cluster = ~ ID)
model_3.8.1.8_GER_o <- feols(Fairness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Opposer + Post_C_ONLY_Opposer + Post_B_C_Opposer |    ID, data = rename(adjust_3.8.1(data_3_GER, "Fairness"), Fairness = value), cluster = ~ ID)
model_3.8.1.8_GER_e <- feols(Fairness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_C_ONLY_Over + Post_B_C_Over |                                ID, data = rename(adjust_3.8.1(data_3_GER, "Fairness"), Fairness = value), cluster = ~ ID)

etable(model_3.8.1.8_GER_z, model_3.8.1.8_GER, model_3.8.1.8_GER_c, model_3.8.1.8_GER_o, model_3.8.1.8_GER_e,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments B and C on perception of fairness in Germany",
       headers = c("(I)", "(II)", "(III)", "(IV)", "(V)"),
       label = "tab_BC_GER_1",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_BC_GER_1.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from five OLS regressions of our video information treatment (B) and our cost information treatment (C) on overall perception of fairness among respondents from Germany.
                        Column (I) shows such results with the outcome standardized using z-scores based on average and SD of overall perception of fairness in the first period. These values are shown in Figures \\ref{fig:2}, \\ref{fig:3} and \\ref{fig:2:3}.
                        Column (II) shows such results with the non-standardized outcome (three-point Likert scale).
                        Column (III) shows effects on those that find the information not credible and on those that find it credible.
                        Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                        Column (V) shows effects on those that have received cost information lower than expected and on those that have received cost information higher than expected.
                        All estimations include a respondent-level fixed effect. Coefficients in column (IV) can be understood as a split-sample estimation between opposers and non-opposers.
                        Sample excludes respondents that answered 'I don't know' in either period.
                        Standard errors clustered at the level of respondents in parentheses.")))

# Fairness
model_3.8.1.8_ROM   <- feols(Fairness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   ID, data = rename(adjust_3.8.1(data_3_ROM, "Fairness"), Fairness = value),   cluster = ~ ID)
model_3.8.1.8_ROM_z <- feols(Fairness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   ID, data = rename(adjust_3.8.1(data_3_ROM, "Fairness"), Fairness = value_z), cluster = ~ ID)
model_3.8.1.8_ROM_c <- feols(Fairness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Credible + Post_C_ONLY_Credible + Post_B_C_Credible | ID, data = rename(adjust_3.8.1(data_3_ROM, "Fairness"), Fairness = value), cluster = ~ ID)
model_3.8.1.8_ROM_o <- feols(Fairness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Opposer + Post_C_ONLY_Opposer + Post_B_C_Opposer |    ID, data = rename(adjust_3.8.1(data_3_ROM, "Fairness"), Fairness = value), cluster = ~ ID)
model_3.8.1.8_ROM_e <- feols(Fairness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_C_ONLY_Over + Post_B_C_Over |                                ID, data = rename(adjust_3.8.1(data_3_ROM, "Fairness"), Fairness = value), cluster = ~ ID)

etable(model_3.8.1.8_ROM_z, model_3.8.1.8_ROM, model_3.8.1.8_ROM_c, model_3.8.1.8_ROM_o, model_3.8.1.8_ROM_e,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments B and C on perception of fairness in Romania",
       headers = c("(I)", "(II)", "(III)", "(IV)", "(V)"),
       label = "tab_BC_ROM_1",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_BC_ROM_1.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from five OLS regressions of our video information treatment (B) and our cost information treatment (C) on overall perception of fairness among respondents from Romania
                        Column (I) shows such results with the outcome standardized using z-scores based on average and SD of overall perception of fairness in the first period. These values are shown in Figures \\ref{fig:2}, \\ref{fig:3} and \\ref{fig:2:3}.
                        Column (II) shows such results with the non-standardized outcome (three-point Likert scale).
                        Column (III) shows effects on those that find the information not credible and on those that find it credible.
                        Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                        Column (V) shows effects on those that have received cost information lower than expected and on those that have received cost information higher than expected.
                        All estimations include a respondent-level fixed effect. Coefficients in column (IV) can be understood as a split-sample estimation between opposers and non-opposers.
                        Sample excludes respondents that answered 'I don't know' in either period.
                        Standard errors clustered at the level of respondents in parentheses.")))

# Effectiveness
model_3.8.1.9_ESP   <- feols(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   ID, data = rename(adjust_3.8.1(data_3_ESP, "Effectiveness"), Effectiveness = value),   cluster = ~ ID)
model_3.8.1.9_ESP_z <- feols(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   ID, data = rename(adjust_3.8.1(data_3_ESP, "Effectiveness"), Effectiveness = value_z), cluster = ~ ID)
model_3.8.1.9_ESP_c <- feols(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Credible + Post_C_ONLY_Credible + Post_B_C_Credible | ID, data = rename(adjust_3.8.1(data_3_ESP, "Effectiveness"), Effectiveness = value), cluster = ~ ID)
model_3.8.1.9_ESP_o <- feols(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Opposer + Post_C_ONLY_Opposer + Post_B_C_Opposer |    ID, data = rename(adjust_3.8.1(data_3_ESP, "Effectiveness"), Effectiveness = value), cluster = ~ ID)
model_3.8.1.9_ESP_e <- feols(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_C_ONLY_Over + Post_B_C_Over |                                ID, data = rename(adjust_3.8.1(data_3_ESP, "Effectiveness"), Effectiveness = value), cluster = ~ ID)

etable(model_3.8.1.9_ESP_z, model_3.8.1.9_ESP, model_3.8.1.9_ESP_c, model_3.8.1.9_ESP_o, model_3.8.1.9_ESP_e,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments B and C on perception of effectiveness in Spain",
       headers = c("(I)", "(II)", "(III)", "(IV)", "(V)"),
       label = "tab_BC_ESP_2",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_BC_ESP_2.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from five OLS regressions of our video information treatment (B) and our cost information treatment (C) on overall perception of effectiveness among respondents from Spain.
                        Column (I) shows such results with the outcome standardized using z-scores based on average and SD of overall perception of fairness in the first period. These values are shown in Figures \\ref{fig:2}, \\ref{fig:3} and \\ref{fig:2:3}.
                        Column (II) shows such results with the non-standardized outcome (four-point Likert scale).
                        Column (III) shows effects on those that find the information not credible and on those that find it credible.
                        Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                        Column (V) shows effects on those that have received cost information lower than expected and on those that have received cost information higher than expected.
                        All estimations include a respondent-level fixed effect. Coefficients in column (IV) can be understood as a split-sample estimation between opposers and non-opposers.
                        Sample excludes respondents that answered 'I don't know' in either period.
                        Standard errors clustered at the level of respondents in parentheses.")))

# Effectiveness
model_3.8.1.9_FRA   <- feols(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   ID, data = rename(adjust_3.8.1(data_3_FRA, "Effectiveness"), Effectiveness = value),   cluster = ~ ID)
model_3.8.1.9_FRA_z <- feols(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   ID, data = rename(adjust_3.8.1(data_3_FRA, "Effectiveness"), Effectiveness = value_z), cluster = ~ ID)
model_3.8.1.9_FRA_c <- feols(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Credible + Post_C_ONLY_Credible + Post_B_C_Credible | ID, data = rename(adjust_3.8.1(data_3_FRA, "Effectiveness"), Effectiveness = value), cluster = ~ ID)
model_3.8.1.9_FRA_o <- feols(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Opposer + Post_C_ONLY_Opposer + Post_B_C_Opposer |    ID, data = rename(adjust_3.8.1(data_3_FRA, "Effectiveness"), Effectiveness = value), cluster = ~ ID)
model_3.8.1.9_FRA_e <- feols(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_C_ONLY_Over + Post_B_C_Over |                                ID, data = rename(adjust_3.8.1(data_3_FRA, "Effectiveness"), Effectiveness = value), cluster = ~ ID)

etable(model_3.8.1.9_FRA_z, model_3.8.1.9_FRA, model_3.8.1.9_FRA_c, model_3.8.1.9_FRA_o, model_3.8.1.9_FRA_e,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments B and C on perception of effectiveness in France",
       headers = c("(I)", "(II)", "(III)", "(IV)", "(V)"),
       label = "tab_BC_FRA_2",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_BC_FRA_2.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from five OLS regressions of our video information treatment (B) and our cost information treatment (C) on overall perception of effectiveness among respondents from France.
                        Column (I) shows such results with the outcome standardized using z-scores based on average and SD of overall perception of fairness in the first period. These values are shown in Figures \\ref{fig:2}, \\ref{fig:3} and \\ref{fig:2:3}. 
                        Column (II) shows such results with the non-standardized outcome (four-point Likert scale).
                        Column (III) shows effects on those that find the information not credible and on those that find it credible.
                        Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                        Column (V) shows effects on those that have received cost information lower than expected and on those that have received cost information higher than expected.
                        All estimations include a respondent-level fixed effect. Coefficients in column (IV) can be understood as a split-sample estimation between opposers and non-opposers.
                        Sample excludes respondents that answered 'I don't know' in either period.
                        Standard errors clustered at the level of respondents in parentheses.")))

# Effectiveness
model_3.8.1.9_GER   <- feols(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   ID, data = rename(adjust_3.8.1(data_3_GER, "Effectiveness"), Effectiveness = value),   cluster = ~ ID)
model_3.8.1.9_GER_z <- feols(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   ID, data = rename(adjust_3.8.1(data_3_GER, "Effectiveness"), Effectiveness = value_z), cluster = ~ ID)
model_3.8.1.9_GER_c <- feols(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Credible + Post_C_ONLY_Credible + Post_B_C_Credible | ID, data = rename(adjust_3.8.1(data_3_GER, "Effectiveness"), Effectiveness = value), cluster = ~ ID)
model_3.8.1.9_GER_o <- feols(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Opposer + Post_C_ONLY_Opposer + Post_B_C_Opposer |    ID, data = rename(adjust_3.8.1(data_3_GER, "Effectiveness"), Effectiveness = value), cluster = ~ ID)
model_3.8.1.9_GER_e <- feols(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_C_ONLY_Over + Post_B_C_Over |                                ID, data = rename(adjust_3.8.1(data_3_GER, "Effectiveness"), Effectiveness = value), cluster = ~ ID)

etable(model_3.8.1.9_GER_z, model_3.8.1.9_GER, model_3.8.1.9_GER_c, model_3.8.1.9_GER_o, model_3.8.1.9_GER_e,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments B and C on perception of effectiveness in Germany",
       headers = c("(I)", "(II)", "(III)", "(IV)", "(V)"),
       label = "tab_BC_GER_2",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_BC_GER_2.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from five OLS regressions of our video information treatment (B) and our cost information treatment (C) on overall perception of effectiveness among respondents from Germany.
                        Column (I) shows such results with the outcome standardized using z-scores based on average and SD of overall perception of fairness in the first period. These values are shown in Figures \\ref{fig:2}, \\ref{fig:3} and \\ref{fig:2:3}.
                        Column (II) shows such results with the non-standardized outcome (four-point Likert scale).
                        Column (III) shows effects on those that find the information not credible and on those that find it credible.
                        Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                        Column (V) shows effects on those that have received cost information lower than expected and on those that have received cost information higher than expected.
                        All estimations include a respondent-level fixed effect. Coefficients in column (IV) can be understood as a split-sample estimation between opposers and non-opposers.
                        Sample excludes respondents that answered 'I don't know' in either period.
                        Standard errors clustered at the level of respondents in parentheses.")))

# Effectiveness
model_3.8.1.9_ROM   <- feols(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   ID, data = rename(adjust_3.8.1(data_3_ROM, "Effectiveness"), Effectiveness = value),   cluster = ~ ID)
model_3.8.1.9_ROM_z <- feols(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   ID, data = rename(adjust_3.8.1(data_3_ROM, "Effectiveness"), Effectiveness = value_z), cluster = ~ ID)
model_3.8.1.9_ROM_c <- feols(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Credible + Post_C_ONLY_Credible + Post_B_C_Credible | ID, data = rename(adjust_3.8.1(data_3_ROM, "Effectiveness"), Effectiveness = value), cluster = ~ ID)
model_3.8.1.9_ROM_o <- feols(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Opposer + Post_C_ONLY_Opposer + Post_B_C_Opposer |    ID, data = rename(adjust_3.8.1(data_3_ROM, "Effectiveness"), Effectiveness = value), cluster = ~ ID)
model_3.8.1.9_ROM_e <- feols(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_C_ONLY_Over + Post_B_C_Over |                                ID, data = rename(adjust_3.8.1(data_3_ROM, "Effectiveness"), Effectiveness = value), cluster = ~ ID)

etable(model_3.8.1.9_ROM_z, model_3.8.1.9_ROM, model_3.8.1.9_ROM_c, model_3.8.1.9_ROM_o, model_3.8.1.9_ROM_e,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments B and C on perception of effectiveness in Romania",
       headers = c("(I)", "(II)", "(III)", "(IV)", "(V)"),
       label = "tab_BC_ROM_2",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_BC_ROM_2.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from five OLS regressions of our video information treatment (B) and our cost information treatment (C) on overall perception of effectiveness among respondents from Romania.
                        Column (I) shows such results with the outcome standardized using z-scores based on average and SD of overall perception of fairness in the first period. These values are shown in Figures \\ref{fig:2}, \\ref{fig:3} and \\ref{fig:2:3}.
                        Column (II) shows such results with the non-standardized outcome (four-point Likert scale).
                        Column (III) shows effects on those that find the information not credible and on those that find it credible.
                        Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                        Column (V) shows effects on those that have received cost information lower than expected and on those that have received cost information higher than expected.
                        All estimations include a respondent-level fixed effect. Coefficients in column (IV) can be understood as a split-sample estimation between opposers and non-opposers.
                        Sample excludes respondents that answered 'I don't know' in either period.
                        Standard errors clustered at the level of respondents in parentheses.")))

etable(model_3.8.1.7_ESP, model_3.8.1.7_FRA, model_3.8.1.7_GER, model_3.8.1.7_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments B and C on policy support",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_BC_1",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_BC_1.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

etable(model_3.8.1.8_ESP, model_3.8.1.8_FRA, model_3.8.1.8_GER, model_3.8.1.8_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments B and C on perception of fairness",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_BC_2",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_BC_2.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

etable(model_3.8.1.9_ESP, model_3.8.1.9_FRA, model_3.8.1.9_GER, model_3.8.1.9_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments B and C on perception of effectiveness",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_BC_3",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_BC_3.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

tidy_3.8.0_ESP <- tidy(model_3.8.1.7_ESP_z)%>% mutate(Country = "Spain",   Outcome = "Support")
tidy_3.8.0_FRA <- tidy(model_3.8.1.7_FRA_z)%>% mutate(Country = "France",  Outcome = "Support")
tidy_3.8.0_GER <- tidy(model_3.8.1.7_GER_z)%>% mutate(Country = "Germany", Outcome = "Support")
tidy_3.8.0_ROM <- tidy(model_3.8.1.7_ROM_z)%>% mutate(Country = "Romania", Outcome = "Support")

tidy_3.8.1_ESP <- tidy(model_3.8.1.8_ESP_z)%>% mutate(Country = "Spain",   Outcome = "Fairness")
tidy_3.8.1_FRA <- tidy(model_3.8.1.8_FRA_z)%>% mutate(Country = "France",  Outcome = "Fairness")
tidy_3.8.1_GER <- tidy(model_3.8.1.8_GER_z)%>% mutate(Country = "Germany", Outcome = "Fairness")
tidy_3.8.1_ROM <- tidy(model_3.8.1.8_ROM_z)%>% mutate(Country = "Romania", Outcome = "Fairness")

tidy_3.8.2_ESP <- tidy(model_3.8.1.9_ESP_z)%>% mutate(Country = "Spain",   Outcome = "Effectiveness")
tidy_3.8.2_FRA <- tidy(model_3.8.1.9_FRA_z)%>% mutate(Country = "France",  Outcome = "Effectiveness")
tidy_3.8.2_GER <- tidy(model_3.8.1.9_GER_z)%>% mutate(Country = "Germany", Outcome = "Effectiveness")
tidy_3.8.2_ROM <- tidy(model_3.8.1.9_ROM_z)%>% mutate(Country = "Romania", Outcome = "Effectiveness")

tidy_3.8.5_ESP <- tidy(model_3.8.5_ESP_z)%>% mutate(Country = "Spain",   Outcome = "Cost perception (error)")
tidy_3.8.5_FRA <- tidy(model_3.8.5_FRA_z)%>% mutate(Country = "France",  Outcome = "Cost perception (error)")
tidy_3.8.5_GER <- tidy(model_3.8.5_GER_z)%>% mutate(Country = "Germany", Outcome = "Cost perception (error)")
tidy_3.8.5_ROM <- tidy(model_3.8.5_ROM_z)%>% mutate(Country = "Romania", Outcome = "Cost perception (error)")

tidy_3.8.6_ESP <- tidy(model_3.8.5_1_ESP_z)%>% mutate(Country = "Spain",   Outcome = "Cost perception")
tidy_3.8.6_FRA <- tidy(model_3.8.5_1_FRA_z)%>% mutate(Country = "France",  Outcome = "Cost perception")
tidy_3.8.6_GER <- tidy(model_3.8.5_1_GER_z)%>% mutate(Country = "Germany", Outcome = "Cost perception")
tidy_3.8.6_ROM <- tidy(model_3.8.5_1_ROM_z)%>% mutate(Country = "Romania", Outcome = "Cost perception")

tidy_3.8 <- bind_rows(tidy_3.8.0_ESP, tidy_3.8.1_ESP, tidy_3.8.2_ESP, tidy_3.8.5_ESP, tidy_3.8.6_ESP, 
                      tidy_3.8.0_FRA, tidy_3.8.1_FRA, tidy_3.8.2_FRA, tidy_3.8.5_FRA, tidy_3.8.6_FRA,
                      tidy_3.8.0_GER, tidy_3.8.1_GER, tidy_3.8.2_GER, tidy_3.8.5_GER, tidy_3.8.6_GER,
                      tidy_3.8.0_ROM, tidy_3.8.1_ROM, tidy_3.8.2_ROM, tidy_3.8.5_ROM, tidy_3.8.6_ROM)%>%
  mutate(ci_high = estimate + 1.96*std.error,
         ci_low  = estimate - 1.96*std.error)%>%
  mutate(VAR = case_when(Outcome == "Support" ~ "Policy support",
                         Outcome == "Fairness" ~ "Perception of fairness",
                         Outcome == "Effectiveness" ~ "Perception of effectiveness",
                         Outcome == "Cost perception" ~ "Perception of costs",
                         Outcome == "Cost perception (error)" ~ "Perception of costs (error)"))%>%
  mutate(Country = factor(Country, levels = c("Romania","Germany", "France", "Spain")))

P_3.8.1 <- ggplot(filter(tidy_3.8, term == "Post_B_ONLY" & Outcome != "Perception of costs (error)" & Outcome != "Perception of costs"), aes(x = estimate, y = Country))+
  geom_vline(aes(xintercept = 0), linewidth = 0.25)+
  facet_wrap(. ~ VAR)+
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), linewidth = 0.25, width = 0.3)+
  geom_point(shape = 22, stroke = 0.3, size = 3, fill = "#3C5488FF")+
  expand_limits(x = c(-0.05,0.25))+
  theme_bw()+
  xlab("Effect of video information treatment")+
  theme(panel.grid.minor = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 7),
        panel.border = element_rect(color = "black", fill = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.2),
        axis.ticks = element_line(linewidth = 0.2),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.title  = element_text(size = 7))

P_3.8.2 <- ggplot(filter(tidy_3.8, term == "Post_C_ONLY" & Outcome != "Effectiveness" & Outcome != "Cost perception (error)"), aes(x = estimate, y = Country))+
  geom_vline(aes(xintercept = 0), linewidth = 0.25)+
  facet_wrap(. ~ VAR)+
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), linewidth = 0.25, width = 0.3)+
  geom_point(shape = 22, stroke = 0.3, size = 3, fill = "#3C5488FF")+
  expand_limits(x = c(-0.05,0.2))+
  theme_bw()+
  xlab("Effect of cost information treatment")+
  theme(panel.grid.minor = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 7),
        panel.border = element_rect(color = "black", fill = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.2),
        axis.ticks = element_line(linewidth = 0.2),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.title  = element_text(size = 7))

P_3.8.3 <- ggplot(filter(tidy_3.8, term == "Post_B_C" & Outcome != "Cost perception (error)"), aes(x = estimate, y = Country))+
  geom_vline(aes(xintercept = 0), linewidth = 0.25)+
  facet_wrap(. ~ VAR, nrow = 1)+
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), linewidth = 0.25, width = 0.3)+
  geom_point(shape = 22, stroke = 0.3, size = 3, fill = "#3C5488FF")+
  expand_limits(x = c(-0.05,0.2))+
  theme_bw()+
  xlab("Effect of video and cost information treatment")+
  theme(panel.grid.minor = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 7),
        panel.border = element_rect(color = "black", fill = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.2),
        axis.ticks = element_line(linewidth = 0.2),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.title  = element_text(size = 7))

pdf("../6_EUETS2_Citizens_Survey/1_Figures/Figure_2_1.pdf", width = 160/25.4, height = 50/25.4)
print(P_3.8.1)
dev.off()

pdf("../6_EUETS2_Citizens_Survey/1_Figures/Figure_2_2.pdf", width = 160/25.4, height = 50/25.4)
print(P_3.8.2)
dev.off()

pdf("../6_EUETS2_Citizens_Survey/1_Figures/Figure_2_3.pdf", width = 190/25.4, height = 50/25.4)
print(P_3.8.3)
dev.off()

# All countries together 

data_3.8.1_ESP_Effectiveness <- rename(adjust_3.8.1(data_3_ESP, "Effectiveness"), Effectiveness = value)
data_3.8.1_FRA_Effectiveness <- rename(adjust_3.8.1(data_3_FRA, "Effectiveness"), Effectiveness = value)
data_3.8.1_GER_Effectiveness <- rename(adjust_3.8.1(data_3_GER, "Effectiveness"), Effectiveness = value)
data_3.8.1_ROM_Effectiveness <- rename(adjust_3.8.1(data_3_ROM, "Effectiveness"), Effectiveness = value)

data_3.8.1_Effectiveness <- bind_rows(data_3.8.1_ESP_Effectiveness,
                                      data_3.8.1_FRA_Effectiveness,
                                      data_3.8.1_GER_Effectiveness,
                                      data_3.8.1_ROM_Effectiveness)%>%
  mutate(Country_ID = paste0(Country, "_", ID))

data_3.8.1_ESP_Effectiveness_z <- rename(adjust_3.8.1(data_3_ESP, "Effectiveness"), Effectiveness = value_z)
data_3.8.1_FRA_Effectiveness_z <- rename(adjust_3.8.1(data_3_FRA, "Effectiveness"), Effectiveness = value_z)
data_3.8.1_GER_Effectiveness_z <- rename(adjust_3.8.1(data_3_GER, "Effectiveness"), Effectiveness = value_z)
data_3.8.1_ROM_Effectiveness_z <- rename(adjust_3.8.1(data_3_ROM, "Effectiveness"), Effectiveness = value_z)

data_3.8.1_Effectiveness_z <- bind_rows(data_3.8.1_ESP_Effectiveness_z,
                                        data_3.8.1_FRA_Effectiveness_z,
                                        data_3.8.1_GER_Effectiveness_z,
                                        data_3.8.1_ROM_Effectiveness_z)%>%
  mutate(Country_ID = paste0(Country, "_", ID))

# Effectiveness
model_3.8.1_E   <- feols(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   Country_ID, data = data_3.8.1_Effectiveness,   cluster = ~ ID)
model_3.8.1_E_z <- feols(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   Country_ID, data = data_3.8.1_Effectiveness_z, cluster = ~ ID)
model_3.8.1_E_c <- feols(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Credible + Post_C_ONLY_Credible + Post_B_C_Credible | Country_ID, data = data_3.8.1_Effectiveness,   cluster = ~ ID)
model_3.8.1_E_o <- feols(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Opposer + Post_C_ONLY_Opposer + Post_B_C_Opposer |    Country_ID, data = data_3.8.1_Effectiveness,   cluster = ~ ID)
model_3.8.1_E_e <- feols(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_C_ONLY_Over + Post_B_C_Over |                                Country_ID, data = data_3.8.1_Effectiveness,   cluster = ~ ID)

etable(model_3.8.1_E_z, model_3.8.1_E, model_3.8.1_E_c, model_3.8.1_E_o, model_3.8.1_E_e,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments B and C on perception of effectiveness",
       headers = c("(I)", "(II)", "(III)", "(IV)", "(V)"),
       label = "tab_BC_ALL_2",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_BC_ALL_2.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from five OLS regressions of our video information treatment (B) and our cost information treatment (C) on overall perception of effectiveness among respondents from four countries.
                        Column (I) shows such results with the outcome standardized using z-scores based on average and SD of overall perception of fairness in the first period. These values are shown in Figures \\ref{fig:2}, \\ref{fig:3} and \\ref{fig:2:3}.
                        Column (II) shows such results with the non-standardized outcome (four-point Likert scale).
                        Column (III) shows effects on those that find the information not credible and on those that find it credible.
                        Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                        Column (V) shows effects on those that have received cost information lower than expected and on those that have received cost information higher than expected.
                        All estimations include a respondent-level fixed effect. Coefficients in column (IV) can be understood as a split-sample estimation between opposers and non-opposers.
                        Sample excludes respondents that answered 'I don't know' in either period.
                        Standard errors clustered at the level of respondents in parentheses.")))

data_3.8.1_ESP_Fairness <- rename(adjust_3.8.1(data_3_ESP, "Fairness"), Fairness = value)
data_3.8.1_FRA_Fairness <- rename(adjust_3.8.1(data_3_FRA, "Fairness"), Fairness = value)
data_3.8.1_GER_Fairness <- rename(adjust_3.8.1(data_3_GER, "Fairness"), Fairness = value)
data_3.8.1_ROM_Fairness <- rename(adjust_3.8.1(data_3_ROM, "Fairness"), Fairness = value)

data_3.8.1_Fairness <- bind_rows(data_3.8.1_ESP_Fairness,
                                 data_3.8.1_FRA_Fairness,
                                 data_3.8.1_GER_Fairness,
                                 data_3.8.1_ROM_Fairness)%>%
  mutate(Country_ID = paste0(Country, "_", ID))

data_3.8.1_ESP_Fairness_z <- rename(adjust_3.8.1(data_3_ESP, "Fairness"), Fairness = value_z)
data_3.8.1_FRA_Fairness_z <- rename(adjust_3.8.1(data_3_FRA, "Fairness"), Fairness = value_z)
data_3.8.1_GER_Fairness_z <- rename(adjust_3.8.1(data_3_GER, "Fairness"), Fairness = value_z)
data_3.8.1_ROM_Fairness_z <- rename(adjust_3.8.1(data_3_ROM, "Fairness"), Fairness = value_z)

data_3.8.1_Fairness_z <- bind_rows(data_3.8.1_ESP_Fairness_z,
                                   data_3.8.1_FRA_Fairness_z,
                                   data_3.8.1_GER_Fairness_z,
                                   data_3.8.1_ROM_Fairness_z)%>%
  mutate(Country_ID = paste0(Country, "_", ID))

# Fairness
model_3.8.1_F   <- feols(Fairness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   Country_ID, data = data_3.8.1_Fairness,   cluster = ~ ID)
model_3.8.1_F_z <- feols(Fairness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   Country_ID, data = data_3.8.1_Fairness_z, cluster = ~ ID)
model_3.8.1_F_c <- feols(Fairness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Credible + Post_C_ONLY_Credible + Post_B_C_Credible | Country_ID, data = data_3.8.1_Fairness,   cluster = ~ ID)
model_3.8.1_F_o <- feols(Fairness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Opposer + Post_C_ONLY_Opposer + Post_B_C_Opposer |    Country_ID + Country, data = data_3.8.1_Fairness,   cluster = ~ ID)
model_3.8.1_F_e <- feols(Fairness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_C_ONLY_Over + Post_B_C_Over |                                Country_ID, data = data_3.8.1_Fairness,   cluster = ~ ID)

etable(model_3.8.1_F_z, model_3.8.1_F, model_3.8.1_F_c, model_3.8.1_F_o, model_3.8.1_F_e,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments B and C on perception of fairness",
       headers = c("(I)", "(II)", "(III)", "(IV)", "(V)"),
       label = "tab_BC_ALL_1",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_BC_ALL_1.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from five OLS regressions of our video information treatment (B) and our cost information treatment (C) on overall perception of fairness among respondents from four countries.
                        Column (I) shows such results with the outcome standardized using z-scores based on average and SD of overall perception of fairness in the first period. These values are shown in Figures \\ref{fig:2}, \\ref{fig:3} and \\ref{fig:2:3}.
                        Column (II) shows such results with the non-standardized outcome (three-point Likert scale).
                        Column (III) shows effects on those that find the information not credible and on those that find it credible.
                        Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                        Column (V) shows effects on those that have received cost information lower than expected and on those that have received cost information higher than expected.
                        All estimations include a respondent-level fixed effect. Coefficients in column (IV) can be understood as a split-sample estimation between opposers and non-opposers.
                        Sample excludes respondents that answered 'I don't know' in either period.
                        Standard errors clustered at the level of respondents in parentheses.")))

data_3.8.1_ESP_Support <- rename(adjust_3.8.1(data_3_ESP, "Support"), Support = value)
data_3.8.1_FRA_Support <- rename(adjust_3.8.1(data_3_FRA, "Support"), Support = value)
data_3.8.1_GER_Support <- rename(adjust_3.8.1(data_3_GER, "Support"), Support = value)
data_3.8.1_ROM_Support <- rename(adjust_3.8.1(data_3_ROM, "Support"), Support = value)

data_3.8.1_Support <- bind_rows(data_3.8.1_ESP_Support,
                                data_3.8.1_FRA_Support,
                                data_3.8.1_GER_Support,
                                data_3.8.1_ROM_Support)%>%
  mutate(Country_ID = paste0(Country, "_", ID))

data_3.8.1_ESP_Support_z <- rename(adjust_3.8.1(data_3_ESP, "Support"), Support = value_z)
data_3.8.1_FRA_Support_z <- rename(adjust_3.8.1(data_3_FRA, "Support"), Support = value_z)
data_3.8.1_GER_Support_z <- rename(adjust_3.8.1(data_3_GER, "Support"), Support = value_z)
data_3.8.1_ROM_Support_z <- rename(adjust_3.8.1(data_3_ROM, "Support"), Support = value_z)

data_3.8.1_Support_z <- bind_rows(data_3.8.1_ESP_Support_z,
                                  data_3.8.1_FRA_Support_z,
                                  data_3.8.1_GER_Support_z,
                                  data_3.8.1_ROM_Support_z)%>%
  mutate(Country_ID = paste0(Country, "_", ID))

model_3.8.1_S   <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   Country_ID, data = data_3.8.1_Support,   cluster = ~ ID)
model_3.8.1_S_z <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C |                                                                   Country_ID, data = data_3.8.1_Support_z, cluster = ~ ID)
model_3.8.1_S_c <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Credible + Post_C_ONLY_Credible + Post_B_C_Credible | Country_ID, data = data_3.8.1_Support, cluster = ~ ID)
model_3.8.1_S_o <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_B_ONLY_Opposer + Post_C_ONLY_Opposer + Post_B_C_Opposer |    Country_ID + Country, data = data_3.8.1_Support, cluster = ~ ID)
model_3.8.1_S_e <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_C_ONLY_Over + Post_B_C_Over |                                Country_ID, data = data_3.8.1_Support, cluster = ~ ID)
model_3.8.1_S_p <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Post_C_ONLY_85 + Post_C_ONLY_125 + Post_B_C_85 + Post_B_C_125 |   Country_ID, data = data_3.8.1_Support, cluster = ~ ID)

etable(model_3.8.1_S_z, model_3.8.1_S, model_3.8.1_S_c, model_3.8.1_S_e, model_3.8.1_S_p,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments B and C on policy support",
       headers = c("(I)", "(II)", "(III)", "(IV)", "(V)"),
       label = "tab_BC_ALLP",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_BC_ALL.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from five OLS regressions of our video information treatment (B) and our cost information treatment (C) on overall policy support among respondents from four countries.
                        Column (I) shows such results with the outcome standardized using z-scores based on average and SD of overall policy support in the first period. These values are shown in Figures \\ref{fig:2}, \\ref{fig:3} and \\ref{fig:2:3}.
                        Column (II) shows such results with the non-standardized outcome (five-point Likert scale).
                        Column (III) shows effects on those that find the information not credible and on those that find it credible.
                        Column (IV) shows effects on those that have received cost information lower than expected and on those that have received cost information higher than expected.
                        Column (V) shows effects on those that have received cost information for different price levels (Baseline: 45€/tCO\\textsubscript{2}, 85€/tCO\\textsubscript{2}, 125€/tCO\\textsubscript{2}).
                        All estimations include a respondent-level fixed effect. Coefficients in column (IV) can be understood as a split-sample estimation between opposers and non-opposers.
                        Sample excludes respondents that answered 'I don't know' in either period.
                        Standard errors clustered at the level of respondents in parentheses.")))

etable(model_3.8.1_F_o, model_3.8.1_E_o,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments B and C on policy support, perception of fairness and effectiveness (opposers)",
       headers = c("(I)", "(II)"),
       label = "tab_BC_ALL_Opposer",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_BC_ALL_O.tex"))

# 3.8.2  Interaction with finding treatment credible ####

model_3.8.2.1_ESP <- feols(Support ~ Post_B_Credible + Post_C1_Credible + Post_C2_Credible + Post_C3_Credible + Post_C4_Credible + Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B | ID + Period, data = rename(adjust_3.8.1(data_3_ESP, "Support"), Support = value), cluster = ~ ID)
model_3.8.2.1_FRA <- feols(Support ~ Post_B_Credible + Post_C1_Credible + Post_C2_Credible + Post_C3_Credible + Post_C4_Credible + Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B | ID + Period, data = rename(adjust_3.8.1(data_3_FRA, "Support"), Support = value), cluster = ~ ID)
model_3.8.2.1_GER <- feols(Support ~ Post_B_Credible + Post_C1_Credible + Post_C2_Credible + Post_C3_Credible + Post_C4_Credible + Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B | ID + Period, data = rename(adjust_3.8.1(data_3_GER, "Support"), Support = value), cluster = ~ ID)
model_3.8.2.1_ROM <- feols(Support ~ Post_B_Credible + Post_C1_Credible + Post_C2_Credible + Post_C3_Credible + Post_C4_Credible + Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B | ID + Period, data = rename(adjust_3.8.1(data_3_ROM, "Support"), Support = value), cluster = ~ ID)

etable(model_3.8.2.1_ESP, model_3.8.2.1_FRA, model_3.8.2.1_GER, model_3.8.2.1_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments on policy support (Interaction with credibility)",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_Add_1_Credible",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_Add_1_Credible.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

# 3.8.3  Interaction with overestimating or underestimating treatment C ####

model_3.8.3.1_ESP <- feols(Support ~ C1_O_Post + C2_O_Post + C3_O_Post + C3_OR_Post + C4_O_Post + Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B | ID + Period, data = rename(adjust_3.8.1(data_3_ESP, "Support"), Support = value), cluster = ~ ID)
model_3.8.3.1_FRA <- feols(Support ~ C1_O_Post + C2_O_Post + C3_O_Post + C3_OR_Post + C4_O_Post + Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B | ID + Period, data = rename(adjust_3.8.1(data_3_FRA, "Support"), Support = value), cluster = ~ ID)
model_3.8.3.1_GER <- feols(Support ~ C1_O_Post + C2_O_Post + C3_O_Post + C3_OR_Post + C4_O_Post + Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B | ID + Period, data = rename(adjust_3.8.1(data_3_GER, "Support"), Support = value), cluster = ~ ID)
model_3.8.3.1_ROM <- feols(Support ~ C1_O_Post + C2_O_Post + C3_O_Post + C3_OR_Post + C4_O_Post + Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B | ID + Period, data = rename(adjust_3.8.1(data_3_ROM, "Support"), Support = value), cluster = ~ ID)

etable(model_3.8.3.1_ESP, model_3.8.3.1_FRA, model_3.8.3.1_GER, model_3.8.3.1_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments on policy support (Interaction with overestimating)",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_Add_1_Overestimating",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_Add_1_Over.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

# 3.8.4  Interaction with being inital supporter/opposer ####

data_3.8.4_ESP <- rename(adjust_3.8.1(data_3_ESP, "Support"), Support = value) %>%
  mutate(Opposer = ifelse(Support < 3 & Period == 1,1,0))%>%
  mutate(Supporter = ifelse(Support > 3 & Period == 1,1,0))%>%
  group_by(ID)%>%
  mutate(Opposer = sum(Opposer),
         Supporter = sum(Supporter))%>%
  ungroup()

data_3.8.4_FRA <- rename(adjust_3.8.1(data_3_FRA, "Support"), Support = value) %>%
  mutate(Opposer = ifelse(Support < 3 & Period == 1,1,0))%>%
  mutate(Supporter = ifelse(Support > 3 & Period == 1,1,0))%>%
  group_by(ID)%>%
  mutate(Opposer = sum(Opposer),
         Supporter = sum(Supporter))%>%
  ungroup()

data_3.8.4_GER <- rename(adjust_3.8.1(data_3_GER, "Support"), Support = value) %>%
  mutate(Opposer = ifelse(Support < 3 & Period == 1,1,0))%>%
  mutate(Supporter = ifelse(Support > 3 & Period == 1,1,0))%>%
  group_by(ID)%>%
  mutate(Opposer = sum(Opposer),
         Supporter = sum(Supporter))%>%
  ungroup()

data_3.8.4_ROM <- rename(adjust_3.8.1(data_3_ROM, "Support"), Support = value) %>%
  mutate(Opposer = ifelse(Support < 3 & Period == 1,1,0))%>%
  mutate(Supporter = ifelse(Support > 3 & Period == 1,1,0))%>%
  group_by(ID)%>%
  mutate(Opposer = sum(Opposer),
         Supporter = sum(Supporter))%>%
  ungroup()

model_3.8.4.1_ESP <- feols(Support ~ Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B | ID + Period, data = filter(data_3.8.4_ESP, Opposer == 1), cluster = ~ ID)
model_3.8.4.1_FRA <- feols(Support ~ Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B | ID + Period, data = filter(data_3.8.4_FRA, Opposer == 1), cluster = ~ ID)
model_3.8.4.1_GER <- feols(Support ~ Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B | ID + Period, data = filter(data_3.8.4_GER, Opposer == 1), cluster = ~ ID)
model_3.8.4.1_ROM <- feols(Support ~ Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B | ID + Period, data = filter(data_3.8.4_ROM, Opposer == 1), cluster = ~ ID)

model_3.8.4.2_ESP <- feols(Support ~ Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B | ID + Period, data = filter(data_3.8.4_ESP, Supporter == 1), cluster = ~ ID)
model_3.8.4.2_FRA <- feols(Support ~ Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B | ID + Period, data = filter(data_3.8.4_FRA, Supporter == 1), cluster = ~ ID)
model_3.8.4.2_GER <- feols(Support ~ Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B | ID + Period, data = filter(data_3.8.4_GER, Supporter == 1), cluster = ~ ID)
model_3.8.4.2_ROM <- feols(Support ~ Post_C1 + Post_C2 + Post_C3 + Post_C4 + Post_B | ID + Period, data = filter(data_3.8.4_ROM, Supporter == 1), cluster = ~ ID)

etable(model_3.8.4.1_ESP, model_3.8.4.1_FRA, model_3.8.4.1_GER, model_3.8.4.1_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments on policy support (on initial opposers - descriptive)",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_Add_1_Opposers",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_Add_1_Opposers.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

etable(model_3.8.4.2_ESP, model_3.8.4.2_FRA, model_3.8.4.2_GER, model_3.8.4.2_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments on policy support (on initial supporters - descriptive)",
       headers = c("Spain", "France", "Germany", "Romania"),
       label = "tab_A_Add_1_Opposers",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_Add_1_Supporters.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table TBD. Standard errors clustered at the level of respondents in parentheses.")))

# 3.8.5  Effects on cost perception ####

adjust_3.8.5 <- function(data_3_0, filter_1){
  data_3_8.5 <- data_3_0 %>%
    select(ID, Treatment_B, Treatment_C, Q30_2N, Dif_cost_1, Dif_cost_2, Dif_cost_1_ABS, Dif_cost_2_ABS, Q42_1N, Q42_2N, 
           Dif_Percentile_1, Pricelevel, Q52B, Q58, Q46_1N)%>%
    mutate(Opposer = ifelse(Q46_1N < 3,1,0))%>%
    mutate(Overestimated_Absolute = ifelse(Dif_cost_1 > 0, "Overestimated",
                                           ifelse(Dif_cost_1 < 0, "Underestimated", NA)))%>%
    mutate(Overestimated_Distribution = ifelse(Dif_Percentile_1 > 0, "Overestimated",
                                               ifelse(Dif_Percentile_1 < 0, "Underestimated", NA)))%>%
    pivot_longer(Dif_cost_1:Q42_2N, names_to = "Variable", values_to = "value")%>%
    mutate(Period  = ifelse(Variable %in% c("Dif_cost_1", "Dif_cost_1_ABS", "Q42_1N"),1,2),
           Outcome = ifelse(Variable %in% c("Dif_cost_1", "Dif_cost_2"), "Dif_cost", 
                            ifelse(Variable %in% c("Dif_cost_1_ABS", "Dif_cost_2_ABS"), "Dif_cost_ABS", 
                                   ifelse(Variable %in% c("Q42_1N", "Q42_2N"), "Q42", NA))))%>%
    mutate(Post_B      = ifelse(Period == 2 & Treatment_B == "Treatment",1,0),
           Post_B_ONLY = ifelse(Period == 2 & Treatment_B == "Treatment" & Treatment_C == "Control",1,0),
           Post_C_ONLY = ifelse(Period == 2 & Treatment_B == "Control" & Treatment_C != "Control",1,0),
           Post_C1     = ifelse(Period == 2 & Treatment_C == "C1",1,0),
           Post_C2     = ifelse(Period == 2 & Treatment_C == "C2",1,0),
           Post_C3     = ifelse(Period == 2 & Treatment_C == "C3",1,0),
           Post_C4     = ifelse(Period == 2 & Treatment_C == "C4",1,0),
           Post_C12    = ifelse(Period == 2 & (Treatment_C == "C1" | Treatment_C == "C2"),1,0),
           Post_C34    = ifelse(Period == 2 & (Treatment_C == "C3" | Treatment_C == "C4"),1,0),
           Post_C1234  = ifelse(Period == 2 & Treatment_C != "Control",1,0))%>%
    # Interactions
    mutate(Post_B_C1 = ifelse(Period == 2 & Treatment_B == "Treatment" & Treatment_C == "C1",1,0),
           Post_B_C2 = ifelse(Period == 2 & Treatment_B == "Treatment" & Treatment_C == "C2",1,0),
           Post_B_C3 = ifelse(Period == 2 & Treatment_B == "Treatment" & Treatment_C == "C3",1,0),
           Post_B_C4 = ifelse(Period == 2 & Treatment_B == "Treatment" & Treatment_C == "C4",1,0),
           Post_B_C  = ifelse(Period == 2 & Treatment_B == "Treatment" & Treatment_C != "Control",1,0))%>%
    mutate(Group_BC = case_when(Treatment_B == "Control" & Treatment_C == "Control" ~ "None",
                                Treatment_B == "Treatment" & Treatment_C == "Control" ~ "B_Only",
                                Treatment_B == "Control" & Treatment_C == "C1" ~ "C1_Only",
                                Treatment_B == "Treatment" & Treatment_C == "C1" ~ "B_C1",
                                Treatment_B == "Control" & Treatment_C == "C2" ~ "C2_Only",
                                Treatment_B == "Treatment" & Treatment_C == "C2" ~ "B_C2",
                                Treatment_B == "Control" & Treatment_C == "C3" ~ "C3_Only",
                                Treatment_B == "Treatment" & Treatment_C == "C3" ~ "B_C3",
                                Treatment_B == "Control" & Treatment_C == "C4" ~ "C4_Only",
                                Treatment_B == "Treatment" & Treatment_C == "C4" ~ "B_C4",
                                TRUE ~ NA))%>%
    # C1 - overestimated/underestimated absolute
    mutate(C1_overestimated  = ifelse(Treatment_C == "C1" & Overestimated_Absolute == "Overestimated" & !is.na(Overestimated_Absolute),1,0),
           C1_underestimated = ifelse(Treatment_C == "C1" & Overestimated_Absolute == "Underestimated" & !is.na(Overestimated_Absolute),1,0))%>%
    # C2 - overestimated/underestimated relative
    mutate(C2_overestimated  = ifelse(Treatment_C == "C2" & Overestimated_Distribution == "Overestimated" & !is.na(Overestimated_Distribution),1,0),
           C2_underestimated = ifelse(Treatment_C == "C2" & Overestimated_Distribution == "Underestimated" & !is.na(Overestimated_Distribution),1,0))%>%
    # C3 - overestimated/underestimated absolute and relative
    mutate(C3_overestimated  = ifelse(Treatment_C == "C3" & Overestimated_Absolute == "Overestimated" & !is.na(Overestimated_Absolute),1,0),
           C3_underestimated = ifelse(Treatment_C == "C3" & Overestimated_Absolute == "Underestimated" & !is.na(Overestimated_Absolute),1,0))%>%
    mutate(C3_overestimated_dist  = ifelse(Treatment_C == "C3" & Overestimated_Distribution == "Overestimated" & !is.na(Overestimated_Distribution),1,0),
           C3_underestimated_dist = ifelse(Treatment_C == "C3" & Overestimated_Distribution == "Underestimated" & !is.na(Overestimated_Distribution),1,0))%>%
    # C4 - overestimated/underestimated relative
    mutate(C4_overestimated  = ifelse(Treatment_C == "C4" & Overestimated_Distribution == "Overestimated" & !is.na(Overestimated_Distribution),1,0),
           C4_underestimated = ifelse(Treatment_C == "C4" & Overestimated_Distribution == "Underestimated" & !is.na(Overestimated_Distribution),1,0))%>%
    mutate(C_overestimated  = ifelse(C1_overestimated == 1 | C2_overestimated == 1 | C3_overestimated == 1 | C4_overestimated == 1,1,0))%>%
    mutate(C1_O_Post = ifelse(C1_overestimated == 1  & Period == 2,1,0),
           C1_U_Post = ifelse(C1_underestimated == 1 & Period == 2,1,0),
           C2_O_Post = ifelse(C2_overestimated == 1  & Period == 2,1,0),
           C2_U_Post = ifelse(C2_underestimated == 1 & Period == 2,1,0),
           C3_O_Post = ifelse(C3_overestimated == 1  & Period == 2,1,0),
           C3_U_Post = ifelse(C3_underestimated == 1 & Period == 2,1,0),
           C3_OR_Post = ifelse(C3_overestimated_dist == 1  & Period == 2,1,0),
           C3_UR_Post = ifelse(C3_underestimated_dist == 1 & Period == 2,1,0),
           C4_O_Post = ifelse(C4_overestimated == 1  & Period == 2,1,0),
           C4_U_Post = ifelse(C4_underestimated == 1 & Period == 2,1,0))%>%
    mutate(Post_C_ONLY_Over = ifelse(Period == 2 & Treatment_B == "Control" & Treatment_C != "Control" & C_overestimated == 1,1,0),
           Post_B_C_Over    = ifelse(Period == 2 & Treatment_B == "Treatment" & Treatment_C != "Control" & C_overestimated == 1,1,0))%>%
    # Clear / credible
    mutate(Post_0 = ifelse(Period == 2,1,0))%>%
    filter(Outcome == filter_1)%>%
    # mutate(tau = ifelse(Q30_2N < 3,1,0))%>%
    # mutate(tau_Post_B = ifelse(tau == 1 & Post_B == 1,1,0))%>%
    group_by(ID, Outcome)%>%
    mutate(NAS = sum(is.na(value)))%>%
    ungroup()%>%
    mutate(Pricelevel = factor(Pricelevel, levels = c("45", "85", "125")))%>%
    mutate(Credible_B = ifelse(Treatment_B == "Control" | Q52B %in% c("Ja", "Sí", "Oui", "Da"),1,0))%>%
    mutate(Credible_C = ifelse(Treatment_C == "Control" | Q58  %in% c("Ja", "Sí", "Oui", "Da"),1,0))%>%
    mutate(Post_B_Credible  = ifelse(Post_B == 1 & Credible_B == 1,1,0),
           Post_C1_Credible = ifelse(Post_C1 == 1 & Credible_C == 1,1,0),
           Post_C2_Credible = ifelse(Post_C2 == 1 & Credible_C == 1,1,0),
           Post_C3_Credible = ifelse(Post_C3 == 1 & Credible_C == 1,1,0),
           Post_C4_Credible = ifelse(Post_C4 == 1 & Credible_C == 1,1,0))%>%
    # Interaction Credible B/C
    mutate(Post_B_ONLY_Credible = ifelse(Post_B_ONLY == 1 & Credible_B == 1,1,0),
           Post_C_ONLY_Credible = ifelse(Post_C_ONLY == 1 & Credible_C == 1,1,0),
           Post_B_C_Credible    = ifelse(Post_B_C == 1 & Credible_B == 1 & Credible_C == 1,1,0))%>%
    # Interaction Opposer B/C
    mutate(Post_B_ONLY_Opposer = ifelse(Post_B_ONLY == 1 & Opposer == 1,1,0),
           Post_C_ONLY_Opposer = ifelse(Post_C_ONLY == 1 & Opposer == 1,1,0),
           Post_B_C_Opposer    = ifelse(Post_B_C == 1 & Opposer == 1,1,0))
  
  # Correction for z-values
  z_values <- data_3_8.5 %>%
    filter(Period == 1)%>%
    summarise(mean_Pre = mean(value, na.rm = TRUE),
              sd_Pre   = sd(value, na.rm = TRUE))
  
  data_3_8.5 <- data_3_8.5 %>%
    mutate(value_z = (value - z_values$mean_Pre)/z_values$sd_Pre)
  
  return(data_3_8.5)
}

model_3.8.5_ESP   <- feols(Dif_cost_ABS ~ Post_C_ONLY + Post_B_C |                                                                   ID, data = rename(adjust_3.8.5(data_3_ESP, "Dif_cost_ABS"), Dif_cost_ABS = value),   cluster = ~ ID)
model_3.8.5_ESP_z <- feols(Dif_cost_ABS ~ Post_C_ONLY + Post_B_C |                                                                   ID, data = rename(adjust_3.8.5(data_3_ESP, "Dif_cost_ABS"), Dif_cost_ABS = value_z), cluster = ~ ID)
model_3.8.5_ESP_c <- feols(Dif_cost_ABS ~ Post_C_ONLY + Post_B_C + Post_C_ONLY_Credible + Post_B_C_Credible | ID, data = rename(adjust_3.8.5(data_3_ESP, "Dif_cost_ABS"), Dif_cost_ABS = value),   cluster = ~ ID)
model_3.8.5_ESP_o <- feols(Dif_cost_ABS ~ Post_C_ONLY + Post_B_C + Post_C_ONLY_Opposer + Post_B_C_Opposer |    ID, data = rename(adjust_3.8.5(data_3_ESP, "Dif_cost_ABS"), Dif_cost_ABS = value),   cluster = ~ ID)
model_3.8.5_ESP_e <- feols(Dif_cost_ABS ~ Post_C_ONLY + Post_B_C + Post_C_ONLY_Over + Post_B_C_Over |                                ID, data = rename(adjust_3.8.5(data_3_ESP, "Dif_cost_ABS"), Dif_cost_ABS = value),   cluster = ~ ID)

etable(model_3.8.5_ESP_z, model_3.8.5_ESP, model_3.8.5_ESP_c, model_3.8.5_ESP_o, model_3.8.5_ESP_e,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments B and C on perception of additional costs (error) in Spain",
       headers = c("(I)", "(II)", "(III)", "(IV)", "(V)"),
       label = "tab_BC_ESP_3",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_BC_ESP_3.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from five OLS regressions of our video information treatment (B) and our cost information treatment (C) on the error in the perception of additional costs among respondents from Spain. 
                        Responses to question Q42\\_1 and Q42\\_2 are compared against estimated additional costs for each respondent profile. Outcome is the absolute deviation in number of brackets, with a maximum of 6.
                        Column (I) shows such results with the outcome standardized using z-scores based on average and SD of overall perception of additional costs in the first period. 
                        Column (II) shows such results with the non-standardized outcome (four-point Likert scale).
                        Column (III) shows effects on those that find the information not credible and on those that find it credible.
                        Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                        Column (V) shows effects on those that have received cost information lower than expected and on those that have received cost information higher than expected.
                        All estimations include a respondent-level fixed effect. Coefficients in column (IV) can be understood as a split-sample estimation between opposers and non-opposers.
                        Sample excludes respondents that answered 'I don't know' in either period.
                        Sample excludes respondents that are part of the control group for the cost information treatment C, as observations for Q42\\_2 are lacking.
                        Standard errors clustered at the level of respondents in parentheses.")))

model_3.8.5_FRA   <- feols(Dif_cost_ABS ~ Post_C_ONLY + Post_B_C |                                            ID, data = rename(adjust_3.8.5(data_3_FRA, "Dif_cost_ABS"), Dif_cost_ABS = value),   cluster = ~ ID)
model_3.8.5_FRA_z <- feols(Dif_cost_ABS ~ Post_C_ONLY + Post_B_C |                                            ID, data = rename(adjust_3.8.5(data_3_FRA, "Dif_cost_ABS"), Dif_cost_ABS = value_z), cluster = ~ ID)
model_3.8.5_FRA_c <- feols(Dif_cost_ABS ~ Post_C_ONLY + Post_B_C + Post_C_ONLY_Credible + Post_B_C_Credible | ID, data = rename(adjust_3.8.5(data_3_FRA, "Dif_cost_ABS"), Dif_cost_ABS = value),   cluster = ~ ID)
model_3.8.5_FRA_o <- feols(Dif_cost_ABS ~ Post_C_ONLY + Post_B_C + Post_C_ONLY_Opposer + Post_B_C_Opposer |   ID, data = rename(adjust_3.8.5(data_3_FRA, "Dif_cost_ABS"), Dif_cost_ABS = value),   cluster = ~ ID)
model_3.8.5_FRA_e <- feols(Dif_cost_ABS ~ Post_C_ONLY + Post_B_C + Post_C_ONLY_Over + Post_B_C_Over |         ID, data = rename(adjust_3.8.5(data_3_FRA, "Dif_cost_ABS"), Dif_cost_ABS = value),   cluster = ~ ID)

etable(model_3.8.5_FRA_z, model_3.8.5_FRA, model_3.8.5_FRA_c, model_3.8.5_FRA_o, model_3.8.5_FRA_e,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments B and C on perception of additional costs (error) in France",
       headers = c("(I)", "(II)", "(III)", "(IV)", "(V)"),
       label = "tab_BC_FRA_3",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_BC_FRA_3.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from five OLS regressions of our video information treatment (B) and our cost information treatment (C) on the error in the perception of additional costs among respondents from France.
                        Responses to question Q42\\_1 and Q42\\_2 are compared against estimated additional costs for each respondent profile. Outcome is the absolute deviation in number of brackets, with a maximum of 6.
                        Column (I) shows such results with the outcome standardized using z-scores based on average and SD of overall perception of additional costs in the first period.
                        Column (II) shows such results with the non-standardized outcome (four-point Likert scale).
                        Column (III) shows effects on those that find the information not credible and on those that find it credible.
                        Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                        Column (V) shows effects on those that have received cost information lower than expected and on those that have received cost information higher than expected.
                        All estimations include a respondent-level fixed effect. Coefficients in column (IV) can be understood as a split-sample estimation between opposers and non-opposers.
                        Sample excludes respondents that answered 'I don't know' in either period.
                        Sample excludes respondents that are part of the control group for the cost information treatment C, as observations for Q42\\_2 are lacking.
                        Standard errors clustered at the level of respondents in parentheses.")))

model_3.8.5_GER   <- feols(Dif_cost_ABS ~ Post_C_ONLY + Post_B_C |                                            ID, data = rename(adjust_3.8.5(data_3_GER, "Dif_cost_ABS"), Dif_cost_ABS = value),   cluster = ~ ID)
model_3.8.5_GER_z <- feols(Dif_cost_ABS ~ Post_C_ONLY + Post_B_C |                                            ID, data = rename(adjust_3.8.5(data_3_GER, "Dif_cost_ABS"), Dif_cost_ABS = value_z), cluster = ~ ID)
model_3.8.5_GER_c <- feols(Dif_cost_ABS ~ Post_C_ONLY + Post_B_C + Post_C_ONLY_Credible + Post_B_C_Credible | ID, data = rename(adjust_3.8.5(data_3_GER, "Dif_cost_ABS"), Dif_cost_ABS = value),   cluster = ~ ID)
model_3.8.5_GER_o <- feols(Dif_cost_ABS ~ Post_C_ONLY + Post_B_C + Post_C_ONLY_Opposer + Post_B_C_Opposer |   ID, data = rename(adjust_3.8.5(data_3_GER, "Dif_cost_ABS"), Dif_cost_ABS = value),   cluster = ~ ID)
model_3.8.5_GER_e <- feols(Dif_cost_ABS ~ Post_C_ONLY + Post_B_C + Post_C_ONLY_Over + Post_B_C_Over |         ID, data = rename(adjust_3.8.5(data_3_GER, "Dif_cost_ABS"), Dif_cost_ABS = value),   cluster = ~ ID)

etable(model_3.8.5_GER_z, model_3.8.5_GER, model_3.8.5_GER_c, model_3.8.5_GER_o, model_3.8.5_GER_e,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments B and C on perception of additional costs (error) in Germany",
       headers = c("(I)", "(II)", "(III)", "(IV)", "(V)"),
       label = "tab_BC_GER_3",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_BC_GER_3.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from five OLS regressions of our video information treatment (B) and our cost information treatment (C) on the error in the perception of additional costs among respondents from Germany. 
                        Responses to question Q42\\_1 and Q42\\_2 are compared against estimated additional costs for each respondent profile. Outcome is the absolute deviation in number of brackets, with a maximum of 6.
                        Column (I) shows such results with the outcome standardized using z-scores based on average and SD of overall perception of additional costs in the first period. 
                        Column (II) shows such results with the non-standardized outcome (four-point Likert scale).
                        Column (III) shows effects on those that find the information not credible and on those that find it credible.
                        Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                        Column (V) shows effects on those that have received cost information lower than expected and on those that have received cost information higher than expected.
                        All estimations include a respondent-level fixed effect. Coefficients in column (IV) can be understood as a split-sample estimation between opposers and non-opposers.
                        Sample excludes respondents that answered 'I don't know' in either period.
                        Sample excludes respondents that are part of the control group for the cost information treatment C, as observations for Q42\\_2 are lacking.
                        Standard errors clustered at the level of respondents in parentheses.")))

model_3.8.5_ROM   <- feols(Dif_cost_ABS ~ Post_C_ONLY + Post_B_C |                                            ID, data = rename(adjust_3.8.5(data_3_ROM, "Dif_cost_ABS"), Dif_cost_ABS = value),   cluster = ~ ID)
model_3.8.5_ROM_z <- feols(Dif_cost_ABS ~ Post_C_ONLY + Post_B_C |                                            ID, data = rename(adjust_3.8.5(data_3_ROM, "Dif_cost_ABS"), Dif_cost_ABS = value_z), cluster = ~ ID)
model_3.8.5_ROM_c <- feols(Dif_cost_ABS ~ Post_C_ONLY + Post_B_C + Post_C_ONLY_Credible + Post_B_C_Credible | ID, data = rename(adjust_3.8.5(data_3_ROM, "Dif_cost_ABS"), Dif_cost_ABS = value),   cluster = ~ ID)
model_3.8.5_ROM_o <- feols(Dif_cost_ABS ~ Post_C_ONLY + Post_B_C + Post_C_ONLY_Opposer + Post_B_C_Opposer |   ID, data = rename(adjust_3.8.5(data_3_ROM, "Dif_cost_ABS"), Dif_cost_ABS = value),   cluster = ~ ID)
model_3.8.5_ROM_e <- feols(Dif_cost_ABS ~ Post_C_ONLY + Post_B_C + Post_C_ONLY_Over + Post_B_C_Over |         ID, data = rename(adjust_3.8.5(data_3_ROM, "Dif_cost_ABS"), Dif_cost_ABS = value),   cluster = ~ ID)

etable(model_3.8.5_ROM_z, model_3.8.5_ROM, model_3.8.5_ROM_c, model_3.8.5_ROM_o, model_3.8.5_ROM_e,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments B and C on perception of additional costs (error) in Romania",
       headers = c("(I)", "(II)", "(III)", "(IV)", "(V)"),
       label = "tab_BC_ROM_3",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_BC_ROM_3.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from five OLS regressions of our video information treatment (B) and our cost information treatment (C) on the error in the perception of additional costs among respondents from Romania. 
                        Responses to question Q42\\_1 and Q42\\_2 are compared against estimated additional costs for each respondent profile. Outcome is the absolute deviation in number of brackets, with a maximum of 6.
                        Column (I) shows such results with the outcome standardized using z-scores based on average and SD of overall perception of additional costs in the first period. 
                        Column (II) shows such results with the non-standardized outcome (four-point Likert scale).
                        Column (III) shows effects on those that find the information not credible and on those that find it credible.
                        Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                        Column (V) shows effects on those that have received cost information lower than expected and on those that have received cost information higher than expected.
                        All estimations include a respondent-level fixed effect. Coefficients in column (IV) can be understood as a split-sample estimation between opposers and non-opposers.
                        Sample excludes respondents that answered 'I don't know' in either period.
                        Sample excludes respondents that are part of the control group for the cost information treatment C, as observations for Q42\\_2 are lacking.
                        Standard errors clustered at the level of respondents in parentheses.")))

model_3.8.5_1_ESP   <- feols(Q42 ~ Post_C_ONLY + Post_B_C |                                            ID, data = rename(adjust_3.8.5(data_3_ESP, "Q42"), Q42 = value),   cluster = ~ ID)
model_3.8.5_1_ESP_z <- feols(Q42 ~ Post_C_ONLY + Post_B_C |                                            ID, data = rename(adjust_3.8.5(data_3_ESP, "Q42"), Q42 = value_z), cluster = ~ ID)
model_3.8.5_1_ESP_c <- feols(Q42 ~ Post_C_ONLY + Post_B_C + Post_C_ONLY_Credible + Post_B_C_Credible | ID, data = rename(adjust_3.8.5(data_3_ESP, "Q42"), Q42 = value),   cluster = ~ ID)
model_3.8.5_1_ESP_o <- feols(Q42 ~ Post_C_ONLY + Post_B_C + Post_C_ONLY_Opposer + Post_B_C_Opposer |   ID, data = rename(adjust_3.8.5(data_3_ESP, "Q42"), Q42 = value),   cluster = ~ ID)
model_3.8.5_1_ESP_e <- feols(Q42 ~ Post_C_ONLY + Post_B_C + Post_C_ONLY_Over + Post_B_C_Over |         ID, data = rename(adjust_3.8.5(data_3_ESP, "Q42"), Q42 = value),   cluster = ~ ID)

etable(model_3.8.5_1_ESP_z, model_3.8.5_1_ESP, model_3.8.5_1_ESP_c, model_3.8.5_1_ESP_o, model_3.8.5_1_ESP_e,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments B and C on perception of additional costs in Spain",
       headers = c("(I)", "(II)", "(III)", "(IV)", "(V)"),
       label = "tab_BC_ESP_4",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_BC_ESP_4.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from five OLS regressions of our video information treatment (B) and our cost information treatment (C) on the perception of additional costs among respondents from Spain. 
                        Column (I) shows such results with the outcome standardized using z-scores based on average and SD of overall perception of additional costs in the first period. These values are shown in Figures \\ref{fig:3} and \\ref{fig:2:3}.
                        Column (II) shows such results with the non-standardized outcome (seven cost brackets).
                        Column (III) shows effects on those that find the information not credible and on those that find it credible.
                        Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                        Column (V) shows effects on those that have received cost information lower than expected and on those that have received cost information higher than expected.
                        All estimations include a respondent-level fixed effect. Coefficients in column (IV) can be understood as a split-sample estimation between opposers and non-opposers.
                        Sample excludes respondents that answered 'I don't know' in either period.
                        Sample excludes respondents that are part of the control group for the cost information treatment C, as observations for Q42\\_2 are lacking.
                        Standard errors clustered at the level of respondents in parentheses.")))

model_3.8.5_1_FRA   <- feols(Q42 ~ Post_C_ONLY + Post_B_C |                                            ID, data = rename(adjust_3.8.5(data_3_FRA, "Q42"), Q42 = value),   cluster = ~ ID)
model_3.8.5_1_FRA_z <- feols(Q42 ~ Post_C_ONLY + Post_B_C |                                            ID, data = rename(adjust_3.8.5(data_3_FRA, "Q42"), Q42 = value_z), cluster = ~ ID)
model_3.8.5_1_FRA_c <- feols(Q42 ~ Post_C_ONLY + Post_B_C + Post_C_ONLY_Credible + Post_B_C_Credible | ID, data = rename(adjust_3.8.5(data_3_FRA, "Q42"), Q42 = value),   cluster = ~ ID)
model_3.8.5_1_FRA_o <- feols(Q42 ~ Post_C_ONLY + Post_B_C + Post_C_ONLY_Opposer + Post_B_C_Opposer |   ID, data = rename(adjust_3.8.5(data_3_FRA, "Q42"), Q42 = value),   cluster = ~ ID)
model_3.8.5_1_FRA_e <- feols(Q42 ~ Post_C_ONLY + Post_B_C + Post_C_ONLY_Over + Post_B_C_Over |         ID, data = rename(adjust_3.8.5(data_3_FRA, "Q42"), Q42 = value),   cluster = ~ ID)

etable(model_3.8.5_1_FRA_z, model_3.8.5_1_FRA, model_3.8.5_1_FRA_c, model_3.8.5_1_FRA_o, model_3.8.5_1_FRA_e,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments B and C on perception of additional costs in France",
       headers = c("(I)", "(II)", "(III)", "(IV)", "(V)"),
       label = "tab_BC_FRA_4",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_BC_FRA_4.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from five OLS regressions of our video information treatment (B) and our cost information treatment (C) on the perception of additional costs among respondents from France.
                        Column (I) shows such results with the outcome standardized using z-scores based on average and SD of overall perception of additional costs in the first period. These values are shown in Figures \\ref{fig:3} and \\ref{fig:2:3}.
                        Column (II) shows such results with the non-standardized outcome (seven cost brackets).
                        Column (III) shows effects on those that find the information not credible and on those that find it credible.
                        Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                        Column (V) shows effects on those that have received cost information lower than expected and on those that have received cost information higher than expected.
                        All estimations include a respondent-level fixed effect. Coefficients in column (IV) can be understood as a split-sample estimation between opposers and non-opposers.
                        Sample excludes respondents that answered 'I don't know' in either period.
                        Sample excludes respondents that are part of the control group for the cost information treatment C, as observations for Q42\\_2 are lacking.
                        Standard errors clustered at the level of respondents in parentheses.")))

model_3.8.5_1_GER   <- feols(Q42 ~ Post_C_ONLY + Post_B_C |                                            ID, data = rename(adjust_3.8.5(data_3_GER, "Q42"), Q42 = value),   cluster = ~ ID)
model_3.8.5_1_GER_z <- feols(Q42 ~ Post_C_ONLY + Post_B_C |                                            ID, data = rename(adjust_3.8.5(data_3_GER, "Q42"), Q42 = value_z), cluster = ~ ID)
model_3.8.5_1_GER_c <- feols(Q42 ~ Post_C_ONLY + Post_B_C + Post_C_ONLY_Credible + Post_B_C_Credible | ID, data = rename(adjust_3.8.5(data_3_GER, "Q42"), Q42 = value),   cluster = ~ ID)
model_3.8.5_1_GER_o <- feols(Q42 ~ Post_C_ONLY + Post_B_C + Post_C_ONLY_Opposer + Post_B_C_Opposer |   ID, data = rename(adjust_3.8.5(data_3_GER, "Q42"), Q42 = value),   cluster = ~ ID)
model_3.8.5_1_GER_e <- feols(Q42 ~ Post_C_ONLY + Post_B_C + Post_C_ONLY_Over + Post_B_C_Over |         ID, data = rename(adjust_3.8.5(data_3_GER, "Q42"), Q42 = value),   cluster = ~ ID)

etable(model_3.8.5_1_GER_z, model_3.8.5_1_GER, model_3.8.5_1_GER_c, model_3.8.5_1_GER_o, model_3.8.5_1_GER_e,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments B and C on perception of additional costs in Germany",
       headers = c("(I)", "(II)", "(III)", "(IV)", "(V)"),
       label = "tab_BC_GER_4",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_BC_GER_4.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from five OLS regressions of our video information treatment (B) and our cost information treatment (C) on the perception of additional costs among respondents from Germany. 
                        Column (I) shows such results with the outcome standardized using z-scores based on average and SD of overall perception of additional costs in the first period. These values are shown in Figures \\ref{fig:3} and \\ref{fig:2:3}.
                        Column (II) shows such results with the non-standardized outcome (seven cost brackets).
                        Column (III) shows effects on those that find the information not credible and on those that find it credible.
                        Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                        Column (V) shows effects on those that have received cost information lower than expected and on those that have received cost information higher than expected.
                        All estimations include a respondent-level fixed effect. Coefficients in column (IV) can be understood as a split-sample estimation between opposers and non-opposers.
                        Sample excludes respondents that answered 'I don't know' in either period.
                        Sample excludes respondents that are part of the control group for the cost information treatment C, as observations for Q42\\_2 are lacking.
                        Standard errors clustered at the level of respondents in parentheses.")))

model_3.8.5_1_ROM   <- feols(Q42 ~ Post_C_ONLY + Post_B_C |                                            ID, data = rename(adjust_3.8.5(data_3_ROM, "Q42"), Q42 = value),   cluster = ~ ID)
model_3.8.5_1_ROM_z <- feols(Q42 ~ Post_C_ONLY + Post_B_C |                                            ID, data = rename(adjust_3.8.5(data_3_ROM, "Q42"), Q42 = value_z), cluster = ~ ID)
model_3.8.5_1_ROM_c <- feols(Q42 ~ Post_C_ONLY + Post_B_C + Post_C_ONLY_Credible + Post_B_C_Credible | ID, data = rename(adjust_3.8.5(data_3_ROM, "Q42"), Q42 = value),   cluster = ~ ID)
model_3.8.5_1_ROM_o <- feols(Q42 ~ Post_C_ONLY + Post_B_C + Post_C_ONLY_Opposer + Post_B_C_Opposer |   ID, data = rename(adjust_3.8.5(data_3_ROM, "Q42"), Q42 = value),   cluster = ~ ID)
model_3.8.5_1_ROM_e <- feols(Q42 ~ Post_C_ONLY + Post_B_C + Post_C_ONLY_Over + Post_B_C_Over |         ID, data = rename(adjust_3.8.5(data_3_ROM, "Q42"), Q42 = value),   cluster = ~ ID)

etable(model_3.8.5_1_ROM_z, model_3.8.5_1_ROM, model_3.8.5_1_ROM_c, model_3.8.5_1_ROM_o, model_3.8.5_1_ROM_e,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of information treatments B and C on perception of additional costs in Romania",
       headers = c("(I)", "(II)", "(III)", "(IV)", "(V)"),
       label = "tab_BC_ROM_4",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_BC_ROM_4.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from five OLS regressions of our video information treatment (B) and our cost information treatment (C) on the perception of additional costs among respondents from Romania. 
                        Column (I) shows such results with the outcome standardized using z-scores based on average and SD of overall perception of additional costs in the first period. These values are shown in Figures \\ref{fig:3} and \\ref{fig:2:3}.
                        Column (II) shows such results with the non-standardized outcome (seven cost brackets).
                        Column (III) shows effects on those that find the information not credible and on those that find it credible.
                        Column (IV) shows effects on those that had not been opposing in the first period and on those that have.
                        Column (V) shows effects on those that have received cost information lower than expected and on those that have received cost information higher than expected.
                        All estimations include a respondent-level fixed effect. Coefficients in column (IV) can be understood as a split-sample estimation between opposers and non-opposers.
                        Sample excludes respondents that answered 'I don't know' in either period.
                        Sample excludes respondents that are part of the control group for the cost information treatment C, as observations for Q42\\_2 are lacking.
                        Standard errors clustered at the level of respondents in parentheses.")))

# 3.8.6  Mediation analysis ####

# Create dataframe with perception of costs / perception of fairness / perception of effectiveness and policy support, all standardized to first period.

adjust_3.8.6 <- function(data_3_0){
  
  data_3_8.6.1 <- data_3_0 %>%
    # Select variables of interest
    select(ID, Treatment_B, Treatment_C, 
           Dif_cost_1_ABS, Q41_1N, Q42_1N, Q45_1N, Q46_1N)%>%
    mutate(Period = 1)
  
  z_values <- data_3_8.6.1 %>%
    summarise(mean_Pre_Dif_cost_ABS = mean(Dif_cost_1_ABS, na.rm = TRUE),
              sd_Pre_Dif_cost_ABS   = sd(Dif_cost_1_ABS, na.rm = TRUE),
              mean_Pre_Q41_1N       = mean(Q41_1N, na.rm = TRUE),
              sd_Pre_Q41_1N         = sd(Q41_1N, na.rm = TRUE),
              mean_Pre_Q42_1N       = mean(Q42_1N, na.rm = TRUE),
              sd_Pre_Q42_1N         = sd(Q42_1N, na.rm = TRUE),
              mean_Pre_Q45_1N       = mean(Q45_1N, na.rm = TRUE),
              sd_Pre_Q45_1N         = sd(Q45_1N, na.rm = TRUE),
              mean_Pre_Q46_1N       = mean(Q46_1N, na.rm = TRUE),
              sd_Pre_Q46_1N         = sd(Q46_1N, na.rm = TRUE))
  
  data_3_8.6.1 <- data_3_8.6.1 %>%
    mutate(Dif_cost_1_ABS = (Dif_cost_1_ABS - z_values$mean_Pre_Dif_cost_ABS)/z_values$sd_Pre_Dif_cost_ABS,
           Q41_1N         = (Q41_1N - z_values$mean_Pre_Q41_1N)/z_values$sd_Pre_Q41_1N,
           Q42_1N         = (Q42_1N - z_values$mean_Pre_Q42_1N)/z_values$sd_Pre_Q42_1N,
           Q45_1N         = (Q45_1N - z_values$mean_Pre_Q45_1N)/z_values$sd_Pre_Q45_1N,
           Q46_1N         = (Q46_1N - z_values$mean_Pre_Q46_1N)/z_values$sd_Pre_Q46_1N)%>%
    rename(Effectiveness = Q41_1N, Fairness = Q45_1N, Support = Q46_1N, Dif_cost_ABS = Dif_cost_1_ABS, Costs = Q42_1N)
  
  data_3_8.6.2 <- data_3_0 %>%
    # Select variables of interest
    select(ID, Treatment_B, Treatment_C, 
           Dif_cost_2_ABS, Q41_2N, Q42_2N, Q45_2N, Q46_2N)%>%
    mutate(Period = 2)%>%
    mutate(Dif_cost_2_ABS = (Dif_cost_2_ABS - z_values$mean_Pre_Dif_cost_ABS)/z_values$sd_Pre_Dif_cost_ABS,
           Q41_2N         = (Q41_2N - z_values$mean_Pre_Q41_1N)/z_values$sd_Pre_Q41_1N,
           Q42_2N         = (Q42_2N - z_values$mean_Pre_Q42_1N)/z_values$sd_Pre_Q42_1N,
           Q45_2N         = (Q45_2N - z_values$mean_Pre_Q45_1N)/z_values$sd_Pre_Q45_1N,
           Q46_2N         = (Q46_2N - z_values$mean_Pre_Q46_1N)/z_values$sd_Pre_Q46_1N)%>%
    rename(Effectiveness = Q41_2N, Fairness = Q45_2N, Support = Q46_2N, Dif_cost_ABS = Dif_cost_2_ABS, Costs = Q42_2N)
  
  data_3.8.6.3 <- bind_rows(data_3_8.6.1, data_3_8.6.2)%>%
    mutate(Post_B_ONLY = ifelse(Period == 2 & Treatment_B == "Treatment" & Treatment_C == "Control",1,0),
           Post_C_ONLY = ifelse(Period == 2 & Treatment_B == "Control" & Treatment_C != "Control",1,0))%>%
    # Interactions
    mutate(Post_B_C  = ifelse(Period == 2 & Treatment_B == "Treatment" & Treatment_C != "Control",1,0))

  return(data_3.8.6.3)
}

model_3.8.6.1_ESP <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Fairness | ID, data = adjust_3.8.6(data_3_ESP), cluster = ~ ID)
model_3.8.6.2_ESP <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Effectiveness | ID, data = adjust_3.8.6(data_3_ESP), cluster = ~ ID)
model_3.8.6.3_ESP <- feols(Support ~ Post_C_ONLY + Post_B_C + Costs | ID, data = adjust_3.8.6(data_3_ESP), cluster = ~ ID)
model_3.8.6.4_ESP <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Fairness + Effectiveness | ID, data = adjust_3.8.6(data_3_ESP), cluster = ~ ID)
model_3.8.6.5_ESP <- feols(Support ~ Post_C_ONLY + Post_B_C + Fairness + Effectiveness + Costs | ID, data = adjust_3.8.6(data_3_ESP), cluster = ~ ID)

etable(model_3.8.6.2_ESP, model_3.8.6.1_ESP, model_3.8.6.3_ESP, model_3.8.6.4_ESP, model_3.8.6.5_ESP,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of changing policy perceptions on policy support in Spain",
       headers = c("(I)", "(II)", "(III)", "(IV)", "(V)"),
       label = "tab_A_Interaction_ESP",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_Interaction_ESP.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from five OLS regressions of policy perception variables on overall policy support among respondents from Spain.
                 All variables are standardized using z-scores based on average and SD policy perception variables and overall policy support in the first period.
                 Column (I) shows the correlation between the perception of effectiveness and support, controlling for video and cost information treatment effects.
                 Column (II) shows such results for the perception of fairness; Column (III) shows such results for the perception of additional costs;
                 Column (IV) shows such results for the perception of effectiveness and fairness combined. Column (V) includes all three policy perception variables.
                 Respondents not exposed to the cost information treatment are not asked about their cost perception a second time -- columns (III) and (IV) do thus not include no regressor for the video information treatment.
                 All estimations include the respondent-level fixed effect.
                 Samples excludes respondents that answered 'I don't know' in either period on either variable.
                        Standard errors clustered at the level of respondents in parentheses.")))

model_3.8.6.1_FRA <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Fairness                  | ID, data = adjust_3.8.6(data_3_FRA), cluster = ~ ID)
model_3.8.6.2_FRA <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Effectiveness             | ID, data = adjust_3.8.6(data_3_FRA), cluster = ~ ID)
model_3.8.6.3_FRA <- feols(Support ~ Post_C_ONLY + Post_B_C + Costs                            | ID, data = adjust_3.8.6(data_3_FRA), cluster = ~ ID)
model_3.8.6.4_FRA <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Fairness + Effectiveness  | ID, data = adjust_3.8.6(data_3_FRA), cluster = ~ ID)
model_3.8.6.5_FRA <- feols(Support ~ Post_C_ONLY + Post_B_C + Fairness + Effectiveness + Costs | ID, data = adjust_3.8.6(data_3_FRA), cluster = ~ ID)

etable(model_3.8.6.2_FRA, model_3.8.6.1_FRA, model_3.8.6.3_FRA, model_3.8.6.4_FRA, model_3.8.6.5_FRA,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of changing policy perceptions on policy support in France",
       headers = c("(I)", "(II)", "(III)", "(IV)", "(V)"),
       label = "tab_A_Interaction_FRA",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_Interaction_FRA.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from five OLS regressions of policy perception variables on overall policy support among respondents from France.
                 All variables are standardized using z-scores based on average and SD policy perception variables and overall policy support in the first period.
                 Column (I) shows the correlation between the perception of effectiveness and support, controlling for video and cost information treatment effects.
                 Column (II) shows such results for the perception of fairness; Column (III) shows such results for the perception of additional costs;
                 Column (IV) shows such results for the perception of effectiveness and fairness combined. Column (V) includes all three policy perception variables.
                 Respondents not exposed to the cost information treatment are not asked about their cost perception a second time -- columns (III) and (IV) do thus not include no regressor for the video information treatment.
                 All estimations include the respondent-level fixed effect.
                 Samples excludes respondents that answered 'I don't know' in either period on either variable.
                        Standard errors clustered at the level of respondents in parentheses.")))

model_3.8.6.1_GER <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Fairness                  | ID, data = adjust_3.8.6(data_3_GER), cluster = ~ ID)
model_3.8.6.2_GER <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Effectiveness             | ID, data = adjust_3.8.6(data_3_GER), cluster = ~ ID)
model_3.8.6.3_GER <- feols(Support ~ Post_C_ONLY + Post_B_C + Costs                            | ID, data = adjust_3.8.6(data_3_GER), cluster = ~ ID)
model_3.8.6.4_GER <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Fairness + Effectiveness  | ID, data = adjust_3.8.6(data_3_GER), cluster = ~ ID)
model_3.8.6.5_GER <- feols(Support ~ Post_C_ONLY + Post_B_C + Fairness + Effectiveness + Costs | ID, data = adjust_3.8.6(data_3_GER), cluster = ~ ID)

etable(model_3.8.6.2_GER, model_3.8.6.1_GER, model_3.8.6.3_GER, model_3.8.6.4_GER, model_3.8.6.5_GER,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of changing policy perceptions on policy support in Germany",
       headers = c("(I)", "(II)", "(III)", "(IV)", "(V)"),
       label = "tab_A_Interaction_GER",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_Interaction_GER.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from five OLS regressions of policy perception variables on overall policy support among respondents from Germany.
                 All variables are standardized using z-scores based on average and SD policy perception variables and overall policy support in the first period.
                 Column (I) shows the correlation between the perception of effectiveness and support, controlling for video and cost information treatment effects.
                 Column (II) shows such results for the perception of fairness; Column (III) shows such results for the perception of additional costs;
                 Column (IV) shows such results for the perception of effectiveness and fairness combined. Column (V) includes all three policy perception variables.
                 Respondents not exposed to the cost information treatment are not asked about their cost perception a second time -- columns (III) and (IV) do thus not include no regressor for the video information treatment.
                 All estimations include the respondent-level fixed effect.
                 Samples excludes respondents that answered 'I don't know' in either period on either variable.
                        Standard errors clustered at the level of respondents in parentheses.")))

model_3.8.6.1_ROM <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Fairness                  | ID, data = adjust_3.8.6(data_3_ROM), cluster = ~ ID)
model_3.8.6.2_ROM <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Effectiveness             | ID, data = adjust_3.8.6(data_3_ROM), cluster = ~ ID)
model_3.8.6.3_ROM <- feols(Support ~ Post_C_ONLY + Post_B_C + Costs                            | ID, data = adjust_3.8.6(data_3_ROM), cluster = ~ ID)
model_3.8.6.4_ROM <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Fairness + Effectiveness  | ID, data = adjust_3.8.6(data_3_ROM), cluster = ~ ID)
model_3.8.6.5_ROM <- feols(Support ~ Post_C_ONLY + Post_B_C + Fairness + Effectiveness + Costs | ID, data = adjust_3.8.6(data_3_ROM), cluster = ~ ID)

etable(model_3.8.6.2_ROM, model_3.8.6.1_ROM, model_3.8.6.3_ROM, model_3.8.6.4_ROM, model_3.8.6.5_ROM,
       se.below = TRUE, ci = 0.95, fitstat = ~ n + r2 + ar2, signif.code = NA, digits = 3, digits.stats = 2,
       replace = TRUE, style.tex = tex.style, tpt = TRUE,
       title = "Effects of changing policy perceptions on policy support in Romania",
       headers = c("(I)", "(II)", "(III)", "(IV)", "(V)"),
       label = "tab_A_Interaction_ROM",
       placement = "htbp!",
       dict = dict_latex,
       tex = TRUE,
       #extralines = list("Including disbelieving" = rep(c("Yes", "No"),3)),
       file = c("../6_EUETS2_Citizens_Survey/2_Tables/Table_A_Interaction_ROM.tex"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table shows results from five OLS regressions of policy perception variables on overall policy support among respondents from Romania.
                 All variables are standardized using z-scores based on average and SD policy perception variables and overall policy support in the first period.
                 Column (I) shows the correlation between the perception of effectiveness and support, controlling for video and cost information treatment effects.
                 Column (II) shows such results for the perception of fairness; Column (III) shows such results for the perception of additional costs;
                 Column (IV) shows such results for the perception of effectiveness and fairness combined. Column (V) includes all three policy perception variables.
                 Respondents not exposed to the cost information treatment are not asked about their cost perception a second time -- columns (III) and (IV) do thus not include no regressor for the video information treatment.
                 All estimations include the respondent-level fixed effect.
                 Samples excludes respondents that answered 'I don't know' in either period on either variable.
                        Standard errors clustered at the level of respondents in parentheses.")))


# Standard errors for ACMEs, if necessary.

# library(boot)
# 
# ids <- unique(adjust_3.8.6(data_3_ESP)$ID)
# 
# acme_boot <- function(ids_vec, idx) {
#   sampled_ids <- ids_vec[idx]
#   d <- adjust_3.8.6(data_3_ESP)[adjust_3.8.6(data_3_ESP)$ID %in% sampled_ids, ]
#   
#   m_f <- lm(Fairness      ~ Post_B_ONLY + Post_C_ONLY + Post_B_C, data = d)
#   m_e <- lm(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C, data = d)
#   o   <- lm(Support ~ Fairness + Effectiveness + Post_B_ONLY + Post_C_ONLY + Post_B_C, data = d)
#   
#   b_f <- coef(o)["Fairness"]
#   b_e <- coef(o)["Effectiveness"]
#   
#   c(
#     coef(m_f)["Post_B_ONLY"] * b_f,
#     coef(m_f)["Post_C_ONLY"] * b_f,
#     coef(m_f)["Post_B_C"]    * b_f,
#     coef(m_e)["Post_B_ONLY"] * b_e,
#     coef(m_e)["Post_C_ONLY"] * b_e,
#     coef(m_e)["Post_B_C"]    * b_e
#   )
# }
# 
# set.seed(42)
# B <- 1000
# 
# B <- 1000
# acme_mat <- matrix(NA, nrow = B, ncol = 6)
# 
# for (b in 1:B) {
#   sampled_ids <- sample(ids, length(ids), replace = TRUE)
#   d <- adjust_3.8.6(data_3_ESP)[adjust_3.8.6(data_3_ESP)$ID %in% sampled_ids, ]
#   
#   m_f <- lm(Fairness      ~ Post_B_ONLY + Post_C_ONLY + Post_B_C, data = d)
#   m_e <- lm(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C, data = d)
#   o   <- lm(Support ~ Fairness + Effectiveness + Post_B_ONLY + Post_C_ONLY + Post_B_C, data = d)
#   
#   b_f <- coef(o)["Fairness"]
#   b_e <- coef(o)["Effectiveness"]
#   
#   acme_mat[b, ] <- c(
#     coef(m_f)["Post_B_ONLY"] * b_f,
#     coef(m_f)["Post_C_ONLY"] * b_f,
#     coef(m_f)["Post_B_C"]    * b_f,
#     coef(m_e)["Post_B_ONLY"] * b_e,
#     coef(m_e)["Post_C_ONLY"] * b_e,
#     coef(m_e)["Post_B_C"]    * b_e
#   )
#   
#   if (b %% 100 == 0) cat("Iteration", b, "\n")
# }
# 
# acme_names <- c("acme_fair_B", "acme_fair_C", "acme_fair_BC",
#                 "acme_eff_B",  "acme_eff_C",  "acme_eff_BC")
# 
# acme_results <- data.frame(
#   term     = acme_names,
#   estimate = apply(acme_mat, 2, mean),
#   ci_low   = apply(acme_mat, 2, quantile, 0.025),
#   ci_high  = apply(acme_mat, 2, quantile, 0.975)
# )
# 
# print(acme_results)

# 3.8.7  Mediation analysis (identical sample) ####

transform_3.8.7 <- function(data_3_0){
  data_3_3_1 <- data_3_0 %>%
    select(ID, Treatment_B, Treatment_C, Q41_1N, Q45_1N, Q46_1N)%>%
    mutate(Period  = 1)%>%
    rename(Effectiveness = Q41_1N, Fairness = Q45_1N, Support = Q46_1N)%>%
    mutate(Post_B = ifelse(Period == 2 & Treatment_B == "Treatment",1,0),
           Post_C = ifelse(Period == 1, "Baseline", as.character(Treatment_C)))%>%
    mutate(Post_B_ONLY = ifelse(Period == 2 & Treatment_B == "Treatment" & Treatment_C == "Control",1,0),
           Post_C_ONLY = ifelse(Period == 2 & Treatment_B == "Control" & Treatment_C != "Control",1,0),
           Post_B_C    = ifelse(Period == 2 & Treatment_B == "Treatment" & Treatment_C != "Control",1,0))
  
  data_3_3_2 <- data_3_0 %>%
    select(ID, Treatment_B, Treatment_C, Q41_2N, Q45_2N, Q46_2N)%>%
    mutate(Period  = 2)%>%
    rename(Effectiveness = Q41_2N, Fairness = Q45_2N, Support = Q46_2N)%>%
    mutate(Post_B = ifelse(Period == 2 & Treatment_B == "Treatment",1,0),
           Post_C = ifelse(Period == 1, "Baseline", as.character(Treatment_C)))%>%
    mutate(Post_B_ONLY = ifelse(Period == 2 & Treatment_B == "Treatment" & Treatment_C == "Control",1,0),
           Post_C_ONLY = ifelse(Period == 2 & Treatment_B == "Control" & Treatment_C != "Control",1,0),
           Post_B_C    = ifelse(Period == 2 & Treatment_B == "Treatment" & Treatment_C != "Control",1,0))
  
  data_3_3_3 <- bind_rows(data_3_3_1, data_3_3_2)
  
  # Correction for z-values
  z_values_Support <- data_3_3_1 %>%
    summarise(mean_Pre = mean(Support, na.rm = TRUE),
              sd_Pre   = sd(Support, na.rm = TRUE))
  
  z_values_Fairness <- data_3_3_1 %>%
    summarise(mean_Pre = mean(Fairness, na.rm = TRUE),
              sd_Pre   = sd(Fairness, na.rm = TRUE))
  
  z_values_Effectiveness <- data_3_3_1 %>%
    summarise(mean_Pre = mean(Effectiveness, na.rm = TRUE),
              sd_Pre   = sd(Effectiveness, na.rm = TRUE))
  
  data_3_3_4 <- data_3_3_3 %>%
    mutate(Fairness      = (Fairness - z_values_Fairness$mean_Pre)/z_values_Fairness$sd_Pre,
           Support       = (Support - z_values_Support$mean_Pre)/z_values_Support$sd_Pre,
           Effectiveness = (Effectiveness - z_values_Effectiveness$mean_Pre)/z_values_Effectiveness$sd_Pre)%>%
    arrange(ID, Period)%>%
    group_by(ID)%>%
    mutate(NAS = sum(is.na(Fairness)) + sum(is.na(Support)) + sum(is.na(Effectiveness)))%>%
    ungroup()%>%
    # Same sample across specifications
    filter(NAS == 0)
  
  return(data_3_3_4)
}

data_3.8.7_ESP <- transform_3.8.7(data_3_ESP)
data_3.8.7_FRA <- transform_3.8.7(data_3_FRA)
data_3.8.7_GER <- transform_3.8.7(data_3_GER)
data_3.8.7_ROM <- transform_3.8.7(data_3_ROM)

# Model 1: Treatment on support
model_1_ESP <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C | ID, data = data_3.8.7_ESP, cluster = ~ ID)
model_1_FRA <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C | ID, data = data_3.8.7_FRA, cluster = ~ ID)
model_1_GER <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C | ID, data = data_3.8.7_GER, cluster = ~ ID)
model_1_ROM <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C | ID, data = data_3.8.7_ROM, cluster = ~ ID)

# Model 2: Treatment on fairness
model_2_ESP <- feols(Fairness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C | ID, data = data_3.8.7_ESP, cluster = ~ ID)
model_2_FRA <- feols(Fairness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C | ID, data = data_3.8.7_FRA, cluster = ~ ID)
model_2_GER <- feols(Fairness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C | ID, data = data_3.8.7_GER, cluster = ~ ID)
model_2_ROM <- feols(Fairness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C | ID, data = data_3.8.7_ROM, cluster = ~ ID)

# Model 3: Treatment on effectiveness
model_3_ESP <- feols(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C | ID, data = data_3.8.7_ESP, cluster = ~ ID)
model_3_FRA <- feols(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C | ID, data = data_3.8.7_FRA, cluster = ~ ID)
model_3_GER <- feols(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C | ID, data = data_3.8.7_GER, cluster = ~ ID)
model_3_ROM <- feols(Effectiveness ~ Post_B_ONLY + Post_C_ONLY + Post_B_C | ID, data = data_3.8.7_ROM, cluster = ~ ID)

# Model 4: Mediation: Fairness
model_4_ESP <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Fairness | ID, data = data_3.8.7_ESP, cluster = ~ ID)
model_4_FRA <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Fairness | ID, data = data_3.8.7_FRA, cluster = ~ ID)
model_4_GER <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Fairness | ID, data = data_3.8.7_GER, cluster = ~ ID)
model_4_ROM <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Fairness | ID, data = data_3.8.7_ROM, cluster = ~ ID)

# Model 5: Mediation: Effectiveness
model_5_ESP <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Effectiveness | ID, data = data_3.8.7_ESP, cluster = ~ ID)
model_5_FRA <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Effectiveness | ID, data = data_3.8.7_FRA, cluster = ~ ID)
model_5_GER <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Effectiveness | ID, data = data_3.8.7_GER, cluster = ~ ID)
model_5_ROM <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Effectiveness | ID, data = data_3.8.7_ROM, cluster = ~ ID)

# Model 6: Mediation: Fairness and Effectiveness
model_6_ESP <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Fairness + Effectiveness | ID, data = data_3.8.7_ESP, cluster = ~ ID)
model_6_FRA <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Fairness + Effectiveness | ID, data = data_3.8.7_FRA, cluster = ~ ID)
model_6_GER <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Fairness + Effectiveness | ID, data = data_3.8.7_GER, cluster = ~ ID)
model_6_ROM <- feols(Support ~ Post_B_ONLY + Post_C_ONLY + Post_B_C + Fairness + Effectiveness | ID, data = data_3.8.7_ROM, cluster = ~ ID)

table_3.8.7 <- data.frame("Country" = c("Spain", "France", "Germany", "Romania"),
                          "A_0" = c((coef(model_1_ESP)[["Post_B_ONLY"]]),
                                    (coef(model_1_FRA)[["Post_B_ONLY"]]),
                                    (coef(model_1_GER)[["Post_B_ONLY"]]),
                                    (coef(model_1_ROM)[["Post_B_ONLY"]])),
                          "B_0" = c(coef(model_2_ESP)[["Post_B_ONLY"]],
                                    coef(model_2_FRA)[["Post_B_ONLY"]],
                                    coef(model_2_GER)[["Post_B_ONLY"]],
                                    coef(model_2_ROM)[["Post_B_ONLY"]]),
                          "B_1" = c(coef(model_4_ESP)[["Fairness"]],
                                    coef(model_4_FRA)[["Fairness"]],
                                    coef(model_4_GER)[["Fairness"]],
                                    coef(model_4_ROM)[["Fairness"]]),
                          "B_3" = c(coef(model_4_ESP)[["Post_B_ONLY"]],
                                    coef(model_4_FRA)[["Post_B_ONLY"]],
                                    coef(model_4_GER)[["Post_B_ONLY"]],
                                    coef(model_4_ROM)[["Post_B_ONLY"]]),
                          "C_0" = c(coef(model_3_ESP)[["Post_B_ONLY"]],
                                    coef(model_3_FRA)[["Post_B_ONLY"]],
                                    coef(model_3_GER)[["Post_B_ONLY"]],
                                    coef(model_3_ROM)[["Post_B_ONLY"]]),
                          "C_1" = c(coef(model_5_ESP)[["Effectiveness"]],
                                    coef(model_5_FRA)[["Effectiveness"]],
                                    coef(model_5_GER)[["Effectiveness"]],
                                    coef(model_5_ROM)[["Effectiveness"]]),
                          "C_3" = c(coef(model_5_ESP)[["Post_B_ONLY"]],
                                    coef(model_5_FRA)[["Post_B_ONLY"]],
                                    coef(model_5_GER)[["Post_B_ONLY"]],
                                    coef(model_5_ROM)[["Post_B_ONLY"]]),
                          "D_1" = c(coef(model_6_ESP)[["Fairness"]],
                                    coef(model_6_FRA)[["Fairness"]],
                                    coef(model_6_GER)[["Fairness"]],
                                    coef(model_6_ROM)[["Fairness"]]),
                          "D_3" = c(coef(model_6_ESP)[["Effectiveness"]],
                                    coef(model_6_FRA)[["Effectiveness"]],
                                    coef(model_6_GER)[["Effectiveness"]],
                                    coef(model_6_ROM)[["Effectiveness"]]),
                          "D_5" = c(coef(model_6_ESP)[["Post_B_ONLY"]],
                                    coef(model_6_FRA)[["Post_B_ONLY"]],
                                    coef(model_6_GER)[["Post_B_ONLY"]],
                                    coef(model_6_ROM)[["Post_B_ONLY"]]))%>%
  mutate(B_2 = B_0*B_1,
         C_2 = C_0*C_1,
         D_2 = B_0*D_1,
         D_4 = C_0*D_3)%>%
  select(Country, A_0, 
         B_0, B_1, B_2, B_3, 
         C_0, C_1, C_2, C_3, 
         D_1, D_2, D_3, D_4, D_5)

kbl(table_3.8.7, digits = 2, escape = FALSE, format = "latex", booktabs = TRUE, label = "interaction_exercise",
    caption = "Stylized Mediation Analysis",
    col.names = c("Country",
                  "$\\beta_{1,\\Omega,B}$",
                  "$\\beta_{1,F,B}$",
                  "$\\beta_{2,F}$",
                  "$\\delta_{F,B}$",
                  "$\\beta_{2,B}$",
                  "$\\beta_{1,E,B}$",
                  "$\\beta_{2,E}$",
                  "$\\delta_{E,B}$",
                  "$\\beta_{2,B}$",
                  "$\\beta_{2,F,a}$",
                  "$\\delta_{F,B,a}$",
                  "$\\beta_{2,E,a}$",
                  "$\\delta_{E,B,a}$",
                  "$\\beta_{2,B,a}$"))%>%
  column_spec(c(1, 2, 6, 10), border_right = TRUE)%>%
  kable_styling(latex_options = "scale_down") %>%
  save_kable(., "../6_EUETS2_Citizens_Survey/2_Tables/Table_A_Interaction_Analysis.tex")

# 4.     Joint Figures ####
# 4.1    Figure 1 (Panel a and Panel b) ####

data_4.1.1 <- data_2 %>%
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
  # mutate(label_0 = paste0(round(share,2)*100, "%"))%>%
  mutate(Country = factor(Country, levels = c("Spain", "France", "Germany", "Romania")))%>%
  mutate(Q46_1N_label = case_when(Q46_1N == 1 ~ "Strongly\noppose",
                                  Q46_1N == 2 ~ "Rather\noppose",
                                  Q46_1N == 3 ~ "Neutral",
                                  Q46_1N == 4 ~ "Rather\nsupport",
                                  Q46_1N == 5 ~ "Strongly\nsupport"))%>%
  mutate(Q46_1N_label = factor(Q46_1N_label, levels = c("Neutral", "Rather\noppose", "Strongly\noppose", "Rather\nsupport", "Strongly\nsupport")))%>%
  mutate(Period = "t=0")%>%
  mutate(share = ifelse(Q46_1N < 3, -share, share))%>%
  mutate(share = ifelse(Q46_1N == 3, share/2, share))%>%
  mutate(side = ifelse(Q46_1N == 3, "right", NA))

data_4.1.2 <- data_4.1.1 %>%
  bind_rows(mutate(mutate(filter(data_4.1.1, Q46_1N == 3), share = -share), side = "left"))%>%
  arrange(Country, Q46_1N)

P_4.1.1 <- ggplot(data_4.1.2, aes(x = share, y = fct_rev(Country), fill = fct_rev(Q46_1N_label)))+
  geom_col(position = "stack", colour = "black", width = 0.65, linewidth = 0.2)+
  geom_vline(aes(xintercept = 0), linewidth = 0.3)+
  theme_bw()+
  coord_cartesian(xlim = c(-0.76,0.76))+
  scale_fill_manual(guide = guide_legend(title.position = "top", nrow = 1),
                    values = c("#DC0000FF", "#E64B35FF", "#B09C85FF", "#91D1C2FF", "#00A087FF"),
                    breaks = c("Strongly\noppose", "Rather\noppose", "Neutral", "Rather\nsupport", "Strongly\nsupport"))+
  # labs(fill = "Do you support or oppose the EU ETS2?")+
  labs(fill = "")+
  # guides(fill = "none")+
  scale_x_continuous(labels = \(x) scales::percent(abs(x)),
                     breaks = c(-0.75,-0.5,-0.25,0,0.25,0.5,0.75),
                     position = "top")+
  xlab("Share of respondents")+
  ylab("Country")+
  ggtitle("a) Do you support or oppose the EU ETS2?")+
  theme(panel.grid.minor  = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.2),
        axis.ticks = element_line(linewidth = 0.2),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0),
        title = element_text(size = 7),
        legend.position = "bottom",
        legend.key.width = unit(0.28, "cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.spacing.x = unit(0, "cm"),
        legend.box.just = "left",
        # legend.title = element_text(hjust = 0.5, size = 8),
        legend.title = element_blank(),
        legend.text = element_text(size = 5))

shap_a <- data.frame()
shap_b <- data.frame()

for(i in c("Spain", "France", "Germany", "Romania")){
  for(j in c("support_2", "support_3", "support_4", "support_5")){
    
    shap_2.2.1.1 <- read_parquet(sprintf("../2_Data/1_Support_Datasets/1_SHAP_1/SHAP_%s_%s.parquet", i, j))
    data_2.2.1.1 <- read_parquet(sprintf("../2_Data/1_Support_Datasets/1_SHAP_1/Data_%s_%s.parquet", i, j))
    
    shap_2.2.1.1.0 <- shap_2.2.1.1 %>%
      summarise_all(~ mean(abs(.)))%>%
      select(-"(Intercept)")%>%
      pivot_longer(everything(), names_to = "variable", values_to = "SHAP_contribution")%>%
      arrange(desc(SHAP_contribution))%>%
      mutate(tot_contribution = sum(SHAP_contribution))%>%
      mutate(share_SHAP       = SHAP_contribution/tot_contribution)%>%
      select(-tot_contribution)%>%
      mutate(Country = i,
             Outcome = j)
    
    shap_2.2.1.1.1 <- shap_2.2.1.1 %>%
      summarise_all(~ mean(.))%>%
      select(-"(Intercept)")%>%
      pivot_longer(everything(), names_to = "variable", values_to = "SHAP_contribution")%>%
      mutate(Variable   = ifelse(variable != "Q42_1_relative", str_replace(variable, "_[^_]+$", ""), variable))%>%
      group_by(Variable)%>%
      summarise(direction = sign(SHAP_contribution[which.max(abs(SHAP_contribution))]))%>%
      ungroup()%>%
      mutate(Country = i,
             Outcome = j)
    
    shap_2.2.1.2.0 <- shap_2.2.1.1.0 %>%
      mutate(Variable   = ifelse(variable != "Q42_1_relative", str_replace(variable, "_[^_]+$", ""), variable))%>%
      group_by(Variable)%>%
      summarise(share_SHAP = sum(share_SHAP))%>%
      ungroup()%>%
      arrange(desc(share_SHAP))%>%
      mutate(Country = i,
             Outcome = j)%>%
      left_join(shap_2.2.1.1.1)
    
    shap_a <- shap_a %>%
      bind_rows(shap_2.2.1.1.0)
    
    shap_b <- shap_b %>%
      bind_rows(shap_2.2.1.2.0)
    
  }
}

shap_b_3 <- shap_b %>%
  pivot_wider(names_from = "Variable", values_from = "share_SHAP", values_fill = 0)%>%
  pivot_longer(-c(Country, Outcome), names_to = "Variable", values_to = "share_SHAP")%>%
  group_by(Outcome, Variable)%>%
  summarise(mean_SHAP = mean(share_SHAP))%>%
  arrange(desc(mean_SHAP))%>%
  mutate(rank = 1:n())%>%
  ungroup()%>%
  filter(rank < 6)

shap_b_3.1 <- shap_b_3 %>%
  filter(Outcome == "support_3")
  
shap_b_4 <- shap_b %>%
  filter(Outcome == "support_3")%>%
  left_join(shap_b_3.1)%>%
  filter(!is.na(rank))%>%
  # Data transformation
  mutate(Country = factor(Country, levels = c("Spain", "France", "Germany", "Romania")))%>%
  mutate(direction = factor(direction))%>%
  mutate(Variable = case_when(Variable == "Q45_1"          ~ "Perception\nfairness",
                              Variable == "Q44_1"          ~ "Effects on\nvulnerable",
                              Variable == "Q43_1"          ~ "Relative costs (Q43)",
                              Variable == "Q42_1_relative" ~ "Individual costs [%] (Q42)",
                              Variable == "Q42_1_true"     ~ "Individual costs (Q42)",
                              Variable == "Q41_1"          ~ "Perception\neffectiveness",
                              Variable == "Q38"            ~ "Political\nparty",
                              Variable == "Q37_c"          ~ "Impact on emissions (Q37)",
                              Variable == "Q37_b"          ~ "Impact on life (Q37)",
                              Variable == "Q37_a"          ~ "Impact on economy (Q37)",
                              Variable == "Q36"            ~ "Climate change\nconcern",
                              Variable == "Q35_1"          ~ "Communication: Public (Q35)",
                              Variable == "Q35_4"          ~ "Communcation: Scientists (Q35)",
                              Variable == "Q31_Gov_nat"    ~ "Integrity national gov. (Q31)",
                              Variable == "Q31_EU_Comm"    ~ "Integrity EU commission (Q31)",
                              Variable == "Q30_1"          ~ "Trust in local gov. (Q30)",
                              Variable == "Q30_2"          ~ "Trust in national gov. (Q30)",
                              Variable == "Q30_3"          ~ "Trust in EU (Q30)",
                              Variable == "Q28"            ~ "Expenditures (Q28)",
                              Variable == "noise"          ~ "Random term",
                              Variable == "Q10"            ~ "Age (Q10)",
                              TRUE ~ Variable))%>%
  mutate(Variable = fct_reorder(Variable, rank))%>%
  mutate(label_0 = paste0(round(share_SHAP*100,0),"%"))

P_4.1.2 <- ggplot(data = shap_b_4, aes(y = fct_rev(Country), x = Variable))+
  # geom_point(shape = 22,fill = NA,colour = "black",stroke = 0.3,size = 9)+
  geom_point(aes(alpha = share_SHAP), shape = 22, size = 14, fill = "#3C5488FF", colour = "black", stroke = 0.3)+
  geom_point(shape = 22, size = 14, fill = NA, colour = "black", stroke = 0.3)+
  geom_text(aes(label = label_0), size = 2.5)+
  scale_alpha_continuous(range = c(0,0.7))+
  scale_x_discrete(position = "top")+
  # geom_point(alpha = 0.85, shape = 21, fill = "#4DBBD5FF", colour = "black")+
  # scale_size_continuous(range = c(2,8),
  #                       breaks = c(0.01, 0.05, 0.1, 0.2, 0.5),
  #                       name    = "Average SHAP Contribution", 
  #                       labels = percent)+l
  theme_bw()+
  xlab("Predictor")+
  ylab("Country")+
  ggtitle("b) Average Importance (SHAP): Opposition")+
  guides(alpha = "none")+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        axis.ticks = element_line(linewidth = 0.2),
        axis.text  = element_text(size = 5),
        axis.title = element_text(size = 7),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        title = element_text(size = 7),
        legend.text = element_text(size = 8),
        legend.title = element_text(hjust = 0.5, size = 8))
  
P_4.1 <- ggarrange(P_4.1.1, P_4.1.2, align = "h", widths = c(1.2,1))


pdf("../6_EUETS2_Citizens_Survey/1_Figures/Figure_1.pdf", width = 140/25.4, height = 90/25.4)
print(P_4.1)
dev.off()

jpeg("../5_Analysis/2_Presentation/A_Figure_1_Panela.jpeg", width = 140 / 25.4, height = 90 / 25.4, units = "in", res = 300, quality = 100)
print(P_4.1.1)
dev.off()

jpeg("../5_Analysis/2_Presentation/A_Figure_1_Panelb.jpeg", width = 140 / 25.4, height = 90 / 25.4, units = "in", res = 300, quality = 100)
print(P_4.1.2)
dev.off()

jpeg("../5_Analysis/2_Presentation/A_Figure_1_Panel.jpeg", width = 140 / 25.4, height = 90 / 25.4, units = "in", res = 300, quality = 100)
print(P_4.1)
dev.off()

shap_b_5 <- shap_b %>%
  filter(Outcome == "support_3")%>%
  group_by(Variable)%>%
  summarise(share_SHAP = mean(share_SHAP))%>%
  ungroup()

rm(data_2.1.1, data_2.1.1.1, data_2.1.1.2, P_2.1.1, P_2.1.2, data_2.1.1.3, data_2.1.1.4, P_2.1.4)
# 4.2    Figure Appendix (Average Feature Importance across models) ####

shap_a <- data.frame()
shap_b1 <- data.frame()
shap_b2 <- data.frame()

# With policy perception variables
for(i in c("Spain", "France", "Germany", "Romania")){
  for(j in c("support_2", "support_3", "support_4", "support_5")){
    
    shap_2.2.1.1 <- read_parquet(sprintf("../2_Data/1_Support_Datasets/1_SHAP_1/SHAP_%s_%s.parquet", i, j))
    data_2.2.1.1 <- read_parquet(sprintf("../2_Data/1_Support_Datasets/1_SHAP_1/Data_%s_%s.parquet", i, j))
    
    shap_2.2.1.1.0 <- shap_2.2.1.1 %>%
      summarise_all(~ mean(abs(.)))%>%
      select(-"(Intercept)")%>%
      pivot_longer(everything(), names_to = "variable", values_to = "SHAP_contribution")%>%
      arrange(desc(SHAP_contribution))%>%
      mutate(tot_contribution = sum(SHAP_contribution))%>%
      mutate(share_SHAP       = SHAP_contribution/tot_contribution)%>%
      select(-tot_contribution)%>%
      mutate(Country = i,
             Outcome = j)
    
    shap_2.2.1.1.1 <- shap_2.2.1.1 %>%
      summarise_all(~ mean(.))%>%
      select(-"(Intercept)")%>%
      pivot_longer(everything(), names_to = "variable", values_to = "SHAP_contribution")%>%
      mutate(Variable   = ifelse(variable != "Q42_1_relative", str_replace(variable, "_[^_]+$", ""), variable))%>%
      group_by(Variable)%>%
      summarise(direction = sign(SHAP_contribution[which.max(abs(SHAP_contribution))]))%>%
      ungroup()%>%
      mutate(Country = i,
             Outcome = j)
    
    shap_2.2.1.2.0 <- shap_2.2.1.1.0 %>%
      mutate(Variable   = ifelse(variable != "Q42_1_relative", str_replace(variable, "_[^_]+$", ""), variable))%>%
      group_by(Variable)%>%
      summarise(share_SHAP = sum(share_SHAP))%>%
      ungroup()%>%
      arrange(desc(share_SHAP))%>%
      mutate(Country = i,
             Outcome = j)%>%
      left_join(shap_2.2.1.1.1)
    
    shap_b1 <- shap_b1 %>%
      bind_rows(shap_2.2.1.2.0)
    
  }
}

# Without policy perception variables
for(i in c("Spain", "France", "Germany", "Romania")){
  for(j in c("support_2", "support_3", "support_4", "support_5")){
    
    shap_2.3.1.1 <- read_parquet(sprintf("../2_Data/1_Support_Datasets/1_SHAP_1/SHAP_wo_%s_%s.parquet", i, j))
    data_2.3.1.1 <- read_parquet(sprintf("../2_Data/1_Support_Datasets/1_SHAP_1/Data_wo_%s_%s.parquet", i, j))
    
    shap_2.3.1.1.0 <- shap_2.3.1.1 %>%
      summarise_all(~ mean(abs(.)))%>%
      select(-"(Intercept)")%>%
      pivot_longer(everything(), names_to = "variable", values_to = "SHAP_contribution")%>%
      arrange(desc(SHAP_contribution))%>%
      mutate(tot_contribution = sum(SHAP_contribution))%>%
      mutate(share_SHAP       = SHAP_contribution/tot_contribution)%>%
      select(-tot_contribution)%>%
      mutate(Country = i,
             Outcome = j)
    
    shap_2.3.1.1.1 <- shap_2.3.1.1 %>%
      summarise_all(~ mean(.))%>%
      select(-"(Intercept)")%>%
      pivot_longer(everything(), names_to = "variable", values_to = "SHAP_contribution")%>%
      mutate(Variable   = str_replace(variable, "_[^_]+$", ""))%>%
      group_by(Variable)%>%
      summarise(direction = sign(SHAP_contribution[which.max(abs(SHAP_contribution))]))%>%
      ungroup()%>%
      mutate(Country = i,
             Outcome = j)
    
    shap_2.3.1.2.0 <- shap_2.3.1.1.0 %>%
      mutate(Variable   = str_replace(variable, "_[^_]+$", ""))%>%
      group_by(Variable)%>%
      summarise(share_SHAP = sum(share_SHAP))%>%
      ungroup()%>%
      arrange(desc(share_SHAP))%>%
      mutate(Country = i,
             Outcome = j)%>%
      left_join(shap_2.3.1.1.1)
    
    shap_b2 <- shap_b2 %>%
      bind_rows(shap_2.3.1.2.0)
    
  }
}

rm(shap_a)

shap_b1 <- shap_b1 %>%
  mutate(full_model = "Yes")

shap_b2 <- shap_b2 %>%
  mutate(full_model = "No")

shap_4 <- bind_rows(shap_b1, shap_b2)%>%
  select(-direction)

shap_4.0 <- shap_4 %>%
  group_by(Variable)%>%
  summarise(mean_SHAP = mean(share_SHAP))%>%
  ungroup()%>%
  arrange(desc(mean_SHAP))%>%
  mutate(rank = 1:n())%>%
  mutate(VAR = ifelse(Variable %in% c("Q41_1", "Q42_1_true", "Q42_1_relative", "Q43_1", "Q44_1", "Q45_1") | rank < 11, Variable, "Other"))%>%
  select(Variable, VAR)

shap_4.1 <- shap_4 %>%
  left_join(shap_4.0)%>%
  # Combine "Other"
  group_by(Country, Outcome, full_model, VAR)%>%
  summarise(share_SHAP = sum(share_SHAP))%>%
  ungroup()%>%
  mutate(Country = factor(Country, levels = c("Spain", "France", "Germany", "Romania")))%>%
  mutate(Model = case_when(Outcome == "support_2" & full_model == "Yes" ~ "> Rather oppose (Incl.)",
                           Outcome == "support_2" & full_model == "No"  ~ "> Rather oppose (Excl.)",
                           Outcome == "support_3" & full_model == "Yes" ~ "> Neutral (Incl.)",
                           Outcome == "support_3" & full_model == "No"  ~ "> Neutral (Excl.)",
                           Outcome == "support_4" & full_model == "Yes" ~ "> Rather support (Incl.)",
                           Outcome == "support_4" & full_model == "No"  ~ "> Rather support (Excl.)",
                           Outcome == "support_5" & full_model == "Yes" ~ "> Strongly support (Incl.)",
                           Outcome == "support_5" & full_model == "No"  ~ "> Strongly support (Excl.)"))%>%
  mutate(Model = factor(Model, levels = c("> Rather oppose (Incl.)",
                                          "> Rather oppose (Excl.)",
                                          "> Neutral (Incl.)",
                                          "> Neutral (Excl.)",
                                          "> Rather support (Incl.)",
                                          "> Rather support (Excl.)",
                                          "> Strongly support (Incl.)",
                                          "> Strongly support (Excl.)")))%>%
  mutate(VAR = case_when(VAR == "Q45_1"          ~ "Q45: Fairness perception",
                         VAR == "Q44_1"          ~ "Q44: Effects on vulnerable",
                         VAR == "Q43_1"          ~ "Q43: Relative costs",
                         VAR == "Q42_1_relative" ~ "Q42: Individual costs [%]",
                         VAR == "Q42_1_true"     ~ "Q42: Individual costs [€]",
                         VAR == "Q41_1"          ~ "Q41: Effectiveness perception",
                         # VAR == "Q38"            ~ "Political Party (Q38)",
                         VAR == "Q37_c"          ~ "Q37: Impact on emissions",
                         VAR == "Q37_b"          ~ "Q37: Impact on life",
                         VAR == "Q37_a"          ~ "Q37: Impact on economy",
                         VAR == "Q36"            ~ "Q36: Climate change concern",
                         # VAR == "Q35_1"          ~ "Communication: Public (Q35)",
                         # VAR == "Q35_4"          ~ "Communcation: Scientists (Q35)",
                         VAR == "Q31_Gov_nat"    ~ "Q31: Integrity national gov.",
                         VAR == "Q31_EU_Comm"    ~ "Q31: Integrity EU commission",
                         VAR == "Q30_1"          ~ "Trust in local gov. (Q30)",
                         VAR == "Q30_2"          ~ "Trust in national gov. (Q30)",
                         VAR == "Q30_3"          ~ "Q30: Trust in EU",
                         # VAR == "Q28"            ~ "Expenditures (Q28)",
                         # VAR == "noise"          ~ "Random term",
                         VAR == "Other"            ~ "Other features (Sum)",
                              TRUE ~ VAR))%>%
  mutate(VAR = factor(VAR, levels = c("Q41: Effectiveness perception", "Q42: Individual costs [€]", "Q42: Individual costs [%]", "Q43: Relative costs", "Q44: Effects on vulnerable", "Q45: Fairness perception",
                                      "Q30: Trust in EU", "Q31: Integrity national gov.", "Q31: Integrity EU commission", "Q36: Climate change concern", "Q37: Impact on economy", "Q37: Impact on life", "Q37: Impact on emissions", "Other features (Sum)")))%>%
  arrange(Country, Model)%>%
  mutate(label_0 = paste0(round(share_SHAP*100,0),"%"))

P_4.2 <- ggplot(shap_4.1, aes(x = VAR, y = fct_rev(Model)))+
  facet_grid(Country ~ ., switch = "y")+
  geom_point(aes(fill = share_SHAP), shape = 22, size = 6, colour = "black", stroke = 0.3)+
  scale_fill_gradient(low = "white", high = "#3C5488FF", trans = "log1p")+
  guides(fill = "none")+
  geom_text(aes(label = label_0), size = 1.8)+
  scale_x_discrete(position = "top")+
  theme_bw()+
  ylab("Model")+
  xlab("Feature/Predictor")+
  guides(alpha = "none")+
  theme(legend.position = "bottom",
        strip.placement    = "outside",
        axis.title.y       = element_blank(),
        strip.background   = element_blank(),
        strip.text         = element_text(size = 7),
        panel.grid.major   = element_blank(),
        # panel.grid.major.y = element_line(linewidth = 0.2),
        axis.ticks = element_line(linewidth = 0.2),
        axis.text.x  = element_text(size = 7, vjust = 0, hjust = 0, angle = 90),
        axis.text.y  = element_text(size = 7, hjust = 1, vjust = 0.5),
        axis.title = element_text(size = 8),
        title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(hjust = 0.5, size = 8))

pdf("../6_EUETS2_Citizens_Survey/1_Figures/A_Figure_Feature_Importance.pdf", width = 130/25.4, height = 200/25.4)
print(P_4.2)
dev.off()
  
# 5.     Joint Tables ####

data_5.1 <- read.xlsx("../2_Data/1_Support_Datasets/1_SHAP_1/Performance_SHAP_1.xlsx")%>%
  mutate(full_model = "Yes")
data_5.2 <- read.xlsx("../2_Data/1_Support_Datasets/1_SHAP_1/Performance_SHAP_wo_1.xlsx")%>%
  mutate(full_model = "No")

data_5_combined <- bind_rows(data_5.1, data_5.2)%>%
  mutate(Country = factor(Country, levels = c("Spain", "France", "Germany", "Romania")))%>%
  mutate(full_model = factor(full_model, levels = c("Yes", "No")))%>%
  mutate(Outcome = case_when(Outcome == "support_2" ~ "$\\geq$ Rather oppose",
                             Outcome == "support_3" ~ "$\\geq$ Neutral",
                             Outcome == "support_4" ~ "$\\geq$ Rather support",
                             Outcome == "support_5" ~ "$\\geq$ Strongly support"))%>%
  mutate(Outcome = factor(Outcome, levels = c("$\\geq$ Rather oppose", "$\\geq$ Neutral", "$\\geq$ Rather support", "$\\geq$ Strongly support")))%>%
  arrange(Country, Outcome, full_model)%>%
  select(Country, Outcome, full_model, test_sample, test_class, ROC_AUC, ROC_AUC_CI_1, ROC_AUC_CI_2, sens, spec)%>%

  mutate(CI = paste0("(", round(ROC_AUC_CI_1,2), "; " , round(ROC_AUC_CI_2,2),")"))%>%
  select(Country:ROC_AUC, CI, sens, spec)%>%
  rename("Full model" = full_model, n = test_sample, "n positive" = test_class, AUC = ROC_AUC, "AUC CI" = CI, "Sens." = sens, "Spec." = spec)

kbl(data_5_combined, format = "latex", linesep = "", booktabs = T, caption = "Performance of boosted classification tree models across countries and levels of overall policy support",
    format.args = list(big.mark = ",", scientific = FALSE), align = "llcrrcccc", label = "model_performance", digits = 2, na = "",
    escape = FALSE)%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"), font_size = 9)%>%
  row_spec(c(8,16,24), hline_after = TRUE)%>%
  footnote(general = "This table shows performance metrics for boosted classification tree models that predict different levels of outcome for different countries. For example, the outcome '$\\geq$ Neutral' describes whether respondents have answered 'Neutral', 'Rather support', 'Strongly support' or not to Q46_1 (Do you support or oppose this policy?).
           This table shows such metrics for two models -- one including all policy perception variables (Q41_1 to Q45_1) as predictors ('Yes' in column 'Full model') and one without ('No' in column 'Full model').
           The table reports the sample size of the test set used for model evaluation and the number of positive cases,
           area under the curve (AUC) and 95% bootstrap CI, sensitivity and specificity.
           Sensitivity and specificity calculated at a classification threshold of 0.5. Evaluation of all model on test data that was not used for training or hyperparameter tuning.", threeparttable = T)%>%
  save_kable(., "../6_EUETS2_Citizens_Survey/2_Tables/Table_C_Performance.tex")
