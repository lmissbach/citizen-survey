# 0     General ####
# Author: L. Missbach (missbach@mcc-berlin.net)
# Date: 7th of January 2026

# 0.1   Packages ####

if(!require("pacman")) install.packages("pacman")

p_load("arrow", "boot", "broom", "extrafont", "fixest", "ggpubr", "ggrepel",
       "ggsci", "Hmisc", "jsonlite", "knitr", "kableExtra", "openxlsx", "rattle", "readxl", "scales", "showtext", "stringi", "tidymodels", "tidyverse", "xtable")

options(scipen=999)

# 0.2   Load data ####

data_0_0_ESP <- read_csv("../2_Data/0_Qualtrics_Output/20250826_Pilot/Spanish/Spanish_26. August 2025_04.16.csv")
data_0_0_FRA <- read_csv("../2_Data/0_Qualtrics_Output/20250826_Pilot/French/French_26. August 2025_04.19.csv")
data_0_0_GER <- read_csv("../2_Data/0_Qualtrics_Output/20250826_Pilot/German/German_26. August 2025_04.18.csv")
data_0_0_ROM <- read_csv("../2_Data/0_Qualtrics_Output/20250826_Pilot/Romanian/Romanian_26. August 2025_04.18.csv")

data_0_ESP <- read_csv("../2_Data/0_Qualtrics_Output/20260107_Pre_Final/Spanish_7.+Januar+2026_15.24.csv")
data_0_FRA <- read_csv("../2_Data/0_Qualtrics_Output/20260107_Pre_Final/French_7.+Januar+2026_15.24.csv")
data_0_GER <- read_csv("../2_Data/0_Qualtrics_Output/20260107_Pre_Final/German_7.+Januar+2026_15.24.csv")
data_0_ROM <- read_csv("../2_Data/0_Qualtrics_Output/20260107_Pre_Final/Romanian_7.+Januar+2026_15.22.csv")

data_0.1_ESP <- read_csv("../2_Data/0_Qualtrics_Output/20260107_Pre_Final/Spanish_14.+Januar+2026_15.10.csv")
data_0.1_FRA <- read_csv("../2_Data/0_Qualtrics_Output/20260107_Pre_Final/French_14.+Januar+2026_15.11.csv")
data_0.1_GER <- read_csv("../2_Data/0_Qualtrics_Output/20260107_Pre_Final/German_14.+Januar+2026_15.11.csv")
data_0.1_ROM <- read_csv("../2_Data/0_Qualtrics_Output/20260107_Pre_Final/Romanian_14.+Januar+2026_15.10.csv")

data_0.2_ESP <- read_csv("../2_Data/0_Qualtrics_Output/20260107_Pre_Final/Spanish_12.+März+2026_08.35.csv")
data_0.2_ROM <- read_csv("../2_Data/0_Qualtrics_Output/20260107_Pre_Final/Romanian_12.+März+2026_08.34.csv")

com_0_ESP <- read_parquet("../2_Data/Output/Output data/Combinations_Qualtrics_Spain_251117.parquet")
com_0_FRA <- read_parquet("../2_Data/Output/Output data/Combinations_Qualtrics_France_251117.parquet")
com_0_GER <- read_parquet("../2_Data/Output/Output data/Combinations_Qualtrics_Germany_251117.parquet")
# com_0_ROM <- read_parquet("../2_Data/Output/Output data/Combinations_Qualtrics_Romania_250703.parquet")
com_0_ROM <- read_parquet("../2_Data/Output/Output data/Combinations_Qualtrics_Romania_251117.parquet")

median_costs <- read.xlsx("../2_Data/Supplementary/Median_Costs_Countries.xlsx")

# 1     Data transformation ####
# 1.1   Spain ####

data_1_ESP <- data_0_ESP %>%
  filter(Status == "IP-Adresse")%>%
  # Convert date
  mutate(StartDate = as.POSIXct(StartDate, format = "%Y-%m-%d %H:%M:%S"))%>%
  mutate(Date = format(StartDate, "%Y-%m-%d"))%>%
  filter(as.Date(Date) > as.Date("2025-12-17"))%>%
  # Create basic columns
  mutate(Country = "Spain")%>%
  mutate(ID = 1:n())%>%
  select(Country, ID, everything())%>%
  rename(time = "Duration (in seconds)")%>%
  filter(Q02 != "No")%>%
  # Order of columns
  select(Country, ID, time, Q10:Date)%>%
  filter(!is.na(Q11))

data_1.1_ESP <- data_0.1_ESP %>%
  filter(Status == "IP-Adresse")%>%
  # Convert date
  mutate(StartDate = as.POSIXct(StartDate, format = "%Y-%m-%d %H:%M:%S"))%>%
  mutate(Date = format(StartDate, "%Y-%m-%d"))%>%
  filter(as.Date(Date) > as.Date("2025-12-17"))%>%
  # Create basic columns
  mutate(Country = "Spain")%>%
  mutate(ID = 1:n())%>%
  select(Country, ID, everything())%>%
  rename(time = "Duration (in seconds)")%>%
  filter(Q02 != "No")%>%
  # Order of columns
  select(Country, ID, time, Q10:Date)%>%
  filter(!is.na(Q11))

data_1.2_ESP <- data_0.2_ESP %>%
  filter(Status == "IP-Adresse")%>%
  # Convert date
  mutate(StartDate = as.POSIXct(StartDate, format = "%Y-%m-%d %H:%M:%S"))%>%
  mutate(Date = format(StartDate, "%Y-%m-%d"))%>%
  filter(as.Date(Date) > as.Date("2025-12-17"))%>%
  # Create basic columns
  mutate(Country = "Spain")%>%
  mutate(ID = 1:n())%>%
  select(Country, ID, everything())%>%
  rename(time = "Duration (in seconds)")%>%
  filter(Q02 != "No")%>%
  # Order of columns
  select(Country, ID, time, Q10:Date)%>%
  filter(!is.na(Q11))%>%
  filter(!is.na(Q15))%>%
  filter(Q38A == "De acuerdo")

# 1.2   France ####

data_1_FRA <- data_0_FRA %>%
  filter(Status == "IP-Adresse")%>%
  # Convert date
  mutate(StartDate = as.POSIXct(StartDate, format = "%Y-%m-%d %H:%M:%S"))%>%
  mutate(Date = format(StartDate, "%Y-%m-%d"))%>%
  filter(as.Date(Date) > as.Date("2025-12-17"))%>%
  # Create basic columns
  mutate(Country = "France")%>%
  mutate(ID = 1:n())%>%
  select(Country, ID, everything())%>%
  rename(time = "Duration (in seconds)")%>%
  filter(Q02 != "Non")%>%
  # Order of columns
  select(Country, ID, time, Q10:Date)%>%
  filter(!is.na(Q11))

data_1.1_FRA <- data_0.1_FRA %>%
  filter(Status == "IP-Adresse")%>%
  # Convert date
  mutate(StartDate = as.POSIXct(StartDate, format = "%Y-%m-%d %H:%M:%S"))%>%
  mutate(Date = format(StartDate, "%Y-%m-%d"))%>%
  filter(as.Date(Date) > as.Date("2025-12-17"))%>%
  # Create basic columns
  mutate(Country = "France")%>%
  mutate(ID = 1:n())%>%
  select(Country, ID, everything())%>%
  rename(time = "Duration (in seconds)")%>%
  filter(Q02 != "Non")%>%
  # Order of columns
  select(Country, ID, time, Q10:Date)%>%
  filter(!is.na(Q11))

# 1.3   Germany ####

data_1_GER <- data_0_GER %>%
  filter(Status == "IP-Adresse")%>%
  # Convert date
  mutate(StartDate = as.POSIXct(StartDate, format = "%Y-%m-%d %H:%M:%S"))%>%
  mutate(Date = format(StartDate, "%Y-%m-%d"))%>%
  filter(as.Date(Date) > as.Date("2025-12-17"))%>%
  # Create basic columns
  mutate(Country = "Germany")%>%
  mutate(ID = 1:n())%>%
  select(Country, ID, everything())%>%
  rename(time = "Duration (in seconds)")%>%
  filter(Q02 != "Nein")%>%
  filter(Q10 != "Unter 18")%>%
  # Order of columns
  select(Country, ID, time, Q10:Date)

data_1.1_GER <- data_0.1_GER %>%
  filter(Status == "IP-Adresse")%>%
  # Convert date
  mutate(StartDate = as.POSIXct(StartDate, format = "%Y-%m-%d %H:%M:%S"))%>%
  mutate(Date = format(StartDate, "%Y-%m-%d"))%>%
  filter(as.Date(Date) > as.Date("2025-12-17"))%>%
  # Create basic columns
  mutate(Country = "Germany")%>%
  mutate(ID = 1:n())%>%
  select(Country, ID, everything())%>%
  rename(time = "Duration (in seconds)")%>%
  filter(Q02 != "Nein")%>%
  filter(Q10 != "Unter 18")%>%
  # Order of columns
  select(Country, ID, time, Q10:Date)

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
  # Order of columns
  select(Country, ID, time, Q10:Date)

data_1.1_ROM <- data_0.1_ROM %>%
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
  # Order of columns
  select(Country, ID, time, Q10:Date)

data_1.2_ROM <- data_0.2_ROM %>%
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
  # Order of columns
  select(Country, ID, time, Q10:Date)%>%
  filter(!is.na(Q15))%>%
  filter(Q38A == "De acord")

# 2. Sampling ####

# 2.1 Gender ####

summarise_sample <- function(data_0, VAR_0){
  data_1 <- data_0 %>%
    rename("var_1" = VAR_0)%>%
    group_by(var_1)%>%
    summarise(number = n())%>%
    ungroup()%>%
    mutate(share = number/sum(number))
  
  return(data_1)
}

summarise_sample(data_1_ESP, "Q11")
summarise_sample(data_1.1_ESP, "Q11")
summarise_sample(data_1.2_ESP, "Q11")
summarise_sample(data_1_FRA, "Q11")
summarise_sample(data_1.1_FRA, "Q11")
summarise_sample(data_1_GER, "Q11")
summarise_sample(data_1.1_GER, "Q11")
summarise_sample(data_1_ROM, "Q11")
summarise_sample(data_1.1_ROM, "Q11")
summarise_sample(data_1.2_ROM, "Q11")

# 2.2 Age ####

summarise_sample(data_1_ESP, "Q10")
summarise_sample(data_1.1_ESP, "Q10")
summarise_sample(data_1.2_ESP, "Q10")
summarise_sample(data_1_FRA, "Q10")
summarise_sample(data_1.1_FRA, "Q10")
summarise_sample(data_1_GER, "Q10")
summarise_sample(data_1.1_GER, "Q10")
summarise_sample(data_1_ROM, "Q10")
summarise_sample(data_1.1_ROM, "Q10")
summarise_sample(data_1.2_ROM, "Q10")

# 2.3 Education ####

t_ESP <- summarise_sample(data_1_ESP, "Q12")
t.1_ESP <- summarise_sample(data_1.1_ESP, "Q12")
t.2_ESP <- summarise_sample(data_1.2_ESP, "Q12")

t_FRA <- summarise_sample(data_1_FRA, "Q12")
t.1_FRA <- summarise_sample(data_1.1_FRA, "Q12")
t_GER <- summarise_sample(data_1_GER, "Q12")
t.1_GER <- summarise_sample(data_1.1_GER, "Q12")
t_ROM <- summarise_sample(data_1_ROM, "Q12")
t.1_ROM <- summarise_sample(data_1.1_ROM, "Q12")
t.2_ROM <- summarise_sample(data_1.2_ROM, "Q12")

# 2.4 Expenditures ####

t_ESP <- summarise_sample(data_1_ESP, "Q28")
t.1_ESP <- summarise_sample(data_1.1_ESP, "Q28")
t.2_ESP <- summarise_sample(data_1.2_ESP, "Q28")
t_FRA <- summarise_sample(data_1_FRA, "Q28")
t.1_FRA <- summarise_sample(data_1.1_FRA, "Q28")
t_GER <- summarise_sample(data_1_GER, "Q28")
t.1_GER <- summarise_sample(data_1.1_GER, "Q28")
t_ROM <- summarise_sample(data_1_ROM, "Q28")
t.1_ROM <- summarise_sample(data_1.1_ROM, "Q28")
t.2_ROM <- summarise_sample(data_1.2_ROM, "Q28")
