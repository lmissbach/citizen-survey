# 0     General ####
# Author: L. Missbach (missbach@mcc-berlin.net)
# Date: 15th of November 2024

# 0.1   Packages ####

if(!require("pacman")) install.packages("pacman")

p_load("arrow", "boot", "broom", "extrafont", "fixest", "ggpubr", "ggrepel",
       "ggsci", "Hmisc", "knitr", "kableExtra", "openxlsx", "rattle", "scales", "showtext", "tidymodels", "tidyverse", "xtable")

font_add("wmpeople1", "wm_people_1/wmpeople1.TTF")
font_import(paths = "wm_people_1", prompt = FALSE)

options(scipen=999)

# 0.2   Load data ####

data_GER_0 <- read_rds("H:/6_Citizen_Survey/2_Data/Microdata/Microdata_Transformed_Germany.rds")
data_ROM_0 <- read_rds("H:/6_Citizen_Survey/2_Data/Microdata/Microdata_Transformed_Romania.rds")
data_ESP_0 <- read_rds("H:/6_Citizen_Survey/2_Data/Microdata/Microdata_Transformed_Spain.rds")
data_FRA_0 <- read_rds("H:/6_Citizen_Survey/2_Data/Microdata/Microdata_Transformed_France.rds")

# 1.    Identifying important variables ####

# Modeling approach to identify which variables predict heterogeneity in additional relative costs

# 1.1   Clean and homogenize loaded data ####

# Check for NA
# should have very few unique observations for factors

# 1.1.1 Germany ####

data_GER <- data_GER_0 %>%
  select(-hh_id, -hh_weights, -urban_01, -starts_with("Exp_"), -CO2_t_national, -CO2_t_national_P, -Expenditure_Group_5,-Expenditure_Group_10)%>%
  mutate(bundesland = ifelse(bundesland == "Baden-Wuerttemberg", "Baden-Württemberg",
                             ifelse(bundesland == "Thueringen", "Thüringen", bundesland)))%>%
  mutate(heating_fuel = ifelse(heating_fuel == "NA" & heating_type == "Fernheizung", "Fernwärme", 
                               ifelse(heating_fuel == "NA" | heating_fuel == "Feste Brennstoffe" | heating_fuel == "Sonstiges", "Feste Brennstoffe oder Sonstiges", 
                                      ifelse(heating_fuel == "Heizoel", "Heizöl", heating_fuel))))%>%
  mutate(renting = ifelse(renting %in% c("Eigentuemer"), "Eigentümer", 
                          ifelse(renting == "Mietfrei", "Andere Wohnform, z.B. mietfrei", renting)))%>%
  mutate(building_type = ifelse(building_type %in% c("Doppelhaushaelfte", "Zweifamilienhaus", "Sonstiges"), "Reihenhaus, Doppelhaushälfte oder Sonstiges", 
                                ifelse(building_type == "Wohngebaeude", "Mehrfamilienhaus", 
                                       ifelse(building_type == "Einfamilienhaus", "Einfamilienhaus (freistehend)", building_type))))%>%
  mutate(building_year = ifelse(building_year == "<1949", "Vor 1949", 
                                ifelse(building_year == "1949-1990", "Zwischen 1949 und 1990",
                                       ifelse(building_year == "1991-2000", "Zwischen 1991 und 2000",
                                              ifelse(building_year == "2001-2010", "Zwischen 2001 und 2010", "Zwischen 2011 und 2018")))))%>%
  mutate(number_of_cars = ifelse(number_of_cars == 0, "Kein Auto",
                                 ifelse(number_of_cars == 1, "Ein Auto",
                                        ifelse(number_of_cars == 2, "Zwei Autos",
                                               ifelse(number_of_cars > 2, "Drei Autos oder mehr", NA)))))%>%
  mutate(urban_type = ifelse(urban_type %in% c("Laendlicher Raum"), "Ländlicher Raum", 
                             ifelse(urban_type %in% c("Verstaedterter Raum"), "Stadtzentrum", "Vorort / Vorstadt")))%>%
  mutate(CO2_t_interest     = CO2_t_transport + CO2_t_gas_direct,
         CO2_t_interest_P   = CO2_t_transport_P + CO2_t_gas_direct_P)%>% # TBD
  mutate(exp_interest       = CO2_t_interest*40,
         exp_interest_P     = CO2_t_interest_P*40)%>% # TBD
  mutate(burden_interest    = exp_interest/hh_expenditures_EURO_2018,
         burden_interest_P  = exp_interest_P/hh_expenditures_EURO_2018)%>%
  select(-CO2_t_interest, -CO2_t_transport, -CO2_t_gas_direct, -exp_interest,
         -CO2_t_interest_P, -CO2_t_transport_P, -CO2_t_gas_direct_P, -exp_interest_P)%>%
  mutate_if(vars(is.character(.)), list(~ as.factor(.)))%>%
  select(-burden_interest)
  # after sanity check with random noise parameter 
  # select(-dishwasher.01, -nationality, -stove.g.01, -tv.01, -stove.e.01, -washing_machine.01, -refrigerator.01)

# 1.1.2 Romania ####

data_ROM <- data_ROM_0 %>%
  select(-hh_id, -starts_with("Exp"), -CO2_t_national, -CO2_t_national_P, -Expenditure_Group_5, -Expenditure_Group_10)%>%
  mutate(heating_fuel = ifelse(heating_fuel %in% c("Natural gas", "Natural gas stove"), "Natural gas",
                               ifelse(heating_fuel %in% c("Wood", "Wood, coal or oil stove"), "Wood, coal or oil", 
                                      ifelse(heating_fuel %in% c("Other", "Other heating", "Not connected", "No heating"), "Other heating", heating_fuel))))%>%
  mutate(cooking_fuel = ifelse(cooking_fuel %in% c("Electricity", "Other"), "Other cooking fuel", cooking_fuel))%>%
  mutate(number_of_cars = ifelse(number_of_cars == 0, "Nu am o masină.",
                                 ifelse(number_of_cars == 1, "Am o masină.",
                                        ifelse(number_of_cars == 2, "Am două masini.",
                                               ifelse(number_of_cars > 2, "Am trei sau mai multe masini.", NA)))))%>%
  mutate(occupation = ifelse(occupation == "employee", "Employee",
                             ifelse(occupation == "pensioner", "Pensioner",
                                    ifelse(occupation == "self-employed in agriculture", "Self-employed in agriculture",
                                           ifelse(occupation == "self-employed in non-agricultural activities", "Self-employed in non-agricultural activities",
                                                  ifelse(occupation == "unemployed", "Unemployed", "Other"))))))%>%
  # Not relevant
  mutate(lighting_fuel = ifelse(lighting_fuel %in% c("Other", "No lighting", "Oil"), "Other", lighting_fuel))%>%
  mutate(construction_year = case_when(construction_year < 1949                             ~ "<1949",
                                       construction_year >= 1949 & construction_year < 1965 ~ "1949-1964",
                                       construction_year >= 1965 & construction_year < 1991 ~ "1965-1990",
                                       construction_year >= 1991 & construction_year < 2001 ~ "1991-2000",
                                       construction_year >= 2001 & construction_year < 2011 ~ "2001-2010",
                                       construction_year >= 2011                            ~ ">2011"))%>%
  mutate(CO2_t_interest   = CO2_t_transport + CO2_t_gas_direct,
         CO2_t_interest_P = CO2_t_transport_P + CO2_t_gas_direct_P)%>% # TBD
  mutate(exp_interest   = CO2_t_interest*162,
         exp_interest_P = CO2_t_interest_P*162)%>% # in LEI
  mutate(burden_interest = exp_interest/hh_expenditures_LEI_2018,
         burden_interest_P = exp_interest_P/hh_expenditures_LEI_2018)%>%
  select(-CO2_t_interest, -CO2_t_transport, -CO2_t_gas_direct, -exp_interest,
         -CO2_t_interest_P, -CO2_t_transport_P, -CO2_t_gas_direct_P, -exp_interest_P)%>%
  mutate_if(vars(is.character(.)), list(~ as.factor(.)))%>%
  select(-burden_interest)

# 1.1.3 Spain ####

data_ESP <- data_ESP_0 %>%
  select(-hh_id, -CO2_t_national, -CO2_t_national_P, -starts_with("Exp"), -Expenditure_Group_5, -Expenditure_Group_10)%>%
  mutate(heating_fuel = ifelse(heating_fuel %in% c("Gas licuado", "Gas natural"), "Gas natural o Gas licuado", 
                               ifelse(heating_fuel == "No aplicable  (si CALEF=6)", "No calefaccion",
                                      ifelse(heating_fuel %in% c("No consta", "Otras", "Otros combustibles líquidos"), "Otras y líquidos", heating_fuel))))%>%
  mutate(tenant      = ifelse(tenant %in% c("Alquiler", "Alquiler reducido (renta antigua)"), "Alquiler",
                              ifelse(tenant %in% c("Cesión gratuita", "Cesión semigratuita"), "Cesión gratuita o semigratuita", "Propriedad")))%>%
  mutate(water_energy = ifelse(water_energy %in% c("Gas licuado", "Gas natural"),"Gas natural o Gas licuado", 
                               ifelse(water_energy %in% c("Combustibles sólidos", "No aplicable  (si CALEF=6)", "No consta", "Otras", "Otros combustibles líquidos"), "Otros combustibles", water_energy)))%>%
  mutate(urban_identif = ifelse(urban_identif %in% c("Municipio con 10.000 o más y menos de 20.000 habitantes", "Municipio con 20.000 o más y menos de 50.000 habitantes", "Municipio con 50.000 o más y menos 100.000 habitantes"), "Municipio con 10.000 o más y menos de 100.000 habitantes", urban_identif))%>%
  mutate(occupation = ifelse(occupation %in% c("Trabajando al menos una hora", "Dedicado/a a las labores del hogar", "Jubilado/a, retirado/a anticipadamente"), occupation, "Otra situación"))%>%
  # Not needed
  mutate(house_age = ifelse(house_age == 1, "Hace menos de 25 anos",
                            ifelse(house_age == 6, "Hace 25 o mas anos", "No consta")))%>%
  mutate(nationality = ifelse(nationality %in% c("Resto de Europa", "Resto de la Unión Europea (27 países)"), "Europa", nationality))%>%
  mutate(CO2_t_interest = CO2_t_transport + CO2_t_gas_direct,
         CO2_t_interest_P = CO2_t_transport_P + CO2_t_gas_direct_P)%>% # TBD
  mutate(exp_interest   = CO2_t_interest*40,
         exp_interest_P = CO2_t_interest_P*40)%>% # in €
  mutate(burden_interest = exp_interest/hh_expenditures_EURO_2018,
         burden_interest_P = exp_interest_P/hh_expenditures_EURO_2018)%>%
  select(-CO2_t_interest, -CO2_t_transport, -CO2_t_gas_direct, -exp_interest,
         -CO2_t_interest_P, -CO2_t_transport_P, -CO2_t_gas_direct_P, -exp_interest_P)%>%
  mutate_if(vars(is.character(.)), list(~ as.factor(.)))%>%
  select(-burden_interest)

# 1.1.4 France ####

data_FRA <- data_FRA_0 %>%
  select(-hh_id, -CO2_t_national, -CO2_t_national_P, -starts_with("Exp"), -Expenditure_Group_5, -Expenditure_Group_10)%>%
  mutate(heating_fuel = ifelse(heating_fuel %in% c("Autre", "Ne sait pas", "Other", "Charbon, coke"), "Autre", 
                               ifelse(heating_fuel %in% c("Aérothermie (pompe à chaleur)", "Géothermie", "Solaire"), "Pompe à chaleur, géothermie ou solaire", 
                                      ifelse(heating_fuel == "Fuel, mazout, pétrole", "Fuel, mazout ou pétrole", 
                                             ifelse(heating_fuel == "Butane, propane, gaz en citerne", "Butane, propane ou gaz en citerne", heating_fuel)))))%>%
  mutate(tenant = ifelse(tenant %in% c("Locataire", "Sous-locataire, co-locataire"), "Locataire", 
                         ifelse(tenant %in% c("Propriétaire ou copropriétaire (y compris en indivision) : vous n'avez pas de remboursement de prêt sur votre habitation", "Accédant à la propriété : vous avez des remboursements de prêts en cours"), "Propriétaire ou copropriétaire", "Autre")))%>%
  mutate(housing_type = ifelse(housing_type %in% c("Un logement dans un immeuble collectif", "Un logement dans un immeuble collectif à usage autre que d'habitation (usine, bureaux, commerce, bâtiment public...)"), "Logement dans un immeuble collectif", 
                               ifelse(housing_type %in% c("Une maison individuelle"), "Une maison individuelle", "Autre")))%>%
  mutate(construction_year = ifelse(construction_year %in% c("En 1948 ou avant"), construction_year,
                                    ifelse(construction_year %in% c("De 1949 à 1961", "De 1962 à 1967", "De 1968 à 1974"), "De 1949 à 1974",
                                           ifelse(construction_year %in% c("De 1975 à 1981", "De 1982 à 1989"), "De 1975 à 1989",
                                                  ifelse(construction_year %in% c("De 1990 à 1998", "De 1999 à 2003"), "De 1990 à 2003",
                                                         ifelse(construction_year %in% c("En 2004 et après", "En construction"), "En 2004 et après", "Je ne sais pas"))))))%>%
  mutate(number_of_cars = ifelse(number_of_cars == 0, "Pas de voiture",
                                 ifelse(number_of_cars == 1, "Une voiture",
                                        ifelse(number_of_cars == 2, "Deux voitures",
                                               ifelse(number_of_cars > 2, "Trois voitures ou plus", NA)))))%>%
  mutate(urban_type = ifelse(urban_type %in% c("Commune appartenant à un grand pôle (10 000 emplois ou plus)", "Commune multipolarisée des grandes aires urbaines"), "Grande ville ou centre urbain",
                               ifelse(urban_type %in% c("Commune appartenant à un moyen pôle (5 000 à moins de 10 000 emplois)", "Commune appartenant à un petit pôle (de 1 500 à moins de 5 000 emplois)"), "Petite ville ou centre régional",
                                      ifelse(urban_type %in% c("Commune appartenant à la couronne d'un grand pôle", "Commune appartenant à la couronne d'un moyen pôle", "Commune appartenant à la couronne d'un petit pôle"), "Commune périurbaine avec navetteurs",
                                             ifelse(urban_type %in% c("Commune multipolarisée", "Autre commune multipolarisée"), "Commune sous influence de plusieurs pôles", "Commune rurale ou isolée")))))%>%
  mutate(urban_type = ifelse(urban_type %in% c("Grande ville ou centre urbain", "Petite ville ou centre régional"), "Une ville ou ses banlieues directes",
                             ifelse(urban_type %in% c("Commune périurbaine avec navetteurs", "Commune sous influence de plusieurs pôles"), "Une commune rurale avec une liaison en transport public vers une ville", "Une commune rurale isolée (sans liaison facile en transport public vers une ville)")))%>%
  mutate(province = ifelse(province %in% c("Bassin parisien", "Région parisienne"), "Région parisienne", province))%>%
  # Not relevant
  mutate(nationality = ifelse(nationality %in% c("Nationalité de l'Union européenne des 15 (sauf France)", "Nationalité des pays entrés en 2004 dans l'Union européenne"), "Nationalité de l'Union européenne", nationality))%>%
  mutate(housing_type_2 = ifelse(housing_type_2 == "Ne sait pas", "Other", housing_type_2))%>%
  mutate(wall  = ifelse(wall %in% c("Dur (Pierre, brique, parpaing)", "Tôle"), wall, "Other"),
         roof  = ifelse(roof %in% c("Béton (maison en cours d'agrandissement)", "Tôle"), roof, "Other"),
         floor = ifelse(floor %in% c("Carrelage", "Revêtement plastique (lino...)"), floor, "Other"))%>%
  mutate(area = ifelse(is.na(area),999,area))%>%
  mutate(occupation = ifelse(occupation %in% c("Anciens employés et ouvriers", "Anciens cadres et professions intermédiaires","Ouvriers qualifiés","Inactifs divers (autres que retraités)",
                                              "Employés de la fonction publique", "Cadres d'entreprise"), occupation, "Other"))%>%
  mutate(CO2_t_interest    = CO2_t_transport + CO2_t_gas_direct,
         CO2_t_interest_P  = CO2_t_transport_P + CO2_t_gas_direct_P)%>% # TBD
  mutate(exp_interest      = CO2_t_interest*40,
         exp_interest_P    = CO2_t_interest_P*40)%>% # in €
  mutate(burden_interest   = exp_interest/hh_expenditures_EURO_2018,
         burden_interest_P = exp_interest_P/hh_expenditures_EURO_2018)%>%
  select(-CO2_t_interest, -CO2_t_transport, -CO2_t_gas_direct, -exp_interest,
         -CO2_t_interest_P, -CO2_t_transport_P, -CO2_t_gas_direct_P, -exp_interest_P)%>%
  mutate_if(vars(is.character(.)), list(~ as.factor(.)))%>%
  select(-burden_interest)
  
# 1.2   Boosted regression trees ####

compute_BRT <- function(data_0, Country, run_0){
  data_0.1 <- data_0 %>%
    # Create noise parameter
    mutate(noise = rnorm(nrow(.),0,1))
  
  data_0.2 <- data_0.1 %>%
    initial_split(prop = 0.8)
  
  # Data for training
  data_0.2.train <- data_0.2 %>%
    training()
  
  # Data for testing
  data_0.2.test <- data_0.2 %>%
    testing()
  
  rm(data_0.1, data_0.2)
  
  # Feature engineering
  if(Country == "Germany"){
    recipe_0 <- recipe(burden_interest_P ~ .,
                       data = data_0.2.train)%>%
      # Deletes all columns with any NA
      step_filter_missing(all_predictors(), threshold = 0)%>%
      # Remove minimum number of columns such that correlations are less than 0.9
      step_corr(all_numeric(), -all_outcomes(), threshold = 0.9)%>%
      # should have very few unique observations for factors
      # step_other(all_nominal(),-building_type,-ausbildung, threshold = 0.05)%>%
      step_dummy(all_nominal())
  }
  
  if(Country == "Romania"){
    recipe_0 <- recipe(burden_interest_P ~ .,
                       data = data_0.2.train)%>%
      # Deletes all columns with any NA
      step_filter_missing(all_predictors(), threshold = 0)%>%
      # Remove minimum number of columns such that correlations are less than 0.9
      step_corr(all_numeric(), -all_outcomes(), threshold = 0.9)%>%
      # should have very few unique observations for factors
      step_other(all_nominal(), threshold = 0.05)%>%
      step_dummy(all_nominal())
  }
  
  if(Country == "Spain"){
    recipe_0 <- recipe(burden_interest_P ~ .,
                       data = data_0.2.train)%>%
      # Deletes all columns with any NA
      step_filter_missing(all_predictors(), threshold = 0)%>%
      # Remove minimum number of columns such that correlations are less than 0.9
      step_corr(all_numeric(), -all_outcomes(), threshold = 0.9)%>%
      # should have very few unique observations for factors
      step_other(all_nominal(), -district, -province, threshold = 0.03)%>%
      step_dummy(all_nominal())
  }
  
  
  if(Country == "France"){
   recipe_0 <- recipe(burden_interest_P ~ .,
                       data = data_0.2.train)%>%
    # Deletes all columns with any NA
    step_filter_missing(all_predictors(), threshold = 0)%>%
    # Remove minimum number of columns such that correlations are less than 0.9
    step_corr(all_numeric(), -all_outcomes(), threshold = 0.9)%>%
    # should have very few unique observations for factors
    step_other(all_nominal(), -province, -education, -urban_type, threshold = 0.05)%>%
    step_dummy(all_nominal())
      
  }
  
  data_0.2.training <- recipe_0 %>%
    prep(training = data_0.2.train)%>%
    bake(new_data = NULL)
  
  data_0.2.testing <- recipe_0 %>%
    prep(training = data_0.2.test)%>%
    bake(new_data = NULL) 
  
  # Five-fold cross-validation
  
  folds_1 <- vfold_cv(data_0.2.training, v = 5)
  
  # Setup model to be tuned
  
  model_brt <- boost_tree(
    trees         = 1000,
    tree_depth    = tune(), # maximum depth of tree
    learn_rate    = tune(), # the higher the learning rate the faster - default 0.3
    # min_n       = tune(),
    mtry          = tune(), # fraction of features to be selected for each tree (0.5/0.7/1)
    # stop_iter   = tune(),
    # sample_size = tune()
  )%>%
    set_mode("regression")%>%
    set_engine("xgboost")
  
  # Create a tuning grid - 16 different models for the tuning space
  
  grid_0 <- grid_latin_hypercube(
    tree_depth(),
    learn_rate(c(-3,-0.5)),# tuning parameters
    mtry(c(round((ncol(data_0.2.training)-1)/2,0), ncol(data_0.2.training)-1)),
    size = 15)%>%
    # default parameters
    bind_rows(data.frame(tree_depth = 6, learn_rate = 0.3, mtry = ncol(data_0.2.training)-1))
  
  # Tune the model - cover the entire parameter space without running every combination
  
  print("Start computing")
  
  doParallel::registerDoParallel()
  
  time_1 <- Sys.time()
  
  model_brt_1 <- tune_grid(model_brt,
                           burden_interest_P ~ .,
                           resamples = folds_1,
                           grid      = grid_0,
                           metrics   = metric_set(mae, rmse, rsq))
  
  time_2 <- Sys.time()
  
  doParallel::stopImplicitCluster()
  
  print("End computing")
  
  # Collect metrics of tuned models
  
  metrics_1 <- collect_metrics(model_brt_1)
  
  model_brt_1.1 <- select_best(model_brt_1, metric = "mae")
  
  metrics_1.1 <- metrics_1 %>%
    filter(.config == model_brt_1.1$.config[1])
  
  # tree_depth: 3, learning rate: 0.0411,      mtry: 42 --> r2: 0.3 (GERMANY)
  # tree_depth: 6, learning rate: 0.008,       mtry: 32 --> r2: 0.3 (GERMANY)
  # tree_depth: 9, learning rate: 0.0132,      mtry: 27 --> r2: 0.39(Romania)
  # tree_depth: 7, learning rate: 0.008327652, mtry: 59 --> r2: 0.145 (SPAIN)
  # tree_depth: 3, learning rate: 0.01242,     mtry: 56 --> r2: 0.174 (France)
  
  if(Country == "Germany"){
    tree_depth_0 <- 10
    learn_rate_0   <- 0.005
    mtry_0         <- 32
  }
  
  if(Country == "Spain"){
    tree_depth_0 <- 10
    learn_rate_0   <- 0.005
    mtry_0         <- 60
  }
  
  # Fit best model after tuning
  model_brt <- boost_tree(
    trees         = 1000,
    tree_depth    = tree_depth_0, # 3
    learn_rate    = learn_rate_0, # 0.0411
    mtry          = mtry_0        # 42
  )%>%
    set_mode("regression")%>%
    set_engine("xgboost")
  
  model_brt_2 <- model_brt %>%
    fit(burden_interest_P ~ .,
        data = data_0.2.training)
  
  predictions_0 <- augment(model_brt_2, new_data = data_0.2.testing)
  rsq_0  <- rsq(predictions_0,  truth = burden_interest_P, estimate = .pred)
  
  data_0.2.testing_matrix <- data_0.2.testing %>%
    select(-burden_interest_P)%>%
    as.matrix()
  
  data_0.2.training_matrix <- data_0.2.training %>%
    select(-burden_interest_P)%>%
    as.matrix()
  
  time_3 <- Sys.time()
  
  shap_1 <- predict(extract_fit_engine(model_brt_2),
                    data_0.2.testing_matrix,
                    predcontrib = TRUE,
                    approxcontrib = FALSE)
  
  time_4 <- Sys.time()
  
  shap_1.1 <- shap_1 %>%
    as_tibble()%>%
    summarise_all(~ mean(abs(.)))%>%
    select(-BIAS)%>%
    pivot_longer(everything(), names_to = "variable", values_to = "SHAP_contribution")%>%
    arrange(desc(SHAP_contribution))%>%
    mutate(tot_contribution = sum(SHAP_contribution))%>%
    mutate(share_SHAP       = SHAP_contribution/tot_contribution)%>%
    select(-tot_contribution)
  
  shap_1.2 <- shap_1.1 %>%
    mutate(VAR_0 = ifelse(grepl("education", variable), "Education", 
                          ifelse(grepl("heating_fuel", variable), "Heating_Fuel", 
                                 ifelse(grepl("building_type", variable), "Building_Type", 
                                        ifelse(grepl("urban_type", variable), "Urban_Type", 
                                               ifelse(grepl("renting", variable), "Renting", 
                                                      ifelse(grepl("employment", variable), "Employment", 
                                                             ifelse(grepl("bundesland", variable), "Bundesland", 
                                                                    ifelse(grepl("heating_type", variable), "Heating_Type", 
                                                                           ifelse(grepl("building_year", variable), "Building_Year", 
                                                                                  ifelse(grepl("industry", variable), "Industry", 
                                                                                         ifelse(grepl("ausbildung", variable), "Ausbildung", 
                                                                                                ifelse(grepl("cooking_fuel", variable), "Cooking_Fuel", 
                                                                                                       ifelse(grepl("housing_type", variable), "Housing_Type", 
                                                                                                              ifelse(grepl("construction_year", variable), "construction_year", 
                                                                                                                     ifelse(grepl("occupation", variable), "Occupation", 
                                                                                                                            ifelse(grepl("province", variable), "Province", 
                                                                                                                                   ifelse(grepl("nationality", variable), "Nationality", variable))))))))))))))))))%>%
    mutate(VAR_0 = ifelse(grepl("district", variable), "District",
                          ifelse(grepl("house_age", variable), "House Age",
                                 ifelse(grepl("tenant", variable), "Tenant",
                                        ifelse(grepl("urban_identif_2", variable), "Urban_Identif_2",
                                               ifelse(grepl("urban_identif", variable) & VAR_0 != "Urban_Identif_2", "Urban_Identif",
                                                      ifelse(grepl("water_energy", variable), "Water energy", 
                                                             ifelse(grepl("house_type", variable), "House Type", VAR_0))))))))%>%
    group_by(VAR_0)%>%
    summarise(share_SHAP = sum(share_SHAP))%>%
    ungroup()%>%
    arrange(desc(share_SHAP))
  
  if(run_0 == 1) write.xlsx(shap_1.2, sprintf("../2_Data/Output/SHAP values/SHAP_all_vars_%s.xlsx", Country))
  if(run_0 == 2) write.xlsx(shap_1.2, sprintf("../2_Data/Output/SHAP values/SHAP_sparse_vars_%s.xlsx", Country))
  
  rm(folds_1, grid_0, metrics_1, metrics_1.1, recipe_0, time_1, time_2, data_0.2.test, data_0.2.testing, data_0.2.train, data_0.2.training, data_0.2.testing_matrix,
     model_brt, model_brt_1, model_brt_1.1, model_brt_2, rsq_0, shap_1, shap_1.1, shap_1.2, predictions_0)
  
}

# 1.2.1 For initial identification ####

# compute_BRT(data_GER, "Germany", 1)
# compute_BRT(data_ROM, "Romania", 1)
# compute_BRT(data_ESP, "Spain", 1)
compute_BRT(data_FRA, "France", 1)

# 1.2.2 For second run ####

data_GER <- data_GER %>%
  select(-ost_west, -dishwasher.01, -gender_hhh, -motorcycle.01, -washing_machine.01, - stove.e.01, -tv.01, -refrigerator.01, -nationality, -ausbildung)

data_ROM <- data_ROM %>%
  select(-lighting_fuel, -motorcycle.01, -children,-tv.01,-nationality,-washing_machine.01,-adults,-gender,-refrigerator.01,-construction_year,-freezer.01,-nationality_1,-hh_size)

data_ESP <- data_ESP %>%
  select(-nationality, -housing_type, -hh_size, -house_age, -children)

data_FRA <- data_FRA %>%
  select(-motorcycle.01, -refrigerator.01, -floor,-washing_machine.01,-gender,-tv.01,-roof,-ac.01,-wall)

# compute_BRT(data_GER, "Germany", 2)
# compute_BRT(data_ROM, "Romania", 2)
# compute_BRT(data_ESP, "Spain",   2)
# compute_FRA(data_FRA, "France", 2)

# Descriptive statistics

# 1.2.3 Final run ####

# Select criteria to be included.

criteria_GER <- c("hh_expenditures_EURO_2018", "heating_fuel", "renting", "building_type", "bundesland", "space", "building_year", "number_of_cars", "urban_type") # captures 90% of SHAP, exclude heating_type
criteria_ESP <- c("hh_expenditures_EURO_2018", "heating_fuel", "tenant", "water_energy", "urban_identif", "age_hhh", "District",
                  "occupation", "gender") # Captures 82% of variation, exclude urban_identif_2
criteria_ROM <- c("hh_expenditures_LEI_2018", "heating_fuel", "cooking_fuel", "number_of_cars", "province", "occupation", "area", "housing_type") # Captures 95% of SHAP
criteria_FRA <- c("hh_expenditures_EURO_2018", "number_of_cars", "heating_fuel", "tenant", "urban_type", "housing_type", "construction_year", "province")

# 2.    Calculation for household profiles and output ####

# 2.1   Setup grids #### 

# Germany
# Household size currently not relevant

income_groups <- data_GER_0 %>%
  mutate(Income_Group_10 = as.numeric(binning(hh_expenditures_EURO_2018, bins = 10, weights = hh_weights, method = "wtd.quantile")))%>%
  group_by(Income_Group_10)%>%
  summarise(min_exp = min(hh_expenditures_EURO_2018),
            max_exp = max(hh_expenditures_EURO_2018),
            hh_expenditures_EURO_2018 = wtd.mean(hh_expenditures_EURO_2018, hh_weights))%>%
  ungroup()

space <- data_GER_0 %>%
  mutate(space_5 = as.numeric(binning(space, bins = 5, weights = hh_weights, method = "wtd.quantile")))%>%
  group_by(space_5)%>%
  summarise(min_space = min(space),
            mean_space = wtd.mean(space, hh_weights),
            max_space = max(space))%>%
  ungroup()

grid_GER <- expand_grid(distinct(data_GER, heating_fuel), # 2
                        distinct(data_GER, renting),      # 3
                        distinct(data_GER, building_type),# 4
                        # distinct(data_GER, heating_type), # 7
                        distinct(data_GER, building_year),# 8
                        distinct(data_GER, number_of_cars), # 6
                        distinct(data_GER, urban_type), # 10
                        distinct(data_GER, bundesland), # 5
                        "hh_expenditures_EURO_2018" = income_groups$hh_expenditures_EURO_2018, # 2
                        "space" = space$mean_space)

rm(income_groups, space)

# 15 Millionen Kombinationen ...

# Romania

income_groups <- data_ROM_0 %>%
  mutate(Income_Group_10 = as.numeric(binning(hh_expenditures_LEI_2018, bins = 10, weights = hh_weights, method = "wtd.quantile")))%>%
  group_by(Income_Group_10)%>%
  summarise(n_households = n(),
            weights = sum(hh_weights),
            min_exp = min(hh_expenditures_LEI_2018),
            max_exp = max(hh_expenditures_LEI_2018),
            hh_expenditures_LEI_2018 = wtd.mean(hh_expenditures_LEI_2018, hh_weights))%>%
  ungroup()

area <- data_ROM_0 %>%
  mutate(area_5 = as.numeric(binning(area, bins = 5, weights = hh_weights, method = "wtd.quantile")))%>%
  group_by(area_5)%>%
  summarise(min_area = min(area),
            max_area = max(area),
            area = wtd.mean(area, hh_weights))%>%
  ungroup()

grid_ROM <- expand_grid(distinct(data_ROM, heating_fuel),
                        distinct(data_ROM, cooking_fuel),
                        distinct(data_ROM, number_of_cars),
                        distinct(data_ROM, province),
                        distinct(data_ROM, occupation),
                        distinct(data_ROM, housing_type),
                        "area" = area$area,
                        "hh_expenditures_LEI_2018" = income_groups$hh_expenditures_LEI_2018)

rm(income_groups, area)

# Spain

age <- data_ESP_0 %>%
  mutate(age_5 = as.numeric(binning(age_hhh, bins = 3, weights = hh_weights, method = "wtd.quantile")))%>%
  group_by(age_5)%>%
  summarise(min_age = min(age_hhh),
            age = round(wtd.mean(age_hhh, hh_weights),1),
            max_age = max(age_hhh))%>%
  ungroup()

income_groups <- data_ESP_0 %>%
  mutate(Income_Group_10 = as.numeric(binning(hh_expenditures_EURO_2018, bins = 10, weights = hh_weights, method = "wtd.quantile")))%>%
  group_by(Income_Group_10)%>%
  summarise(hh_expenditures_EURO_2018 = wtd.mean(hh_expenditures_EURO_2018, hh_weights))%>%
  ungroup()

grid_ESP <- expand_grid(distinct(data_ESP, heating_fuel),
                        distinct(data_ESP, tenant),
                        distinct(data_ESP, water_energy),
                        distinct(data_ESP, urban_identif),   # Safe
                        distinct(data_ESP, district),
                        # distinct(data_ESP, urban_identif_2), # Safe
                        "occupation" = c("Trabajando al menos una hora", "Jubilado.a..retirado.a.anticipadamente", "Dedicado/a a las labores del hogar","Otra situación"),
                        # distinct(data_ESP, province),        # Not necessary
                        distinct(data_ESP, gender),          # Safe
                        "age_hhh" = age$age,
                        "hh_expenditures_EURO_2018" = income_groups$hh_expenditures_EURO_2018)          

# France
income_groups <- data_FRA_0 %>%
  mutate(Income_Group_10 = as.numeric(binning(hh_expenditures_EURO_2018, bins = 10, weights = hh_weights, method = "wtd.quantile")))%>%
  group_by(Income_Group_10)%>%
  summarise(min_exp = min(hh_expenditures_EURO_2018),
            max_exp = max(hh_expenditures_EURO_2018),
            hh_expenditures_EURO_2018 = wtd.mean(hh_expenditures_EURO_2018, hh_weights))%>%
  ungroup()

grid_FRA <- expand_grid("hh_expenditures_EURO_2018" = income_groups$hh_expenditures_EURO_2018,
                        distinct(data_FRA, heating_fuel),
                        distinct(data_FRA, number_of_cars),
                        distinct(data_FRA, tenant),
                        distinct(data_FRA, urban_type),
                        distinct(data_FRA, housing_type),
                        distinct(data_FRA, province),
                        distinct(data_FRA, construction_year)
                        # "occupation" = c("Anciens cadres et professions intermédiaires", "Anciens employés et ouvriers",
                        #                  "Ouvriers qualifiés", "Other")
                        )


# 2.2   Function and fitting ####

compute_BRT_and_fit <- function(data_0, Country, criteria_0, grid_0){
  data_0.1 <- data_0 %>%
    select(burden_interest_P, any_of(criteria_0))
  
  data_0.2 <- data_0.1 %>%
    initial_split(prop = 0.8)
  
  # Data for training
  data_0.2.train <- data_0.2 %>%
    training()
  
  # Data for testing
  data_0.2.test <- data_0.2 %>%
    testing()
  
  rm(data_0.1, data_0.2)
  
  # Feature engineering
  if(Country == "Germany"){
    recipe_0 <- recipe(burden_interest_P ~ .,
                       data = data_0.2.train)%>%
      # Deletes all columns with any NA
      step_filter_missing(all_predictors(), threshold = 0)%>%
      # Remove minimum number of columns such that correlations are less than 0.9
      step_corr(all_numeric(), -all_outcomes(), threshold = 0.9)%>%
      # should have very few unique observations for factors
      step_other(all_nominal(),-building_type, -bundesland, -building_year, -renting, threshold = 0.03)%>%
      step_dummy(all_nominal())
    
    grid_0 <- grid_0 %>%
      mutate(burden_interest_P = 0)
  }
  
  if(Country == "Romania"){
    recipe_0 <- recipe(burden_interest_P ~ .,
                       data = data_0.2.train)%>%
      # Deletes all columns with any NA
      step_filter_missing(all_predictors(), threshold = 0)%>%
      # Remove minimum number of columns such that correlations are less than 0.9
      step_corr(all_numeric(), -all_outcomes(), threshold = 0.9)%>%
      # should have very few unique observations for factors
      # step_other(all_nominal(), -heating_fuel, -cooking_fuel, threshold = 0.03)%>%
      step_dummy(all_nominal())
    
    grid_0 <- grid_0 %>%
      mutate(burden_interest_P = 0)
  }
  
  if(Country == "Spain"){
    recipe_0 <- recipe(burden_interest_P ~ .,
                       data = data_0.2.train)%>%
      # Deletes all columns with any NA
      step_filter_missing(all_predictors(), threshold = 0)%>%
      # Remove minimum number of columns such that correlations are less than 0.9
      step_corr(all_numeric(), -all_outcomes(), threshold = 0.9)%>%
      # should have very few unique observations for factors
      step_other(all_nominal(), -heating_fuel, -water_energy, -occupation, threshold = 0.03)%>%
      step_dummy(all_nominal())
    
    grid_0 <- grid_0 %>%
      mutate(burden_interest_P = 0)
  }
  
  if(Country == "France"){
    recipe_0 <- recipe(burden_interest_P ~ .,
                       data = data_0.2.train)%>%
      # Deletes all columns with any NA
      step_filter_missing(all_predictors(), threshold = 0)%>%
      # Remove minimum number of columns such that correlations are less than 0.9
      step_corr(all_numeric(), -all_outcomes(), threshold = 0.9)%>%
      # should have very few unique observations for factors
      # step_other(occupation, threshold = 0.08)%>%
      # step_other(all_nominal(), -heating_fuel, -housing_type, -urban_type, -province, - occupation, threshold = 0.05)%>%
      step_dummy(all_nominal())
    
    grid_0 <- grid_0 %>%
      mutate(burden_interest_P = 0)
  }
  
  
  data_0.2.training <- recipe_0 %>%
    prep(training = data_0.2.train)%>%
    bake(new_data = NULL)
  
  data_0.2.testing <- recipe_0 %>%
    prep(training = data_0.2.test)%>%
    bake(new_data = NULL)
  
  if(Country == "Germany"){
    data_grid_0      <- recipe_0 %>%
      prep(training = grid_0)%>%
      bake(new_data = NULL)
  }
  
  if(Country == "Romania"){
    data_grid_0      <- recipe_0 %>%
      prep(training = grid_0)%>%
      bake(new_data = NULL)
  }
  
  if(Country == "Spain"){
    data_grid_0      <- recipe_0 %>%
      prep(training = grid_0)%>%
      bake(new_data = NULL)
  }
  
  if(Country == "France"){
    data_grid_0 <- recipe_0 %>%
      prep(training = grid_0)%>%
      bake(new_data = NULL)
  }
  
  # Five-fold cross-validation
  
  folds_1 <- vfold_cv(data_0.2.training, v = 5)
  
  # Setup model to be tuned
  
  model_brt <- boost_tree(
    trees         = 1000,
    tree_depth    = tune(), # maximum depth of tree
    learn_rate    = tune(), # the higher the learning rate the faster - default 0.3
    # min_n       = tune(),
    mtry          = tune(), # fraction of features to be selected for each tree (0.5/0.7/1)
    # stop_iter   = tune(),
    # sample_size = tune()
  )%>%
    set_mode("regression")%>%
    set_engine("xgboost")
  
  # Create a tuning grid - 16 different models for the tuning space
  
  grid_1 <- grid_latin_hypercube(
    tree_depth(),
    learn_rate(c(-3,-0.5)),# tuning parameters
    mtry(c(round((ncol(data_0.2.training)-1)/2,0), ncol(data_0.2.training)-1)),
    size = 15)%>%
    # default parameters
    bind_rows(data.frame(tree_depth = 6, learn_rate = 0.3, mtry = ncol(data_0.2.training)-1))
  
  # Tune the model - cover the entire parameter space without running every combination
  
  print("Start computing")
  
  doParallel::registerDoParallel()
  
  time_1 <- Sys.time()
  
  model_brt_1 <- tune_grid(model_brt,
                           burden_interest_P ~ .,
                           resamples = folds_1,
                           grid      = grid_1,
                           metrics   = metric_set(mae, rmse, rsq))
  
  time_2 <- Sys.time()
  
  doParallel::stopImplicitCluster()
  
  print("End computing")
  
  # Collect metrics of tuned models
  
  metrics_1 <- collect_metrics(model_brt_1)
  
  model_brt_1.1 <- select_best(model_brt_1, metric = "mae")
  
  metrics_1.1 <- metrics_1 %>%
    filter(.config == model_brt_1.1$.config[1])
  
  rm(metrics_1, model_brt_1.1, metrics_1.1, model_brt, model_brt_1)
  
  if(Country == "Germany"){
    tree_depth_0 <- 6
    learn_rate_0 <- 0.01873427
    mtry_0       <- 23
  }
  
  if(Country == "Spain"){
    tree_depth_0 <- 4
    learn_rate_0 <- 0.0135
    mtry_0       <- 10
  }
  
  if(Country == "Romania"){
    tree_depth_0 <- 4
    learn_rate_0 <- 0.0166
    mtry_0       <- 24
  }
  
  if(Country == "France"){
    tree_depth_0 <- 9
    learn_rate_0 <- 0.00783
    mtry_0       <- 25
  }
  
  # Fit best model after tuning
  model_brt <- boost_tree(
    trees         = 1000,
    tree_depth    = tree_depth_0,
    learn_rate    = learn_rate_0,
    mtry          = mtry_0       
  )%>%
    set_mode("regression")%>%
    set_engine("xgboost")
  
  model_brt_2 <- model_brt %>%
    fit(burden_interest_P ~ .,
        data = data_0.2.training)
  
  predictions_0 <- augment(model_brt_2, new_data = data_0.2.testing)
  rsq_0  <- rsq(predictions_0,  truth = burden_interest_P, estimate = .pred)
  
  print(paste0("R-Squared for ", Country, " amounts to ", round(rsq_0$.estimate, 2)*100, "%."))

  predictions_1 <- augment(model_brt_2, new_data = data_grid_0)
  
  predictions_1.1 <- grid_0 %>%
    select(-burden_interest_P)%>%
    mutate(.pred = predictions_1$.pred)
  
  write_rds(predictions_1.1, sprintf("../2_Data/Output/Predictions/Predictions_P_%s.rds", Country))
  
  # Extract SHAP for general assessment
  
  data_0.2.testing_matrix <- data_0.2.testing %>%
    select(-burden_interest_P)%>%
    as.matrix()
  
  time_3 <- Sys.time()
  
  shap_1 <- predict(extract_fit_engine(model_brt_2),
                    data_0.2.testing_matrix,
                    predcontrib = TRUE,
                    approxcontrib = FALSE)
  
  time_4 <- Sys.time()
  
  shap_1 <- shap_1 %>%
    as_tibble()
  
  shap_1.1 <- shap_1 %>%
    as_tibble()%>%
    summarise_all(~ mean(abs(.)))%>%
    select(-BIAS)%>%
    pivot_longer(everything(), names_to = "variable", values_to = "SHAP_contribution")%>%
    arrange(desc(SHAP_contribution))%>%
    mutate(tot_contribution = sum(SHAP_contribution))%>%
    mutate(share_SHAP       = SHAP_contribution/tot_contribution)%>%
    select(-tot_contribution)
  
  shap_1.2 <- shap_1.1 %>%
    mutate(VAR_0 = ifelse(grepl("education", variable), "Education", 
                          ifelse(grepl("heating_fuel", variable), "Heating_Fuel", 
                                 ifelse(grepl("building_type", variable), "Building_Type", 
                                        ifelse(grepl("urban_type", variable), "Urban_Type", 
                                               ifelse(grepl("renting", variable), "Renting", 
                                                      ifelse(grepl("employment", variable), "Employment", 
                                                             ifelse(grepl("bundesland", variable), "Bundesland", 
                                                                    ifelse(grepl("heating_type", variable), "Heating_Type", 
                                                                           ifelse(grepl("building_year", variable), "Building_Year", 
                                                                                  ifelse(grepl("industry", variable), "Industry", 
                                                                                         ifelse(grepl("ausbildung", variable), "Ausbildung", 
                                                                                                ifelse(grepl("cooking_fuel", variable), "Cooking_Fuel", 
                                                                                                       ifelse(grepl("housing_type", variable), "Housing_Type", 
                                                                                                              ifelse(grepl("construction_year", variable), "construction_year", 
                                                                                                                     ifelse(grepl("occupation", variable), "Occupation", 
                                                                                                                            ifelse(grepl("province", variable), "Province", 
                                                                                                                                   ifelse(grepl("nationality", variable), "Nationality", variable))))))))))))))))))%>%
    mutate(VAR_0 = ifelse(grepl("district", variable), "District",
                          ifelse(grepl("house_age", variable), "House Age",
                                 ifelse(grepl("tenant", variable), "Tenant",
                                        ifelse(grepl("urban_identif_2", variable), "Urban_Identif_2",
                                               ifelse(grepl("urban_identif", variable) & VAR_0 != "Urban_Identif_2", "Urban_Identif",
                                                      ifelse(grepl("water_energy", variable), "Water energy", 
                                                             ifelse(grepl("house_type", variable), "House Type", VAR_0))))))))%>%
    group_by(VAR_0)%>%
    mutate(sum_0 = sum(share_SHAP))%>%
    ungroup()%>%
    arrange(desc(sum_0), desc(share_SHAP))
  
  if(Country == "Germany"){
    write_parquet(shap_1.2, "../2_Data/Output/SHAP values/SHAP_Summary_Germany_P.parquet")
    write_parquet(shap_1, "../2_Data/Output/SHAP values/SHAP_Detail_Germany_P.parquet")
    write_parquet(data_0.2.test, "../2_Data/Output/SHAP values/SHAP_Test_Germany_P.parquet")
  }
  
  if(Country == "France"){
    write_parquet(shap_1.2,      "../2_Data/Output/SHAP values/SHAP_Summary_France_P.parquet")
    write_parquet(shap_1,        "../2_Data/Output/SHAP values/SHAP_Detail_France_P.parquet")
    write_parquet(data_0.2.test, "../2_Data/Output/SHAP values/SHAP_Test_France_P.parquet")
  }
  
  if(Country == "Spain"){
    write_parquet(shap_1.2,      "../2_Data/Output/SHAP values/SHAP_Summary_Spain_P.parquet")
    write_parquet(shap_1,        "../2_Data/Output/SHAP values/SHAP_Detail_Spain_P.parquet")
    write_parquet(data_0.2.test, "../2_Data/Output/SHAP values/SHAP_Test_Spain_P.parquet")
  }
  
 if(Country == "Romania"){
   write_parquet(shap_1.2,      "../2_Data/Output/SHAP values/SHAP_Summary_Romania_P.parquet")
   write_parquet(shap_1,        "../2_Data/Output/SHAP values/SHAP_Detail_Romania_P.parquet")
   write_parquet(data_0.2.test, "../2_Data/Output/SHAP values/SHAP_Test_Romania_P.parquet")
 }
  
  
  rm(folds_1, grid_0, recipe_0, time_1, time_2, data_0.2.test, data_0.2.testing, data_0.2.train, data_0.2.training,
     model_brt, model_brt_2, rsq_0, predictions_0, predictions_1, predictions_1.1, grid_0, learn_rate_0, mtry_0, tree_depth_0, data_grid_0)
  
}

compute_BRT_and_fit(data_GER, "Germany", criteria_GER, grid_GER)
compute_BRT_and_fit(data_ROM, "Romania", criteria_ROM, grid_ROM)
compute_BRT_and_fit(data_ESP, "Spain",   criteria_ESP, grid_ESP)
compute_BRT_and_fit(data_FRA, "France",  criteria_FRA, grid_FRA)

rm(grid_GER, grid_ROM, grid_ESP, grid_FRA)

# 2.3   Create supplementary categories ####

# Spain

data_ESP_2.3 <- read_rds("../2_Data/Output/Predictions/Predictions_P_Spain.rds")%>%
  mutate_at(vars(heating_fuel:district, gender), ~ as.character(.))

# heating_fuel
data_ESP_2.3.1 <- filter(data_ESP_2.3, heating_fuel == "Gas natural o Gas licuado")%>%
  mutate(heating_fuel = "No lo sé")%>%
  bind_rows(data_ESP_2.3)

# water heating
data_ESP_2.3.2 <- filter(data_ESP_2.3.1, water_energy == "Gas natural o Gas licuado")%>%
  mutate(water_energy = "No lo sé")%>%
  bind_rows(data_ESP_2.3.1)

# urban_identifier
data_ESP_2.3.3 <- filter(data_ESP_2.3.2, urban_identif == "Municipio con 10.000 o más y menos de 100.000 habitantes")%>%
  mutate(urban_identif = "No lo sé")%>%
  bind_rows(data_ESP_2.3.2)

# occupation
data_ESP_2.3.4 <- filter(data_ESP_2.3.3, occupation == "Trabajando al menos una hora")%>%
  mutate(occupation = "Prefiero no revelarlo")%>%
  bind_rows(data_ESP_2.3.3)

# gender
data_ESP_2.3.5 <- filter(data_ESP_2.3.4, gender == "Hombre")%>%
  mutate(gender = "Otro / prefiero no revelarlo")%>%
  bind_rows(data_ESP_2.3.4)

# age_hhh
data_ESP_2.3.6 <- filter(data_ESP_2.3.5, age_hhh == 56.1)%>%
  mutate(age_hhh = "Prefiero no revelarlo")%>%
  bind_rows(mutate(data_ESP_2.3.5, age_hhh = as.character(age_hhh)))

data_ESP_2.3.7 <- data_ESP_2.3.6 %>%
  mutate(.pred = round(.pred,5))

data_ESP_2.3.7[] <- lapply(data_ESP_2.3.7, function(x) if (is.character(x)) as.factor(x) else x)

write_rds(data_ESP_2.3.7, "../2_Data/Output/Predictions/Predictions_P_Spain_Update.rds", compress = "gz")

rm(data_ESP_2.3.1, data_ESP_2.3.2, data_ESP_2.3, data_ESP_2.3.3, data_ESP_2.3.4, data_ESP_2.3.5, data_ESP_2.3.6, data_ESP_2.3.7)

# Germany

data_GER_2.3 <- read_rds("../2_Data/Output/Predictions/Predictions_P_Germany.rds")%>%
  mutate_at(vars(heating_fuel:bundesland), ~ as.character(.))%>%
  mutate(space = round(space))

# heating_fuel
data_GER_2.3.1 <- filter(data_GER_2.3, heating_fuel == "Gas")%>%
  mutate(heating_fuel = "Weiß nicht")%>%
  bind_rows(data_GER_2.3)

# building_type
data_GER_2.3.2 <- filter(data_GER_2.3.1, building_type == "Mehrfamilienhaus")%>%
  mutate(building_type = "Weiß nicht")%>%
  bind_rows(data_GER_2.3.1)

# building_year
data_GER_2.3.3 <- filter(data_GER_2.3.2, building_year == "Zwischen 1949 und 1990")%>%
  mutate(building_year = "Weiß nicht")%>%
  bind_rows(data_GER_2.3.2)

# space
data_GER_2.3.4 <- filter(data_GER_2.3.3, space == 94)%>%
  mutate(space = "Weiß nicht")%>%
  bind_rows(mutate(data_GER_2.3.3, space = as.character(space)))

data_GER_2.3.5 <- data_GER_2.3.4 %>%
  mutate(.pred = round(.pred,5))

data_GER_2.3.5[] <- lapply(data_GER_2.3.5, function(x) if (is.character(x)) as.factor(x) else x)

write_rds(data_GER_2.3.5, "../2_Data/Output/Predictions/Predictions_P_Germany_Update.rds", compress = "gz")

rm(data_GER_2.3.1, data_GER_2.3.2, data_GER_2.3, data_GER_2.3.3, data_GER_2.3.4, data_GER_2.3.5)

# France

data_FRA_2.3 <- read_rds("../2_Data/Output/Predictions/Predictions_P_France.rds")%>%
  mutate_at(vars(heating_fuel:construction_year), ~ as.character(.))

# heating_fuel
data_FRA_2.3.1 <- filter(data_FRA_2.3, heating_fuel == "Gaz de ville")%>%
  mutate(heating_fuel = "Je ne sais pas")%>%
  bind_rows(data_FRA_2.3)

# urban_type
data_FRA_2.3.2 <- filter(data_FRA_2.3.1, urban_type == "Une ville ou ses banlieues directes")%>%
  mutate(urban_type = "Je ne sais pas")%>%
  bind_rows(data_FRA_2.3.1)

data_FRA_2.3.2[] <- lapply(data_FRA_2.3.2, function(x) if (is.character(x)) as.factor(x) else x)

write_rds(data_FRA_2.3.2, "../2_Data/Output/Predictions/Predictions_P_France_Update.rds", compress = "gz")

rm(data_FRA_2.3.1, data_FRA_2.3.2)

# Romania
data_ROM_2.3 <- read_rds("../2_Data/Output/Predictions/Predictions_P_Romania.rds")%>%
  mutate_at(vars(heating_fuel:housing_type), ~ as.character(.))

# heating_fuel
data_ROM_2.3.1 <- filter(data_ROM_2.3, heating_fuel == "Wood, coal or oil")%>%
  mutate(heating_fuel = "Nu stiu asta")%>%
  bind_rows(data_ROM_2.3)

# cooking_fuel
data_ROM_2.3.2 <- filter(data_ROM_2.3, cooking_fuel == "Natural gas")%>%
  mutate(cooking_fuel = "Nu stiu asta")%>%
  bind_rows(data_ROM_2.3.1)

# housing_type
data_ROM_2.3.3 <- filter(data_ROM_2.3.2, housing_type == "Detached house")%>%
  mutate(housing_type = "Nu stiu asta")%>%
  bind_rows(data_ROM_2.3.2)

# space
data_ROM_2.3.4 <- filter(data_ROM_2.3.3, area > 45.6 & area < 47)%>%
  mutate(area = "Nu stiu asta")%>%
  bind_rows(mutate(data_ROM_2.3.3, area = as.character(area)))

data_ROM_2.3.4[] <- lapply(data_ROM_2.3.4, function(x) if (is.character(x)) as.factor(x) else x)

write_rds(data_ROM_2.3.4, "../2_Data/Output/Predictions/Predictions_P_Romania_Update.rds", compress = "gz")

rm(data_ROM_2.3.1, data_ROM_2.3.2, data_ROM_2.3.3, data_ROM_2.3.4)

# 2.4   Inclusion in distribution ####

pred_GER <- read_rds("../2_Data/Output/Predictions/Predictions_P_Germany_Update.rds")
pred_FRA <- read_rds("../2_Data/Output/Predictions/Predictions_P_France_Update.rds")
pred_ESP <- read_rds("../2_Data/Output/Predictions/Predictions_P_Spain_Update.rds")
pred_ROM <- read_rds("../2_Data/Output/Predictions/Predictions_P_Romania_Update.rds")

pred_GER[] <- lapply(pred_GER, function(x) if (is.character(x)) as.factor(x) else x)
pred_FRA[] <- lapply(pred_FRA, function(x) if (is.character(x)) as.factor(x) else x)
pred_ESP[] <- lapply(pred_ESP, function(x) if (is.character(x)) as.factor(x) else x)
pred_ROM[] <- lapply(pred_ROM, function(x) if (is.character(x)) as.factor(x) else x)

percentiles_0 <- data.frame()

for(i in c("Germany", "Spain", "France", "Romania")){
  if(i == "Romania") data_0 <- data_ROM_0%>%
      mutate(hh_expenditures_EURO_2018 = hh_expenditures_LEI_2018*0.24676177)
  if(i == "Spain")   data_0 <- data_ESP_0
  if(i == "France")  data_0 <- data_FRA_0
  if(i == "Germany") data_0 <- data_GER_0
  
  percentiles <- data_0 %>%
    mutate(CO2_t_interest_P = CO2_t_transport_P + CO2_t_gas_direct_P)%>% # TBD
    mutate(exp_interest_P   = CO2_t_interest_P*40)%>% # TBD
    mutate(burden_interest_P = exp_interest_P/hh_expenditures_EURO_2018)%>%
    mutate(Percentiles = as.numeric(binning(burden_interest_P, bins = 100, method = "wtd.quantile", weights = hh_weights)))%>%
    group_by(Percentiles)%>%
    summarise(burden_upper = max(burden_interest_P))%>% # If you are that much affected, 100 - Percentiles will be more heavily affected than you are.
    ungroup()%>%
    mutate(burden_lower = lag(burden_upper))%>%
    mutate(burden_lower = ifelse(is.na(burden_lower),-1,burden_lower))%>%
    select(Percentiles, burden_lower, burden_upper)%>%
    mutate(Country = i)
  
  percentiles_0 <- percentiles_0 %>%
    bind_rows(percentiles)
}

# find_percentile <- function(A, i){
#   percentile_1 <- percentiles_0 %>%
#     filter(Country == i)%>%
#     filter(A >= burden_lower & A <= burden_upper)%>%
#     pull(Percentiles)
#   
#   return(percentile_1)
# }

# sample_GER <- pred_GER %>%
#   sample_n(., 1000)%>%
#   mutate(Percentile = map_dbl(.pred, ~ find_percentile(., "Germany")))

percentiles_GER <- percentiles_0 %>%
  filter(Country == "Germany")

percentiles_FRA <- percentiles_0 %>%
  filter(Country == "France")

percentiles_ESP <- percentiles_0 %>%
  filter(Country == "Spain")

percentiles_ROM <- percentiles_0 %>%
  filter(Country == "Romania")

sample_GER <- pred_GER %>%
  mutate(Percentile = cut(.$.pred,
                          breaks = c(-Inf, percentiles_GER$burden_upper),
                          labels = percentiles_GER$Percentiles,
                          right = FALSE))%>%
  mutate(absolute = round(hh_expenditures_EURO_2018*.pred,1),
         .pred = round(.pred,4))

# hh_expenditures_EURO_2018
sample_GER_2.4.1 <- sample_GER %>%
  filter(hh_expenditures_EURO_2018 > 24000 & hh_expenditures_EURO_2018 < 25000)%>%
  mutate(hh_expenditures_EURO_2018 = "Weiß nicht")%>%
  bind_rows(mutate(sample_GER, hh_expenditures_EURO_2018 = as.character(round(hh_expenditures_EURO_2018))))%>%
  mutate(hh_expenditures_EURO_2018 = as.factor(hh_expenditures_EURO_2018))

sample_FRA <- pred_FRA %>%
  mutate(Percentile = cut(.$.pred,
                          breaks = c(-Inf, percentiles_FRA$burden_upper),
                          labels = percentiles_FRA$Percentiles,
                          right = FALSE))%>%
  mutate(absolute = round(hh_expenditures_EURO_2018*.pred,1),
         .pred = round(.pred,4))

# hh_expenditures_EURO_2018
sample_FRA_2.4.1 <- sample_FRA %>%
  filter(hh_expenditures_EURO_2018 > 20000 & hh_expenditures_EURO_2018 < 21000)%>%
  mutate(hh_expenditures_EURO_2018 = "Je ne sais pas")%>%
  bind_rows(mutate(sample_FRA, hh_expenditures_EURO_2018 = as.character(round(hh_expenditures_EURO_2018))))%>%
  mutate(hh_expenditures_EURO_2018 = as.factor(hh_expenditures_EURO_2018))

sample_ESP <- pred_ESP %>%
  mutate(Percentile = cut(.$.pred,
                          breaks = c(-Inf, percentiles_ESP$burden_upper),
                          labels = percentiles_ESP$Percentiles,
                          right = FALSE))%>%
  mutate(absolute = round(hh_expenditures_EURO_2018*.pred,1),
         .pred = round(.pred,4))

# hh_expenditures_EURO_2018
sample_ESP_2.4.1 <- sample_ESP %>%
  filter(hh_expenditures_EURO_2018 > 17000 & hh_expenditures_EURO_2018 < 19000)%>%
  mutate(hh_expenditures_EURO_2018 = "No lo sé")%>%
  bind_rows(mutate(sample_ESP, hh_expenditures_EURO_2018 = as.character(round(hh_expenditures_EURO_2018))))%>%
  mutate(hh_expenditures_EURO_2018 = as.factor(hh_expenditures_EURO_2018))

sample_ROM <- pred_ROM %>%
  mutate(Percentile = cut(.$.pred,
                          breaks = c(-Inf, percentiles_ROM$burden_upper),
                          labels = percentiles_ROM$Percentiles,
                          right = FALSE))%>%
  mutate(absolute = round(hh_expenditures_LEI_2018*.pred,1),
         .pred    = round(.pred,4))

# hh_expenditures_EURO_2018
sample_ROM_2.4.1 <- sample_ROM %>%
  filter(hh_expenditures_LEI_2018 > 29000 & hh_expenditures_LEI_2018 < 30000)%>%
  mutate(hh_expenditures_LEI_2018 = "Nu stiu asta")%>%
  bind_rows(mutate(sample_ROM, hh_expenditures_LEI_2018 = as.character(round(hh_expenditures_LEI_2018))))%>%
  mutate(hh_expenditures_LEI_2018 = as.factor(hh_expenditures_LEI_2018))

write_parquet(sample_GER_2.4.1, "../2_Data/Output/Output data/Combinations_Germany_P.parquet", compression = "gzip")  
write_parquet(sample_FRA_2.4.1, "../2_Data/Output/Output data/Combinations_France_P.parquet",  compression = "gzip")  
write_parquet(sample_ESP_2.4.1, "../2_Data/Output/Output data/Combinations_Spain_P.parquet",   compression = "gzip")  
write_parquet(sample_ROM_2.4.1, "../2_Data/Output/Output data/Combinations_Romania_P.parquet", compression = "gzip")  

# sample_GER <- pred_GER %>%
#   filter(heating_fuel == "Heizoel" & heating_type == "Block- o. Zentralheizung" & renting == "Eigentuemer")%>%
#   # filter(heating_fuel == "Fernheizung" & bundesland == "Berlin" & building_type == "Wohngebaeude" & heating_type == "Fernheizung" & building_year == "<1949" & number_of_cars == 0 & urban_type == "Verstaedterter Raum" & hh_expenditures_EURO_2018 > 50000)%>%
#   sample_n(., 1)%>%
#   mutate(Percentile = map_dbl(.pred, ~ find_percentile(., "Germany")))%>%
#   mutate("Text" = paste0(100-Percentile, "% aller Haushalte werden stärker betroffen sein. ", Percentile, " % aller Haushalte werden weniger stark betroffen sein."))%>%
#   mutate(absolute = hh_expenditures_EURO_2018*.pred)%>%
#   mutate(Text_costs = paste0("On average, households with your characteristics will face additional costs of ", round(absolute), "€ per year."))%>%
#   mutate(Text_rel_costs = paste0("This is equivalent to ", round(.pred*100,1), "% of your total annual expenditures."))

rm(sample_ESP, sample_ESP_2.4.1, percentiles_ESP, percentiles_FRA, percentiles_GER, percentiles_ROM, percentiles, percentiles_0,
   pred_ESP, pred_GER, pred_FRA, pred_ROM)

# 2.5   Figure ####

data_hist <- data_GER_0 %>%
  mutate(CO2_t_interest = CO2_t_transport + CO2_t_gas_direct)%>% # TBD
  mutate(exp_interest   = CO2_t_interest*40)%>% # TBD
  mutate(burden_interest = exp_interest/hh_expenditures_EURO_2018)%>%
  mutate(weights_adj = hh_weights/sum(hh_weights))

data_hist_1 <- density(data_hist$burden_interest, from = 0, weights = data_hist$weights_adj, bw = 0.002, adjust = 0.5)
data_hist_2 <- bind_cols(x = data_hist_1$x, y = data_hist_1$y)%>%
  mutate(area = ifelse(x > sample_GER$.pred, "Stärker betroffen", "Weniger stark betroffen"))%>%
  mutate(area = factor(area, levels = c("Weniger stark betroffen", "Stärker betroffen")))

P_1 <- ggplot()+
  geom_line(data = data_hist_2, aes(x = x, y = y), linetype = 1)+
  geom_ribbon(data = data_hist_2, aes(x = x, ymin = 0, ymax = y, fill = area))+
  geom_vline(xintercept = max(data_hist_2$x[data_hist_2$x < sample_GER$.pred]), color = "#0072B5FF")+
  geom_vline(xintercept = min(data_hist_2$x[data_hist_2$x > sample_GER$.pred]), color = "#0072B5FF")+
  theme_minimal()+
  coord_cartesian(xlim = c(0,0.1), ylim = c(0,max(data_hist_2$y)+2))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1))+
  scale_y_continuous(expand = c(0,0))+
  xlab("Zusätzliche Ausgaben in % der Gesamtausgaben")+
  scale_fill_manual(values = c("#FFDC91FF","#BC3C29FF"))+
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  labs(fill = "")

data_plot_1 <- expand_grid(x = seq(1,10,1), y = seq(1,10,1))%>%
  mutate(Position = (y-1)*10+x)%>%
  arrange(Position)%>%
  mutate(Percentile = c(100:1))%>%
  mutate(Interest = ifelse(Percentile == sample_GER$Percentile,1,0),
         Less     = ifelse(Percentile  < sample_GER$Percentile,1,0),
         More     = ifelse(Percentile  > sample_GER$Percentile,1,0))%>%
  mutate(Status = ifelse(Interest == 1, "Sie",
                         ifelse(Less == 1, "Weniger stark betroffen", "Stärker betroffen")))%>%
  mutate(Status_ENG = ifelse(Interest == 1, "You",
                             ifelse(Less == 1, "Less affected", "More affected")))%>%
  arrange(y)%>%
  mutate(x = rep(c(10:1),10))%>%
  mutate(Status = factor(Status, levels = c("Sie", "Weniger stark betroffen", "Stärker betroffen")))%>%
  mutate(Status_ENG = factor(Status_ENG, levels = c("You", "Less affected", "More affected")))

P_2 <- ggplot(data = data_plot_1, aes(x = x, y = y))+
  geom_point(shape = 22, size = 10, aes(fill = Status))+
  theme_void()+
  scale_fill_manual(values = c("#0072B5FF","#FFDC91FF", "#BC3C29FF"))+
  labs(fill = "")+
  theme(legend.position = "bottom")

data_plot_2 <- data.frame(x = c(1, sample_GER$Percentile - 0.75, sample_GER$Percentile - 0.25, sample_GER$Percentile + 0.25, sample_GER$Percentile + 0.75, 100))%>%
  expand_grid(., y = c(1,2))%>%
  mutate(group = c(rep(1,4), rep(2,4), rep(3,4)))

P_3 <- ggplot()+
  geom_ribbon(data = filter(data_plot_2, group == 1), 
              aes(x = x, ymin = 1, ymax = 2), fill = "#FFDC91FF")+
  geom_ribbon(data = filter(data_plot_2, group == 2), 
              aes(x = x, ymin = 1, ymax = 2), fill = "#0072B5FF")+
  geom_ribbon(data = filter(data_plot_2, group == 3), 
              aes(x = x, ymin = 1, ymax = 2), fill = "#BC3C29FF")+
  theme_void()

# With icons

data_plot_3 <- data_plot_1

P_4 <- ggplot(data_plot_3)+
  geom_text(aes(x = 100-Position, y = 1, colour = Status_ENG), label = "p", family = "wmpeople1", size = 3)+
  # geom_bracket(xmin = 0, xmax = 59, label = "Weniger stark betroffen", y.position = 1.5)+
  # geom_bracket(xmin = 61, xmax = 100, label = "Stärker betroffen", y.position = 1.5)+
  theme_void()+
  scale_colour_manual(values = c("#0072B5FF","#FFDC91FF","#BC3C29FF"))+
  labs(colour = "")+
  # coord_cartesian(ylim = c(0,2))+
  #guides(colour = "none")+
  theme(legend.position = "bottom",
        plot.margin = margin(0,0,0,0),
        legend.text = element_text(size = 9))

P_5 <- ggplot(data = data_plot_3, aes(x = x, y = y))+
  geom_text(aes(colour = Status), label = "p", family = "wmpeople1", size = 20)+
  theme_void()+
  scale_colour_manual(values = c("#0072B5FF","#FFDC91FF", "#BC3C29FF"))+
  labs(colour = "")+
  #guides(colour = "none")+
  theme(legend.position = "bottom",
        plot.margin = margin(0,0,0,0),
        legend.text = element_text(size = 25))

jpeg("../2_data/Output/Test/Figure_1.jpg", width = 15, height = 15, unit = "cm", res = 600)
print(P_1)
dev.off()

jpeg("../2_data/Output/Test/Figure_2.jpg", width = 15, height = 15, unit = "cm", res = 600)
print(P_2)
dev.off()

jpeg("../2_data/Output/Test/Figure_3.jpg", width = 15, height = 1, unit = "cm", res = 600)
print(P_3)
dev.off()

jpeg("../2_data/Output/Test/Figure_4.jpg", width = 15, height = 2, unit = "cm", res = 600)
print(P_4)
dev.off()

jpeg("../2_data/Output/Test/Figure_5.jpg", width = 6, height = 6, unit = "cm", res = 600)
print(P_5)
dev.off()

# 2.6   Figure Distribution ####

for(i in 1:100){
  
  data_plot_1 <- expand_grid(x = seq(1,10,1), y = seq(1,10,1))%>%
    mutate(Position = (y-1)*10+x)%>%
    arrange(Position)%>%
    mutate(Percentile = c(100:1))%>%
    mutate(Interest = ifelse(Percentile == i,1,0),
           Less     = ifelse(Percentile  < i,1,0),
           More     = ifelse(Percentile  > i,1,0))%>%
    mutate(Status = ifelse(Interest == 1, "Sie",
                           # ifelse(Less == 1, "Weniger stark betroffen", "Stärker betroffen")))%>%
                           ifelse(Less == 1, "Haushalte mit niedrigeren Kosten", "Haushalte mit höheren Kosten")))%>%
    mutate(Status_ESP = ifelse(Interest == 1, "Usted",
                               ifelse(Less == 1, "Hogares con menores costes", "Hogares con mayores costes")))%>%
    mutate(Status_FRA = ifelse(Interest == 1, "Vous",
                               ifelse(Less == 1, "Ménages à coûts moins élevés", "Ménages à coûts plus élevés")))%>%
    mutate(Status_ROM = ifelse(Interest == 1, "Tu", 
                               ifelse(Less == 1, "Familii cu costuri mai mici", "Familii cu costuri mai mari")))%>%
    arrange(y)%>%
    mutate(x = rep(c(10:1),10))%>%
    mutate(Status = factor(Status, levels = c("Sie", "Haushalte mit niedrigeren Kosten", "Haushalte mit höheren Kosten")))%>%
    mutate(Status_ESP = factor(Status_ESP, levels = c("Usted", "Hogares con menores costes", "Hogares con mayores costes")))%>%
    mutate(Status_FRA = factor(Status_FRA, levels = c("Vous", "Ménages à coûts moins élevés", "Ménages à coûts plus élevés")))%>%
    mutate(Status_ROM = factor(Status_ROM, levels = c("Tu", "Familii cu costuri mai mici", "Familii cu costuri mai mari")))
  
  # With icons
  
  data_plot_3 <- data_plot_1
  
  for(j in c("ROM", "ESP", "FRA", "GER")){
    
    if(j == "ROM"){
      if(i == 1){
        P_4 <- ggplot(data_plot_3)+
          geom_text(aes(x = 100-Position, y = 1, colour = Status_ROM), label = "p", family = "wmpeople1", size = 3)+
          # geom_bracket(xmin = 0, xmax = 59, label = "Weniger stark betroffen", y.position = 1.5)+
          # geom_bracket(xmin = 61, xmax = 100, label = "Stärker betroffen", y.position = 1.5)+
          theme_void()+
          scale_colour_manual(values = c("#0072B5FF","#BC3C29FF"))+
          labs(colour = "")+
          # coord_cartesian(ylim = c(0,2))+
          #guides(colour = "none")+
          theme(legend.position = "bottom",
                plot.margin = margin(0,0,0,0),
                legend.text = element_text(size = 9))
      }
      
      if(i != 1){
        P_4 <- ggplot(data_plot_3)+
          geom_text(aes(x = 100-Position, y = 1, colour = Status_ROM), label = "p", family = "wmpeople1", size = 3)+
          # geom_bracket(xmin = 0, xmax = 59, label = "Weniger stark betroffen", y.position = 1.5)+
          # geom_bracket(xmin = 61, xmax = 100, label = "Stärker betroffen", y.position = 1.5)+
          theme_void()+
          scale_colour_manual(values = c("#0072B5FF","#FFDC91FF","#BC3C29FF"))+
          labs(colour = "")+
          # coord_cartesian(ylim = c(0,2))+
          #guides(colour = "none")+
          theme(legend.position = "bottom",
                plot.margin = margin(0,0,0,0),
                legend.text = element_text(size = 9))
      }
      
      jpeg(sprintf("../2_Data/Output/Percentile Figures/%s/Figure_%s_%s.jpg",j, j, i), width = 15, height = 2, unit = "cm", res = 600)
      print(P_4)
      dev.off()
    }
    
    if(j == "ESP"){
      if(i == 1){
        P_4 <- ggplot(data_plot_3)+
          geom_text(aes(x = 100-Position, y = 1, colour = Status_ESP), label = "p", family = "wmpeople1", size = 3)+
          # geom_bracket(xmin = 0, xmax = 59, label = "Weniger stark betroffen", y.position = 1.5)+
          # geom_bracket(xmin = 61, xmax = 100, label = "Stärker betroffen", y.position = 1.5)+
          theme_void()+
          scale_colour_manual(values = c("#0072B5FF","#BC3C29FF"))+
          labs(colour = "")+
          # coord_cartesian(ylim = c(0,2))+
          #guides(colour = "none")+
          theme(legend.position = "bottom",
                plot.margin = margin(0,0,0,0),
                legend.text = element_text(size = 9))
      }
      
      if(i != 1){
        P_4 <- ggplot(data_plot_3)+
          geom_text(aes(x = 100-Position, y = 1, colour = Status_ESP), label = "p", family = "wmpeople1", size = 3)+
          # geom_bracket(xmin = 0, xmax = 59, label = "Weniger stark betroffen", y.position = 1.5)+
          # geom_bracket(xmin = 61, xmax = 100, label = "Stärker betroffen", y.position = 1.5)+
          theme_void()+
          scale_colour_manual(values = c("#0072B5FF","#FFDC91FF","#BC3C29FF"))+
          labs(colour = "")+
          # coord_cartesian(ylim = c(0,2))+
          #guides(colour = "none")+
          theme(legend.position = "bottom",
                plot.margin = margin(0,0,0,0),
                legend.text = element_text(size = 9))
      }
      
      jpeg(sprintf("../2_Data/Output/Percentile Figures/%s/Figure_%s_%s.jpg",j, j, i), width = 15, height = 2, unit = "cm", res = 600)
      print(P_4)
      dev.off()
    }
    
    if(j == "FRA"){
      if(i == 1){
        P_4 <- ggplot(data_plot_3)+
          geom_text(aes(x = 100-Position, y = 1, colour = Status_FRA), label = "p", family = "wmpeople1", size = 3)+
          # geom_bracket(xmin = 0, xmax = 59, label = "Weniger stark betroffen", y.position = 1.5)+
          # geom_bracket(xmin = 61, xmax = 100, label = "Stärker betroffen", y.position = 1.5)+
          theme_void()+
          scale_colour_manual(values = c("#0072B5FF","#BC3C29FF"))+
          labs(colour = "")+
          # coord_cartesian(ylim = c(0,2))+
          #guides(colour = "none")+
          theme(legend.position = "bottom",
                plot.margin = margin(0,0,0,0),
                legend.text = element_text(size = 9))
      }
      
      if(i != 1){
        P_4 <- ggplot(data_plot_3)+
          geom_text(aes(x = 100-Position, y = 1, colour = Status_FRA), label = "p", family = "wmpeople1", size = 3)+
          # geom_bracket(xmin = 0, xmax = 59, label = "Weniger stark betroffen", y.position = 1.5)+
          # geom_bracket(xmin = 61, xmax = 100, label = "Stärker betroffen", y.position = 1.5)+
          theme_void()+
          scale_colour_manual(values = c("#0072B5FF","#FFDC91FF","#BC3C29FF"))+
          labs(colour = "")+
          # coord_cartesian(ylim = c(0,2))+
          #guides(colour = "none")+
          theme(legend.position = "bottom",
                plot.margin = margin(0,0,0,0),
                legend.text = element_text(size = 9))
      }
      
      jpeg(sprintf("../2_Data/Output/Percentile Figures/%s/Figure_%s_%s.jpg",j, j, i), width = 15, height = 2, unit = "cm", res = 600)
      print(P_4)
      dev.off()
    }
    
    if(j == "GER"){
      if(i == 1){
        P_4 <- ggplot(data_plot_3)+
          geom_text(aes(x = 100-Position, y = 1, colour = Status), label = "p", family = "wmpeople1", size = 3)+
          # geom_bracket(xmin = 0, xmax = 59, label = "Weniger stark betroffen", y.position = 1.5)+
          # geom_bracket(xmin = 61, xmax = 100, label = "Stärker betroffen", y.position = 1.5)+
          theme_void()+
          scale_colour_manual(values = c("#0072B5FF","#BC3C29FF"))+
          labs(colour = "")+
          # coord_cartesian(ylim = c(0,2))+
          #guides(colour = "none")+
          theme(legend.position = "bottom",
                plot.margin = margin(0,0,0,0),
                legend.text = element_text(size = 9))
      }
      
      if(i != 1){
        P_4 <- ggplot(data_plot_3)+
          geom_text(aes(x = 100-Position, y = 1, colour = Status), label = "p", family = "wmpeople1", size = 3)+
          # geom_bracket(xmin = 0, xmax = 59, label = "Weniger stark betroffen", y.position = 1.5)+
          # geom_bracket(xmin = 61, xmax = 100, label = "Stärker betroffen", y.position = 1.5)+
          theme_void()+
          scale_colour_manual(values = c("#0072B5FF","#FFDC91FF","#BC3C29FF"))+
          labs(colour = "")+
          # coord_cartesian(ylim = c(0,2))+
          #guides(colour = "none")+
          theme(legend.position = "bottom",
                plot.margin = margin(0,0,0,0),
                legend.text = element_text(size = 9))
      }
      
      jpeg(sprintf("../2_Data/Output/Percentile Figures/%s/Figure_%s_%s.jpg",j, j, i), width = 15, height = 2, unit = "cm", res = 600)
      print(P_4)
      dev.off()
    }
    
  }
  
}



# 3     Model per percentiles - what are characteristics of being more/less affected? ####

# data_3 <- data_GER_0 %>%
#   mutate(CO2_t_interest = CO2_t_transport + CO2_t_gas_direct)%>% # TBD
#   mutate(exp_interest   = CO2_t_interest*40)%>% # TBD
#   mutate(burden_interest = exp_interest/hh_expenditures_EURO_2018)%>%
#   mutate(weights_adj = hh_weights/sum(hh_weights))%>%
#   mutate(Percentiles = as.numeric(binning(burden_interest, bins = 100, method = "wtd.quantile", weights = hh_weights)))

# data_3 <- data_FRA %>%
#   mutate(weights_adj = hh_weights/sum(hh_weights))%>%
#   mutate(Percentiles = as.numeric(binning(burden_interest, bins = 100, method = "wtd.quantile", weights = hh_weights)))

# data_3 <- data_ESP %>%
#   mutate(weights_adj = hh_weights/sum(hh_weights))%>%
#   mutate(Percentiles = as.numeric(binning(burden_interest, bins = 100, method = "wtd.quantile", weights = hh_weights)))

# data_3 <- data_ROM %>%
#   mutate(weights_adj = hh_weights/sum(hh_weights))%>%
#   mutate(Percentiles = as.numeric(binning(burden_interest, bins = 100, method = "wtd.quantile", weights = hh_weights)))

data_out_0 <- data.frame()

data_shap_0 <- data.frame()
  
for (i in c(1:99)){
  # for 99 it is the same as for 100, but reverse
  print(paste0("Start: ", i))
  Country <- "France"
  time_1 <- Sys.time()
  
  data_3.1 <- data_3 %>%
    mutate(more = ifelse(Percentiles > i,1,0))
  
  data_0.1 <- data_3.1 %>%
    #select(more, any_of(criteria_GER))
    select(more, any_of(criteria_FRA))
    #select(more, any_of(criteria_ESP))
    #select(more, any_of(criteria_ROM))
    
  data_0.2 <- data_0.1 %>%
    initial_split(prop = 0.8)
  
  # Data for training
  data_0.2.train <- data_0.2 %>%
    training()
  
  # Data for testing
  data_0.2.test <- data_0.2 %>%
    testing()
  
  rm(data_0.1, data_0.2)
  
  # Feature engineering
  if(Country == "Germany"){
    recipe_0 <- recipe(more ~ .,
                       data = data_0.2.train)%>%
      # Deletes all columns with any NA
      step_filter_missing(all_predictors(), threshold = 0)%>%
      # Remove minimum number of columns such that correlations are less than 0.9
      step_corr(all_numeric(), -all_outcomes(), threshold = 0.9)%>%
      # should have very few unique observations for factors
      step_other(all_nominal(),-building_type, -heating_type, -bundesland, -building_year, -renting, threshold = 0.03)%>%
      step_dummy(all_nominal())
  }
  
  if(Country == "Romania"){
    recipe_0 <- recipe(more ~ .,
                       data = data_0.2.train)%>%
      # Deletes all columns with any NA
      step_filter_missing(all_predictors(), threshold = 0)%>%
      # Remove minimum number of columns such that correlations are less than 0.9
      step_corr(all_numeric(), -all_outcomes(), threshold = 0.9)%>%
      # should have very few unique observations for factors
      step_other(all_nominal(), -heating_fuel, -cooking_fuel, threshold = 0.03)%>%
      step_dummy(all_nominal())
  }
  
  if(Country == "Spain"){
    recipe_0 <- recipe(more ~ .,
                       data = data_0.2.train)%>%
      # Deletes all columns with any NA
      step_filter_missing(all_predictors(), threshold = 0)%>%
      # Remove minimum number of columns such that correlations are less than 0.9
      step_corr(all_numeric(), -all_outcomes(), threshold = 0.9)%>%
      # should have very few unique observations for factors
      step_other(all_nominal(), -heating_fuel, -water_energy, threshold = 0.03)%>%
      step_dummy(all_nominal())
  }
  
  if(Country == "France"){
    recipe_0 <- recipe(more ~ .,
                       data = data_0.2.train)%>%
      # Deletes all columns with any NA
      step_filter_missing(all_predictors(), threshold = 0)%>%
      # Remove minimum number of columns such that correlations are less than 0.9
      step_corr(all_numeric(), -all_outcomes(), threshold = 0.9)%>%
      # should have very few unique observations for factors
      step_other(occupation, threshold = 0.05)%>%
      step_other(all_nominal(), -heating_fuel, -housing_type, -urban_type, -province, -occupation, threshold = 0.05)%>%
      step_dummy(all_nominal())
  }
  
  data_0.2.training <- recipe_0 %>%
    prep(training = data_0.2.train)%>%
    bake(new_data = NULL)
  
  data_0.2.testing <- recipe_0 %>%
    prep(training = data_0.2.test)%>%
    bake(new_data = NULL)
  
  # # Five-fold cross-validation
  # 
  # folds_1 <- vfold_cv(data_0.2.training, v = 5)
  # 
  # # Setup model to be tuned
  # 
  # model_brt <- boost_tree(
  #   trees         = 1000,
  #   tree_depth    = tune(), # maximum depth of tree
  #   learn_rate    = tune(), # the higher the learning rate the faster - default 0.3
  #   # min_n       = tune(),
  #   mtry          = tune(), # fraction of features to be selected for each tree (0.5/0.7/1)
  #   # stop_iter   = tune(),
  #   # sample_size = tune()
  # )%>%
  #   set_mode("regression")%>%
  #   set_engine("xgboost")
  # 
  # # Create a tuning grid - 16 different models for the tuning space
  # 
  # grid_1 <- grid_latin_hypercube(
  #   tree_depth(),
  #   learn_rate(c(-3,-0.5)),# tuning parameters
  #   mtry(c(round((ncol(data_0.2.training)-1)/2,0), ncol(data_0.2.training)-1)),
  #   size = 15)%>%
  #   # default parameters
  #   bind_rows(data.frame(tree_depth = 6, learn_rate = 0.3, mtry = ncol(data_0.2.training)-1))
  # 
  # # Tune the model - cover the entire parameter space without running every combination
  # 
  # print("Start computing")
  # 
  # doParallel::registerDoParallel()
  # 
  # time_1 <- Sys.time()
  # 
  # model_brt_1 <- tune_grid(model_brt,
  #                          equal ~ .,
  #                          resamples = folds_1,
  #                          grid      = grid_1,
  #                          metrics   = metric_set(mae, rmse, rsq))
  # 
  # time_2 <- Sys.time()
  # 
  # doParallel::stopImplicitCluster()
  # 
  # print("End computing")
  # 
  # # Collect metrics of tuned models
  # 
  # metrics_1 <- collect_metrics(model_brt_1)
  # 
  # model_brt_1.1 <- select_best(model_brt_1, metric = "mae")
  # 
  # metrics_1.1 <- metrics_1 %>%
  #   filter(.config == model_brt_1.1$.config[1])
  # 
  # rm(metrics_1, model_brt_1.1, metrics_1.1, model_brt, model_brt_1)
  
  if(Country == "Germany"){
    tree_depth_0 <- 7
    learn_rate_0 <- 0.01
    mtry_0       <- 30
  }
  
  if(Country == "Spain"){
    tree_depth_0 <- 7
    learn_rate_0 <- 0.01
    mtry_0       <- 20
  }
  
  if(Country == "Romania"){
    tree_depth_0 <- 7
    learn_rate_0 <- 0.01
    mtry_0       <- 20
  }
  
  if(Country == "France"){
    tree_depth_0 <- 9
    learn_rate_0 <- 0.01
    mtry_0       <- 38
  }
  
  # Fit best model after tuning
  model_brt <- boost_tree(
    trees         = 1000,
    tree_depth    = tree_depth_0,
    learn_rate    = learn_rate_0,
    mtry          = mtry_0       
  )%>%
    set_mode("regression")%>%
    set_engine("xgboost")
  
  model_brt_2 <- model_brt %>%
    fit(more ~ .,
        data = data_0.2.training)
  
  predictions_0 <- augment(model_brt_2, new_data = data_0.2.testing)
  rsq_0  <- rsq(predictions_0,  truth = more, estimate = .pred)
  
  print(paste0("R-Squared for ", Country, " amounts to ", round(rsq_0$.estimate, 2)*100, "%."))
  
  data_0.2.testing_matrix <- data_0.2.testing %>%
    select(-more)%>%
    as.matrix()
  
  data_0.2.testing.out <- data_0.2.testing %>%
    mutate(Percentile = i)%>%
    mutate(ID = 1:n())
  
  time_3 <- Sys.time()
  
  shap_1 <- predict(extract_fit_engine(model_brt_2),
                    data_0.2.testing_matrix,
                    predcontrib = TRUE,
                    approxcontrib = FALSE)
  
  time_4 <- Sys.time()
  
  shap_1.0 <- shap_1 %>%
    as_tibble()%>%
    mutate(Percentile = i)%>%
    mutate(ID = 1:n())
  
  data_out_0 <- data_out_0 %>%
    bind_rows(data_0.2.testing.out)
  
  data_shap_0 <- data_shap_0 %>%
    bind_rows(shap_1.0)
  
  rm(recipe_0, data_0.2.test, data_0.2.testing, data_0.2.train, data_0.2.training,
     model_brt, model_brt_2, rsq_0, predictions_0, learn_rate_0, mtry_0, tree_depth_0, data_0.2.testing_matrix, data_3.1, shap_1, shap_1.0, data_0.2.testing.out)
  
  time_2 <- Sys.time()
  
  print(paste0("Time: ", time_2 - time_1, " seconds"))
  print(paste0("Shap time: ", time_4 - time_3, "seconds"))
  
}

# Output to parquet

# write_parquet(data_out_0,  "../2_Data/Output/Percentiles/Data_Out_GER.parquet")
# write_parquet(data_shap_0, "../2_Data/Output/Percentiles/Data_SHAP_GER.parquet")
# write_parquet(data_out_0,  "../2_Data/Output/Percentiles/Data_Out_FRA.parquet")
# write_parquet(data_shap_0, "../2_Data/Output/Percentiles/Data_SHAP_FRA.parquet")
# write_parquet(data_out_0,  "../2_Data/Output/Percentiles/Data_Out_ESP.parquet")
# write_parquet(data_shap_0, "../2_Data/Output/Percentiles/Data_SHAP_ESP.parquet")
# write_parquet(data_out_0,  "../2_Data/Output/Percentiles/Data_Out_ROM.parquet")
# write_parquet(data_shap_0, "../2_Data/Output/Percentiles/Data_SHAP_ROM.parquet")

# Input parquet (2)
# data_out_GER  <- read_parquet("../2_Data/Output/Percentiles/Data_Out_GER.parquet")
# data_shap_GER <- read_parquet("../2_Data/Output/Percentiles/Data_SHAP_GER.parquet")
# data_out_FRA  <- read_parquet("../2_Data/Output/Percentiles/Data_Out_FRA.parquet")
# data_shap_FRA <- read_parquet("../2_Data/Output/Percentiles/Data_SHAP_FRA.parquet")
# data_out_ESP  <- read_parquet("../2_Data/Output/Percentiles/Data_Out_ESP.parquet")
# data_shap_ESP <- read_parquet("../2_Data/Output/Percentiles/Data_SHAP_ESP.parquet")
data_out_ROM  <- read_parquet("../2_Data/Output/Percentiles/Data_Out_ROM.parquet")
data_shap_ROM <- read_parquet("../2_Data/Output/Percentiles/Data_SHAP_ROM.parquet")

data_analysis <- data.frame()

# Needs to extract the five most important factors
for (i in c(1:99)){
  print(i)
  
  # data_out_0 <- data_out_GER %>%
  # data_out_0 <- data_out_FRA %>%
  # data_out_0 <- data_out_ESP %>%
  data_out_0 <- data_out_ROM %>%
    filter(Percentile == i)
  
  # data_shap_0 <- data_shap_GER %>%
  # data_shap_0 <- data_shap_FRA %>%
  # data_shap_0 <- data_shap_ESP %>%
  data_shap_0 <- data_shap_ROM %>%
    filter(Percentile == i)
  
  data_shap_1 <- data_shap_0 %>%
    summarise_all(~ mean(abs(.)))%>%
    select(-BIAS, -ID, - Percentile)%>%
    pivot_longer(everything(), names_to = "variable", values_to = "SHAP_contribution")%>%
    arrange(desc(SHAP_contribution))%>%
    mutate(tot_contribution = sum(SHAP_contribution))%>%
    mutate(share_SHAP       = SHAP_contribution/tot_contribution)%>%
    select(-tot_contribution)
  
  data_shap_2 <- data_shap_1 %>%
    mutate(VAR_0 = ifelse(grepl("education", variable), "Education", 
                          ifelse(grepl("heating_fuel", variable), "Heating_Fuel", 
                                 ifelse(grepl("building_type", variable), "Building_Type", 
                                        ifelse(grepl("urban_type", variable), "Urban_Type", 
                                               ifelse(grepl("renting", variable), "Renting", 
                                                      ifelse(grepl("employment", variable), "Employment", 
                                                             ifelse(grepl("bundesland", variable), "Bundesland", 
                                                                    ifelse(grepl("heating_type", variable), "Heating_Type", 
                                                                           ifelse(grepl("building_year", variable), "Building_Year", 
                                                                                  ifelse(grepl("industry", variable), "Industry", 
                                                                                         ifelse(grepl("ausbildung", variable), "Ausbildung", 
                                                                                                ifelse(grepl("cooking_fuel", variable), "Cooking_Fuel", 
                                                                                                       ifelse(grepl("housing_type", variable), "Housing_Type", 
                                                                                                              ifelse(grepl("construction_year", variable), "construction_year", 
                                                                                                                     ifelse(grepl("occupation", variable), "Occupation", 
                                                                                                                            ifelse(grepl("province", variable), "Province", 
                                                                                                                                   ifelse(grepl("nationality", variable), "Nationality", variable))))))))))))))))))%>%
    mutate(VAR_0 = ifelse(grepl("district", variable), "District",
                          ifelse(grepl("house_age", variable), "House Age",
                                 ifelse(grepl("tenant", variable), "Tenant",
                                        ifelse(grepl("urban_identif_2", variable), "Urban_Identif_2",
                                               ifelse(grepl("urban_identif", variable) & VAR_0 != "Urban_Identif_2", "Urban_Identif",
                                                      ifelse(grepl("water_energy", variable), "Water energy", 
                                                             ifelse(grepl("house_type", variable), "House Type", VAR_0))))))))
    
  data_shap_2.1 <- data_shap_2 %>%
    group_by(VAR_0)%>%
    summarise(share_SHAP = sum(share_SHAP))%>%
    ungroup()%>%
    arrange(desc(share_SHAP))%>%
    mutate(share_sum = cumsum(share_SHAP))%>%
    filter(share_sum < 0.85)%>%
    mutate(number = 1:n())
  
  data_shap_2.2 <- data_shap_2 %>%
    filter(VAR_0 %in% data_shap_2.1$VAR_0)
  
  # For each variable, we now need one specific analysis
  
  data_analysis_0 <- data.frame(value = c(NA))
  
  if("Heating_Fuel" %in% data_shap_2.1$VAR_0){
    print("Heating Fuel")
    
    data_shap_HF <- data_shap_0 %>%
      select(ID, starts_with("heating_fuel"))%>%
      rename_at(vars(starts_with("heating_fuel")), ~ str_replace(., "^", "SHAP_"))
    
    data_out_HF <- data_out_0 %>%
      select(ID, starts_with("heating_fuel"))
    
    data_HF <- left_join(data_shap_HF, data_out_HF, by = "ID")
    
    data_HF_0 <- data.frame()
    
    if("heating_fuel_Gas" %in% colnames(data_HF)){
      data_HF_1 <- data_HF %>%
        group_by(heating_fuel_Gas)%>%
        summarise(SHAP_heating_fuel_Gas = mean(SHAP_heating_fuel_Gas))%>%
        ungroup()%>%
        mutate(VAR = "heating_fuel_Gas")%>%
        rename(value = "heating_fuel_Gas", SHAP = "SHAP_heating_fuel_Gas")
      
      data_HF_0 <- data_HF_0 %>%
        bind_rows(data_HF_1)
    }
    
    if("heating_fuel_Heizoel" %in% colnames(data_HF)){
      data_HF_2 <- data_HF %>%
        group_by(heating_fuel_Heizoel)%>%
        summarise(SHAP_heating_fuel_Heizoel = mean(SHAP_heating_fuel_Heizoel))%>%
        ungroup()%>%
        mutate(VAR = "heating_fuel_Heizoel")%>%
        rename(value = "heating_fuel_Heizoel", SHAP = "SHAP_heating_fuel_Heizoel")
      
      data_HF_0 <- data_HF_0 %>%
        bind_rows(data_HF_2)
    }
    
    if("heating_fuel_Strom" %in% colnames(data_HF)){
      data_HF_3 <- data_HF %>%
        group_by(heating_fuel_Strom)%>%
        summarise(SHAP_heating_fuel_Strom = mean(SHAP_heating_fuel_Strom))%>%
        ungroup()%>%
        mutate(VAR = "heating_fuel_Strom")%>%
        rename(value = "heating_fuel_Strom", SHAP = "SHAP_heating_fuel_Strom")
      
      data_HF_0 <- data_HF_0 %>%
        bind_rows(data_HF_3)
    }
    
    if("heating_fuel_NA." %in% colnames(data_HF)){
      data_HF_4 <- data_HF %>%
        group_by(heating_fuel_NA.)%>%
        summarise(SHAP_heating_fuel_NA. = mean(SHAP_heating_fuel_NA.))%>%
        ungroup()%>%
        mutate(VAR = "heating_fuel_NA.")%>%
        rename(value = "heating_fuel_NA.", SHAP = "SHAP_heating_fuel_NA.")
      
      data_HF_0 <- data_HF_0 %>%
        bind_rows(data_HF_4)
    }
    
    if("heating_fuel_other" %in% colnames(data_HF)){
      data_HF_5 <- data_HF %>%
        group_by(heating_fuel_other)%>%
        summarise(SHAP_heating_fuel_other = mean(SHAP_heating_fuel_other))%>%
        ungroup()%>%
        mutate(VAR = "heating_fuel_other")%>%
        rename(value = "heating_fuel_other", SHAP = "SHAP_heating_fuel_other")
      
      data_HF_0 <- data_HF_0 %>%
        bind_rows(data_HF_5)
    }
    
    if("heating_fuel_Bois" %in% colnames(data_HF)){
      data_HF_1 <- data_HF %>%
        group_by(heating_fuel_Bois)%>%
        summarise(SHAP_heating_fuel_Bois = mean(SHAP_heating_fuel_Bois))%>%
        ungroup()%>%
        mutate(VAR = "heating_fuel_Bois")%>%
        rename(value = "heating_fuel_Bois", SHAP = "SHAP_heating_fuel_Bois")
      
      data_HF_0 <- data_HF_0 %>%
        bind_rows(data_HF_1)
    }
    
    if("heating_fuel_Butane..propane..gaz.en.citerne" %in% colnames(data_HF)){
      data_HF_2 <- data_HF %>%
        group_by(heating_fuel_Butane..propane..gaz.en.citerne)%>%
        summarise(SHAP_heating_fuel_Butane..propane..gaz.en.citerne = mean(SHAP_heating_fuel_Butane..propane..gaz.en.citerne))%>%
        ungroup()%>%
        mutate(VAR = "heating_fuel_Butane..propane..gaz.en.citerne")%>%
        rename(value = "heating_fuel_Butane..propane..gaz.en.citerne", SHAP = "SHAP_heating_fuel_Butane..propane..gaz.en.citerne")
      
      data_HF_0 <- data_HF_0 %>%
        bind_rows(data_HF_2)
    }
    
    if("heating_fuel_Charbon..coke" %in% colnames(data_HF)){
      data_HF_3 <- data_HF %>%
        group_by(heating_fuel_Charbon..coke)%>%
        summarise(SHAP_heating_fuel_Charbon..coke = mean(SHAP_heating_fuel_Charbon..coke))%>%
        ungroup()%>%
        mutate(VAR = "heating_fuel_Charbon..coke")%>%
        rename(value = "heating_fuel_Charbon..coke", SHAP = "SHAP_heating_fuel_Charbon..coke")
      
      data_HF_0 <- data_HF_0 %>%
        bind_rows(data_HF_3)
    }
    
    if("heating_fuel_Electricité" %in% colnames(data_HF)){
      data_HF_4 <- data_HF %>%
        group_by(heating_fuel_Electricité)%>%
        summarise(SHAP_heating_fuel_Electricité = mean(SHAP_heating_fuel_Electricité))%>%
        ungroup()%>%
        mutate(VAR = "heating_fuel_Electricité")%>%
        rename(value = "heating_fuel_Electricité", SHAP = "SHAP_heating_fuel_Electricité")
      
      data_HF_0 <- data_HF_0 %>%
        bind_rows(data_HF_4)
    }
    
    if("heating_fuel_Fuel..mazout..pétrole" %in% colnames(data_HF)){
      data_HF_5 <- data_HF %>%
        group_by(heating_fuel_Fuel..mazout..pétrole)%>%
        summarise(SHAP_heating_fuel_Fuel..mazout..pétrole = mean(SHAP_heating_fuel_Fuel..mazout..pétrole))%>%
        ungroup()%>%
        mutate(VAR = "heating_fuel_Fuel..mazout..pétrole")%>%
        rename(value = "heating_fuel_Fuel..mazout..pétrole", SHAP = "SHAP_heating_fuel_Fuel..mazout..pétrole")
      
      data_HF_0 <- data_HF_0 %>%
        bind_rows(data_HF_5)
    }
    
    if("heating_fuel_Gaz.de.ville" %in% colnames(data_HF)){
      data_HF_6 <- data_HF %>%
        group_by(heating_fuel_Gaz.de.ville)%>%
        summarise(SHAP_heating_fuel_Gaz.de.ville = mean(SHAP_heating_fuel_Gaz.de.ville))%>%
        ungroup()%>%
        mutate(VAR = "heating_fuel_Gaz.de.ville")%>%
        rename(value = "heating_fuel_Gaz.de.ville", SHAP = "SHAP_heating_fuel_Gaz.de.ville")
      
      data_HF_0 <- data_HF_0 %>%
        bind_rows(data_HF_6)
    }
    
    if("heating_fuel_Géothermie" %in% colnames(data_HF)){
      data_HF_7 <- data_HF %>%
        group_by(heating_fuel_Géothermie)%>%
        summarise(SHAP_heating_fuel_Géothermie = mean(SHAP_heating_fuel_Géothermie))%>%
        ungroup()%>%
        mutate(VAR = "heating_fuel_Géothermie")%>%
        rename(value = "heating_fuel_Géothermie", SHAP = "SHAP_heating_fuel_Géothermie")
      
      data_HF_0 <- data_HF_0 %>%
        bind_rows(data_HF_7)
    }
    
    if("heating_fuel_Other" %in% colnames(data_HF)){
      data_HF_8 <- data_HF %>%
        group_by(heating_fuel_Other)%>%
        summarise(SHAP_heating_fuel_Other = mean(SHAP_heating_fuel_Other))%>%
        ungroup()%>%
        mutate(VAR = "heating_fuel_Other")%>%
        rename(value = "heating_fuel_Other", SHAP = "SHAP_heating_fuel_Other")
      
      data_HF_0 <- data_HF_0 %>%
        bind_rows(data_HF_8)
    }
    
    if("heating_fuel_Solaire" %in% colnames(data_HF)){
      data_HF_9 <- data_HF %>%
        group_by(heating_fuel_Solaire)%>%
        summarise(SHAP_heating_fuel_Solaire = mean(SHAP_heating_fuel_Solaire))%>%
        ungroup()%>%
        mutate(VAR = "heating_fuel_Solaire")%>%
        rename(value = "heating_fuel_Solaire", SHAP = "SHAP_heating_fuel_Solaire")
      
      data_HF_0 <- data_HF_0 %>%
        bind_rows(data_HF_9)
    }
    
    if("heating_fuel_Electricidad" %in% colnames(data_HF)){
      data_HF_1 <- data_HF %>%
        group_by(heating_fuel_Electricidad)%>%
        summarise(SHAP_heating_fuel_Electricidad = mean(SHAP_heating_fuel_Electricidad))%>%
        ungroup()%>%
        mutate(VAR = "heating_fuel_Electricidad")%>%
        rename(value = "heating_fuel_Electricidad", SHAP = "SHAP_heating_fuel_Electricidad")
      
      data_HF_0 <- data_HF_0 %>%
        bind_rows(data_HF_1)
    }
    
    if("heating_fuel_Gas.licuado" %in% colnames(data_HF)){
      data_HF_2 <- data_HF %>%
        group_by(heating_fuel_Gas.licuado)%>%
        summarise(SHAP_heating_fuel_Gas.licuado = mean(SHAP_heating_fuel_Gas.licuado))%>%
        ungroup()%>%
        mutate(VAR = "heating_fuel_Gas.licuado")%>%
        rename(value = "heating_fuel_Gas.licuado", SHAP = "SHAP_heating_fuel_Gas.licuado")
      
      data_HF_0 <- data_HF_0 %>%
        bind_rows(data_HF_2)
    }
    
    if("heating_fuel_Gas.natural" %in% colnames(data_HF)){
      data_HF_3 <- data_HF %>%
        group_by(heating_fuel_Gas.natural)%>%
        summarise(SHAP_heating_fuel_Gas.natural = mean(SHAP_heating_fuel_Gas.natural))%>%
        ungroup()%>%
        mutate(VAR = "heating_fuel_Gas.natural")%>%
        rename(value = "heating_fuel_Gas.natural", SHAP = "SHAP_heating_fuel_Gas.natural")
      
      data_HF_0 <- data_HF_0 %>%
        bind_rows(data_HF_3)
    }
    
    if("heating_fuel_No.aplicable...si.CALEF.6." %in% colnames(data_HF)){
      data_HF_4 <- data_HF %>%
        group_by(heating_fuel_No.aplicable...si.CALEF.6.)%>%
        summarise(SHAP_heating_fuel_No.aplicable...si.CALEF.6. = mean(SHAP_heating_fuel_No.aplicable...si.CALEF.6.))%>%
        ungroup()%>%
        mutate(VAR = "heating_fuel_No.aplicable...si.CALEF.6.")%>%
        rename(value = "heating_fuel_No.aplicable...si.CALEF.6.", SHAP = "SHAP_heating_fuel_No.aplicable...si.CALEF.6.")
      
      data_HF_0 <- data_HF_0 %>%
        bind_rows(data_HF_4)
    }
    
    if("heating_fuel_Otras.y.líquidos" %in% colnames(data_HF)){
      data_HF_5 <- data_HF %>%
        group_by(heating_fuel_Otras.y.líquidos)%>%
        summarise(SHAP_heating_fuel_Otras.y.líquidos = mean(SHAP_heating_fuel_Otras.y.líquidos))%>%
        ungroup()%>%
        mutate(VAR = "heating_fuel_Otras.y.líquidos")%>%
        rename(value = "heating_fuel_Otras.y.líquidos", SHAP = "SHAP_heating_fuel_Otras.y.líquidos")
      
      data_HF_0 <- data_HF_0 %>%
        bind_rows(data_HF_5)
    }
    
    if("heating_fuel_Electricity" %in% colnames(data_HF)){
      data_HF_1 <- data_HF %>%
        group_by(heating_fuel_Electricity)%>%
        summarise(SHAP_heating_fuel_Electricity = mean(SHAP_heating_fuel_Electricity))%>%
        ungroup()%>%
        mutate(VAR = "heating_fuel_Electricity")%>%
        rename(value = "heating_fuel_Electricity", SHAP = "SHAP_heating_fuel_Electricity")
      
      data_HF_0 <- data_HF_0 %>%
        bind_rows(data_HF_1)
    }
    
    if("heating_fuel_Natural.gas" %in% colnames(data_HF)){
      data_HF_2 <- data_HF %>%
        group_by(heating_fuel_Natural.gas)%>%
        summarise(SHAP_heating_fuel_Natural.gas = mean(SHAP_heating_fuel_Natural.gas))%>%
        ungroup()%>%
        mutate(VAR = "heating_fuel_Natural.gas")%>%
        rename(value = "heating_fuel_Natural.gas", SHAP = "SHAP_heating_fuel_Natural.gas")
      
      data_HF_0 <- data_HF_0 %>%
        bind_rows(data_HF_2)
    }
    
    if("heating_fuel_Natural.gas.stove" %in% colnames(data_HF)){
      data_HF_3 <- data_HF %>%
        group_by(heating_fuel_Natural.gas.stove)%>%
        summarise(SHAP_heating_fuel_Natural.gas.stove = mean(SHAP_heating_fuel_Natural.gas.stove))%>%
        ungroup()%>%
        mutate(VAR = "heating_fuel_Natural.gas.stove")%>%
        rename(value = "heating_fuel_Natural.gas.stove", SHAP = "SHAP_heating_fuel_Natural.gas.stove")
      
      data_HF_0 <- data_HF_0 %>%
        bind_rows(data_HF_3)
    }
    
    if("heating_fuel_Wood" %in% colnames(data_HF)){
      data_HF_4 <- data_HF %>%
        group_by(heating_fuel_Wood)%>%
        summarise(SHAP_heating_fuel_Wood = mean(SHAP_heating_fuel_Wood))%>%
        ungroup()%>%
        mutate(VAR = "heating_fuel_Wood")%>%
        rename(value = "heating_fuel_Wood", SHAP = "SHAP_heating_fuel_Wood")
      
      data_HF_0 <- data_HF_0 %>%
        bind_rows(data_HF_4)
    }
    
    if("heating_fuel_Wood..coal.or.oil.stove" %in% colnames(data_HF)){
      data_HF_5 <- data_HF %>%
        group_by(heating_fuel_Wood..coal.or.oil.stove)%>%
        summarise(SHAP_heating_fuel_Wood..coal.or.oil.stove = mean(SHAP_heating_fuel_Wood..coal.or.oil.stove))%>%
        ungroup()%>%
        mutate(VAR = "heating_fuel_Wood..coal.or.oil.stove")%>%
        rename(value = "heating_fuel_Wood..coal.or.oil.stove", SHAP = "SHAP_heating_fuel_Wood..coal.or.oil.stove")
      
      data_HF_0 <- data_HF_0 %>%
        bind_rows(data_HF_5)
    }
    
    data_HF_0 <- data_HF_0 %>%
      mutate(number = data_shap_2.1$number[data_shap_2.1$VAR_0 == "Heating_Fuel"])
    
    data_analysis_0 <- data_analysis_0 %>%
      bind_rows(data_HF_0)
    
    rm(data_HF_0, data_HF, data_shap_HF, data_out_HF, data_HF_1, data_HF_2, data_HF_3, data_HF_4, data_HF_5, data_HF_8)
    
  }
  
  if("Cooking_Fuel" %in% data_shap_2.1$VAR_0){
    print("Cooking Fuel")
    
    data_shap_CF <- data_shap_0 %>%
      select(ID, starts_with("cooking_fuel"))%>%
      rename_at(vars(starts_with("cooking_fuel")), ~ str_replace(., "^", "SHAP_"))
    
    data_out_CF <- data_out_0 %>%
      select(ID, starts_with("cooking_fuel"))
    
    data_CF <- left_join(data_shap_CF, data_out_CF, by = "ID")
    
    data_CF_0 <- data.frame()
    
    if("cooking_fuel_Other" %in% colnames(data_CF)){
      data_CF_1 <- data_CF %>%
        group_by(cooking_fuel_Other)%>%
        summarise(SHAP_cooking_fuel_Other = mean(SHAP_cooking_fuel_Other))%>%
        ungroup()%>%
        mutate(VAR = "cooking_fuel_Other")%>%
        rename(value = "cooking_fuel_Other", SHAP = "SHAP_cooking_fuel_Other")
      
      data_CF_0 <- data_CF_0 %>%
        bind_rows(data_CF_1)
    }

    if("cooking_fuel_Natural.gas" %in% colnames(data_CF)){
      data_CF_2 <- data_CF %>%
        group_by(cooking_fuel_Natural.gas)%>%
        summarise(SHAP_cooking_fuel_Natural.gas = mean(SHAP_cooking_fuel_Natural.gas))%>%
        ungroup()%>%
        mutate(VAR = "cooking_fuel_Natural.gas")%>%
        rename(value = "cooking_fuel_Natural.gas", SHAP = "SHAP_cooking_fuel_Natural.gas")
      
      data_CF_0 <- data_CF_0 %>%
        bind_rows(data_CF_2)
    }
    
    data_CF_0 <- data_CF_0 %>%
      mutate(number = data_shap_2.1$number[data_shap_2.1$VAR_0 == "Cooking_Fuel"])
    
    data_analysis_0 <- data_analysis_0 %>%
      bind_rows(data_CF_0)
    
    rm(data_CF_0, data_CF, data_shap_CF, data_out_CF, data_CF_1, data_CF_2)
    
  }
  
  if("number_of_cars" %in% data_shap_2.1$VAR_0){
    print("number_of_cars")
    
    data_shap_car <- data_shap_0 %>%
      select(ID, starts_with("number_of_cars"))%>%
      rename_at(vars(starts_with("number_of_cars")), ~ str_replace(., "^", "SHAP_"))
    
    data_out_car <- data_out_0 %>%
      select(ID, starts_with("number_of_cars"))
    
    data_car <- left_join(data_shap_car, data_out_car, by = "ID")
    
    data_car_0 <- data_car %>%
      group_by(number_of_cars)%>%
      summarise(SHAP_number_of_cars = mean(SHAP_number_of_cars))%>%
      ungroup()%>%
      mutate(VAR = "number_of_cars")%>%
      rename(value = "number_of_cars", SHAP = "SHAP_number_of_cars")
    
    data_car_0 <- data_car_0 %>%
      mutate(number = data_shap_2.1$number[data_shap_2.1$VAR_0 == "number_of_cars"])
    
    data_analysis_0 <- data_analysis_0 %>%
      bind_rows(data_car_0)
    
    rm(data_shap_car, data_out_car, data_car, data_car_0)
    
  }
  
  if("Renting" %in% data_shap_2.1$VAR_0){
    print("Renting")
    
    data_shap_Rent <- data_shap_0 %>%
      select(ID, starts_with("Renting"))%>%
      rename_at(vars(starts_with("Renting")), ~ str_replace(., "^", "SHAP_"))
    
    data_out_Rent <- data_out_0 %>%
      select(ID, starts_with("Renting"))
    
    data_Rent <- left_join(data_shap_Rent, data_out_Rent, by = "ID")
    
    data_Rent_0 <- data.frame()
    
    if("renting_Mieter" %in% colnames(data_Rent)){
      data_Rent_1 <- data_Rent %>%
        group_by(renting_Mieter)%>%
        summarise(SHAP_renting_Mieter = mean(SHAP_renting_Mieter))%>%
        ungroup()%>%
        mutate(VAR = "renting_Mieter")%>%
        rename(value = "renting_Mieter", SHAP = "SHAP_renting_Mieter")
      
      data_Rent_0 <- data_Rent_0 %>%
        bind_rows(data_Rent_1)
    }
    
    if("renting_Mietfrei" %in% colnames(data_Rent)){
      data_Rent_2 <- data_Rent %>%
        group_by(renting_Mietfrei)%>%
        summarise(SHAP_renting_Mietfrei = mean(SHAP_renting_Mietfrei))%>%
        ungroup()%>%
        mutate(VAR = "renting_Mietfrei")%>%
        rename(value = "renting_Mietfrei", SHAP = "SHAP_renting_Mietfrei")
      
      data_Rent_0 <- data_Rent_0 %>%
        bind_rows(data_Rent_2)
    }
    
    data_Rent_0 <- data_Rent_0 %>%
      mutate(number = data_shap_2.1$number[data_shap_2.1$VAR_0 == "Renting"])
    
    data_analysis_0 <- data_analysis_0 %>%
      bind_rows(data_Rent_0)
    
    rm(data_Rent_0, data_Rent, data_shap_Rent, data_out_Rent, data_Rent_1, data_Rent_2)
    
  }
  
  if("Tenant" %in% data_shap_2.1$VAR_0){
    print("Tenant")
    
    data_shap_Rent <- data_shap_0 %>%
      select(ID, starts_with("Tenant"))%>%
      rename_at(vars(starts_with("Tenant")), ~ str_replace(., "^", "SHAP_"))
    
    data_out_Rent <- data_out_0 %>%
      select(ID, starts_with("Tenant"))
    
    data_Rent <- left_join(data_shap_Rent, data_out_Rent, by = "ID")
    
    data_Rent_0 <- data.frame()
    
    if("renting_Mieter" %in% colnames(data_Rent)){
      data_Rent_1 <- data_Rent %>%
        group_by(renting_Mieter)%>%
        summarise(SHAP_renting_Mieter = mean(SHAP_renting_Mieter))%>%
        ungroup()%>%
        mutate(VAR = "renting_Mieter")%>%
        rename(value = "renting_Mieter", SHAP = "SHAP_renting_Mieter")
      
      data_Rent_0 <- data_Rent_0 %>%
        bind_rows(data_Rent_1)
    }
    
    if("renting_Mietfrei" %in% colnames(data_Rent)){
      data_Rent_2 <- data_Rent %>%
        group_by(renting_Mietfrei)%>%
        summarise(SHAP_renting_Mietfrei = mean(SHAP_renting_Mietfrei))%>%
        ungroup()%>%
        mutate(VAR = "renting_Mietfrei")%>%
        rename(value = "renting_Mietfrei", SHAP = "SHAP_renting_Mietfrei")
      
      data_Rent_0 <- data_Rent_0 %>%
        bind_rows(data_Rent_2)
    }
    
    if("tenant_Locataire" %in% colnames(data_Rent)){
      data_Rent_1 <- data_Rent %>%
        group_by(tenant_Locataire)%>%
        summarise(SHAP_tenant_Locataire = mean(SHAP_tenant_Locataire))%>%
        ungroup()%>%
        mutate(VAR = "tenant_Locataire")%>%
        rename(value = "tenant_Locataire", SHAP = "SHAP_tenant_Locataire")
      
      data_Rent_0 <- data_Rent_0 %>%
        bind_rows(data_Rent_1)
    }
    
    if("tenant_Other" %in% colnames(data_Rent)){
      data_Rent_2 <- data_Rent %>%
        group_by(tenant_Other)%>%
        summarise(SHAP_tenant_Other = mean(SHAP_tenant_Other))%>%
        ungroup()%>%
        mutate(VAR = "tenant_Other")%>%
        rename(value = "tenant_Other", SHAP = "SHAP_tenant_Other")
      
      data_Rent_0 <- data_Rent_0 %>%
        bind_rows(data_Rent_2)
    }
    
    if("tenant_Propriétaire.ou.copropriétaire..y.compris.en.indivision....vous.n.avez.pas.de.remboursement.de.prêt.sur.votre.habitation" %in% colnames(data_Rent)){
      data_Rent_3 <- data_Rent %>%
        group_by(tenant_Propriétaire.ou.copropriétaire..y.compris.en.indivision....vous.n.avez.pas.de.remboursement.de.prêt.sur.votre.habitation)%>%
        summarise(SHAP_tenant_Propriétaire.ou.copropriétaire..y.compris.en.indivision....vous.n.avez.pas.de.remboursement.de.prêt.sur.votre.habitation = mean(SHAP_tenant_Propriétaire.ou.copropriétaire..y.compris.en.indivision....vous.n.avez.pas.de.remboursement.de.prêt.sur.votre.habitation))%>%
        ungroup()%>%
        mutate(VAR = "tenant_Propriétaire.ou.copropriétaire..y.compris.en.indivision....vous.n.avez.pas.de.remboursement.de.prêt.sur.votre.habitation")%>%
        rename(value = "tenant_Propriétaire.ou.copropriétaire..y.compris.en.indivision....vous.n.avez.pas.de.remboursement.de.prêt.sur.votre.habitation", SHAP = "SHAP_tenant_Propriétaire.ou.copropriétaire..y.compris.en.indivision....vous.n.avez.pas.de.remboursement.de.prêt.sur.votre.habitation")
      
      data_Rent_0 <- data_Rent_0 %>%
        bind_rows(data_Rent_3)
    }
    
    if("tenant_Cesión" %in% colnames(data_Rent)){
      data_Rent_1 <- data_Rent %>%
        group_by(tenant_Cesión)%>%
        summarise(SHAP_tenant_Cesión = mean(SHAP_tenant_Cesión))%>%
        ungroup()%>%
        mutate(VAR = "tenant_Cesión")%>%
        rename(value = "tenant_Cesión", SHAP = "SHAP_tenant_Cesión")
      
      data_Rent_0 <- data_Rent_0 %>%
        bind_rows(data_Rent_1)
    }
    
    if("tenant_Propriedad" %in% colnames(data_Rent)){
      data_Rent_2 <- data_Rent %>%
        group_by(tenant_Propriedad)%>%
        summarise(SHAP_tenant_Propriedad = mean(SHAP_tenant_Propriedad))%>%
        ungroup()%>%
        mutate(VAR = "tenant_Propriedad")%>%
        rename(value = "tenant_Propriedad", SHAP = "SHAP_tenant_Propriedad")
      
      data_Rent_0 <- data_Rent_0 %>%
        bind_rows(data_Rent_2)
    }
    
    data_Rent_0 <- data_Rent_0 %>%
      mutate(number = data_shap_2.1$number[data_shap_2.1$VAR_0 == "Renting" | data_shap_2.1$VAR_0 == "Tenant"])
    
    data_analysis_0 <- data_analysis_0 %>%
      bind_rows(data_Rent_0)
    
    rm(data_Rent_0, data_Rent, data_shap_Rent, data_out_Rent, data_Rent_1, data_Rent_2)
    
  }
  
  if("Building_Type" %in% data_shap_2.1$VAR_0){
    print("Building Type")
    
    data_shap_Build <- data_shap_0 %>%
      select(ID, starts_with("building_type"))%>%
      rename_at(vars(starts_with("building_type")), ~ str_replace(., "^", "SHAP_"))
    
    data_out_Build <- data_out_0 %>%
      select(ID, starts_with("building_type"))
    
    data_Build <- left_join(data_shap_Build, data_out_Build, by = "ID")
    
    data_Build_0 <- data.frame()
    
    if("building_type_Einfamilienhaus" %in% colnames(data_Build)){
      data_Build_1 <- data_Build %>%
        group_by(building_type_Einfamilienhaus)%>%
        summarise(SHAP_building_type_Einfamilienhaus = mean(SHAP_building_type_Einfamilienhaus))%>%
        ungroup()%>%
        mutate(VAR = "building_type_Einfamilienhaus")%>%
        rename(value = "building_type_Einfamilienhaus", SHAP = "SHAP_building_type_Einfamilienhaus")
      
      data_Build_0 <- data_Build_0 %>%
        bind_rows(data_Build_1)
    }
    
    if("building_type_Sonstiges" %in% colnames(data_Build)){
      data_Build_2 <- data_Build %>%
        group_by(building_type_Sonstiges)%>%
        summarise(SHAP_building_type_Sonstiges = mean(SHAP_building_type_Sonstiges))%>%
        ungroup()%>%
        mutate(VAR = "building_type_Sonstiges")%>%
        rename(value = "building_type_Sonstiges", SHAP = "SHAP_building_type_Sonstiges")
      
      data_Build_0 <- data_Build_0 %>%
        bind_rows(data_Build_2)
    }
    
    if("building_type_Wohngebaeude" %in% colnames(data_Build)){
      data_Build_3 <- data_Build %>%
        group_by(building_type_Wohngebaeude)%>%
        summarise(SHAP_building_type_Wohngebaeude = mean(SHAP_building_type_Wohngebaeude))%>%
        ungroup()%>%
        mutate(VAR = "building_type_Wohngebaeude")%>%
        rename(value = "building_type_Wohngebaeude", SHAP = "SHAP_building_type_Wohngebaeude")
      
      data_Build_0 <- data_Build_0 %>%
        bind_rows(data_Build_3)
    }
    
    if("building_type_Zweifamilienhaus" %in% colnames(data_Build)){
      data_Build_4 <- data_Build %>%
        group_by(building_type_Zweifamilienhaus)%>%
        summarise(SHAP_building_type_Zweifamilienhaus = mean(SHAP_building_type_Zweifamilienhaus))%>%
        ungroup()%>%
        mutate(VAR = "building_type_Zweifamilienhaus")%>%
        rename(value = "building_type_Zweifamilienhaus", SHAP = "SHAP_building_type_Zweifamilienhaus")
      
      data_Build_0 <- data_Build_0 %>%
        bind_rows(data_Build_4)
    }
    
    data_Build_0 <- data_Build_0 %>%
      mutate(number = data_shap_2.1$number[data_shap_2.1$VAR_0 == "Building_Type"])
    
    data_analysis_0 <- data_analysis_0 %>%
      bind_rows(data_Build_0)
    
    rm(data_Build_0, data_Build, data_shap_Build, data_out_Build, data_Build_1, data_Build_2, data_Build_3, data_Build_4)
    
  }
  
  if("Housing_Type" %in% data_shap_2.1$VAR_0){
    print("Housing Type")
    
    data_shap_Build <- data_shap_0 %>%
      select(ID, starts_with("housing_type"))%>%
      rename_at(vars(starts_with("housing_type")), ~ str_replace(., "^", "SHAP_"))
    
    data_out_Build <- data_out_0 %>%
      select(ID, starts_with("housing_type"))
    
    data_Build <- left_join(data_shap_Build, data_out_Build, by = "ID")
    
    data_Build_0 <- data.frame()
    
    if("housing_type_Other" %in% colnames(data_Build)){
      data_Build_1 <- data_Build %>%
        group_by(housing_type_Other)%>%
        summarise(SHAP_housing_type_Other = mean(SHAP_housing_type_Other))%>%
        ungroup()%>%
        mutate(VAR = "housing_type_Other")%>%
        rename(value = "housing_type_Other", SHAP = "SHAP_housing_type_Other")
      
      data_Build_0 <- data_Build_0 %>%
        bind_rows(data_Build_1)
    }
    
    if("housing_type_Une.maison.individuelle" %in% colnames(data_Build)){
      data_Build_2 <- data_Build %>%
        group_by(housing_type_Une.maison.individuelle)%>%
        summarise(SHAP_housing_type_Une.maison.individuelle = mean(SHAP_housing_type_Une.maison.individuelle))%>%
        ungroup()%>%
        mutate(VAR = "housing_type_Une.maison.individuelle")%>%
        rename(value = "housing_type_Une.maison.individuelle", SHAP = "SHAP_housing_type_Une.maison.individuelle")
      
      data_Build_0 <- data_Build_0 %>%
        bind_rows(data_Build_2)
    }
    
    data_Build_0 <- data_Build_0 %>%
      mutate(number = data_shap_2.1$number[data_shap_2.1$VAR_0 == "Housing_Type"])
    
    data_analysis_0 <- data_analysis_0 %>%
      bind_rows(data_Build_0)
    
    rm(data_Build_0, data_Build, data_shap_Build, data_out_Build, data_Build_1, data_Build_2)
    
  }
  
  if("hh_expenditures_EURO_2018" %in% data_shap_2.1$VAR_0){
    print("hh_expenditures_EURO_2018")
    
    data_shap_EXP <- data_shap_0 %>%
      select(ID, starts_with("hh_expenditures_EURO_2018"))%>%
      rename_at(vars(starts_with("hh_expenditures_EURO_2018")), ~ str_replace(., "^", "SHAP_"))
    
    data_out_EXP <- data_out_0 %>%
      select(ID, starts_with("hh_expenditures_EURO_2018"))
    
    data_EXP <- left_join(data_shap_EXP, data_out_EXP, by = "ID")
    
    model_EXP <- lm(SHAP_hh_expenditures_EURO_2018 ~ hh_expenditures_EURO_2018 + I(hh_expenditures_EURO_2018^2), data = data_EXP)
    
    tidy_EXP <- tidy(model_EXP)%>%
      mutate(VAR = "hh_expenditures_EURO_2018")%>%
      rename(SHAP = estimate, value = term)%>%
      select(VAR, SHAP, value)
    
    tidy_EXP <- tidy_EXP %>%
      mutate(number = data_shap_2.1$number[data_shap_2.1$VAR_0 == "hh_expenditures_EURO_2018"])
    
    data_analysis_0 <- data_analysis_0 %>%
      mutate(value = as.character(value))%>%
      bind_rows(tidy_EXP)
    
    rm(data_EXP, data_shap_EXP, data_out_EXP, model_EXP, tidy_EXP)
    
  }
  
  if("hh_expenditures_LEI_2018" %in% data_shap_2.1$VAR_0){
    print("hh_expenditures_LEI_2018")
    
    data_shap_EXP <- data_shap_0 %>%
      select(ID, starts_with("hh_expenditures_LEI_2018"))%>%
      rename_at(vars(starts_with("hh_expenditures_LEI_2018")), ~ str_replace(., "^", "SHAP_"))
    
    data_out_EXP <- data_out_0 %>%
      select(ID, starts_with("hh_expenditures_LEI_2018"))
    
    data_EXP <- left_join(data_shap_EXP, data_out_EXP, by = "ID")
    
    model_EXP <- lm(SHAP_hh_expenditures_LEI_2018 ~ hh_expenditures_LEI_2018 + I(hh_expenditures_LEI_2018^2), data = data_EXP)
    
    tidy_EXP <- tidy(model_EXP)%>%
      mutate(VAR = "hh_expenditures_LEI_2018")%>%
      rename(SHAP = estimate, value = term)%>%
      select(VAR, SHAP, value)
    
    tidy_EXP <- tidy_EXP %>%
      mutate(number = data_shap_2.1$number[data_shap_2.1$VAR_0 == "hh_expenditures_LEI_2018"])
    
    data_analysis_0 <- data_analysis_0 %>%
      mutate(value = as.character(value))%>%
      bind_rows(tidy_EXP)
    
    rm(data_EXP, data_shap_EXP, data_out_EXP, model_EXP, tidy_EXP)
    
  }
  
  if("Bundesland" %in% data_shap_2.1$VAR_0){
    print("Bundesland")
    
    data_shap_BL <- data_shap_0 %>%
      select(ID, starts_with("bundesland"))%>%
      rename_at(vars(starts_with("bundesland")), ~ str_replace(., "^", "SHAP_"))
    
    data_out_BL <- data_out_0 %>%
      select(ID, starts_with("bundesland"))
    
    data_BL <- left_join(data_shap_BL, data_out_BL, by = "ID")
    
    data_BL_0 <- data.frame()
    
    if("bundesland_Bayern" %in% colnames(data_BL)){
      data_BL_1 <- data_BL %>%
        group_by(bundesland_Bayern)%>%
        summarise(SHAP_bundesland_Bayern = mean(SHAP_bundesland_Bayern))%>%
        ungroup()%>%
        mutate(VAR = "bundesland_Bayern")%>%
        rename(value = "bundesland_Bayern", SHAP = "SHAP_bundesland_Bayern")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_1)
    }
    
    if("bundesland_Berlin" %in% colnames(data_BL)){
      data_BL_2 <- data_BL %>%
        group_by(bundesland_Berlin)%>%
        summarise(SHAP_bundesland_Berlin = mean(SHAP_bundesland_Berlin))%>%
        ungroup()%>%
        mutate(VAR = "bundesland_Berlin")%>%
        rename(value = "bundesland_Berlin", SHAP = "SHAP_bundesland_Berlin")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_2)
    }
    
    if("bundesland_Brandenburg" %in% colnames(data_BL)){
      data_BL_3 <- data_BL %>%
        group_by(bundesland_Brandenburg)%>%
        summarise(SHAP_bundesland_Brandenburg = mean(SHAP_bundesland_Brandenburg))%>%
        ungroup()%>%
        mutate(VAR = "bundesland_Brandenburg")%>%
        rename(value = "bundesland_Brandenburg", SHAP = "SHAP_bundesland_Brandenburg")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_3)
    }
    
    if("bundesland_Bremen" %in% colnames(data_BL)){
      data_BL_4 <- data_BL %>%
        group_by(bundesland_Bremen)%>%
        summarise(SHAP_bundesland_Bremen = mean(SHAP_bundesland_Bremen))%>%
        ungroup()%>%
        mutate(VAR = "bundesland_Bremen")%>%
        rename(value = "bundesland_Bremen", SHAP = "SHAP_bundesland_Bremen")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_4)
    }
    
    if("bundesland_Hamburg" %in% colnames(data_BL)){
      data_BL_5 <- data_BL %>%
        group_by(bundesland_Hamburg)%>%
        summarise(SHAP_bundesland_Hamburg = mean(SHAP_bundesland_Hamburg))%>%
        ungroup()%>%
        mutate(VAR = "bundesland_Hamburg")%>%
        rename(value = "bundesland_Hamburg", SHAP = "SHAP_bundesland_Hamburg")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_5)
    }
    
    if("bundesland_Hessen" %in% colnames(data_BL)){
      data_BL_6 <- data_BL %>%
        group_by(bundesland_Hessen)%>%
        summarise(SHAP_bundesland_Hessen = mean(SHAP_bundesland_Hessen))%>%
        ungroup()%>%
        mutate(VAR = "bundesland_Hessen")%>%
        rename(value = "bundesland_Hessen", SHAP = "SHAP_bundesland_Hessen")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_6)
    }
    
    if("bundesland_Mecklenburg.Vorpommern" %in% colnames(data_BL)){
      data_BL_7 <- data_BL %>%
        group_by(bundesland_Mecklenburg.Vorpommern)%>%
        summarise(SHAP_bundesland_Mecklenburg.Vorpommern = mean(SHAP_bundesland_Mecklenburg.Vorpommern))%>%
        ungroup()%>%
        mutate(VAR = "bundesland_Mecklenburg.Vorpommern")%>%
        rename(value = "bundesland_Mecklenburg.Vorpommern", SHAP = "SHAP_bundesland_Mecklenburg.Vorpommern")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_7)
    }
    
    if("bundesland_Niedersachsen" %in% colnames(data_BL)){
      data_BL_8 <- data_BL %>%
        group_by(bundesland_Niedersachsen)%>%
        summarise(SHAP_bundesland_Niedersachsen = mean(SHAP_bundesland_Niedersachsen))%>%
        ungroup()%>%
        mutate(VAR = "bundesland_Niedersachsen")%>%
        rename(value = "bundesland_Niedersachsen", SHAP = "SHAP_bundesland_Niedersachsen")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_8)
    }
    
    if("bundesland_Nordrhein.Westfalen" %in% colnames(data_BL)){
      data_BL_9 <- data_BL %>%
        group_by(bundesland_Nordrhein.Westfalen)%>%
        summarise(SHAP_bundesland_Nordrhein.Westfalen = mean(SHAP_bundesland_Nordrhein.Westfalen))%>%
        ungroup()%>%
        mutate(VAR = "bundesland_Nordrhein.Westfalen")%>%
        rename(value = "bundesland_Nordrhein.Westfalen", SHAP = "SHAP_bundesland_Nordrhein.Westfalen")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_9)
    }
    
    if("bundesland_Rheinland.Pfalz" %in% colnames(data_BL)){
      data_BL_10 <- data_BL %>%
        group_by(bundesland_Rheinland.Pfalz)%>%
        summarise(SHAP_bundesland_Rheinland.Pfalz = mean(SHAP_bundesland_Rheinland.Pfalz))%>%
        ungroup()%>%
        mutate(VAR = "bundesland_Rheinland.Pfalz")%>%
        rename(value = "bundesland_Rheinland.Pfalz", SHAP = "SHAP_bundesland_Rheinland.Pfalz")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_10)
    }
    
    if("bundesland_Saarland" %in% colnames(data_BL)){
      data_BL_11 <- data_BL %>%
        group_by(bundesland_Saarland)%>%
        summarise(SHAP_bundesland_Saarland = mean(SHAP_bundesland_Saarland))%>%
        ungroup()%>%
        mutate(VAR = "bundesland_Saarland")%>%
        rename(value = "bundesland_Saarland", SHAP = "SHAP_bundesland_Saarland")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_11)
    }
    
    if("bundesland_Sachsen" %in% colnames(data_BL)){
      data_BL_12 <- data_BL %>%
        group_by(bundesland_Sachsen)%>%
        summarise(SHAP_bundesland_Sachsen = mean(SHAP_bundesland_Sachsen))%>%
        ungroup()%>%
        mutate(VAR = "bundesland_Sachsen")%>%
        rename(value = "bundesland_Sachsen", SHAP = "SHAP_bundesland_Sachsen")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_12)
    }
    
    if("bundesland_Sachsen.Anhalt" %in% colnames(data_BL)){
      data_BL_13 <- data_BL %>%
        group_by(bundesland_Sachsen.Anhalt)%>%
        summarise(SHAP_bundesland_Sachsen.Anhalt = mean(SHAP_bundesland_Sachsen.Anhalt))%>%
        ungroup()%>%
        mutate(VAR = "bundesland_Sachsen.Anhalt")%>%
        rename(value = "bundesland_Sachsen.Anhalt", SHAP = "SHAP_bundesland_Sachsen.Anhalt")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_13)
    }
    
    if("bundesland_Schleswig.Holstein" %in% colnames(data_BL)){
      data_BL_14 <- data_BL %>%
        group_by(bundesland_Schleswig.Holstein)%>%
        summarise(SHAP_bundesland_Schleswig.Holstein = mean(SHAP_bundesland_Schleswig.Holstein))%>%
        ungroup()%>%
        mutate(VAR = "bundesland_Schleswig.Holstein")%>%
        rename(value = "bundesland_Schleswig.Holstein", SHAP = "SHAP_bundesland_Schleswig.Holstein")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_14)
    }
    
    if("bundesland_Thueringen" %in% colnames(data_BL)){
      data_BL_15 <- data_BL %>%
        group_by(bundesland_Thueringen)%>%
        summarise(SHAP_bundesland_Thueringen = mean(SHAP_bundesland_Thueringen))%>%
        ungroup()%>%
        mutate(VAR = "bundesland_Thueringen")%>%
        rename(value = "bundesland_Thueringen", SHAP = "SHAP_bundesland_Thueringen")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_15)
    }
    
    data_BL_0 <- data_BL_0 %>%
      mutate(number = data_shap_2.1$number[data_shap_2.1$VAR_0 == "Bundesland"])%>%
      mutate(value = as.character(value))
    
    data_analysis_0 <- data_analysis_0 %>%
      mutate(value = as.character(value))%>%
      bind_rows(data_BL_0)
    
    rm(data_BL_0, data_BL, data_shap_BL, data_out_BL, data_BL_1, data_BL_2, data_BL_3, data_BL_4, data_BL_5, data_BL_6, data_BL_7, data_BL_8, data_BL_9, 
       data_BL_10, data_BL_11, data_BL_12, data_BL_13, data_BL_14, data_BL_15)
    
  }
  
  if("Province" %in% data_shap_2.1$VAR_0){
    print("Province")
    
    data_shap_BL <- data_shap_0 %>%
      select(ID, starts_with("Province"))%>%
      rename_at(vars(starts_with("Province")), ~ str_replace(., "^", "SHAP_"))
    
    data_out_BL <- data_out_0 %>%
      select(ID, starts_with("Province"))
    
    data_BL <- left_join(data_shap_BL, data_out_BL, by = "ID")
    
    data_BL_0 <- data.frame()
    
    if("province_Centre.est" %in% colnames(data_BL)){
      data_BL_1 <- data_BL %>%
        group_by(province_Centre.est)%>%
        summarise(SHAP_province_Centre.est = mean(SHAP_province_Centre.est))%>%
        ungroup()%>%
        mutate(VAR = "province_Centre.est")%>%
        rename(value = "province_Centre.est", SHAP = "SHAP_province_Centre.est")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_1)
    }
    
    if("province_Dom" %in% colnames(data_BL)){
      data_BL_2 <- data_BL %>%
        group_by(province_Dom)%>%
        summarise(SHAP_province_Dom = mean(SHAP_province_Dom))%>%
        ungroup()%>%
        mutate(VAR = "province_Dom")%>%
        rename(value = "province_Dom", SHAP = "SHAP_province_Dom")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_2)
    }
    
    if("province_Est" %in% colnames(data_BL)){
      data_BL_3 <- data_BL %>%
        group_by(province_Est)%>%
        summarise(SHAP_province_Est = mean(SHAP_province_Est))%>%
        ungroup()%>%
        mutate(VAR = "province_Est")%>%
        rename(value = "province_Est", SHAP = "SHAP_province_Est")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_3)
    }
    
    if("province_Méditerranée" %in% colnames(data_BL)){
      data_BL_4 <- data_BL %>%
        group_by(province_Méditerranée)%>%
        summarise(SHAP_province_Méditerranée = mean(SHAP_province_Méditerranée))%>%
        ungroup()%>%
        mutate(VAR = "province_Méditerranée")%>%
        rename(value = "province_Méditerranée", SHAP = "SHAP_province_Méditerranée")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_4)
    }
    
    if("province_Nord" %in% colnames(data_BL)){
      data_BL_5 <- data_BL %>%
        group_by(province_Nord)%>%
        summarise(SHAP_province_Nord = mean(SHAP_province_Nord))%>%
        ungroup()%>%
        mutate(VAR = "province_Nord")%>%
        rename(value = "province_Nord", SHAP = "SHAP_province_Nord")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_5)
    }
    
    if("province_Ouest" %in% colnames(data_BL)){
      data_BL_6 <- data_BL %>%
        group_by(province_Ouest)%>%
        summarise(SHAP_province_Ouest = mean(SHAP_province_Ouest))%>%
        ungroup()%>%
        mutate(VAR = "province_Ouest")%>%
        rename(value = "province_Ouest", SHAP = "SHAP_province_Ouest")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_6)
    }
    
    if("province_Région.parisienne" %in% colnames(data_BL)){
      data_BL_7 <- data_BL %>%
        group_by(province_Région.parisienne)%>%
        summarise(SHAP_province_Région.parisienne = mean(SHAP_province_Région.parisienne))%>%
        ungroup()%>%
        mutate(VAR = "province_Région.parisienne")%>%
        rename(value = "province_Région.parisienne", SHAP = "SHAP_province_Région.parisienne")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_7)
    }
    
    if("province_Sud.ouest" %in% colnames(data_BL)){
      data_BL_8 <- data_BL %>%
        group_by(province_Sud.ouest)%>%
        summarise(SHAP_province_Sud.ouest = mean(SHAP_province_Sud.ouest))%>%
        ungroup()%>%
        mutate(VAR = "province_Sud.ouest")%>%
        rename(value = "province_Sud.ouest", SHAP = "SHAP_province_Sud.ouest")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_8)
    }
    
    if("province_Centru" %in% colnames(data_BL)){
      data_BL_1 <- data_BL %>%
        group_by(province_Centru)%>%
        summarise(SHAP_province_Centru = mean(SHAP_province_Centru))%>%
        ungroup()%>%
        mutate(VAR = "province_Centru")%>%
        rename(value = "province_Centru", SHAP = "SHAP_province_Centru")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_1)
    }
    
    if("province_Nord.Est" %in% colnames(data_BL)){
      data_BL_2 <- data_BL %>%
        group_by(province_Nord.Est)%>%
        summarise(SHAP_province_Nord.Est = mean(SHAP_province_Nord.Est))%>%
        ungroup()%>%
        mutate(VAR = "province_Nord.Est")%>%
        rename(value = "province_Nord.Est", SHAP = "SHAP_province_Nord.Est")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_2)
    }
    
    if("province_Nord.Vest" %in% colnames(data_BL)){
      data_BL_3 <- data_BL %>%
        group_by(province_Nord.Vest)%>%
        summarise(SHAP_province_Nord.Vest = mean(SHAP_province_Nord.Vest))%>%
        ungroup()%>%
        mutate(VAR = "province_Nord.Vest")%>%
        rename(value = "province_Nord.Vest", SHAP = "SHAP_province_Nord.Vest")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_3)
    }
    
    if("province_Sud.Est" %in% colnames(data_BL)){
      data_BL_4 <- data_BL %>%
        group_by(province_Sud.Est)%>%
        summarise(SHAP_province_Sud.Est = mean(SHAP_province_Sud.Est))%>%
        ungroup()%>%
        mutate(VAR = "province_Sud.Est")%>%
        rename(value = "province_Sud.Est", SHAP = "SHAP_province_Sud.Est")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_4)
    }
    
    if("province_Sud.Muntenia" %in% colnames(data_BL)){
      data_BL_5 <- data_BL %>%
        group_by(province_Sud.Muntenia)%>%
        summarise(SHAP_province_Sud.Muntenia = mean(SHAP_province_Sud.Muntenia))%>%
        ungroup()%>%
        mutate(VAR = "province_Sud.Muntenia")%>%
        rename(value = "province_Sud.Muntenia", SHAP = "SHAP_province_Sud.Muntenia")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_5)
    }
    
    if("province_Sud.Vest.Oltenia" %in% colnames(data_BL)){
      data_BL_6 <- data_BL %>%
        group_by(province_Sud.Vest.Oltenia)%>%
        summarise(SHAP_province_Sud.Vest.Oltenia = mean(SHAP_province_Sud.Vest.Oltenia))%>%
        ungroup()%>%
        mutate(VAR = "province_Sud.Vest.Oltenia")%>%
        rename(value = "province_Sud.Vest.Oltenia", SHAP = "SHAP_province_Sud.Vest.Oltenia")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_6)
    }
    
    if("province_Vest" %in% colnames(data_BL)){
      data_BL_7 <- data_BL %>%
        group_by(province_Vest)%>%
        summarise(SHAP_province_Vest = mean(SHAP_province_Vest))%>%
        ungroup()%>%
        mutate(VAR = "province_Vest")%>%
        rename(value = "province_Vest", SHAP = "SHAP_province_Vest")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_7)
    }
    
    data_BL_0 <- data_BL_0 %>%
      mutate(number = data_shap_2.1$number[data_shap_2.1$VAR_0 == "Province"])%>%
      mutate(value = as.character(value))
    
    data_analysis_0 <- data_analysis_0 %>%
      mutate(value = as.character(value))%>%
      bind_rows(data_BL_0)
    
    rm(data_BL_0, data_BL, data_shap_BL, data_out_BL, data_BL_1, data_BL_2, data_BL_3, data_BL_4, data_BL_5, data_BL_6, data_BL_7)
    
  }
  
  if("Urban_Type" %in% data_shap_2.1$VAR_0){
    print("Urban_Type")
    
    data_shap_Urban <- data_shap_0 %>%
      select(ID, starts_with("urban_type"))%>%
      rename_at(vars(starts_with("urban_type")), ~ str_replace(., "^", "SHAP_"))
    
    data_out_Urban <- data_out_0 %>%
      select(ID, starts_with("urban_type"))
    
    data_Urban <- left_join(data_shap_Urban, data_out_Urban, by = "ID")
    
    data_Urban_0 <- data.frame()
    
    if("urban_type_Verstaedterter.Raum" %in% colnames(data_Urban)){
      data_Urban_1 <- data_Urban %>%
        group_by(urban_type_Verstaedterter.Raum)%>%
        summarise(SHAP_urban_type_Verstaedterter.Raum = mean(SHAP_urban_type_Verstaedterter.Raum))%>%
        ungroup()%>%
        mutate(VAR = "urban_type_Verstaedterter.Raum")%>%
        rename(value = "urban_type_Verstaedterter.Raum", SHAP = "SHAP_urban_type_Verstaedterter.Raum")
      
      data_Urban_0 <- data_Urban_0 %>%
        bind_rows(data_Urban_1)
    }
    
    if("urban_type_Laendlicher.Raum" %in% colnames(data_Urban)){
      data_Urban_2 <- data_Urban %>%
        group_by(urban_type_Laendlicher.Raum)%>%
        summarise(SHAP_urban_type_Laendlicher.Raum = mean(SHAP_urban_type_Laendlicher.Raum))%>%
        ungroup()%>%
        mutate(VAR = "urban_type_Laendlicher.Raum")%>%
        rename(value = "urban_type_Laendlicher.Raum", SHAP = "SHAP_urban_type_Laendlicher.Raum")
      
      data_Urban_0 <- data_Urban_0 %>%
        bind_rows(data_Urban_2)
    }
    
    data_Urban_0 <- data_Urban_0 %>%
      mutate(value = as.character(value))%>%
      mutate(number = data_shap_2.1$number[data_shap_2.1$VAR_0 == "Urban_Type"])
    
    data_analysis_0 <- data_analysis_0 %>%
      mutate(value = as.character(value))%>%
      bind_rows(data_Urban_0)
    
    rm(data_Urban_0, data_Urban, data_shap_Urban, data_out_Urban, data_Urban_1, data_Urban_2)
    
  }
  
  if("Urban_Identif" %in% data_shap_2.1$VAR_0){
    print("Urban_Identif")
    
    data_shap_Urban <- data_shap_0 %>%
      select(ID, starts_with("urban_identif"))%>%
      rename_at(vars(starts_with("urban_identif")), ~ str_replace(., "^", "SHAP_"))%>%
      select(-starts_with("SHAP_urban_identif_2"))
    
    data_out_Urban <- data_out_0 %>%
      select(ID, starts_with("urban_identif"), - starts_with("urban_identif_2"))
    
    data_Urban <- left_join(data_shap_Urban, data_out_Urban, by = "ID")
    
    data_Urban_0 <- data.frame()
    
    if("urban_identif_Municipio.con.20.000.o.más.y.menos.de.50.000.habitantes" %in% colnames(data_Urban)){
      data_Urban_1 <- data_Urban %>%
        group_by(urban_identif_Municipio.con.20.000.o.más.y.menos.de.50.000.habitantes)%>%
        summarise(SHAP_urban_identif_Municipio.con.20.000.o.más.y.menos.de.50.000.habitantes = mean(SHAP_urban_identif_Municipio.con.20.000.o.más.y.menos.de.50.000.habitantes))%>%
        ungroup()%>%
        mutate(VAR = "urban_identif_Municipio.con.20.000.o.más.y.menos.de.50.000.habitantes")%>%
        rename(value = "urban_identif_Municipio.con.20.000.o.más.y.menos.de.50.000.habitantes", SHAP = "SHAP_urban_identif_Municipio.con.20.000.o.más.y.menos.de.50.000.habitantes")
      
      data_Urban_0 <- data_Urban_0 %>%
        bind_rows(data_Urban_1)
    }
    
    if("urban_identif_Municipio.con.50.000.o.más.y.menos.100.000.habitantes" %in% colnames(data_Urban)){
      data_Urban_2 <- data_Urban %>%
        group_by(urban_identif_Municipio.con.50.000.o.más.y.menos.100.000.habitantes)%>%
        summarise(SHAP_urban_identif_Municipio.con.50.000.o.más.y.menos.100.000.habitantes = mean(SHAP_urban_identif_Municipio.con.50.000.o.más.y.menos.100.000.habitantes))%>%
        ungroup()%>%
        mutate(VAR = "urban_identif_Municipio.con.50.000.o.más.y.menos.100.000.habitantes")%>%
        rename(value = "urban_identif_Municipio.con.50.000.o.más.y.menos.100.000.habitantes", SHAP = "SHAP_urban_identif_Municipio.con.50.000.o.más.y.menos.100.000.habitantes")
      
      data_Urban_0 <- data_Urban_0 %>%
        bind_rows(data_Urban_2)
    }
    
    if("urban_identif_Municipio.con.menos.de.10.000.habitantes" %in% colnames(data_Urban)){
      data_Urban_3 <- data_Urban %>%
        group_by(urban_identif_Municipio.con.menos.de.10.000.habitantes)%>%
        summarise(SHAP_urban_identif_Municipio.con.menos.de.10.000.habitantes = mean(SHAP_urban_identif_Municipio.con.menos.de.10.000.habitantes))%>%
        ungroup()%>%
        mutate(VAR = "urban_identif_Municipio.con.menos.de.10.000.habitantes")%>%
        rename(value = "urban_identif_Municipio.con.menos.de.10.000.habitantes", SHAP = "SHAP_urban_identif_Municipio.con.menos.de.10.000.habitantes")
      
      data_Urban_0 <- data_Urban_0 %>%
        bind_rows(data_Urban_3)
    }
    
    if("urban_identif_Municipio.de.100.000.habitantes.o.más" %in% colnames(data_Urban)){
      data_Urban_4 <- data_Urban %>%
        group_by(urban_identif_Municipio.de.100.000.habitantes.o.más)%>%
        summarise(SHAP_urban_identif_Municipio.de.100.000.habitantes.o.más = mean(SHAP_urban_identif_Municipio.de.100.000.habitantes.o.más))%>%
        ungroup()%>%
        mutate(VAR = "urban_identif_Municipio.de.100.000.habitantes.o.más")%>%
        rename(value = "urban_identif_Municipio.de.100.000.habitantes.o.más", SHAP = "SHAP_urban_identif_Municipio.de.100.000.habitantes.o.más")
      
      data_Urban_0 <- data_Urban_0 %>%
        bind_rows(data_Urban_4)
    }
    
    data_Urban_0 <- data_Urban_0 %>%
      mutate(value = as.character(value))%>%
      mutate(number = data_shap_2.1$number[data_shap_2.1$VAR_0 == "Urban_Identif"])
    
    data_analysis_0 <- data_analysis_0 %>%
      mutate(value = as.character(value))%>%
      bind_rows(data_Urban_0)
    
    rm(data_Urban_0, data_Urban, data_shap_Urban, data_out_Urban, data_Urban_1, data_Urban_2, data_Urban_3, data_Urban_4)
    
  }
  
  if("Urban_Identif_2" %in% data_shap_2.1$VAR_0){
    print("Urban_Identif_2")
    
    data_shap_Urban <- data_shap_0 %>%
      select(ID, starts_with("urban_identif_2"))%>%
      rename_at(vars(starts_with("urban_identif_2")), ~ str_replace(., "^", "SHAP_"))
    
    data_out_Urban <- data_out_0 %>%
      select(ID, starts_with("urban_identif_2"))
    
    data_Urban <- left_join(data_shap_Urban, data_out_Urban, by = "ID")
    
    data_Urban_0 <- data.frame()
    
    if("urban_identif_2_Zona.diseminada" %in% colnames(data_Urban)){
      data_Urban_1 <- data_Urban %>%
        group_by(urban_identif_2_Zona.diseminada)%>%
        summarise(SHAP_urban_identif_2_Zona.diseminada = mean(SHAP_urban_identif_2_Zona.diseminada))%>%
        ungroup()%>%
        mutate(VAR = "urban_identif_2_Zona.diseminada")%>%
        rename(value = "urban_identif_2_Zona.diseminada", SHAP = "SHAP_urban_identif_2_Zona.diseminada")
      
      data_Urban_0 <- data_Urban_0 %>%
        bind_rows(data_Urban_1)
    }
    
    if("urban_identif_2_Zona.intermedia" %in% colnames(data_Urban)){
      data_Urban_2 <- data_Urban %>%
        group_by(urban_identif_2_Zona.intermedia)%>%
        summarise(SHAP_urban_identif_2_Zona.intermedia = mean(SHAP_urban_identif_2_Zona.intermedia))%>%
        ungroup()%>%
        mutate(VAR = "urban_identif_2_Zona.intermedia")%>%
        rename(value = "urban_identif_2_Zona.intermedia", SHAP = "SHAP_urban_identif_2_Zona.intermedia")
      
      data_Urban_0 <- data_Urban_0 %>%
        bind_rows(data_Urban_2)
    }
    
    data_Urban_0 <- data_Urban_0 %>%
      mutate(value = as.character(value))%>%
      mutate(number = data_shap_2.1$number[data_shap_2.1$VAR_0 == "Urban_Identif_2"])
    
    data_analysis_0 <- data_analysis_0 %>%
      mutate(value = as.character(value))%>%
      bind_rows(data_Urban_0)
    
    rm(data_Urban_0, data_Urban, data_shap_Urban, data_out_Urban, data_Urban_1, data_Urban_2)
    
  }
  
  if("Heating_Type" %in% data_shap_2.1$VAR_0){
    print("Heating_Type")
    
    data_shap_Heating <- data_shap_0 %>%
      select(ID, starts_with("heating_type"))%>%
      rename_at(vars(starts_with("heating_type")), ~ str_replace(., "^", "SHAP_"))
    
    data_out_Heating <- data_out_0 %>%
      select(ID, starts_with("heating_type"))
    
    data_Heating <- left_join(data_shap_Heating, data_out_Heating, by = "ID")
    
    data_Heating_0 <- data.frame()
    
    if("heating_type_Einzeloefen" %in% colnames(data_Heating)){
      data_Heating_1 <- data_Heating %>%
        group_by(heating_type_Einzeloefen)%>%
        summarise(SHAP_heating_type_Einzeloefen = mean(SHAP_heating_type_Einzeloefen))%>%
        ungroup()%>%
        mutate(VAR = "heating_type_Einzeloefen")%>%
        rename(value = "heating_type_Einzeloefen", SHAP = "SHAP_heating_type_Einzeloefen")
      
      data_Heating_0 <- data_Heating_0 %>%
        bind_rows(data_Heating_1)
    }
    
    if("heating_type_Etagenheizung" %in% colnames(data_Heating)){
      data_Heating_2 <- data_Heating %>%
        group_by(heating_type_Etagenheizung)%>%
        summarise(SHAP_heating_type_Etagenheizung = mean(SHAP_heating_type_Etagenheizung))%>%
        ungroup()%>%
        mutate(VAR = "heating_type_Etagenheizung")%>%
        rename(value = "heating_type_Etagenheizung", SHAP = "SHAP_heating_type_Etagenheizung")
      
      data_Heating_0 <- data_Heating_0 %>%
        bind_rows(data_Heating_2)
    }
    
    if("heating_type_Fernheizung" %in% colnames(data_Heating)){
      data_Heating_3 <- data_Heating %>%
        group_by(heating_type_Fernheizung)%>%
        summarise(SHAP_heating_type_Fernheizung = mean(SHAP_heating_type_Fernheizung))%>%
        ungroup()%>%
        mutate(VAR = "heating_type_Fernheizung")%>%
        rename(value = "heating_type_Fernheizung", SHAP = "SHAP_heating_type_Fernheizung")
      
      data_Heating_0 <- data_Heating_0 %>%
        bind_rows(data_Heating_3)
    }
    
    data_Heating_0 <- data_Heating_0 %>%
      mutate(value = as.character(value))%>%
      mutate(number = data_shap_2.1$number[data_shap_2.1$VAR_0 == "Heating_Type"])
    
    data_analysis_0 <- data_analysis_0 %>%
      bind_rows(data_Heating_0)
    
    rm(data_Heating_0, data_Heating, data_shap_Heating, data_out_Heating, data_Heating_1, data_Heating_2, data_Heating_3)
    
  }
  
  if("Water energy" %in% data_shap_2.1$VAR_0){
    print("Water energy")
    
    data_shap_Water <- data_shap_0 %>%
      select(ID, starts_with("water_energy"))%>%
      rename_at(vars(starts_with("water_energy")), ~ str_replace(., "^", "SHAP_"))
    
    data_out_Water <- data_out_0 %>%
      select(ID, starts_with("water_energy"))
    
    data_Water <- left_join(data_shap_Water, data_out_Water, by = "ID")
    
    data_Water_0 <- data.frame()
    
    if("water_energy_Electricidad" %in% colnames(data_Water)){
      data_Water_1 <- data_Water %>%
        group_by(water_energy_Electricidad)%>%
        summarise(SHAP_water_energy_Electricidad = mean(SHAP_water_energy_Electricidad))%>%
        ungroup()%>%
        mutate(VAR = "water_energy_Electricidad")%>%
        rename(value = "water_energy_Electricidad", SHAP = "SHAP_water_energy_Electricidad")
      
      data_Water_0 <- data_Water_0 %>%
        bind_rows(data_Water_1)
    }
    
    if("water_energy_Gas.licuado" %in% colnames(data_Water)){
      data_Water_2 <- data_Water %>%
        group_by(water_energy_Gas.licuado)%>%
        summarise(SHAP_water_energy_Gas.licuado = mean(SHAP_water_energy_Gas.licuado))%>%
        ungroup()%>%
        mutate(VAR = "water_energy_Gas.licuado")%>%
        rename(value = "water_energy_Gas.licuado", SHAP = "SHAP_water_energy_Gas.licuado")
      
      data_Water_0 <- data_Water_0 %>%
        bind_rows(data_Water_2)
    }
    
    if("water_energy_Gas.natural" %in% colnames(data_Water)){
      data_Water_3 <- data_Water %>%
        group_by(water_energy_Gas.natural)%>%
        summarise(SHAP_water_energy_Gas.natural = mean(SHAP_water_energy_Gas.natural))%>%
        ungroup()%>%
        mutate(VAR = "water_energy_Gas.natural")%>%
        rename(value = "water_energy_Gas.natural", SHAP = "SHAP_water_energy_Gas.natural")
      
      data_Water_0 <- data_Water_0 %>%
        bind_rows(data_Water_3)
    }
    
    if("water_energy_Otras" %in% colnames(data_Water)){
      data_Water_4 <- data_Water %>%
        group_by(water_energy_Otras)%>%
        summarise(SHAP_water_energy_Otras = mean(SHAP_water_energy_Otras))%>%
        ungroup()%>%
        mutate(VAR = "water_energy_Otras")%>%
        rename(value = "water_energy_Otras", SHAP = "SHAP_water_energy_Otras")
      
      data_Water_0 <- data_Water_0 %>%
        bind_rows(data_Water_4)
    }
    
    if("water_energy_Otros.combustibles.líquidos" %in% colnames(data_Water)){
      data_Water_5 <- data_Water %>%
        group_by(water_energy_Otros.combustibles.líquidos)%>%
        summarise(SHAP_water_energy_Otros.combustibles.líquidos = mean(SHAP_water_energy_Otros.combustibles.líquidos))%>%
        ungroup()%>%
        mutate(VAR = "water_energy_Otros.combustibles.líquidos")%>%
        rename(value = "water_energy_Otros.combustibles.líquidos", SHAP = "SHAP_water_energy_Otros.combustibles.líquidos")
      
      data_Water_0 <- data_Water_0 %>%
        bind_rows(data_Water_5)
    }
    
    data_Water_0 <- data_Water_0 %>%
      mutate(value = as.character(value))%>%
      mutate(number = data_shap_2.1$number[data_shap_2.1$VAR_0 == "Water energy"])
    
    data_analysis_0 <- data_analysis_0 %>%
      bind_rows(data_Water_0)
    
    rm(data_Water_0, data_Water, data_shap_Water, data_out_Water, data_Water_1, data_Water_2, data_Water_3)
    
  }
  
  if("Building_Year" %in% data_shap_2.1$VAR_0){
    print("Building_Year")
    
    data_shap_Year <- data_shap_0 %>%
      select(ID, starts_with("building_year"))%>%
      rename_at(vars(starts_with("building_year")), ~ str_replace(., "^", "SHAP_"))
    
    data_out_Year <- data_out_0 %>%
      select(ID, starts_with("building_year"))
    
    data_Year <- left_join(data_shap_Year, data_out_Year, by = "ID")
    
    data_Year_0 <- data.frame()
    
    if("building_year_X1949.1990" %in% colnames(data_Year)){
      data_Year_1 <- data_Year %>%
        group_by(building_year_X1949.1990)%>%
        summarise(SHAP_building_year_X1949.1990 = mean(SHAP_building_year_X1949.1990))%>%
        ungroup()%>%
        mutate(VAR = "building_year_X1949.1990")%>%
        rename(value = "building_year_X1949.1990", SHAP = "SHAP_building_year_X1949.1990")
      
      data_Year_0 <- data_Year_0 %>%
        bind_rows(data_Year_1)
    }
    
    if("building_year_X1991.2000" %in% colnames(data_Year)){
      data_Year_2 <- data_Year %>%
        group_by(building_year_X1991.2000)%>%
        summarise(SHAP_building_year_X1991.2000 = mean(SHAP_building_year_X1991.2000))%>%
        ungroup()%>%
        mutate(VAR = "building_year_X1991.2000")%>%
        rename(value = "building_year_X1991.2000", SHAP = "SHAP_building_year_X1991.2000")
      
      data_Year_0 <- data_Year_0 %>%
        bind_rows(data_Year_2)
    }
    
    if("building_year_X2001.2010" %in% colnames(data_Year)){
      data_Year_3 <- data_Year %>%
        group_by(building_year_X2001.2010)%>%
        summarise(SHAP_building_year_X2001.2010 = mean(SHAP_building_year_X2001.2010))%>%
        ungroup()%>%
        mutate(VAR = "building_year_X2001.2010")%>%
        rename(value = "building_year_X2001.2010", SHAP = "SHAP_building_year_X2001.2010")
      
      data_Year_0 <- data_Year_0 %>%
        bind_rows(data_Year_3)
    }
    
    if("building_year_X2011.2018" %in% colnames(data_Year)){
      data_Year_4 <- data_Year %>%
        group_by(building_year_X2011.2018)%>%
        summarise(SHAP_building_year_X2011.2018 = mean(SHAP_building_year_X2011.2018))%>%
        ungroup()%>%
        mutate(VAR = "building_year_X2011.2018")%>%
        rename(value = "building_year_X2011.2018", SHAP = "SHAP_building_year_X2011.2018")
      
      data_Year_0 <- data_Year_0 %>%
        bind_rows(data_Year_4)
    }
    
    data_Year_0 <- data_Year_0 %>%
      mutate(value = as.character(value))%>%
      mutate(number = data_shap_2.1$number[data_shap_2.1$VAR_0 == "Building_Year"])
    
    data_analysis_0 <- data_analysis_0 %>%
      bind_rows(data_Year_0)
    
    rm(data_Year_0, data_Year, data_shap_Year, data_out_Year, data_Year_1, data_Year_2, data_Year_3, data_Year_4)
    
  }
  
  if("construction_year" %in% data_shap_2.1$VAR_0){
    print("construction_year")
    
    data_shap_Year <- data_shap_0 %>%
      select(ID, starts_with("construction_year"))%>%
      rename_at(vars(starts_with("construction_year")), ~ str_replace(., "^", "SHAP_"))
    
    data_out_Year <- data_out_0 %>%
      select(ID, starts_with("construction_year"))
    
    data_Year <- left_join(data_shap_Year, data_out_Year, by = "ID")
    
    data_Year_0 <- data.frame()
    
    if("construction_year_De.1975.à.1989" %in% colnames(data_Year)){
      data_Year_1 <- data_Year %>%
        group_by(construction_year_De.1975.à.1989)%>%
        summarise(SHAP_construction_year_De.1975.à.1989 = mean(SHAP_construction_year_De.1975.à.1989))%>%
        ungroup()%>%
        mutate(VAR = "construction_year_De.1975.à.1989")%>%
        rename(value = "construction_year_De.1975.à.1989", SHAP = "SHAP_construction_year_De.1975.à.1989")
      
      data_Year_0 <- data_Year_0 %>%
        bind_rows(data_Year_1)
    }
    
    if("construction_year_De.1990.à.2003" %in% colnames(data_Year)){
      data_Year_2 <- data_Year %>%
        group_by(construction_year_De.1990.à.2003)%>%
        summarise(SHAP_construction_year_De.1990.à.2003 = mean(SHAP_construction_year_De.1990.à.2003))%>%
        ungroup()%>%
        mutate(VAR = "construction_year_De.1990.à.2003")%>%
        rename(value = "construction_year_De.1990.à.2003", SHAP = "SHAP_construction_year_De.1990.à.2003")
      
      data_Year_0 <- data_Year_0 %>%
        bind_rows(data_Year_2)
    }
    
    if("construction_year_En.1948.ou.avant" %in% colnames(data_Year)){
      data_Year_3 <- data_Year %>%
        group_by(construction_year_En.1948.ou.avant)%>%
        summarise(SHAP_construction_year_En.1948.ou.avant = mean(SHAP_construction_year_En.1948.ou.avant))%>%
        ungroup()%>%
        mutate(VAR = "construction_year_En.1948.ou.avant")%>%
        rename(value = "construction_year_En.1948.ou.avant", SHAP = "SHAP_construction_year_En.1948.ou.avant")
      
      data_Year_0 <- data_Year_0 %>%
        bind_rows(data_Year_3)
    }
    
    if("construction_year_En.2004.et.après" %in% colnames(data_Year)){
      data_Year_4 <- data_Year %>%
        group_by(construction_year_En.2004.et.après)%>%
        summarise(SHAP_construction_year_En.2004.et.après = mean(SHAP_construction_year_En.2004.et.après))%>%
        ungroup()%>%
        mutate(VAR = "construction_year_En.2004.et.après")%>%
        rename(value = "construction_year_En.2004.et.après", SHAP = "SHAP_construction_year_En.2004.et.après")
      
      data_Year_0 <- data_Year_0 %>%
        bind_rows(data_Year_4)
    }
    
    if("construction_year_Ne.sait.pas" %in% colnames(data_Year)){
      data_Year_5 <- data_Year %>%
        group_by(construction_year_Ne.sait.pas)%>%
        summarise(SHAP_construction_year_Ne.sait.pas = mean(SHAP_construction_year_Ne.sait.pas))%>%
        ungroup()%>%
        mutate(VAR = "construction_year_Ne.sait.pas")%>%
        rename(value = "construction_year_Ne.sait.pas", SHAP = "SHAP_construction_year_Ne.sait.pas")
      
      data_Year_0 <- data_Year_0 %>%
        bind_rows(data_Year_5)
    }

    
    data_Year_0 <- data_Year_0 %>%
      mutate(value = as.character(value))%>%
      mutate(number = data_shap_2.1$number[data_shap_2.1$VAR_0 == "construction_year"])
    
    data_analysis_0 <- data_analysis_0 %>%
      bind_rows(data_Year_0)
    
    rm(data_Year_0, data_Year, data_shap_Year, data_out_Year, data_Year_1, data_Year_2, data_Year_3, data_Year_4, data_Year_5)
    
  }
  
  if("space" %in% data_shap_2.1$VAR_0){
    print("space")
    
    data_shap_Space <- data_shap_0 %>%
      select(ID, starts_with("space"))%>%
      rename_at(vars(starts_with("space")), ~ str_replace(., "^", "SHAP_"))
    
    data_out_Space <- data_out_0 %>%
      select(ID, starts_with("space"))
    
    data_Space <- left_join(data_shap_Space, data_out_Space, by = "ID")
    
    model_Space <- lm(SHAP_space ~ space + I(space^2), data = data_Space)
    
    tidy_Space <- tidy(model_Space)%>%
      mutate(VAR = "space")%>%
      rename(SHAP = estimate, value = term)%>%
      select(VAR, SHAP, value)
    
    tidy_Space <- tidy_Space %>%
      mutate(number = data_shap_2.1$number[data_shap_2.1$VAR_0 == "space"])
    
    data_analysis_0 <- data_analysis_0 %>%
      mutate(value = as.character(value))%>%
      bind_rows(tidy_Space)
    
    rm(data_Space, data_shap_Space, data_out_Space, model_Space, tidy_Space)
    
  }
  
  if("age_hhh" %in% data_shap_2.1$VAR_0){
    print("age_hhh")
    
    data_shap_Age <- data_shap_0 %>%
      select(ID, starts_with("age_hhh"))%>%
      rename_at(vars(starts_with("age_hhh")), ~ str_replace(., "^", "SHAP_"))
    
    data_out_Age <- data_out_0 %>%
      select(ID, starts_with("age_hhh"))
    
    data_Age <- left_join(data_shap_Age, data_out_Age, by = "ID")
    
    model_Age <- lm(SHAP_age_hhh ~ age_hhh + I(age_hhh^2), data = data_Age)
    
    tidy_Age <- tidy(model_Age)%>%
      mutate(VAR = "age_hhh")%>%
      rename(SHAP = estimate, value = term)%>%
      select(VAR, SHAP, value)
    
    tidy_Age <- tidy_Age %>%
      mutate(number = data_shap_2.1$number[data_shap_2.1$VAR_0 == "age_hhh"])
    
    data_analysis_0 <- data_analysis_0 %>%
      mutate(value = as.character(value))%>%
      bind_rows(tidy_Age)
    
    rm(data_Age, data_shap_Age, data_out_Age, model_Age, tidy_Age)
    
  }
  
  if("Occupation" %in% data_shap_2.1$VAR_0){
    print("Occupation")
    
    data_shap_Occ <- data_shap_0 %>%
      select(ID, starts_with("Occupation"))%>%
      rename_at(vars(starts_with("Occupation")), ~ str_replace(., "^", "SHAP_"))
    
    data_out_Occ <- data_out_0 %>%
      select(ID, starts_with("Occupation"))
    
    data_BL <- left_join(data_shap_Occ, data_out_Occ, by = "ID")
    
    data_BL_0 <- data.frame()
    
    if("occupation_Anciens.employés.et.ouvriers" %in% colnames(data_BL)){
      data_BL_1 <- data_BL %>%
        group_by(occupation_Anciens.employés.et.ouvriers)%>%
        summarise(SHAP_occupation_Anciens.employés.et.ouvriers = mean(SHAP_occupation_Anciens.employés.et.ouvriers))%>%
        ungroup()%>%
        mutate(VAR = "occupation_Anciens.employés.et.ouvriers")%>%
        rename(value = "occupation_Anciens.employés.et.ouvriers", SHAP = "SHAP_occupation_Anciens.employés.et.ouvriers")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_1)
    }
    
    if("occupation_Cadres.d.entreprise" %in% colnames(data_BL)){
      data_BL_2 <- data_BL %>%
        group_by(occupation_Cadres.d.entreprise)%>%
        summarise(SHAP_occupation_Cadres.d.entreprise = mean(SHAP_occupation_Cadres.d.entreprise))%>%
        ungroup()%>%
        mutate(VAR = "occupation_Cadres.d.entreprise")%>%
        rename(value = "occupation_Cadres.d.entreprise", SHAP = "SHAP_occupation_Cadres.d.entreprise")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_2)
    }
    
    if("occupation_Employés.de.la.fonction.publique" %in% colnames(data_BL)){
      data_BL_3 <- data_BL %>%
        group_by(occupation_Employés.de.la.fonction.publique)%>%
        summarise(SHAP_occupation_Employés.de.la.fonction.publique = mean(SHAP_occupation_Employés.de.la.fonction.publique))%>%
        ungroup()%>%
        mutate(VAR = "occupation_Employés.de.la.fonction.publique")%>%
        rename(value = "occupation_Employés.de.la.fonction.publique", SHAP = "SHAP_occupation_Employés.de.la.fonction.publique")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_3)
    }
    
    if("occupation_Inactifs.divers..autres.que.retraités." %in% colnames(data_BL)){
      data_BL_4 <- data_BL %>%
        group_by(occupation_Inactifs.divers..autres.que.retraités.)%>%
        summarise(SHAP_occupation_Inactifs.divers..autres.que.retraités. = mean(SHAP_occupation_Inactifs.divers..autres.que.retraités.))%>%
        ungroup()%>%
        mutate(VAR = "occupation_Inactifs.divers..autres.que.retraités.")%>%
        rename(value = "occupation_Inactifs.divers..autres.que.retraités.", SHAP = "SHAP_occupation_Inactifs.divers..autres.que.retraités.")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_4)
    }
    
    if("occupation_Other" %in% colnames(data_BL)){
      data_BL_5 <- data_BL %>%
        group_by(occupation_Other)%>%
        summarise(SHAP_occupation_Other = mean(SHAP_occupation_Other))%>%
        ungroup()%>%
        mutate(VAR = "occupation_Other")%>%
        rename(value = "occupation_Other", SHAP = "SHAP_occupation_Other")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_5)
    }
    
    if("occupation_Ouvriers.qualifiés" %in% colnames(data_BL)){
      data_BL_6 <- data_BL %>%
        group_by(occupation_Ouvriers.qualifiés)%>%
        summarise(SHAP_occupation_Ouvriers.qualifiés = mean(SHAP_occupation_Ouvriers.qualifiés))%>%
        ungroup()%>%
        mutate(VAR = "occupation_Ouvriers.qualifiés")%>%
        rename(value = "occupation_Ouvriers.qualifiés", SHAP = "SHAP_occupation_Ouvriers.qualifiés")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_6)
    }
    
    if("occupation_Jubilado.a..retirado.a.anticipadamente" %in% colnames(data_BL)){
      data_BL_1 <- data_BL %>%
        group_by(occupation_Jubilado.a..retirado.a.anticipadamente)%>%
        summarise(SHAP_occupation_Jubilado.a..retirado.a.anticipadamente = mean(SHAP_occupation_Jubilado.a..retirado.a.anticipadamente))%>%
        ungroup()%>%
        mutate(VAR = "occupation_Jubilado.a..retirado.a.anticipadamente")%>%
        rename(value = "occupation_Jubilado.a..retirado.a.anticipadamente", SHAP = "SHAP_occupation_Jubilado.a..retirado.a.anticipadamente")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_1)
    }
    
    if("occupation_Parado.a" %in% colnames(data_BL)){
      data_BL_2 <- data_BL %>%
        group_by(occupation_Parado.a)%>%
        summarise(SHAP_occupation_Parado.a = mean(SHAP_occupation_Parado.a))%>%
        ungroup()%>%
        mutate(VAR = "occupation_Parado.a")%>%
        rename(value = "occupation_Parado.a", SHAP = "SHAP_occupation_Parado.a")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_2)
    }
    
    if("occupation_Trabajando.al.menos.una.hora" %in% colnames(data_BL)){
      data_BL_3 <- data_BL %>%
        group_by(occupation_Trabajando.al.menos.una.hora)%>%
        summarise(SHAP_occupation_Trabajando.al.menos.una.hora = mean(SHAP_occupation_Trabajando.al.menos.una.hora))%>%
        ungroup()%>%
        mutate(VAR = "occupation_Trabajando.al.menos.una.hora")%>%
        rename(value = "occupation_Trabajando.al.menos.una.hora", SHAP = "SHAP_occupation_Trabajando.al.menos.una.hora")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_3)
    }
    
    if("occupation_other" %in% colnames(data_BL)){
      data_BL_4 <- data_BL %>%
        group_by(occupation_other)%>%
        summarise(SHAP_occupation_other = mean(SHAP_occupation_other))%>%
        ungroup()%>%
        mutate(VAR = "occupation_other")%>%
        rename(value = "occupation_other", SHAP = "SHAP_occupation_other")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_4)
    }
    
    if("occupation_pensioner" %in% colnames(data_BL)){
      data_BL_1 <- data_BL %>%
        group_by(occupation_pensioner)%>%
        summarise(SHAP_occupation_pensioner = mean(SHAP_occupation_pensioner))%>%
        ungroup()%>%
        mutate(VAR = "occupation_pensioner")%>%
        rename(value = "occupation_pensioner", SHAP = "SHAP_occupation_pensioner")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_1)
    }
    
    if("occupation_self.employed.in.agriculture" %in% colnames(data_BL)){
      data_BL_2 <- data_BL %>%
        group_by(occupation_self.employed.in.agriculture)%>%
        summarise(SHAP_occupation_self.employed.in.agriculture = mean(SHAP_occupation_self.employed.in.agriculture))%>%
        ungroup()%>%
        mutate(VAR = "occupation_self.employed.in.agriculture")%>%
        rename(value = "occupation_self.employed.in.agriculture", SHAP = "SHAP_occupation_self.employed.in.agriculture")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_2)
    }
    
    if("occupation_self.employed.in.non.agricultural.activities" %in% colnames(data_BL)){
      data_BL_3 <- data_BL %>%
        group_by(occupation_self.employed.in.non.agricultural.activities)%>%
        summarise(SHAP_occupation_self.employed.in.non.agricultural.activities = mean(SHAP_occupation_self.employed.in.non.agricultural.activities))%>%
        ungroup()%>%
        mutate(VAR = "occupation_self.employed.in.non.agricultural.activities")%>%
        rename(value = "occupation_self.employed.in.non.agricultural.activities", SHAP = "SHAP_occupation_self.employed.in.non.agricultural.activities")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_3)
    }
    
    if("occupation_other" %in% colnames(data_BL)){
      data_BL_4 <- data_BL %>%
        group_by(occupation_other)%>%
        summarise(SHAP_occupation_other = mean(SHAP_occupation_other))%>%
        ungroup()%>%
        mutate(VAR = "occupation_other")%>%
        rename(value = "occupation_other", SHAP = "SHAP_occupation_other")
      
      data_BL_0 <- data_BL_0 %>%
        bind_rows(data_BL_4)
    }
    
    data_BL_0 <- data_BL_0 %>%
      mutate(number = data_shap_2.1$number[data_shap_2.1$VAR_0 == "Occupation"])%>%
      mutate(value = as.character(value))
    
    data_analysis_0 <- data_analysis_0 %>%
      mutate(value = as.character(value))%>%
      bind_rows(data_BL_0)
    
    rm(data_BL_0, data_BL, data_shap_Occ, data_out_Occ, data_BL_1, data_BL_2, data_BL_3, data_BL_4)
    
  }
  
  if("gender_Mujer" %in% data_shap_2.1$VAR_0){
    print("Gender")
    
    data_shap_Gen <- data_shap_0 %>%
      select(ID, starts_with("gender"))%>%
      rename_at(vars(starts_with("gender")), ~ str_replace(., "^", "SHAP_"))
    
    data_out_Gen <- data_out_0 %>%
      select(ID, starts_with("gender"))
    
    data_Gen <- left_join(data_shap_Gen, data_out_Gen, by = "ID")
    
    data_Gen_0 <- data.frame()
    
    if("gender_Mujer" %in% colnames(data_Gen)){
      data_Gen_1 <- data_Gen %>%
        group_by(gender_Mujer)%>%
        summarise(SHAP_gender_Mujer = mean(SHAP_gender_Mujer))%>%
        ungroup()%>%
        mutate(VAR = "gender_Mujer")%>%
        rename(value = "gender_Mujer", SHAP = "SHAP_gender_Mujer")
      
      data_Gen_0 <- data_Gen_0 %>%
        bind_rows(data_Gen_1)
    }
     data_Gen_0 <- data_Gen_0 %>%
      mutate(number = data_shap_2.1$number[data_shap_2.1$VAR_0 == "gender_Mujer"])%>%
      mutate(value = as.character(value))
    
    data_analysis_0 <- data_analysis_0 %>%
      mutate(value = as.character(value))%>%
      bind_rows(data_Gen_0)
    
    rm(data_Gen_0, data_Gen, data_shap_Gen, data_out_Gen, data_Gen_1)
    
  }
  
  data_shap_2.2 <- data_shap_2.1 %>%
    filter(!VAR_0 %in% c("Heating_Fuel", "number_of_cars", "Renting", "Building_Type", "hh_expenditures_EURO_2018", "Bundesland", "Heating_Type", "Building_Year", "space", "Tenant", "construction_year", "Province",
                         "Water energy", "age_hhh", "Urban_Identif_2", "Urban_Identif", "gender_Mujer", "Cooking_Fuel", "hh_expenditures_LEI_2018", "Occupation"))
  
  if(nrow(data_shap_2.2) != 0) {
    print("STOP! Some items missing:")
    
    print(data_shap_2.2)
    }
    
  data_analysis_0 <- data_analysis_0 %>%
    mutate(Percentile = i)
  
  data_analysis <- data_analysis %>%
    bind_rows(data_analysis_0)
  
}

# write_parquet(data_analysis, "../2_Data/Output/Percentiles/Data_Analysis_GER.parquet")
# write_parquet(data_analysis, "../2_Data/Output/Percentiles/Data_Analysis_FRA.parquet")
# write_parquet(data_analysis, "../2_Data/Output/Percentiles/Data_Analysis_ESP.parquet")
# write_parquet(data_analysis, "../2_Data/Output/Percentiles/Data_Analysis_ROM.parquet")

# 3.1   Analysis ####

# 3.1.1 Germany  ####

data_analysis_GER <- read_parquet("../2_Data/Output/Percentiles/Data_Analysis_GER.parquet")%>%
  arrange(Percentile, number)

data_summary_GER_0 <- data.frame()

for (i in 1:99){
  print(i)
  
  data_analysis_GER_1 <- data_analysis_GER %>%
    filter(Percentile == i)
  
  data_summary_GER_1 <- data.frame()
  
  for(j in 1:max(data_analysis_GER_1$number)){
    data_analysis_GER_1.1 <- data_analysis_GER_1 %>%
      filter(number == j)%>%
      arrange(desc(abs(SHAP)))
    
    if(!"hh_expenditures_EURO_2018" %in% data_analysis_GER_1.1$VAR & !"space" %in% data_analysis_GER_1.1$VAR){
      data_analysis_GER_1.1 <- data_analysis_GER_1.1 %>%
        slice(1)
      
      if(data_analysis_GER_1.1$VAR == "renting_Mieter" & data_analysis_GER_1.1$value == 0 & data_analysis_GER_1.1$SHAP > 0){out <- "are less likely to rent their apartment,"}
      if(data_analysis_GER_1.1$VAR == "renting_Mieter" & data_analysis_GER_1.1$value == 1 & data_analysis_GER_1.1$SHAP < 0){out <- "are less likely to rent their apartment,"}
      
      if(data_analysis_GER_1.1$VAR == "number_of_cars" & data_analysis_GER_1.1$value == 0 & data_analysis_GER_1.1$SHAP < 0){out <- "are more likely to have a car,"}
      if(data_analysis_GER_1.1$VAR == "number_of_cars" & data_analysis_GER_1.1$value == 5 & data_analysis_GER_1.1$SHAP > 0){out <- "are more likely to have a car,"}
      if(data_analysis_GER_1.1$VAR == "number_of_cars" & data_analysis_GER_1.1$value == 6 & data_analysis_GER_1.1$SHAP > 0){out <- "are more likely to have a car,"}
      if(data_analysis_GER_1.1$VAR == "number_of_cars" & data_analysis_GER_1.1$value == 7 & data_analysis_GER_1.1$SHAP > 0){out <- "are more likely to have a car,"}
      if(data_analysis_GER_1.1$VAR == "number_of_cars" & data_analysis_GER_1.1$value == 8 & data_analysis_GER_1.1$SHAP > 0){out <- "are more likely to have a car,"}
      if(data_analysis_GER_1.1$VAR == "number_of_cars" & data_analysis_GER_1.1$value == 6 & data_analysis_GER_1.1$SHAP < 0){out <- "are more likely to have at least one car,"} # Special case
      if(data_analysis_GER_1.1$VAR == "number_of_cars" & data_analysis_GER_1.1$value == 7 & data_analysis_GER_1.1$SHAP < 0){out <- "are more likely to have at least one car,"} # Special case
      if(data_analysis_GER_1.1$VAR == "number_of_cars" & data_analysis_GER_1.1$value == 8 & data_analysis_GER_1.1$SHAP < 0){out <- "are more likely to have at least one car,"} # Special case
      
      if(data_analysis_GER_1.1$VAR == "heating_fuel_Strom" & data_analysis_GER_1.1$value == 1 & data_analysis_GER_1.1$SHAP < 0){out <- "are less likely to use electricity for heating,"}
      if(data_analysis_GER_1.1$VAR == "heating_fuel_Gas" & data_analysis_GER_1.1$value == 1 & data_analysis_GER_1.1$SHAP > 0){out <- "are more likely to use gas for heating,"}
      if(data_analysis_GER_1.1$VAR == "heating_fuel_Gas" & data_analysis_GER_1.1$value == 0 & data_analysis_GER_1.1$SHAP < 0){out <- "are more likely to use gas for heating,"}
      if(data_analysis_GER_1.1$VAR == "heating_fuel_Heizoel" & data_analysis_GER_1.1$value == 1 & data_analysis_GER_1.1$SHAP > 0){out <- "are more likely to use oil for heating,"}
      
      if(data_analysis_GER_1.1$VAR == "bundesland_Berlin" & data_analysis_GER_1.1$value == 1 & data_analysis_GER_1.1$SHAP > 0){out <- "are more likely to live in Berlin,"}
      if(data_analysis_GER_1.1$VAR == "bundesland_Berlin" & data_analysis_GER_1.1$value == 1 & data_analysis_GER_1.1$SHAP < 0){out <- "are less likely to live in Berlin,"}
      if(data_analysis_GER_1.1$VAR == "bundesland_Bremen" & data_analysis_GER_1.1$value == 1 & data_analysis_GER_1.1$SHAP > 0){out <- "are more likely to live in Bremen,"}
      if(data_analysis_GER_1.1$VAR == "bundesland_Mecklenburg.Vorpommern" & data_analysis_GER_1.1$value == 1 & data_analysis_GER_1.1$SHAP > 0){out <- "are more likely to live in Mecklenburg-Vorpommern,"}
      if(data_analysis_GER_1.1$VAR == "bundesland_Schleswig.Holstein" & data_analysis_GER_1.1$value == 1 & data_analysis_GER_1.1$SHAP > 0){out <- "are more likely to live in Schleswig-Holstein,"}
      if(data_analysis_GER_1.1$VAR == "bundesland_Sachsen" & data_analysis_GER_1.1$value == 1 & data_analysis_GER_1.1$SHAP > 0){out <- "are more likely to live in Sachsen,"}
      if(data_analysis_GER_1.1$VAR == "bundesland_Saarland" & data_analysis_GER_1.1$value == 1 & data_analysis_GER_1.1$SHAP > 0){out <- "are more likely to live in Saarland,"}
      
      if(data_analysis_GER_1.1$VAR == "bundesland_Bayern" & data_analysis_GER_1.1$value == 1 & data_analysis_GER_1.1$SHAP < 0){out <- "are less likely to live in Bayern,"}
      
      if(data_analysis_GER_1.1$VAR == "heating_type_Fernheizung" & data_analysis_GER_1.1$value == 1 & data_analysis_GER_1.1$SHAP > 0){out <- "are more likely to heat with district heating,"}
      if(data_analysis_GER_1.1$VAR == "heating_type_Einzeloefen" & data_analysis_GER_1.1$value == 1 & data_analysis_GER_1.1$SHAP < 0){out <- "are less likely to heat with single stoves,"}
      
      if(data_analysis_GER_1.1$VAR == "urban_type_Laendlicher.Raum" & data_analysis_GER_1.1$value == 1 & data_analysis_GER_1.1$SHAP > 0){out <- "are more likely to live in a rural area,"}
      
      if(data_analysis_GER_1.1$VAR == "building_type_Wohngebaeude" & data_analysis_GER_1.1$value == 0 & data_analysis_GER_1.1$SHAP > 0){out <- "are less likely to live in an apartment building,"}
      if(data_analysis_GER_1.1$VAR == "building_type_Wohngebaeude" & data_analysis_GER_1.1$value == 1 & data_analysis_GER_1.1$SHAP < 0){out <- "are less likely to live in an apartment building,"}
      if(data_analysis_GER_1.1$VAR == "building_type_Einfamilienhaus" & data_analysis_GER_1.1$value == 1 & data_analysis_GER_1.1$SHAP > 0){out <- "are more likely to live in a single-family home,"}
      
      if(data_analysis_GER_1.1$VAR == "building_year_X2011.2018" & data_analysis_GER_1.1$value == 1 & data_analysis_GER_1.1$SHAP < 0){out <- "are less likely to live in more modern building,"}
      if(data_analysis_GER_1.1$VAR == "building_year_X2001.2010" & data_analysis_GER_1.1$value == 1 & data_analysis_GER_1.1$SHAP < 0){out <- "are less likely to live in more modern building,"}
      
    }
    
    if("hh_expenditures_EURO_2018" %in% data_analysis_GER_1.1$VAR){
      if(data_analysis_GER_1.1$SHAP[data_analysis_GER_1.1$value == "hh_expenditures_EURO_2018"]>0){out <- "are more likely to be richer,"}
      if(data_analysis_GER_1.1$SHAP[data_analysis_GER_1.1$value == "hh_expenditures_EURO_2018"]<0){out <- "are more likely to be poorer,"}
    }
    
    if("space" %in% data_analysis_GER_1.1$VAR){
      if(data_analysis_GER_1.1$SHAP[data_analysis_GER_1.1$value == "space"]>0){out <- "are more likely to live in larger places,"}
    }
    
    
    data_summary_GER_1.1 <- data.frame(number = j, out_0 = out, Percentile = i)
    
    data_summary_GER_1 <- data_summary_GER_1 %>%
      bind_rows(data_summary_GER_1.1)
    
    
  }
  
  data_summary_GER_0 <- data_summary_GER_0 %>%
    bind_rows(data_summary_GER_1)
  
}

data_summary_GER_1 <- data_summary_GER_0 %>%
  mutate(A = "Households that will be more affected than you")%>%
  group_by(Percentile)%>%
  mutate(max = ifelse(number == max(number),1,0))%>%
  mutate(max_minus = ifelse(number == max(number)-1,1,0))%>%
  ungroup()%>%
  mutate(out_0 = ifelse(max == 1 | max_minus == 1, str_remove(out_0, ","),out_0))%>%
  mutate(out_0 = ifelse(max == 1, str_replace(out_0, "$","."),out_0))%>%
  mutate(out_0 = ifelse(max == 1, str_replace(out_0, "^","and "),out_0))%>%
  select(-max)

data_summary_GER_2 <- data.frame()

for(i in 1:99){
  print(i)
  
  data_summary_GER_1.1 <- data_summary_GER_1 %>%
    filter(Percentile == i)
  
  max_0 <- max(data_summary_GER_1.1$number)
  
  if(max_0 == 4){out_1 <- paste("Households that will be more affected than you", 
                                data_summary_GER_1.1$out_0[data_summary_GER_1.1$number == 1],
                                data_summary_GER_1.1$out_0[data_summary_GER_1.1$number == 2],
                                data_summary_GER_1.1$out_0[data_summary_GER_1.1$number == 3],
                                data_summary_GER_1.1$out_0[data_summary_GER_1.1$number == 4])}
  
  if(max_0 == 5){out_1 <- paste("Households that will be more affected than you", 
                                data_summary_GER_1.1$out_0[data_summary_GER_1.1$number == 1],
                                data_summary_GER_1.1$out_0[data_summary_GER_1.1$number == 2],
                                data_summary_GER_1.1$out_0[data_summary_GER_1.1$number == 3],
                                data_summary_GER_1.1$out_0[data_summary_GER_1.1$number == 4],
                                data_summary_GER_1.1$out_0[data_summary_GER_1.1$number == 5])}
  
  out_0 <- data.frame(Percentile = i, out = out_1)
  
  data_summary_GER_2 <- data_summary_GER_2 %>%
    bind_rows(out_0)
  
}

# Individually add for Percentile 100

data_summary_GER_2 <- data_summary_GER_2 %>%
  bind_rows(data.frame(Percentile = 100, out = "Households that will be less affected than you are more likely to be richer, are less likely to use oil for heating, are more likely to live in an apartment building, are more likely to rent their apartment and are less likely to live in Sachsen."))

data_summary_GER_3 <- data_summary_GER_2 %>%
  mutate(Text = paste0(100 - Percentile, "% of all households will be more affected. ", Percentile, "% of all households will be less affected."))%>%
  mutate(Text = ifelse(Percentile == 100, lag(Text), Text))

write_csv(data_summary_GER_2, "../2_Data/Output/Percentiles/Percentiles_Analysis_GER.csv")

data_summary_GER_3 <- read_csv("../2_Data/Output/Percentiles/Percentiles_Analysis_GER.csv")

# 3.1.2 France ####

data_analysis_FRA <- read_parquet("../2_Data/Output/Percentiles/Data_Analysis_FRA.parquet")%>%
  arrange(Percentile, number)

data_summary_FRA_0 <- data.frame()

for (i in 1:99){
  print(i)
  
  data_analysis_FRA_1 <- data_analysis_FRA %>%
    filter(Percentile == i)%>%
    filter(!is.na(number))
  
  data_summary_FRA_1 <- data.frame()
  
  for(j in 1:max(data_analysis_FRA_1$number)){
    data_analysis_FRA_1.1 <- data_analysis_FRA_1 %>%
      filter(number == j)%>%
      arrange(desc(abs(SHAP)))
    
    if(!"hh_expenditures_EURO_2018" %in% data_analysis_FRA_1.1$VAR & !"space" %in% data_analysis_FRA_1.1$VAR){
      data_analysis_FRA_1.1 <- data_analysis_FRA_1.1 %>%
        slice(1)
      
      if(data_analysis_FRA_1.1$VAR == "tenant_Locataire" & data_analysis_FRA_1.1$value == 1 & data_analysis_FRA_1.1$SHAP < 0){out <- "are less likely to rent their apartment,"}
      if(data_analysis_FRA_1.1$VAR == "tenant_Other" & data_analysis_FRA_1.1$value == 1 & data_analysis_FRA_1.1$SHAP < 0){out <- "are less likely to own their house,"}
      if(data_analysis_FRA_1.1$VAR == "tenant_Other" & data_analysis_FRA_1.1$value == 1 & data_analysis_FRA_1.1$SHAP > 0){out <- "are more likely to own their house,"}
      
      if(data_analysis_FRA_1.1$VAR == "number_of_cars" & data_analysis_FRA_1.1$value == 0 & data_analysis_FRA_1.1$SHAP < 0){out <- "are more likely to have a car,"}
      if(data_analysis_FRA_1.1$VAR == "number_of_cars" & data_analysis_FRA_1.1$value == 3 & data_analysis_FRA_1.1$SHAP > 0){out <- "are more likely to have more cars,"}
      if(data_analysis_FRA_1.1$VAR == "number_of_cars" & data_analysis_FRA_1.1$value == 4 & data_analysis_FRA_1.1$SHAP > 0){out <- "are more likely to have more cars,"}
      if(data_analysis_FRA_1.1$VAR == "number_of_cars" & data_analysis_FRA_1.1$value == 5 & data_analysis_FRA_1.1$SHAP > 0){out <- "are more likely to have more cars,"}
      if(data_analysis_FRA_1.1$VAR == "number_of_cars" & data_analysis_FRA_1.1$value == 6 & data_analysis_FRA_1.1$SHAP > 0){out <- "are more likely to have more cars,"}
      if(data_analysis_FRA_1.1$VAR == "number_of_cars" & data_analysis_FRA_1.1$value == 7 & data_analysis_FRA_1.1$SHAP > 0){out <- "are more likely to have more cars,"}
      if(data_analysis_FRA_1.1$VAR == "number_of_cars" & data_analysis_FRA_1.1$value == 8 & data_analysis_FRA_1.1$SHAP > 0){out <- "are more likely to have more cars,"}
      
      if(data_analysis_FRA_1.1$VAR == "heating_fuel_Géothermie" & data_analysis_FRA_1.1$value == 1 & data_analysis_FRA_1.1$SHAP < 0){out <- "are less likely to use géothermie for heating,"}
      if(data_analysis_FRA_1.1$VAR == "heating_fuel_Charbon..coke" & data_analysis_FRA_1.1$value == 1 & data_analysis_FRA_1.1$SHAP < 0){out <- "are more likely to use charbon or coke for heating,"}
      if(data_analysis_FRA_1.1$VAR == "heating_fuel_Electricité" & data_analysis_FRA_1.1$value == 1 & data_analysis_FRA_1.1$SHAP < 0){out <- "are less likely to use electricité for heating,"}
      if(data_analysis_FRA_1.1$VAR == "heating_fuel_Butane..propane..gaz.en.citerne" & data_analysis_FRA_1.1$value == 1 & data_analysis_FRA_1.1$SHAP > 0){out <- "are more likely to use butane, propane ou gaz en citerne for heating,"}
      if(data_analysis_FRA_1.1$VAR == "heating_fuel_Fuel..mazout..pétrole" & data_analysis_FRA_1.1$value == 1 & data_analysis_FRA_1.1$SHAP > 0){out <- "are more likely to use fuel, mazout ou pétrole for heating,"}
      
      if(data_analysis_FRA_1.1$VAR == "province_Méditerranée" & data_analysis_FRA_1.1$value == 1 & data_analysis_FRA_1.1$SHAP < 0){out <- "are less likely to live in Méditerranée,"}
      if(data_analysis_FRA_1.1$VAR == "province_Est" & data_analysis_FRA_1.1$value == 1 & data_analysis_FRA_1.1$SHAP < 0){out <- "are less likely to live in Est,"}
      if(data_analysis_FRA_1.1$VAR == "province_Dom" & data_analysis_FRA_1.1$value == 1 & data_analysis_FRA_1.1$SHAP > 0){out <- "are more likely to live in DOM,"}
      if(data_analysis_FRA_1.1$VAR == "province_Sud.ouest" & data_analysis_FRA_1.1$value == 1 & data_analysis_FRA_1.1$SHAP > 0){out <- "are more likely to live in Sud Ouest,"}
      if(data_analysis_FRA_1.1$VAR == "province_Ouest" & data_analysis_FRA_1.1$value == 1 & data_analysis_FRA_1.1$SHAP > 0){out <- "are more likely to live in Ouest,"}
      
      if(data_analysis_FRA_1.1$VAR == "housing_type_Other" & data_analysis_FRA_1.1$value == 1 & data_analysis_FRA_1.1$SHAP > 0){out <- "are less likely to live in an apartment building,"}
      if(data_analysis_FRA_1.1$VAR == "housing_type_Une.maison.individuelle" & data_analysis_FRA_1.1$value == 0 & data_analysis_FRA_1.1$SHAP < 0){out <- "are more likely to live in une maison individuelle,"}
      
      if(data_analysis_FRA_1.1$VAR == "occupation_Anciens.employés.et.ouvriers" & data_analysis_FRA_1.1$value == 1 & data_analysis_FRA_1.1$SHAP < 0){out <- "are less likely to be anciens employés et ouvriers,"}
      if(data_analysis_FRA_1.1$VAR == "occupation_Cadres.d.entreprise" & data_analysis_FRA_1.1$value == 1 & data_analysis_FRA_1.1$SHAP < 0){out <- "are less likely to be cadres d'entreprise,"}
      if(data_analysis_FRA_1.1$VAR == "occupation_Employés.de.la.fonction.publique" & data_analysis_FRA_1.1$value == 1 & data_analysis_FRA_1.1$SHAP > 0){out <- "are more likely to be employés de la fonction publique,"}
      if(data_analysis_FRA_1.1$VAR == "occupation_Ouvriers.qualifiés" & data_analysis_FRA_1.1$value == 1 & data_analysis_FRA_1.1$SHAP > 0){out <- "are more likely to be ouvriers qualifiés,"}
      if(data_analysis_FRA_1.1$VAR == "occupation_Inactifs.divers..autres.que.retraités." & data_analysis_FRA_1.1$value == 1 & data_analysis_FRA_1.1$SHAP > 0){out <- "are more likely to be inactifs, divers, autres que retraités,"}
      if(data_analysis_FRA_1.1$VAR == "occupation_Cadres.d.entreprise" & data_analysis_FRA_1.1$value == 1 & data_analysis_FRA_1.1$SHAP > 0){out <- "are more likely to be cadres d'entreprise,"}
      
      if(data_analysis_FRA_1.1$VAR == "construction_year_En.2004.et.après" & data_analysis_FRA_1.1$value == 1 & data_analysis_FRA_1.1$SHAP < 0){out <- "are less likely to live in more modern buildings (2004 et après),"}
      if(data_analysis_FRA_1.1$VAR == "construction_year_De.1990.à.2003" & data_analysis_FRA_1.1$value == 1 & data_analysis_FRA_1.1$SHAP < 0){out <- "are less likely to live in more modern buildings (1990-2003),"}
      
    }
    
    if("hh_expenditures_EURO_2018" %in% data_analysis_FRA_1.1$VAR){
      if(data_analysis_FRA_1.1$SHAP[data_analysis_FRA_1.1$value == "hh_expenditures_EURO_2018"]>0){out <- "are more likely to be richer,"}
      if(data_analysis_FRA_1.1$SHAP[data_analysis_FRA_1.1$value == "hh_expenditures_EURO_2018"]<0){out <- "are more likely to be poorer,"}
    }
    
    if("space" %in% data_analysis_FRA_1.1$VAR){
      # if(data_analysis_GER_1.1$SHAP[data_analysis_GER_1.1$value == "space"]>0){out <- "are more likely to live in larger places,"}
    }
    
    
    data_summary_FRA_1.1 <- data.frame(number = j, out_0 = out, Percentile = i)
    
    data_summary_FRA_1 <- data_summary_FRA_1 %>%
      bind_rows(data_summary_FRA_1.1)
    
    
  }
  
  data_summary_FRA_0 <- data_summary_FRA_0 %>%
    bind_rows(data_summary_FRA_1)
  
}

data_summary_FRA_1 <- data_summary_FRA_0 %>%
  mutate(A = "Households that will be more affected than you")%>%
  filter(number < 6)%>%
  group_by(Percentile)%>%
  mutate(max = ifelse(number == max(number),1,0))%>%
  mutate(max_minus = ifelse(number == max(number)-1,1,0))%>%
  ungroup()%>%
  mutate(out_0 = ifelse(max == 1 | max_minus == 1, str_remove(out_0, ","),out_0))%>%
  mutate(out_0 = ifelse(max == 1, str_replace(out_0, "$","."),out_0))%>%
  mutate(out_0 = ifelse(max == 1, str_replace(out_0, "^","and "),out_0))%>%
  select(-max)

data_summary_FRA_2 <- data.frame()

for(i in 1:99){
  print(i)
  
  data_summary_FRA_1.1 <- data_summary_FRA_1 %>%
    filter(Percentile == i)
  
  max_0 <- max(data_summary_FRA_1.1$number)
  
  if(max_0 == 5){out_1 <- paste("Households that will be more affected than you", 
                                data_summary_FRA_1.1$out_0[data_summary_FRA_1.1$number == 1],
                                data_summary_FRA_1.1$out_0[data_summary_FRA_1.1$number == 2],
                                data_summary_FRA_1.1$out_0[data_summary_FRA_1.1$number == 3],
                                data_summary_FRA_1.1$out_0[data_summary_FRA_1.1$number == 4],
                                data_summary_FRA_1.1$out_0[data_summary_FRA_1.1$number == 5])}
  
  out_0 <- data.frame(Percentile = i, out = out_1)
  
  data_summary_FRA_2 <- data_summary_FRA_2 %>%
    bind_rows(out_0)
  
}

# Individually add for Percentile 100

data_summary_FRA_2 <- data_summary_FRA_2 %>%
  bind_rows(data.frame(Percentile = 100, out = "Households that will be less affected than you are more likely to be richer, are less likely to own their house, are less likely to live in DOM, are less likely to have a car and less likely to use mazout ou pétrole for heating."))

data_summary_FRA_3 <- data_summary_FRA_2 %>%
  mutate(Text = paste0(100 - Percentile, "% of all households will be more affected. ", Percentile, "% of all households will be less affected."))%>%
  mutate(Text = ifelse(Percentile == 100, lag(Text), Text))

write_csv(data_summary_FRA_2, "../2_Data/Output/Percentiles/Percentiles_Analysis_FRA.csv")

# 3.1.3 Spain ####

data_analysis_ESP <- read_parquet("../2_Data/Output/Percentiles/Data_Analysis_ESP.parquet")%>%
  arrange(Percentile, number)

data_summary_ESP_0 <- data.frame()

for (i in 1:99){
  print(i)
  
  data_analysis_ESP_1 <- data_analysis_ESP %>%
    filter(Percentile == i)%>%
    filter(!is.na(number))
  
  data_summary_ESP_1 <- data.frame()
  
  for(j in 1:max(data_analysis_ESP_1$number)){
    data_analysis_ESP_1.1 <- data_analysis_ESP_1 %>%
      filter(number == j)%>%
      arrange(desc(abs(SHAP)))
    
    if(!"hh_expenditures_EURO_2018" %in% data_analysis_ESP_1.1$VAR & !"age_hhh" %in% data_analysis_ESP_1.1$VAR){
      data_analysis_ESP_1.1 <- data_analysis_ESP_1.1 %>%
        slice(1)
      
      if(data_analysis_ESP_1.1$VAR == "tenant_Propriedad" & data_analysis_ESP_1.1$value == 0 & data_analysis_ESP_1.1$SHAP < 0){out <- "are more likely to own their house,"}
      if(data_analysis_ESP_1.1$VAR == "tenant_Cesión" & data_analysis_ESP_1.1$value == 1 & data_analysis_ESP_1.1$SHAP > 0){out <- "are more likely to own their house,"}
      
      if(data_analysis_ESP_1.1$VAR == "heating_fuel_Gas.licuado" & data_analysis_ESP_1.1$value == 1 & data_analysis_ESP_1.1$SHAP < 0){out <- "are less likely to use gas licuado for heating,"}
      if(data_analysis_ESP_1.1$VAR == "heating_fuel_Otras.y.líquidos" & data_analysis_ESP_1.1$value == 1 & data_analysis_ESP_1.1$SHAP > 0){out <- "are less likely to use Otras y líquidos for heating,"}
      
      if(data_analysis_ESP_1.1$VAR == "water_energy_Gas.natural" & data_analysis_ESP_1.1$value == 1 & data_analysis_ESP_1.1$SHAP > 0){out <- "are more likely to use Gas natural for water heating,"}
      if(data_analysis_ESP_1.1$VAR == "water_energy_Otras" & data_analysis_ESP_1.1$value == 1 & data_analysis_ESP_1.1$SHAP < 0){out <- "are more likely to use Gas natural for water heating,"}
      if(data_analysis_ESP_1.1$VAR == "water_energy_Electricidad" & data_analysis_ESP_1.1$value == 1 & data_analysis_ESP_1.1$SHAP > 0){out <- "are more likely to use Electricidad for water heating,"}
      if(data_analysis_ESP_1.1$VAR == "water_energy_Otros.combustibles.líquidos" & data_analysis_ESP_1.1$value == 1 & data_analysis_ESP_1.1$SHAP < 0){out <- "are less likely to use otros combustibles líquidos for water heating,"}
      if(data_analysis_ESP_1.1$VAR == "water_energy_Electricidad" & data_analysis_ESP_1.1$value == 1 & data_analysis_ESP_1.1$SHAP < 0){out <- "are less likely to use Electricidad for water heating,"}
      if(data_analysis_ESP_1.1$VAR == "water_energy_Otros.combustibles.líquidos" & data_analysis_ESP_1.1$value == 1 & data_analysis_ESP_1.1$SHAP > 0){out <- "are more likely to use otros combustibles líquidos for water heating,"}
      
      if(data_analysis_ESP_1.1$VAR == "gender_Mujer" & data_analysis_ESP_1.1$value == 1 & data_analysis_ESP_1.1$SHAP < 0){out <- "are less likely to have a Mujer as household head,"}
      
      if(data_analysis_ESP_1.1$VAR == "urban_identif_2_Zona.diseminada" & data_analysis_ESP_1.1$value == 1 & data_analysis_ESP_1.1$SHAP > 0){out <- "are more likely to live in Zona diseminada,"}
      
      if(data_analysis_ESP_1.1$VAR == "urban_identif_Municipio.con.20.000.o.más.y.menos.de.50.000.habitantes" & data_analysis_ESP_1.1$value == 1 & data_analysis_ESP_1.1$SHAP < 0){out <- "are less likely to live in Municipio con 20.000 o más y menos de 50.000 habitantes,"}
      if(data_analysis_ESP_1.1$VAR == "urban_identif_Municipio.con.menos.de.10.000.habitantes" & data_analysis_ESP_1.1$value == 1 & data_analysis_ESP_1.1$SHAP > 0){out <- "are more likely to live in Municipio con menos de 10.000 habitantes,"}
      # if(data_analysis_ESP_1.1$VAR == "housing_type_Une.maison.individuelle" & data_analysis_ESP_1.1$value == 0 & data_analysis_ESP_1.1$SHAP < 0){out <- "are more likely to live in une maison individuelle,"}
      
      if(data_analysis_ESP_1.1$VAR == "occupation_other" & data_analysis_ESP_1.1$value == 1 & data_analysis_ESP_1.1$SHAP < 0){out <- "are more likely to be working at least one hour,"}
      if(data_analysis_ESP_1.1$VAR == "occupation_Trabajando.al.menos.una.hora" & data_analysis_ESP_1.1$value == 0 & data_analysis_ESP_1.1$SHAP < 0){out <- "are more likely to be working at least one hour,"}
      if(data_analysis_ESP_1.1$VAR == "occupation_Trabajando.al.menos.una.hora" & data_analysis_ESP_1.1$value == 1 & data_analysis_ESP_1.1$SHAP > 0){out <- "are more likely to be working at least one hour,"}
      if(data_analysis_ESP_1.1$VAR == "occupation_Jubilado.a..retirado.a.anticipadamente" & data_analysis_ESP_1.1$value == 1 & data_analysis_ESP_1.1$SHAP > 0){out <- "are more likely to be Jubilado/a, retirado/a anticipadamente,"}
      if(data_analysis_ESP_1.1$VAR == "occupation_Parado.a" & data_analysis_ESP_1.1$value == 1 & data_analysis_ESP_1.1$SHAP > 0){out <- "are more likely to be Parado/a,"}
      
    }
    
    if("hh_expenditures_EURO_2018" %in% data_analysis_ESP_1.1$VAR){
      if(data_analysis_ESP_1.1$SHAP[data_analysis_ESP_1.1$value == "hh_expenditures_EURO_2018"]>0){out <- "are more likely to be richer,"}
      if(data_analysis_ESP_1.1$SHAP[data_analysis_ESP_1.1$value == "hh_expenditures_EURO_2018"]<0){out <- "are more likely to be poorer,"}
    }
    
    if("age_hhh" %in% data_analysis_ESP_1.1$VAR){
      if(data_analysis_ESP_1.1$SHAP[data_analysis_ESP_1.1$value == "age_hhh"]>0){out <- "are more likely to have an older household head,"}
      if(data_analysis_ESP_1.1$SHAP[data_analysis_ESP_1.1$value == "age_hhh"]<0){out <- "are less likely to have an older household head,"}
      
    }
    
    
    data_summary_ESP_1.1 <- data.frame(number = j, out_0 = out, Percentile = i)
    
    data_summary_ESP_1 <- data_summary_ESP_1 %>%
      bind_rows(data_summary_ESP_1.1)
    
    
  }
  
  data_summary_ESP_0 <- data_summary_ESP_0 %>%
    bind_rows(data_summary_ESP_1)
  
}

data_summary_ESP_1 <- data_summary_ESP_0 %>%
  mutate(A = "Households that will be more affected than you")%>%
  filter(number < 6)%>%
  group_by(Percentile)%>%
  mutate(max = ifelse(number == max(number),1,0))%>%
  mutate(max_minus = ifelse(number == max(number)-1,1,0))%>%
  ungroup()%>%
  mutate(out_0 = ifelse(max == 1 | max_minus == 1, str_remove(out_0, ","),out_0))%>%
  mutate(out_0 = ifelse(max == 1, str_replace(out_0, "$","."),out_0))%>%
  mutate(out_0 = ifelse(max == 1, str_replace(out_0, "^","and "),out_0))%>%
  select(-max)

data_summary_ESP_2 <- data.frame()

for(i in 1:99){
  print(i)
  
  data_summary_ESP_1.1 <- data_summary_ESP_1 %>%
    filter(Percentile == i)
  
  max_0 <- max(data_summary_ESP_1.1$number)
  
  if(max_0 == 4){out_1 <- paste("Households that will be more affected than you", 
                                data_summary_ESP_1.1$out_0[data_summary_ESP_1.1$number == 1],
                                data_summary_ESP_1.1$out_0[data_summary_ESP_1.1$number == 2],
                                data_summary_ESP_1.1$out_0[data_summary_ESP_1.1$number == 3],
                                data_summary_ESP_1.1$out_0[data_summary_ESP_1.1$number == 4])}
  
  if(max_0 == 5){out_1 <- paste("Households that will be more affected than you", 
                                data_summary_ESP_1.1$out_0[data_summary_ESP_1.1$number == 1],
                                data_summary_ESP_1.1$out_0[data_summary_ESP_1.1$number == 2],
                                data_summary_ESP_1.1$out_0[data_summary_ESP_1.1$number == 3],
                                data_summary_ESP_1.1$out_0[data_summary_ESP_1.1$number == 4],
                                data_summary_ESP_1.1$out_0[data_summary_ESP_1.1$number == 5])}
  
  out_0 <- data.frame(Percentile = i, out = out_1)
  
  data_summary_ESP_2 <- data_summary_ESP_2 %>%
    bind_rows(out_0)
  
}

# Individually add for Percentile 100

data_summary_ESP_2 <- data_summary_ESP_2 %>%
  bind_rows(data.frame(Percentile = 100, out = "Households that will be less affected than you are more likely to be richer, are less likely are more likely to live in Municipio con menos de 10.000 habitantes, are less likely to be working at least one hour, are less likely to have an older household head and are less likely to use otros combustibles líquidos for water heating."))

data_summary_ESP_3 <- data_summary_ESP_2 %>%
  mutate(Text = paste0(100 - Percentile, "% of all households will be more affected. ", Percentile, "% of all households will be less affected."))%>%
  mutate(Text = ifelse(Percentile == 100, lag(Text), Text))

write_csv(data_summary_ESP_2, "../2_Data/Output/Percentiles/Percentiles_Analysis_ESP.csv")

# 3.1.4 Romania ####

data_analysis_ROM <- read_parquet("../2_Data/Output/Percentiles/Data_Analysis_ROM.parquet")%>%
  arrange(Percentile, number)

data_summary_ROM_0 <- data.frame()

for (i in 1:99){
  print(i)
  
  data_analysis_ROM_1 <- data_analysis_ROM %>%
    filter(Percentile == i)%>%
    filter(!is.na(number))
  
  data_summary_ROM_1 <- data.frame()
  
  for(j in 1:max(data_analysis_ROM_1$number)){
    data_analysis_ROM_1.1 <- data_analysis_ROM_1 %>%
      filter(number == j)%>%
      arrange(desc(abs(SHAP)))
    
    if(!"hh_expenditures_LEI_2018" %in% data_analysis_ROM_1.1$VAR & !"age_hhh" %in% data_analysis_ROM_1.1$VAR){
      data_analysis_ROM_1.1 <- data_analysis_ROM_1.1 %>%
        slice(1)
      
      # if(data_analysis_ROM_1.1$VAR == "tenant_Propriedad" & data_analysis_ROM_1.1$value == 0 & data_analysis_ROM_1.1$SHAP < 0){out <- "are more likely to own their house,"}
      # if(data_analysis_ROM_1.1$VAR == "tenant_Cesión" & data_analysis_ROM_1.1$value == 1 & data_analysis_ROM_1.1$SHAP > 0){out <- "are more likely to own their house,"}
      # 
      if(data_analysis_ROM_1.1$VAR == "heating_fuel_Wood..coal.or.oil.stove" & data_analysis_ROM_1.1$value == 1 & data_analysis_ROM_1.1$SHAP < 0){out <- "are less likely to use Wood, coal or oil stove for heating,"}
      if(data_analysis_ROM_1.1$VAR == "heating_fuel_Electricity" & data_analysis_ROM_1.1$value == 1 & data_analysis_ROM_1.1$SHAP < 0){out <- "are less likely to electricity for heating,"}
      if(data_analysis_ROM_1.1$VAR == "heating_fuel_Other" & data_analysis_ROM_1.1$value == 1 & data_analysis_ROM_1.1$SHAP < 0){out <- "are more likely to use natural gas for heating,"}
      if(data_analysis_ROM_1.1$VAR == "heating_fuel_Wood" & data_analysis_ROM_1.1$value == 1 & data_analysis_ROM_1.1$SHAP < 0){out <- "are less likely to use wood for heating,"}
      if(data_analysis_ROM_1.1$VAR == "heating_fuel_Natural.gas.stove" & data_analysis_ROM_1.1$value == 1 & data_analysis_ROM_1.1$SHAP > 0){out <- "are more likely to use a natural gas stove for heating,"}
      # if(data_analysis_ROM_1.1$VAR == "heating_fuel_Otras.y.líquidos" & data_analysis_ROM_1.1$value == 1 & data_analysis_ROM_1.1$SHAP > 0){out <- "are less likely to use Otras y líquidos for heating,"}
      # 
      
      if(data_analysis_ROM_1.1$VAR == "cooking_fuel_Other" & data_analysis_ROM_1.1$value == 1 & data_analysis_ROM_1.1$SHAP < 0){out <- "are more likely to use natural gas for cooking,"}
      if(data_analysis_ROM_1.1$VAR == "cooking_fuel_Natural.gas" & data_analysis_ROM_1.1$value == 0 & data_analysis_ROM_1.1$SHAP < 0){out <- "are more likely to use natural gas for cooking,"}
      
      if(data_analysis_ROM_1.1$VAR == "number_of_cars" & data_analysis_ROM_1.1$value == 0 & data_analysis_ROM_1.1$SHAP < 0){out <- "are more likely to have a car,"}
      if(data_analysis_ROM_1.1$VAR == "number_of_cars" & data_analysis_ROM_1.1$value == 1 & data_analysis_ROM_1.1$SHAP > 0){out <- "are more likely to have a car,"}
      if(data_analysis_ROM_1.1$VAR == "number_of_cars" & data_analysis_ROM_1.1$value == 2 & data_analysis_ROM_1.1$SHAP > 0){out <- "are more likely to have one or more cars,"}
      if(data_analysis_ROM_1.1$VAR == "number_of_cars" & data_analysis_ROM_1.1$value == 3 & data_analysis_ROM_1.1$SHAP > 0){out <- "are more likely to have one or more cars,"}
      if(data_analysis_ROM_1.1$VAR == "number_of_cars" & data_analysis_ROM_1.1$value == 4 & data_analysis_ROM_1.1$SHAP > 0){out <- "are more likely to have one or more cars,"}
      if(data_analysis_ROM_1.1$VAR == "number_of_cars" & data_analysis_ROM_1.1$value == 5 & data_analysis_ROM_1.1$SHAP > 0){out <- "are more likely to have one or more cars,"}
      if(data_analysis_ROM_1.1$VAR == "number_of_cars" & data_analysis_ROM_1.1$value == 6 & data_analysis_ROM_1.1$SHAP > 0){out <- "are more likely to have one or more cars,"}
      
      
      # if(data_analysis_ROM_1.1$VAR == "water_energy_Gas.natural" & data_analysis_ROM_1.1$value == 1 & data_analysis_ROM_1.1$SHAP > 0){out <- "are more likely to use Gas natural for water heating,"}
      # if(data_analysis_ROM_1.1$VAR == "water_energy_Otras" & data_analysis_ROM_1.1$value == 1 & data_analysis_ROM_1.1$SHAP < 0){out <- "are more likely to use Gas natural for water heating,"}
      # if(data_analysis_ROM_1.1$VAR == "water_energy_Electricidad" & data_analysis_ROM_1.1$value == 1 & data_analysis_ROM_1.1$SHAP > 0){out <- "are more likely to use Electricidad for water heating,"}
      # if(data_analysis_ROM_1.1$VAR == "water_energy_Otros.combustibles.líquidos" & data_analysis_ROM_1.1$value == 1 & data_analysis_ROM_1.1$SHAP < 0){out <- "are less likely to use otros combustibles líquidos for water heating,"}
      # if(data_analysis_ROM_1.1$VAR == "water_energy_Electricidad" & data_analysis_ROM_1.1$value == 1 & data_analysis_ROM_1.1$SHAP < 0){out <- "are less likely to use Electricidad for water heating,"}
      # if(data_analysis_ROM_1.1$VAR == "water_energy_Otros.combustibles.líquidos" & data_analysis_ROM_1.1$value == 1 & data_analysis_ROM_1.1$SHAP > 0){out <- "are more likely to use otros combustibles líquidos for water heating,"}
      # 
      # if(data_analysis_ROM_1.1$VAR == "gender_Mujer" & data_analysis_ROM_1.1$value == 1 & data_analysis_ROM_1.1$SHAP < 0){out <- "are less likely to have a Mujer as household head,"}
      # 
      # if(data_analysis_ROM_1.1$VAR == "urban_identif_2_Zona.diseminada" & data_analysis_ROM_1.1$value == 1 & data_analysis_ROM_1.1$SHAP > 0){out <- "are more likely to live in Zona diseminada,"}
      # 
      if(data_analysis_ROM_1.1$VAR == "province_Centru" & data_analysis_ROM_1.1$value == 1 & data_analysis_ROM_1.1$SHAP < 0){out <- "are less likely to live in Centru,"}
      if(data_analysis_ROM_1.1$VAR == "province_Centru" & data_analysis_ROM_1.1$value == 1 & data_analysis_ROM_1.1$SHAP > 0){out <- "are more likely to live in Centru,"}
      if(data_analysis_ROM_1.1$VAR == "province_Sud.Muntenia" & data_analysis_ROM_1.1$value == 1 & data_analysis_ROM_1.1$SHAP > 0){out <- "are more likely to live in Sud Muntenia,"}
      # if(data_analysis_ROM_1.1$VAR == "urban_identif_Municipio.con.menos.de.10.000.habitantes" & data_analysis_ROM_1.1$value == 1 & data_analysis_ROM_1.1$SHAP > 0){out <- "are more likely to live in Municipio con menos de 10.000 habitantes,"}
      # # if(data_analysis_ROM_1.1$VAR == "housing_type_Une.maison.individuelle" & data_analysis_ROM_1.1$value == 0 & data_analysis_ROM_1.1$SHAP < 0){out <- "are more likely to live in une maison individuelle,"}
      # 
      if(data_analysis_ROM_1.1$VAR == "occupation_self.employed.in.agriculture" & data_analysis_ROM_1.1$value == 1 & data_analysis_ROM_1.1$SHAP > 0){out <- "are more likely to be self-employed in agriculture,"}
      
    }
    
    if("hh_expenditures_LEI_2018" %in% data_analysis_ROM_1.1$VAR){
      if(data_analysis_ROM_1.1$SHAP[data_analysis_ROM_1.1$value == "hh_expenditures_LEI_2018"]>0){out <- "are more likely to be richer,"}
      if(data_analysis_ROM_1.1$SHAP[data_analysis_ROM_1.1$value == "hh_expenditures_LEI_2018"]<0){out <- "are more likely to be poorer,"}
    }
    
    if("age_hhh" %in% data_analysis_ROM_1.1$VAR){
      if(data_analysis_ROM_1.1$SHAP[data_analysis_ROM_1.1$value == "age_hhh"]>0){out <- "are more likely to have an older household head,"}
      if(data_analysis_ROM_1.1$SHAP[data_analysis_ROM_1.1$value == "age_hhh"]<0){out <- "are less likely to have an older household head,"}
      
    }
    
    
    data_summary_ROM_1.1 <- data.frame(number = j, out_0 = out, Percentile = i)
    
    data_summary_ROM_1 <- data_summary_ROM_1 %>%
      bind_rows(data_summary_ROM_1.1)
    
    
  }
  
  data_summary_ROM_0 <- data_summary_ROM_0 %>%
    bind_rows(data_summary_ROM_1)
  
}

data_summary_ROM_1 <- data_summary_ROM_0 %>%
  mutate(A = "Households that will be more affected than you")%>%
  filter(number < 6)%>%
  group_by(Percentile)%>%
  mutate(max = ifelse(number == max(number),1,0))%>%
  mutate(max_minus = ifelse(number == max(number)-1,1,0))%>%
  ungroup()%>%
  mutate(out_0 = ifelse(max == 1 | max_minus == 1, str_remove(out_0, ","),out_0))%>%
  mutate(out_0 = ifelse(max == 1, str_replace(out_0, "$","."),out_0))%>%
  mutate(out_0 = ifelse(max == 1, str_replace(out_0, "^","and "),out_0))%>%
  select(-max)

data_summary_ROM_2 <- data.frame()

for(i in 1:99){
  print(i)
  
  data_summary_ROM_1.1 <- data_summary_ROM_1 %>%
    filter(Percentile == i)
  
  max_0 <- max(data_summary_ROM_1.1$number)
  
  if(max_0 == 3){out_1 <- paste("Households that will be more affected than you", 
                                data_summary_ROM_1.1$out_0[data_summary_ROM_1.1$number == 1],
                                data_summary_ROM_1.1$out_0[data_summary_ROM_1.1$number == 2],
                                data_summary_ROM_1.1$out_0[data_summary_ROM_1.1$number == 3])}
  
  if(max_0 == 4){out_1 <- paste("Households that will be more affected than you", 
                                data_summary_ROM_1.1$out_0[data_summary_ROM_1.1$number == 1],
                                data_summary_ROM_1.1$out_0[data_summary_ROM_1.1$number == 2],
                                data_summary_ROM_1.1$out_0[data_summary_ROM_1.1$number == 3],
                                data_summary_ROM_1.1$out_0[data_summary_ROM_1.1$number == 4])}
  
  if(max_0 == 5){out_1 <- paste("Households that will be more affected than you", 
                                data_summary_ROM_1.1$out_0[data_summary_ROM_1.1$number == 1],
                                data_summary_ROM_1.1$out_0[data_summary_ROM_1.1$number == 2],
                                data_summary_ROM_1.1$out_0[data_summary_ROM_1.1$number == 3],
                                data_summary_ROM_1.1$out_0[data_summary_ROM_1.1$number == 4],
                                data_summary_ROM_1.1$out_0[data_summary_ROM_1.1$number == 5])}
  
  out_0 <- data.frame(Percentile = i, out = out_1)
  
  data_summary_ROM_2 <- data_summary_ROM_2 %>%
    bind_rows(out_0)
  
}

# Individually add for Percentile 100

data_summary_ROM_2 <- data_summary_ROM_2 %>%
  bind_rows(data.frame(Percentile = 100, out = "Households that will be less affected than you are more likely to be richer, are less likely to use a natural gas stove for heating, are less likely to have one or more cars and are less likely to live in Sud Muntenia."))

data_summary_ROM_3 <- data_summary_ROM_2 %>%
  mutate(Text = paste0(100 - Percentile, "% of all households will be more affected. ", Percentile, "% of all households will be less affected."))%>%
  mutate(Text = ifelse(Percentile == 100, lag(Text), Text))

write_csv(data_summary_ROM_2, "../2_Data/Output/Percentiles/Percentiles_Analysis_ROM.csv")

# 3.2   What are important features on aggregate? ####

# Germany
for(i in c("Germany")){
  SHAP_test <- read_parquet("../2_Data/Output/SHAP values/SHAP_Test_Germany.parquet")
  SHAP_detail <- read_parquet("../2_Data/Output/SHAP values/SHAP_Detail_Germany.parquet")%>%
    rename_all(~ str_replace(., "^", "SHAP_"))
  SHAP_Summary <- read_parquet("../2_Data/Output/SHAP values/SHAP_Summary_Germany.parquet")
  
  data_0 <- bind_cols(SHAP_test, SHAP_detail)
  
  # Reason 1: Germany
  
  ggplot(data_0)+
    geom_hline(aes(yintercept = 0))+
    geom_jitter(aes(x = as.factor(heating_fuel),
                    y = SHAP_heating_fuel_Gas),
                size = 0.5, alpha = 0.2, height = 0, width = 0.25, shape = 21)+
    theme_bw()
  
  ggplot(data_0)+
    geom_hline(aes(yintercept = 0))+
    geom_jitter(aes(x = as.factor(heating_fuel),
                    y = SHAP_heating_fuel_Heizoel),
                size = 0.5, alpha = 0.2, height = 0, width = 0.25, shape = 21)+
    theme_bw()
  
  # Households that heat with natural gas (and heating oil, to some extent) are most heavily affected
  
  # Reason 2: Germany
  
  ggplot(data_0)+
    geom_hline(aes(yintercept = 0))+
    geom_point(aes(x = hh_expenditures_EURO_2018,
                    y = SHAP_hh_expenditures_EURO_2018),
                size = 0.5, alpha = 0.2, shape = 21)+
    theme_bw()
  
  # Households that are poorer are more heavily affected
  
  # Reason 3: Germany
  
  ggplot(data_0)+
    geom_hline(aes(yintercept = 0))+
    geom_jitter(aes(x = as.factor(renting),
                    y = SHAP_renting_Mieter),
                size = 0.5, alpha = 0.2, height = 0, width = 0.25, shape = 21)+
    theme_bw()
  
  # Households that own their house are more heavily affected compared to renters
  
  # Reason 4: Germany
  
  ggplot(data_0)+
    geom_hline(aes(yintercept = 0))+
    geom_jitter(aes(x = as.factor(number_of_cars),
                    y = SHAP_number_of_cars),
                size = 0.5, alpha = 0.2, height = 0, width = 0.25, shape = 21)+
    theme_bw()
  
  # Households that own more cars are more heavily affected
  
  # Reason 5: Germany
  
  ggplot(data_0)+
    geom_hline(aes(yintercept = 0))+
    geom_jitter(aes(x = as.factor(building_type),
                    y = SHAP_building_type_Wohngebaeude),
                size = 0.5, alpha = 0.2, height = 0, width = 0.25, shape = 21)+
    theme_bw()
  
  # Households that live in single- or double-family houses are more heavily affected compared to people living in apartment buildings.
  
}

# France
for(i in c("France")){
  SHAP_test <- read_parquet("../2_Data/Output/SHAP values/SHAP_Test_France.parquet")
  SHAP_detail <- read_parquet("../2_Data/Output/SHAP values/SHAP_Detail_France.parquet")%>%
    rename_all(~ str_replace(., "^", "SHAP_"))
  SHAP_Summary <- read_parquet("../2_Data/Output/SHAP values/SHAP_Summary_France.parquet")
  
  data_0 <- bind_cols(SHAP_test, SHAP_detail)
  
  # Reason 1: France
  
  ggplot(data_0)+
    geom_hline(aes(yintercept = 0))+
    geom_jitter(aes(x = as.factor(number_of_cars),
                    y = SHAP_number_of_cars),
                size = 0.5, alpha = 0.2, height = 0, width = 0.25, shape = 21)+
    theme_bw()
  
  # Households that own more cars are more heavily affected
  
  # Reason 2: France
  
  ggplot(data_0)+
    geom_hline(aes(yintercept = 0))+
    geom_jitter(aes(x = as.factor(heating_fuel),
                    y = SHAP_heating_fuel_Fuel..mazout..pétrole),
                size = 0.5, alpha = 0.5, height = 0, width = 0.25, shape = 21)+
    theme_bw()
  
  ggplot(data_0)+
    geom_hline(aes(yintercept = 0))+
    geom_jitter(aes(x = as.factor(heating_fuel),
                    y = SHAP_heating_fuel_Gaz.de.ville),
                size = 0.5, alpha = 0.5, height = 0, width = 0.25, shape = 21)+
    theme_bw()
  
  # Households that heat with heating oil and city gas are more heavily affected
  
  # Reason 3: France
  
  ggplot(data_0)+
    geom_hline(aes(yintercept = 0))+
    geom_point(aes(x = hh_expenditures_EURO_2018,
                   y = SHAP_hh_expenditures_EURO_2018),
               size = 0.5, alpha = 0.5, shape = 21)+
    geom_smooth(aes(x     = hh_expenditures_EURO_2018,
                    y     = SHAP_hh_expenditures_EURO_2018),
                method = "loess", color = "black", linewidth = 0.4, se = FALSE,
                formula = y ~ x)+
    theme_bw()+
    coord_cartesian(xlim = c(0,60000), ylim = c(-0.02,0.02))
  
  # Households that are poorer are more heavily affected
  
  # Reason 4: France
  
  ggplot(data_0)+
    geom_hline(aes(yintercept = 0))+
    geom_jitter(aes(x = as.factor(tenant),
                    y = SHAP_tenant_Locataire),
                size = 0.5, alpha = 0.2, height = 0, width = 0.25, shape = 21)+
    theme_bw()
  
  # Households that rent their house are less heavily affected compared to households that own their house
  
  # Reason 5: France
  
  ggplot(data_0)+
    geom_hline(aes(yintercept = 0))+
    geom_jitter(aes(x = as.factor(province),
                    y = SHAP_province_Dom),
                size = 0.5, alpha = 0.5, height = 0, width = 0.25, shape = 21)+
    theme_bw()
  
  # Households that live in La France d'outre-mer are more likely to be heavily affected and households that live in Région parisienne are less likely heavily affected
  
}

# Romania
for(i in c("Romania")){
  SHAP_test <- read_parquet("../2_Data/Output/SHAP values/SHAP_Test_Romania.parquet")
  SHAP_detail <- read_parquet("../2_Data/Output/SHAP values/SHAP_Detail_Romania.parquet")%>%
    rename_all(~ str_replace(., "^", "SHAP_"))
  SHAP_Summary <- read_parquet("../2_Data/Output/SHAP values/SHAP_Summary_Romania.parquet")
  
  data_0 <- bind_cols(SHAP_test, SHAP_detail)
  
  # Reason 1: Romania
  
  ggplot(data_0)+
    geom_hline(aes(yintercept = 0))+
    geom_point(aes(x = hh_expenditures_LEI_2018,
                   y = SHAP_hh_expenditures_LEI_2018),
               size = 0.5, alpha = 0.5, shape = 21)+
    geom_smooth(aes(x     = hh_expenditures_LEI_2018,
                    y     = SHAP_hh_expenditures_LEI_2018),
                method = "loess", color = "black", linewidth = 0.4, se = FALSE,
                formula = y ~ x)+
    theme_bw()+
    coord_cartesian(xlim = c(0,120000), ylim = c(-0.02,0.02))
  
  # Households that are poorer are more heavily affected
  
  # Reason 2: Romania
  
  ggplot(data_0)+
    geom_hline(aes(yintercept = 0))+
    geom_jitter(aes(x = as.factor(number_of_cars),
                    y = SHAP_number_of_cars),
                size = 0.5, alpha = 0.2, height = 0, width = 0.25, shape = 21)+
    theme_bw()
  
  # Households that own one car or more are more heavily affected
  
  # Reason 3: Romania
  
  ggplot(data_0)+
    geom_hline(aes(yintercept = 0))+
    geom_jitter(aes(x = as.factor(heating_fuel),
                    y = SHAP_heating_fuel_Wood..coal.or.oil.stove),
                size = 0.5, alpha = 0.5, height = 0, width = 0.25, shape = 21)+
    theme_bw()
  
  ggplot(data_0)+
    geom_hline(aes(yintercept = 0))+
    geom_jitter(aes(x = as.factor(heating_fuel),
                    y = SHAP_heating_fuel_Wood),
                size = 0.5, alpha = 0.5, height = 0, width = 0.25, shape = 21)+
    theme_bw()
  
  ggplot(data_0)+
    geom_hline(aes(yintercept = 0))+
    geom_jitter(aes(x = as.factor(heating_fuel),
                    y = SHAP_heating_fuel_Natural.gas),
                size = 0.5, alpha = 0.5, height = 0, width = 0.25, shape = 21)+
    theme_bw()
  
  # Households that heat with wood are less affected than other households. Households that heat with natural gas are more heavily affected.
  
  # Reason 4: Romania
  
  ggplot(data_0)+
    geom_hline(aes(yintercept = 0))+
    geom_jitter(aes(x = as.factor(cooking_fuel),
                    y = SHAP_cooking_fuel_Natural.gas),
                size = 0.5, alpha = 0.2, height = 0, width = 0.25, shape = 21)+
    theme_bw()
  
  # Households that use natural gas for cooking are more heavily affected than other households.
  
  # Reason 5: France
  
  ggplot(data_0)+
    geom_hline(aes(yintercept = 0))+
    geom_jitter(aes(x = as.factor(occupation),
                    y = SHAP_occupation_pensioner),
                size = 0.5, alpha = 0.5, height = 0, width = 0.25, shape = 21)+
    theme_bw()
  
  # Households in which the household head is pensioneer are more heavily affected than households in which the household head is employed.
  # Could eventually be disregarded.
  
}

# Spain
for(i in c("Spain")){
  SHAP_test <- read_parquet("../2_Data/Output/SHAP values/SHAP_Test_Spain.parquet")
  SHAP_detail <- read_parquet("../2_Data/Output/SHAP values/SHAP_Detail_Spain.parquet")%>%
    rename_all(~ str_replace(., "^", "SHAP_"))
  SHAP_Summary <- read_parquet("../2_Data/Output/SHAP values/SHAP_Summary_Spain.parquet")
  
  data_0 <- bind_cols(SHAP_test, SHAP_detail)
  
  # Reason 1: Spain
  
  ggplot(data_0)+
    geom_hline(aes(yintercept = 0))+
    geom_jitter(aes(x = as.factor(heating_fuel),
                    y = SHAP_heating_fuel_Otras.y.líquidos),
                size = 0.5, alpha = 0.5, height = 0, width = 0.25, shape = 21)+
    theme_bw()
  
  ggplot(data_0)+
    geom_hline(aes(yintercept = 0))+
    geom_jitter(aes(x = as.factor(heating_fuel),
                    y = SHAP_heating_fuel_Electricidad),
                size = 0.5, alpha = 0.5, height = 0, width = 0.25, shape = 21)+
    theme_bw()
  
  ggplot(data_0)+
    geom_hline(aes(yintercept = 0))+
    geom_jitter(aes(x = as.factor(heating_fuel),
                    y = SHAP_heating_fuel_Gas.natural),
                size = 0.5, alpha = 0.5, height = 0, width = 0.25, shape = 21)+
    theme_bw()
  
  # Households that use liquid fuels for heating are more heavily affected. Households that use electricity for heating are less heavily affected.
  
  # Reason 2: Spain
  
  ggplot(data_0)+
    geom_hline(aes(yintercept = 0))+
    geom_jitter(aes(x = as.factor(urban_identif),
                    y = SHAP_urban_identif_Municipio.con.menos.de.10.000.habitantes),
                size = 0.5, alpha = 0.2, height = 0, width = 0.25, shape = 21)+
    theme_bw()
  
  ggplot(data_0)+
    geom_hline(aes(yintercept = 0))+
    geom_jitter(aes(x = as.factor(urban_identif),
                    y = SHAP_urban_identif_Municipio.de.100.000.habitantes.o.más),
                size = 0.5, alpha = 0.2, height = 0, width = 0.25, shape = 21)+
    theme_bw()
  
  # Households that live in "municipios with less than 10.000 habitantes" are more heavily affected.
  # Households that live in "municipios with more than 100.000 habitantes" are less heavily affected.
  
  # Reason 3: Spain
  
  ggplot(data_0)+
    geom_hline(aes(yintercept = 0))+
    geom_jitter(aes(x = as.factor(urban_identif_2),
                    y = SHAP_urban_identif_2_Zona.diseminada),
                size = 0.5, alpha = 0.2, height = 0, width = 0.25, shape = 21)+
    theme_bw()
  
  ggplot(data_0)+
    geom_hline(aes(yintercept = 0))+
    geom_jitter(aes(x = as.factor(urban_identif_2),
                    y = SHAP_urban_identif_2_Zona.intermedia),
                size = 0.5, alpha = 0.2, height = 0, width = 0.25, shape = 21)+
    theme_bw()
  
  # Households that live in less densely populated areas are more heavily affected.
  
  # Reason 4: Spain
  
  ggplot(data_0)+
    geom_hline(aes(yintercept = 0))+
    geom_jitter(aes(x = as.factor(tenant),
                    y = SHAP_tenant_Propriedad),
                size = 0.5, alpha = 0.2, height = 0, width = 0.25, shape = 21)+
    theme_bw()
  
  # Households that own their house are more heavily affected. Households that rent their house are less heavily affected.
  
  # Reason 5: Spain
  
  ggplot(data_0)+
    geom_hline(aes(yintercept = 0))+
    geom_point(aes(x = age_hhh,
                   y = SHAP_age_hhh),
               size = 0.5, alpha = 0.5, shape = 21)+
    geom_smooth(aes(x     = age_hhh,
                    y     = SHAP_age_hhh),
                method = "loess", color = "black", linewidth = 0.4, se = FALSE,
                formula = y ~ x)+
    theme_bw()+
    coord_cartesian(xlim = c(0,80), ylim = c(-0.01,0.01))

  # Households with household heads that are middle-aged (30 to 60) are more likely to be heavily affected.
  
}

