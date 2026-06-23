# LOADING PACKAGES ----
library(extRemes)
library(tseries)
library(openxlsx)
library(dplyr)
library(readxl)
library(readr)
library(writexl)
library(stringr)

# LOADING DATA ----
# Loading main data of covs and q:
data <- read_csv("Data/ON_Final_Complete_ECA_Climate_Cov_Data_1985_2020.csv")

str(data) # to see what are the datatypes of each column

data <- data[, !names(data) %in% "...1"] # Remove a column by name

# Loading guide data for covariates
guide_data <- read_excel("Data/Final_Covariate_Finding_R.xlsx", 
                         sheet = "Covs_unique")
guide_data <- guide_data[!duplicated(guide_data$ID_hydro_st), ]
guide_data <- guide_data[!apply(is.na(guide_data), 1, all), ]


# LOADING FUNCTIONS ----
source("scripts/functions.R")

# MAIN SCRIPT --------
## Station list ----
station_list <- sort(unique(data$hydro_station_id))

undisturbed_stations <- c('02EB013', '02KF013', '02EA010', 
                          '02HL003', '02EA011', '02BF002')
# Checking if I added undisturbed station ids correctly:
undisturbed_stations %in% station_list # All TRUE

# Making disturbed station list by subtracting the undisturbed lsit from total
disturbed_stations <- setdiff(station_list, undisturbed_stations)

# Station detection_one by one ---------
# Done: 
# detected: "02AB006", "02AD010", 02BA003, 02CC005(detected whithout standardized)
### 02CE002, 02DC004, 02JC008, 04GA002, 04JC002, 04JD005, 04LM001, 05QC001, 05QD016,
### 05QE008, 02AC002
# not detected: , 02BB003, 02BF001, 02CD006, 04GB004, 05QA002, 05QA004
#disturbed_stations
station = "02AB006"


# Step 1: Prepare covariate data
cov_output <- prepare_station_covs_standardized(station, data, guide_data)

#for nonstandard
cov_output <- prepare_station_covs(station, data, guide_data)

# Step 2: Generate formulas with ECA
formulas <- complete_generate_formulas_with_eca(
  cov_output = cov_output, 
  station = station, 
  save_excel = FALSE, 
  save_path = "Outputs/Detection/Formulas/",
  save_name = sprintf("Station_%s_Complete_List_of_Formulas_Unique_Covs", station)
)

# Step 3: Fit models and evaluate metrics
primary_output <- fit_models_and_add_metrics(
  formula_output = formulas, 
  cov_output = cov_output, 
  save_excel = TRUE, 
  save_path = "Outputs/Detection/GEV_Evaluation/",
  save_name = sprintf("Station_%s_model_Evaluation_Results_Unique_Covs.xlsx", station)
)

# Step 4: Compare to the simpler model with LRT Test
model_ds <- read_excel("Outputs/Detection/Detection_Best_Models_LRT_Results.xlsx")
station = "02CE002"
cov_output <- prepare_station_covs_standardized(station, data, guide_data)
#cov_output <- prepare_station_covs(station, data, guide_data)

LRT<- run_lrt_test(station, data, guide_data, cov_output, model_ds)
LRT


#Handling errors-----
## 02CE002 ------
station = "02CE002"
cov_output <- prepare_station_covs_standardized(station, data, guide_data)
#cov_output <- prepare_station_covs(station, data, guide_data)

q <- cov_output$dataset$Max_discharge
cov1 <- cov_output$cov1
cov2 <- cov_output$cov2
cov3 <- cov_output$cov3
cov4 <- cov_output$cov4
cov5 <- cov_output$cov5
cov6 <- cov_output$cov6
cov7 <- cov_output$cov7

station_ds <- model_ds[model_ds$ID_hydro_st == station, ]
# Extract model formulas as text from the dataset
eca_mod_txt <- station_ds$cov_formula_eca
climate_mod_txt <- station_ds$cov_formula_climate

# Convert the extracted text formulas into R model objects
eca_mod <- eval(parse(text = eca_mod_txt))
climate_mod <- eval(parse(text = climate_mod_txt))

min_shift <- abs(min(cov_output$cov2 + cov_output$cov4)) + 0.01
cov_fixed <- cov_output$cov2 + cov_output$cov4 + min_shift  # New variable

climate_mod_txt <- "fevd(q, location.fun = ~cov2, scale.fun = ~cov_fixed, type = 'GEV', method = 'MLE')"
climate_mod <- eval(parse(text = climate_mod_txt))
# Perform the likelihood ratio test (LRT) to compare the two models
LRT <- lr.test(climate_mod, eca_mod, alpha = 0.05)
LRT

## 02BA003 ------
station = "04JC002"
#cov_output <- prepare_station_covs_standardized(station, data, guide_data)
cov_output <- prepare_station_covs(station, data, guide_data)

q <- cov_output$dataset$Max_discharge
cov1 <- cov_output$cov1
cov2 <- cov_output$cov2
cov3 <- cov_output$cov3
cov4 <- cov_output$cov4
cov5 <- cov_output$cov5
cov6 <- cov_output$cov6
cov7 <- cov_output$cov7

station_ds <- model_ds[model_ds$ID_hydro_st == station, ]
# Extract model formulas as text from the dataset
eca_mod_txt <- station_ds$cov_formula_eca
climate_mod_txt <- station_ds$cov_formula_climate
climate_mod_txt<- "fevd(q, location.fun = ~cov3, scale.fun = cov2, type = 'GEV', method = 'MLE')"

# Convert the extracted text formulas into R model objects
eca_mod <- eval(parse(text = eca_mod_txt))
climate_mod <- eval(parse(text = climate_mod_txt))

min_shift <- abs(min(cov_output$cov2 + cov_output$cov4)) + 0.01
cov_fixed <- cov_output$cov2 + cov_output$cov4 + min_shift  # New variable

climate_mod <- eval(parse(text = climate_mod_txt))
# Perform the likelihood ratio test (LRT) to compare the two models
LRT <- lr.test(climate_mod, eca_mod, alpha = 0.05)
LRT


# LRT Test --------
station = "02BA003"

# Step 1: Prepare covariate data
cov_output <- prepare_station_covs_standardized(station, data, guide_data)
cov_output <- prepare_station_covs(station, data, guide_data)

q <- cov_output$dataset$Max_discharge
cov1 <- cov_output$cov1
cov2 <- cov_output$cov2
cov3 <- cov_output$cov3
cov4 <- cov_output$cov4
cov5 <- cov_output$cov5
cov6 <- cov_output$cov6
cov7 <- cov_output$cov7

# for mannual
stationary <- fevd(q, type = 'GEV', method = 'MLE')

clean_formula_eca <- "fevd(q, location.fun = ~cov1 + cov2 + cov3, scale.fun = ~cov7, type = 'GEV', method = 'MLE')"
best_model_eca <- eval(parse(text = clean_formula_eca))

Stationary_LRT <- lr.test(stationary, best_model_eca, alpha = 0.05)
Stationary_LRT

clean_formula_climate <- "fevd(q, location.fun = ~cov1 + cov2 + cov3, type = 'GEV', method = 'MLE')"
best_model_climate <- eval(parse(text = clean_formula_climate))

Climatic_LRT <- lr.test(best_model_climate, best_model_eca, alpha = 0.05)
Climatic_LRT



# Development -------------
cov_output <- prepare_station_covs(station, data, guide_data)
cov7 <- cov_output$cov7
q <- cov_output$dataset$Max_discharge
mod19 <- fevd(q, location.fun = ~cov7, type = 'GEV', method = 'MLE')
mod20 <- fevd(q, scale.fun = ~cov7, type = 'GEV', method = 'MLE')
get_AICc(mod19)

params <- extract_gev_params_wide_format(mod19, model_name = "test")
