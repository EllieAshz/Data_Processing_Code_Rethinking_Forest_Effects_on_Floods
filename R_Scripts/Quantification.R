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


# Station Attribution One by One -------
# Done: "02AB006", 02AD010, 02BA003
station = "05QD016"

clean_formula = "fevd(q, location.fun = ~cov2 + cov6 + cov7, scale.fun = ~cov4, type = 'GEV', method = 'MLE')"

quantification_table <- rp_quantification_min_max_eca(station, 
                                                      save_csv = TRUE,
                                                      file_name = paste0("Quantification_Results_For_Min_Max_ECA_Scenrios_", station, ".csv"),
                                                      file_path = "Outputs/Quantification")
# for mannual
quantification_table <- rp_quantification_min_max_eca(station, 
                                                      save_csv = TRUE,
                                                      file_name = paste0("Quantification_Results_For_Min_Max_ECA_Scenrios_", station, ".csv"),
                                                      file_path = "Outputs/Quantification",
                                                      clean_formula_input = clean_formula)
# for not standardized
quantification_table <- rp_quantification_min_max_eca_not_standard(station, 
                                                      save_csv = TRUE,
                                                      file_name = paste0("Quantification_Results_For_Min_Max_ECA_Scenrios_", station, ".csv"),
                                                      file_path = "Outputs/Quantification")

#---------
# Test --------
best_formula_table <- get_lowest_AICc_for_station("Outputs/Detection/GEV_Evaluation", 
                                                  station, input_type = "eca")

best_formula <- best_formula_table$Formula

# Remove "modXXXX <-" (where XXXX is any number)
clean_formula <- gsub("^mod[0-9]+ <-\\s*", "", best_formula)

# get all covariates ready for fitting the best model
cov_output <- prepare_station_covs_standardized(station, data, guide_data)
q <- cov_output$dataset$Max_discharge
cov1 <- cov_output$cov1
cov2 <- cov_output$cov2
cov3 <- cov_output$cov3
cov4 <- cov_output$cov4
cov5 <- cov_output$cov5
cov6 <- cov_output$cov6
cov7 <- cov_output$cov7



best_mod02AD010 <- eval(parse(text = clean_formula))
get_AICc(best_mod02AD010)

parameter_table_cov <- extract_gev_parameters_with_covariates(best_mod02AD010, 
                                                              model_name = "best_mod02AD010")

# Compute average for cov1 to cov6
avg_cov1 <- mean(cov1)
avg_cov2 <- mean(cov2)
avg_cov3 <- mean(cov3)
avg_cov4 <- mean(cov4)
avg_cov5 <- mean(cov5)
avg_cov6 <- mean(cov6)
eca_min <- min(cov7)
eca_max <- max(cov7)

# Average mu
#avrg_mu_eca0 <- 27.6621165 + -6.2314259 * avg_cov4 + 2.1243662 * eca_min
#avrg_mu_eca10 <- 27.6621165 + -6.2314259 * avg_cov4 + 2.1243662 * eca_max

# Average sigma
#avrg_sigm <- -37.0902970 + 0.6209815 * avg_cov2 + 6.1030577 * avg_cov5

average_parameters <- compute_average_mu_sigma(parameter_table_cov, cov_output)
shape <- parameter_table_cov$Value[parameter_table_cov$Parameter == "shape"]

# Now computing Quantiles
rp = c(1.5, 2, 5, 7, 10, 15, 20, 30, 35, 50, 100)  # Return periods in years
ep = 1 / rp  # Exceedance probabilities

q_quant_eca_min <- qevd(1-ep, loc = average_parameters$Average_Mu_ECA_min, 
                        scale = average_parameters$Average_Sigma_ECA_min, 
                        shape = shape, type = "GEV")

q_quant_eca_max <- qevd(1-ep, loc = average_parameters$Average_Mu_ECA_max, 
                        scale = average_parameters$Average_Sigma_ECA_max, 
                        shape = shape, type = "GEV")


