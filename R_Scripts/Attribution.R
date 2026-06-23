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


# Station Attribution One by One (Fixed Climatic covs and changing ECA) -------
# Done: "02AB006", 02AD010, 02BA003, 02CC005, 02CE002, 02DC004, 04GA002, 04JC002, 04JD005, 04LM001
station = "05QD016"

# Finding the best fitted model
best_formula_table <- get_lowest_AICc_for_station("Outputs/Detection/GEV_Evaluation", 
                                                  station, input_type = "eca")
best_formula <- best_formula_table$Formula
# Remove "modXXXX <-" (where XXXX is any number)
clean_formula <- gsub("^mod[0-9]+ <-\\s*", "", best_formula)
# Extract model name (e.g., "mod2960")
model_name <- str_extract(best_formula, "^mod[0-9]+")

# for mannual
clean_formula <- "fevd(q, location.fun = ~cov2 + cov6 + cov7, scale.fun = ~cov4, type = 'GEV', method = 'MLE')"
model_name <- "changed"

# get all covariates ready for fitting the best model
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



best_model <- eval(parse(text = clean_formula))
get_AICc(best_model)
best_model

parameter_table_cov <- extract_gev_parameters_with_covariates(best_model, 
                                                              model_name = model_name)

# Compute average for cov1 to cov6
avg_cov1 <- mean(cov1)
avg_cov2 <- mean(cov2)
avg_cov3 <- mean(cov3)
avg_cov4 <- mean(cov4)
avg_cov5 <- mean(cov5)
avg_cov6 <- mean(cov6)
#eca_min <- min(cov7)
#eca_max <- max(cov7)

#average_parameters <- compute_average_mu_sigma(parameter_table_cov, cov_output)
shape <- parameter_table_cov$Value[parameter_table_cov$Parameter == "shape"]

mu_sigma_table <- compute_all_mu_sigma_for_ecas(parameter_table_cov, cov_output,
                                                save_csv = TRUE, 
                                                file_name = paste0("Complete_mu_sigma_table_", station, ".csv"), 
                                                file_path = "Outputs/Attribution/Mu_Sigma_Tables/")

# Now, for attribution we need to detect trend in Return level plots, 
#which show how the magnitude associated with a
#fixed return period is changing over the study period
# however these return level magnitudes should be calculated for each eca
# Define return periods
rp <- c(1.5, 2, 5, 7, 10, 15, 20, 30, 35, 50, 100)  # Return periods in years
ep <- 1 / rp  # Exceedance probabilities


return_level_for_all_Eca <- compute_return_levels(mu_sigma_table, return_periods = rp,
                                                  save_csv = TRUE,
                                                  file_name = paste0("Complete_Return_Levels_table_", station, ".csv"),
                                                  file_path = "Outputs/Attribution/Return_Levels_Fixed_Climatic_Changing_ECA/")


# Tend analysis Code development: ------
trend_test_results <- analyze_trends_comprehensive(return_level_for_all_Eca, 
                                                   save_csv = TRUE,
                                                   file_name = paste0("Attribution_Trend_Test_Rsults_", station, ".csv"),
                                                   file_path = "Outputs/Attribution/Trend_Test_Results/"
                                                   )








# Attribution Without Isolating ECA effect -----------
station = "02CC005" # input station

# clean formula should be input
clean_formula <- "fevd(q, location.fun = ~cov1 + cov2 + cov3 + cov7, scale.fun = ~cov1 + cov2 + cov3, type = 'GEV', method = 'MLE')"

output_path <- "Outputs/Attribution/Return_Levels_2"
return_levels <- save_return_levels(station, data, guide_data, clean_formula, standardized = FALSE, output_path, save = TRUE)

# Plot the return level for the 2-year return period with dots and lines
plot(test$Year, test$Return_Level_15, type = "o", col = "black", pch = 16,
     xlab = "Year", ylab = "Return Level (2-year)",
     main = "Return Level for 2-year Period Over Time")

# Add a linear regression trend line
trend_model <- lm(Return_Level_2 ~ Year, data = test)
abline(trend_model, col = "grey", lwd = 2)



# Counter Factual (Fixed ECA at Min or Max and changing Climatic covs) ---------
station = "02AB006"

# for mannual
clean_formula <- "fevd(q, location.fun = ~cov1 + cov6, scale.fun = ~+ cov3 + cov7, type = 'GEV', method = 'MLE')"
model_name <- "mod84"

# get all covariates ready for fitting the best model
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



best_model <- eval(parse(text = clean_formula))
get_AICc(best_model)

parameter_table_cov <- extract_gev_parameters_with_covariates(best_model, 
                                                              model_name = model_name)

# finding fixed ecas
eca_min <- min(cov7)
eca_min
eca_max <- max(cov7)
eca_max

eca_min <- 0.001

mu_sigma_table_min <- compute_mu_sigma_for_fixed_eca(parameter_table_cov, cov_output,
                                                 eca_min,
                                                save_csv = TRUE, 
                                                file_name = paste0("mu_sigma_table_Min_eca_", station, ".csv"), 
                                                file_path = "Outputs/Attribution/Counter_Factual/Mu-Sigma/")

mu_sigma_table_max <- compute_mu_sigma_for_fixed_eca(parameter_table_cov, cov_output,
                                                     eca_max,
                                                     save_csv = TRUE, 
                                                     file_name = paste0("mu_sigma_table_Max_eca_", station, ".csv"), 
                                                     file_path = "Outputs/Attribution/Counter_Factual/Mu-Sigma/")
# Now, for attribution we need to detect trend in Return level plots, 
#which show how the magnitude associated with a
#fixed return period is changing over the study period
# however these return level magnitudes should be calculated for each eca
# Define return periods
rp <- c(1.5, 2, 5, 7, 10, 15, 20, 30, 35, 50, 100)  # Return periods in years
ep <- 1 / rp  # Exceedance probabilities


return_level_for_min_Eca <- compute_return_levels(mu_sigma_table_min, return_periods = rp,
                                                  save_csv = TRUE,
                                                  file_name = paste0("Min_ECA_Return_Levels_table_", station, ".csv"),
                                                  file_path = "Outputs/Attribution/Counter_Factual/Return_Levels_Min_ECA/")

return_level_for_max_Eca <- compute_return_levels(mu_sigma_table_max, return_periods = rp,
                                                  save_csv = TRUE,
                                                  file_name = paste0("Max_ECA_Return_Levels_table_", station, ".csv"),
                                                  file_path = "Outputs/Attribution/Counter_Factual/Return_Levels_Max_ECA/")

# Comparison needs to be done by Mann–Whitney U test 

# BootStrap and Uncertainty ---------
station = "02AB006"

# for mannual
clean_formula <- "fevd(q, type = 'GEV', method = 'MLE')"
model_name <- "mod84"

# get all covariates ready for fitting the best model
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


best_model <- eval(parse(text = clean_formula))
get_AICc(best_model)

test <- xbooter(best_model, B = 100, rsize = length(q), 
        return.period = c(10, 20, 50, 100, 200, 500))
test_results <- test$results
boxplot(test_results)