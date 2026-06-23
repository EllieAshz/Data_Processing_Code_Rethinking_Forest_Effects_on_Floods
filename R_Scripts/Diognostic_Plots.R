# LOADING FUNCTIONS ----
source("scripts/functions.R")

# Uncertainty Analaysis ---------
station = "05QE009"

# for mannual
best_model_formula <- "fevd(q, location.fun = ~cov2, scale.fun = ~cov7, type = 'GEV', method = 'MLE')"

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



best_model <- eval(parse(text = best_model_formula))
parameter_table_cov <- extract_gev_parameters_with_covariates(best_model, 
                                                              model_name = model_name)

plot(best_model)
par(mfrow = c(1, 2))  # 1 row, 2 columns
# QQ plot (right)
plot(best_model, type = "qq", main = "")

# Density plot (left)
plot(best_model, type = "density", main = "")


# LRT test

best_climatic <- fevd(q, location.fun = ~cov2 + cov6, scale.fun = ~cov4, type = 'GEV', method = 'MLE')
# Perform Likelihood Ratio Test
lrt_result <- lr.test(best_climatic, best_model)
lrt_result

