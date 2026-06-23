# Prepare Station Covariates data ---------------------------------------------

#' Prepare and Clean Station Data
#'
#' This function processes hydrological data for a specific station by subsetting the dataset,
#' removing rows with missing values, replacing zeros with a very small positive number, and 
#' adjusting columns with negative values to ensure all values are positive.
#'
#' @param data A data frame containing the main dataset with hydrological and covariate data.
#'             Must include a column named `ID_hydro_st` for station IDs.
#' @param station A string representing the unique station ID to subset the data.
#'
#' @return A data frame containing:
#'   - Subset of the original data for the specified station.
#'   - Rows with `NA` removed.
#'   - Zeros replaced with a small positive value (`1e-10`).
#'   - Columns with negative values adjusted to be positive by adding the absolute value of the 
#'     most negative value in the column plus a small buffer (`0.01`).
#'
#' @details
#' - Replacing zeros ensures numerical stability in subsequent analyses like GEV modeling.
#' - Adjusting negative values ensures compatibility with mathematical models requiring positive inputs.
#'
#' @examples
#' cleaned_data <- prepare_subset_data(data = my_data, station = "08LF027")
#'
#' @export
prepare_subset_data <- function(data, station) {
  # Subset the data for the given station
  # Extract only the rows where the 'ID_hydro_st' column matches the specified station ID
  subset_data <- data[data$ID_hydro_st == station, ]
  
  # Remove rows with missing values (NA)
  # Ensures that the subset contains no incomplete data
  subset_data <- na.omit(subset_data)
  
  # Replace zeros with a very small number (e.g., 1e-10)
  # This helps avoid issues with mathematical operations where zero is not allowed
  small_value <- 1e-10
  subset_data[subset_data == 0] <- small_value
  
  # Adjust columns with negative values
  # Iterate through each column in the subset
  for (col in colnames(subset_data)) {
    if (is.numeric(subset_data[[col]])) { # Check if the column is numeric
      # If there are negative values in the column
      if (any(subset_data[[col]] < 0, na.rm = TRUE)) {
        # Calculate the adjustment value: absolute of the most negative value + a small buffer (0.01)
        adjustment <- abs(min(subset_data[[col]], na.rm = TRUE)) + 0.01
        # Add the adjustment value to the column to make all values positive
        subset_data[[col]] <- subset_data[[col]] + adjustment
      }
    }
  }
  
  # Return the cleaned and adjusted dataset
  return(subset_data)
}


#' Prepare Station Data and Covariates Based on Guide Covariates
#'
#' This function processes hydrological data for a specific station by subsetting the main dataset 
#' and using a guide to extract covariates. It returns the covariates, their names, and a combined 
#' dataset with `q` (discharge), `ECA`, and selected covariates.
#'
#' @param station A string representing the unique station ID.
#' @param data A data frame containing the main dataset with hydrological and covariate data.
#' @param guide_data A data frame containing the guide with covariate names for each station.
#'
#' @return A list containing:
#'   - `station`: The ID of the station being processed.
#'   - `cov1_name` to `cov5_name`: Covariate names as specified in `guide_data`.
#'   - `cov1` to `cov7`: Covariate vectors corresponding to the names, or `NULL` if unavailable.
#'   - `dataset`: A data frame with `Year`, `q`, `ECA`, station ID, and the selected covariates.
#'   - A message indicating if no guide data is available for the station.
#'
#' @export
prepare_station_covs <- function(station, data, guide_data) {
  # Subset the main data and guide data for the given station
  subset_data <- prepare_subset_data(data, station)
  # subset_data <- data[data$ID_hydro_st == station, ]
  # subset_data <- na.omit(subset_data)
  
  subset_guide <- guide_data[guide_data$ID_hydro_st == station, ]
  
  # Check if subset_guide has data; otherwise, return NULL
  if (nrow(subset_guide) == 0) {
    return(list(
      message = paste("No guide data found for station:", station),
      cov1_name = NULL,
      cov2_name = NULL,
      cov3_name = NULL,
      cov4_name = NULL,
      cov5_name = NULL,
      cov6_name = "time",
      cov7_name = "eca",
      cov1 = NULL,
      cov2 = NULL,
      cov3 = NULL,
      cov4 = NULL,
      cov5 = NULL,
      cov6 = NULL,
      cov7 = NULL,
      dataset = NULL
    ))
  }
  
  # Get covariate names
  cov1_name <- subset_guide$Cov1
  cov2_name <- subset_guide$Cov2
  cov3_name <- subset_guide$Cov3
  cov4_name <- subset_guide$Cov4
  cov5_name <- subset_guide$Cov5
  cov6_name <- "time"
  cov7_name <- "eca"
  
  # Define covariate vectors
  cov1 <- if (!is.na(cov1_name) && cov1_name %in% names(subset_data)) subset_data[[cov1_name]] else NULL
  cov2 <- if (!is.na(cov2_name) && cov2_name %in% names(subset_data)) subset_data[[cov2_name]] else NULL
  cov3 <- if (!is.na(cov3_name) && cov3_name %in% names(subset_data)) subset_data[[cov3_name]] else NULL
  cov4 <- if (!is.na(cov4_name) && cov4_name %in% names(subset_data)) subset_data[[cov4_name]] else NULL
  cov5 <- if (!is.na(cov5_name) && cov5_name %in% names(subset_data)) subset_data[[cov5_name]] else NULL
  cov6 <- seq(1, nrow(subset_data), 1)  # Time as sequence
  cov7 <- if ("Total_ECA_percentage" %in% names(subset_data)) subset_data$`Total_ECA_percentage` else NULL
  
  # Create the dataset
  dataset <- data.frame(
    Year = subset_data$Year,
    Max_discharge = subset_data$Max_discharge,
    ECA = cov7,
    Time = cov6,
    Station = station
  )
  
  # Add covariates only if they are not NULL
  if (!is.null(cov1)) dataset[[cov1_name]] <- cov1
  if (!is.null(cov2)) dataset[[cov2_name]] <- cov2
  if (!is.null(cov3)) dataset[[cov3_name]] <- cov3
  if (!is.null(cov4)) dataset[[cov4_name]] <- cov4
  if (!is.null(cov5)) dataset[[cov5_name]] <- cov5
  
  # Return all covariate names, vectors, and the dataset
  return(list(
    station = station,
    cov1_name = cov1_name,
    cov2_name = cov2_name,
    cov3_name = cov3_name,
    cov4_name = cov4_name,
    cov5_name = cov5_name,
    cov6_name = cov6_name,
    cov7_name = cov7_name,
    cov1 = cov1,
    cov2 = cov2,
    cov3 = cov3,
    cov4 = cov4,
    cov5 = cov5,
    cov6 = cov6,
    cov7 = cov7,
    dataset = dataset
  ))
}


#' Scale a Covariate Based on a Reference Covariate
#'
#' This function standardizes a given covariate (`cov`) based on the mean and standard 
#' deviation of a reference covariate (`cov7`). It ensures that the transformed `cov`
#' has the same mean and standard deviation as `cov7`, while preserving the relative 
#' differences within `cov`.
#'
#' @param cov A numeric vector representing the covariate to be scaled.
#' @param cov7 A numeric vector serving as the reference covariate, used to define the 
#' scaling target.
#'
#' @return A numeric vector with `cov` values transformed to match the mean and standard 
#' deviation of `cov7`.
#'
#' @examples
#' cov1 <- c(1.2, 1.5, 2.3, 2.8, 3.0)  # Example covariate
#' cov7 <- c(3, 6, 9, 12, 15)          # Reference covariate
#' scaled_cov1 <- scale_based_on_reference(cov1, cov7)
#' print(scaled_cov1)
#'
#' @export
scale_based_on_reference <- function(cov, cov7) {
  if (is.null(cov)) return(NULL)  # Return NULL if cov is missing
  
  if (is.null(cov7) || length(unique(cov7)) == 1) {
    stop("Reference covariate (cov7) is NULL or has no variance")
  }
  
  mean_cov <- mean(cov, na.rm = TRUE)
  sd_cov <- sd(cov, na.rm = TRUE)
  mean_cov7 <- mean(cov7, na.rm = TRUE)
  sd_cov7 <- sd(cov7, na.rm = TRUE)
  
  cov_scaled <- ((cov - mean_cov) / sd_cov) * sd_cov7 + mean_cov7
  return(cov_scaled)
}


#' Prepare Station Data and Covariates Based on Guide Covariates
#'
#' This function processes hydrological data for a specific station by subsetting the main dataset 
#' and using a guide to extract covariates. It returns the covariates, their names, and a combined 
#' dataset with `q` (discharge), `ECA`, and selected covariates.
#'
#' @param station A string representing the unique station ID.
#' @param data A data frame containing the main dataset with hydrological and covariate data.
#' @param guide_data A data frame containing the guide with covariate names for each station.
#'
#' @return A list containing:
#'   - `station`: The ID of the station being processed.
#'   - `cov1_name` to `cov5_name`: Covariate names as specified in `guide_data`.
#'   - `cov1` to `cov7`: Covariate vectors corresponding to the names, or `NULL` if unavailable.
#'   - `dataset`: A data frame with `Year`, `q`, `ECA`, station ID, and the selected covariates.
#'   - A message indicating if no guide data is available for the station.
#'
#' @export
prepare_station_covs_standardized <- function(station, data, guide_data) {
  # Subset the main data and guide data for the given station
  subset_data <- prepare_subset_data(data, station)
  # subset_data <- data[data$ID_hydro_st == station, ]
  # subset_data <- na.omit(subset_data)
  
  subset_guide <- guide_data[guide_data$ID_hydro_st == station, ]
  
  # Check if subset_guide has data; otherwise, return NULL
  if (nrow(subset_guide) == 0) {
    return(list(
      message = paste("No guide data found for station:", station),
      cov1_name = NULL,
      cov2_name = NULL,
      cov3_name = NULL,
      cov4_name = NULL,
      cov5_name = NULL,
      cov6_name = "time",
      cov7_name = "eca",
      cov1 = NULL,
      cov2 = NULL,
      cov3 = NULL,
      cov4 = NULL,
      cov5 = NULL,
      cov6 = NULL,
      cov7 = NULL,
      dataset = NULL
    ))
  }
  
  # Get covariate names
  cov1_name <- subset_guide$Cov1
  cov2_name <- subset_guide$Cov2
  cov3_name <- subset_guide$Cov3
  cov4_name <- subset_guide$Cov4
  cov5_name <- subset_guide$Cov5
  cov6_name <- "time"
  cov7_name <- "eca"
  
  # Define covariate vectors
  cov1 <- if (!is.na(cov1_name) && cov1_name %in% names(subset_data)) subset_data[[cov1_name]] else NULL
  cov2 <- if (!is.na(cov2_name) && cov2_name %in% names(subset_data)) subset_data[[cov2_name]] else NULL
  cov3 <- if (!is.na(cov3_name) && cov3_name %in% names(subset_data)) subset_data[[cov3_name]] else NULL
  cov4 <- if (!is.na(cov4_name) && cov4_name %in% names(subset_data)) subset_data[[cov4_name]] else NULL
  cov5 <- if (!is.na(cov5_name) && cov5_name %in% names(subset_data)) subset_data[[cov5_name]] else NULL
  cov6 <- seq(1, nrow(subset_data), 1)  # Time as sequence
  cov7 <- if ("Total_ECA_percentage" %in% names(subset_data)) subset_data$`Total_ECA_percentage` else NULL
  
  # Create the dataset
  dataset <- data.frame(
    Year = subset_data$Year,
    Max_discharge = subset_data$Max_discharge,
    ECA = cov7,
    Time = cov6,
    Station = station
  )
  
  # Add covariates only if they are not NULL
  if (!is.null(cov1)) dataset[[cov1_name]] <- cov1
  if (!is.null(cov2)) dataset[[cov2_name]] <- cov2
  if (!is.null(cov3)) dataset[[cov3_name]] <- cov3
  if (!is.null(cov4)) dataset[[cov4_name]] <- cov4
  if (!is.null(cov5)) dataset[[cov5_name]] <- cov5
  
  # standarzing before outputting:
  cov1 <- scale_based_on_reference(cov1, cov7)
  cov2 <- scale_based_on_reference(cov2, cov7)
  cov3 <- scale_based_on_reference(cov3, cov7)
  cov4 <- scale_based_on_reference(cov4, cov7)
  cov5 <- scale_based_on_reference(cov5, cov7)
  cov6 <- scale_based_on_reference(cov6, cov7)
  
  # Return all covariate names, vectors, and the dataset
  return(list(
    station = station,
    cov1_name = cov1_name,
    cov2_name = cov2_name,
    cov3_name = cov3_name,
    cov4_name = cov4_name,
    cov5_name = cov5_name,
    cov6_name = cov6_name,
    cov7_name = cov7_name,
    cov1 = cov1,
    cov2 = cov2,
    cov3 = cov3,
    cov4 = cov4,
    cov5 = cov5,
    cov6 = cov6,
    cov7 = cov7,
    dataset = dataset
  ))
}

# Complete generation of all combinations possible ----------------------------
#' Generate All Subsets of Covariates
#'
#' This helper function generates all possible subsets of a given list of covariates, 
#' up to a specified maximum number of covariates (default is 5).
#'
#' @param covs A character vector of covariate names.
#' @param max_covs An integer specifying the maximum size of subsets to generate.
#'                 Defaults to 5.
#'
#' @return A list of character vectorså, where each vector represents a subset of covariates.
#'
#' @details The function iterates through all possible subset sizes from 1 to the 
#' minimum of the number of covariates and the specified maximum. For each subset 
#' size, it computes all combinations of covariates.
#'
#' @examples
#' # Generate all subsets of up to 3 covariates
#' subsets <- all_subsets(c("cov1", "cov2", "cov3", "cov4"), max_covs = 3)
#' print(subsets)
#'
#' @export
all_subsets <- function(covs) {
  unlist(lapply(1:min(length(covs): 5), 
                function(num) combn(covs, num, simplify = FALSE)), 
         recursive = FALSE)
}

#' Generate Comprehensive GEV Model Formulas with ECA
#'
#' This function generates GEV model formulas, including `eca` as a covariate.
#' Single covariate models and multi-covariate models are separated and saved to Excel.
#'
#' @param cov_output A list containing station-specific covariates and their names.
#'                   Expected elements include cov1 to cov5 and their respective names.
#'                   Special covariates (`time` and `eca`) are automatically added.
#' @param station A string specifying the station ID. Must match the station in `cov_output`.
#' @param save_excel Logical. If TRUE, the result is saved as an Excel file.
#' @param save_path A string specifying the directory where the Excel file should be saved.
#'                  Defaults to the current working directory.
#' @param save_name A string specifying the name of the Excel file (without extension).
#'                  Defaults to "Complete_list_of_formulas".
#'
#' @return A list with two data frames: `preliminary_models` and `combination_models`.
#'
#' @export
# Helper function to split combination models
split_combination_models <- function(combination_models) {
  # Rows with cov7 or eca
  eca_combined <- combination_models[
    grepl("\\b(cov7|eca)\\b", combination_models$location_covariates) |
      grepl("\\b(cov7|eca)\\b", combination_models$scale_covariates), 
  ]
  
  # Rows without cov7 or eca
  climate_combined <- combination_models[
    !combination_models$model_number %in% eca_combined$model_number, 
  ]
  
  return(list(climate_combined = climate_combined, eca_combined = eca_combined))
}

complete_generate_formulas_with_eca <- function(cov_output, station, save_excel = FALSE, save_path = ".", save_name = "Complete_list_of_formulas") {
  if (station != cov_output$station) {
    stop("ERROR: The station name you passed does not match the output station.")
  }
  
  # Extract station ID and covariates
  ID_hydro_st <- cov_output$station
  covariates <- list(
    cov1 = cov_output$cov1,
    cov2 = cov_output$cov2,
    cov3 = cov_output$cov3,
    cov4 = cov_output$cov4,
    cov5 = cov_output$cov5
  )
  covariate_names <- list(
    cov1 = cov_output$cov1_name,
    cov2 = cov_output$cov2_name,
    cov3 = cov_output$cov3_name,
    cov4 = cov_output$cov4_name,
    cov5 = cov_output$cov5_name
  )
  
  # Add special covariates: time and eca
  covariates$cov6 <- "time"
  covariates$cov7 <- "eca"
  covariate_names$cov6 <- "Time"
  covariate_names$cov7 <- "ECA"
  
  # Filter valid covariates
  valid_covariates <- names(Filter(function(x) !is.null(x) && !all(is.na(x)), covariates))
  valid_covariate_names <- covariate_names[valid_covariates]
  
  # Initialize results
  preliminary_models <- data.frame(
    ID_hydro_st = character(),
    cov_formula = character(),
    location_covariates = character(),
    scale_covariates = character(),
    location_covariate_names = character(),
    scale_covariate_names = character(),
    model_number = integer(),
    model_name = character(),
    stringsAsFactors = FALSE
  )
  
  combination_models <- data.frame(
    ID_hydro_st = character(),
    cov_formula = character(),
    location_covariates = character(),
    scale_covariates = character(),
    location_covariate_names = character(),
    scale_covariate_names = character(),
    model_number = integer(),
    model_name = character(),
    stringsAsFactors = FALSE
  )
  
  model_number <- 0
  
  # Add stationary model to preliminary models
  preliminary_models <- rbind(preliminary_models, data.frame(
    ID_hydro_st = ID_hydro_st,
    cov_formula = "mod0 <- fevd(q, type = 'GEV', method = 'MLE')",
    location_covariates = "None",
    scale_covariates = "None",
    location_covariate_names = "None",
    scale_covariate_names = "None",
    model_number = model_number,
    model_name = "mod0"
  ))
  model_number <- model_number + 1
  
  # Add single covariate models to preliminary models
  for (cov in valid_covariates) {
    cov_name <- valid_covariate_names[[cov]]
    
    # Location-only model
    preliminary_models <- rbind(preliminary_models, data.frame(
      ID_hydro_st = ID_hydro_st,
      cov_formula = paste0("mod", model_number, " <- fevd(q, location.fun = ~", cov, ", type = 'GEV', method = 'MLE')"),
      location_covariates = cov,
      scale_covariates = "None",
      location_covariate_names = cov_name,
      scale_covariate_names = "None",
      model_number = model_number,
      model_name = paste0("mod", model_number)
    ))
    model_number <- model_number + 1
    
    # Scale-only model
    preliminary_models <- rbind(preliminary_models, data.frame(
      ID_hydro_st = ID_hydro_st,
      cov_formula = paste0("mod", model_number, " <- fevd(q, scale.fun = ~", cov, ", type = 'GEV', method = 'MLE')"),
      location_covariates = "None",
      scale_covariates = cov,
      location_covariate_names = "None",
      scale_covariate_names = cov_name,
      model_number = model_number,
      model_name = paste0("mod", model_number)
    ))
    model_number <- model_number + 1
    
    # Location and Scale model
    preliminary_models <- rbind(preliminary_models, data.frame(
      ID_hydro_st = ID_hydro_st,
      cov_formula = paste0("mod", model_number, " <- fevd(q, location.fun = ~", cov, ", scale.fun = ~", cov, ", type = 'GEV', method = 'MLE')"),
      location_covariates = cov,
      scale_covariates = cov,
      location_covariate_names = cov_name,
      scale_covariate_names = cov_name,
      model_number = model_number,
      model_name = paste0("mod", model_number)
    ))
    model_number <- model_number + 1
  }
  
  # Add multi-covariate combinations to combination models
  subsets <- all_subsets(valid_covariates)
  for (loc_covs in subsets) {
    for (scale_covs in subsets) {
      loc_formula <- paste(loc_covs, collapse = " + ")
      scale_formula <- paste(scale_covs, collapse = " + ")
      
      loc_names <- paste(valid_covariate_names[loc_covs], collapse = " + ")
      scale_names <- paste(valid_covariate_names[scale_covs], collapse = " + ")
      
      # Check against preliminary models
      duplicate <- any(apply(preliminary_models, 1, function(row) {
        setequal(strsplit(row["location_covariates"], " \\+ ")[[1]], loc_covs) &&
          setequal(strsplit(row["scale_covariates"], " \\+ ")[[1]], scale_covs)
      }))
      
      if (duplicate) {
        next
      }
      
      combination_models <- rbind(combination_models, data.frame(
        ID_hydro_st = ID_hydro_st,
        cov_formula = paste0("mod", model_number, " <- fevd(q, location.fun = ~", loc_formula, ", scale.fun = ~", scale_formula, ", type = 'GEV', method = 'MLE')"),
        location_covariates = loc_formula,
        scale_covariates = scale_formula,
        location_covariate_names = loc_names,
        scale_covariate_names = scale_names,
        model_number = model_number,
        model_name = paste0("mod", model_number)
      ))
      model_number <- model_number + 1
    }
  }
  
  # Separate combination models into eca_combined and climate_combined
  eca_combined <- combination_models[
    grepl("\\b(cov7|eca)\\b", combination_models$location_covariates) |
      grepl("\\b(cov7|eca)\\b", combination_models$scale_covariates), 
  ]
  climate_combined <- combination_models[
    !combination_models$model_number %in% eca_combined$model_number, 
  ]
  
  # Optionally save to Excel
  if (save_excel) {
    library(openxlsx)
    save_path <- gsub("/?$", "/", save_path) # Ensure path ends with a slash
    file_name <- paste0(save_path, save_name, ".xlsx")
    
    wb <- createWorkbook()
    addWorksheet(wb, "preliminary_models")
    addWorksheet(wb, "climate_combined")
    addWorksheet(wb, "eca_combined")
    
    writeData(wb, "preliminary_models", preliminary_models)
    writeData(wb, "climate_combined", climate_combined)
    writeData(wb, "eca_combined", eca_combined)
    
    saveWorkbook(wb, file_name, overwrite = TRUE)
    message(sprintf("Results saved to '%s'", file_name))
  }
  
  return(list(preliminary_models = preliminary_models, climate_combined = climate_combined, eca_combined = eca_combined))
}


# FIT GEV & EXTARCTUNG DATA FROM IT ---------------
#' Fit GEV Models from Formula Dataset
#'
#' This function fits Generalized Extreme Value (GEV) models based on the provided formulas 
#' and covariate data. It iterates through each formula in the dataset, evaluates it, and 
#' stores the fitted models in a named list.
#'
#' @param formula_dataset A data frame containing model formulas and metadata. 
#'                        Expected columns include:
#'                        - `cov_formula`: A string representation of the GEV model formula.
#'                        - `model_name`: A unique name for each model.
#' @param cov_output A list containing the covariate data prepared using a preprocessing function
#'                   (e.g., `prepare_station_covs`). This list should include:
#'                   - `dataset`: A data frame with the main dataset (must include `Max_discharge`).
#'                   - `cov1` to `cov7`: Covariates used in the formulas.
#'
#' @return A named list of fitted GEV models. Each list element is named according to the 
#'         `model_name` from the `formula_dataset`. If a model fails to fit, it is skipped, 
#'         and a message is displayed.
#'
#' @details
#' - The function uses `eval(parse(text = ...))` to evaluate and fit the formulas dynamically.
#' - Any errors encountered during model fitting (e.g., convergence issues) are caught and 
#'   reported, and the corresponding model is skipped.
#' - The input `cov_output` provides the necessary covariates for the formulas, including 
#'   discharge data (`q`) and additional covariates (`cov1` to `cov7`).
#'
#' @examples
#' # Example usage:
#' formula_dataset <- data.frame(
#'   cov_formula = c("mod1 <- fevd(q, type = 'GEV', method = 'MLE')",
#'                   "mod2 <- fevd(q, location.fun = ~ cov1, type = 'GEV', method = 'MLE')"),
#'   model_name = c("Stationary", "Covariate_Model")
#' )
#' 
#' cov_output <- list(
#'   dataset = data.frame(Max_discharge = c(100, 200, 150)),
#'   cov1 = c(1, 2, 3),
#'   cov2 = c(2, 3, 4)
#' )
#' 
#' models <- fit_gev_models(formula_dataset, cov_output)
#'
#' @export
fit_gev_models <- function(formula_dataset, cov_output) {
  
  q <- cov_output$dataset$Max_discharge
  cov1 <- cov_output$cov1
  cov2 <- cov_output$cov2
  cov3 <- cov_output$cov3
  cov4 <- cov_output$cov4
  cov5 <- cov_output$cov5
  cov6 <- cov_output$cov6
  cov7 <- cov_output$cov7
  
  model_list <- list()
  for (i in seq_len(nrow(formula_dataset))) {
    formula_text <- formula_dataset$cov_formula[i]
    model_name <- formula_dataset$model_name[i]
    message(sprintf("Fitting model: %s", model_name))
    
    fitted_model <- tryCatch({
      eval(parse(text = formula_text))
    }, error = function(e) {
      message(sprintf("Error in model %s: %s", model_name, e$message))
      NULL
    })
    
    if (!is.null(fitted_model)) {
      model_list[[model_name]] <- fitted_model
    }
  }
  
  return(model_list)
}


#' Evaluate GEV Models and Add Metrics to Formula Dataset
#'
#' This function evaluates a list of fitted GEV models and adds calculated metrics 
#' (AIC, BIC, AICc, and Negative Log-Likelihood) as new columns to the original formula dataset.
#'
#' @param model_list A named list of fitted GEV models.
#' @param formula_dataset A data frame containing model formulas and metadata.
#'
#' @return A data frame identical to `formula_dataset` but with additional columns:
#'   - `AIC`: Akaike Information Criterion
#'   - `BIC`: Bayesian Information Criterion
#'   - `AICc`: Corrected AIC
#'   - `NegativeLogLikelihood`: Negative Log-Likelihood
#'
#' @examples
#' # Assuming `model_list` contains fitted GEV models and `formulas$preliminary_models` is the input
#' updated_dataset <- evaluate_model_metrics(model_list, formulas$preliminary_models)
#'
#' @export
evaluate_model_metrics <- function(model_list, formula_dataset) {
  # Initialize new columns with NA
  formula_dataset$AIC <- NA
  formula_dataset$BIC <- NA
  formula_dataset$AICc <- NA
  formula_dataset$NegativeLogLikelihood <- NA
  
  # Loop through the model list and calculate metrics
  for (i in seq_along(model_list)) {
    model <- model_list[[i]]
    if (!is.null(model)) {
      tryCatch({
        # Check if the Hessian matrix is valid
        if (!is.null(model$results$hessian) && !any(is.na(model$results$hessian))) {
          summary_model <- summary(model)
          aic <- summary_model$AIC
          bic <- summary_model$BIC
          neg_log_likelihood <- model$results$value
          n_params <- length(model$parnames)
          n_obs <- length(model$x)
          
          aicc <- if (n_obs > n_params + 1) {
            aic + ((2 * n_params^2 + 2 * n_params) / (n_obs - n_params - 1))
          } else {
            NA
          }
          
          # Add metrics to the corresponding row in formula_dataset
          formula_dataset$AIC[i] <- aic
          formula_dataset$BIC[i] <- bic
          formula_dataset$AICc[i] <- aicc
          formula_dataset$NegativeLogLikelihood[i] <- neg_log_likelihood
        }
      }, error = function(e) {
        message(sprintf("Error evaluating model %s: %s", formula_dataset$model_name[i], e$message))
      })
    }
  }
  
  return(formula_dataset)
}

#' Fit GEV Models and Add Evaluation Metrics to Formula Dataset
#'
#' This function processes multiple datasets (sheets) containing model formulas, 
#' fits Generalized Extreme Value (GEV) models, evaluates their metrics (AIC, BIC, AICc, 
#' and Negative Log-Likelihood), and adds these metrics as new columns to the original 
#' datasets. Optionally, it saves the updated datasets to an Excel file.
#'
#' @param formula_output A list containing formula datasets for different sheets. 
#'                       Expected structure:
#'                       - `preliminary_models`: Data frame for preliminary models.
#'                       - `climate_combined`: Data frame for climate-based models.
#'                       - `eca_combined`: Data frame for ECA-based models.
#' @param cov_output A list containing the covariate data prepared using a preprocessing 
#'                   function (e.g., `prepare_station_covs`). This list should include:
#'                   - `dataset`: A data frame with the main dataset (must include `Max_discharge`).
#'                   - `cov1` to `cov7`: Covariates used in the formulas.
#' @param save_excel Logical. If `TRUE`, the updated datasets with evaluation metrics are saved 
#'                   to an Excel file. Default is `FALSE`.
#' @param save_path A string specifying the directory path where the Excel file should be saved. 
#'                  Defaults to the current working directory.
#' @param save_name A string specifying the name of the Excel file (including `.xlsx` extension). 
#'                  Default is `"Model_Evaluated_complete.xlsx"`.
#'
#' @return A named list containing updated datasets for each sheet. Each dataset includes the 
#'         original columns from the formula dataset plus the following new columns:
#'         - `AIC`: Akaike Information Criterion.
#'         - `BIC`: Bayesian Information Criterion.
#'         - `AICc`: Corrected Akaike Information Criterion.
#'         - `NegativeLogLikelihood`: Negative Log-Likelihood.
#' @details
#' - The function iterates through each dataset (sheet) in the `formula_output`, fits the models 
#'   using `fit_gev_models`, calculates metrics using `evaluate_model_metrics`, and appends these 
#'   metrics to the dataset.
#' - If `save_excel` is `TRUE`, the results are saved in an Excel file, with each sheet corresponding 
#'   to one dataset in the input.
#'
#' @examples
#' # Example usage:
#' formula_output <- list(
#'   preliminary_models = preliminary_df,
#'   climate_combined = climate_df,
#'   eca_combined = eca_df
#' )
#' 
#' cov_output <- list(
#'   dataset = data.frame(Max_discharge = c(100, 200, 150)),
#'   cov1 = c(1, 2, 3),
#'   cov2 = c(2, 3, 4)
#' )
#' 
#' results <- fit_models_and_add_metrics(formula_output, cov_output, 
#'                                       save_excel = TRUE, 
#'                                       save_path = "results/", 
#'                                       save_name = "Evaluated_Models.xlsx")
#'
#' @export
fit_models_and_add_metrics <- function(formula_output, cov_output, 
                                       save_excel = FALSE, save_path = ".", 
                                       save_name = "Model_Evaluated_complete.xlsx") {
  library(openxlsx)
  
  # Extract the datasets from the formula output
  datasets <- list(
    preliminary_models = formula_output$preliminary_models,
    climate_combined = formula_output$climate_combined,
    eca_combined = formula_output$eca_combined
  )
  
  # Initialize workbook for saving results
  if (save_excel) {
    wb <- createWorkbook()
  }
  
  q <- cov_output$dataset$Max_discharge
  cov1 <- cov_output$cov1
  cov2 <- cov_output$cov2
  cov3 <- cov_output$cov3
  cov4 <- cov_output$cov4
  cov5 <- cov_output$cov5
  cov6 <- cov_output$cov6
  cov7 <- cov_output$cov7
  
  # Store all unique parameter names across models
  all_params <- c()
  
  # Process each dataset (sheet)
  results_list <- lapply(names(datasets), function(sheet_name) {
    message(sprintf("Processing sheet: %s", sheet_name))
    
    formula_dataset <- datasets[[sheet_name]]
    
    # Fit models
    model_list <- fit_gev_models(formula_dataset, cov_output)
    
    # Extract all parameter names across models
    for (model in model_list) {
      if (!is.null(model) && "results" %in% names(model)) {
        all_params <- unique(c(all_params, names(model$results$par)))
      }
    }
    
    # Fit the stationary model
    stationary_formula <- formula_dataset$cov_formula[1]  # Stationary model is always the first
    stationary_model <- tryCatch({
      eval(parse(text = stationary_formula))
    }, error = function(e) {
      message("Error fitting stationary model: ", e$message)
      NULL
    })
    
    # Evaluate models and calculate metrics
    metrics <- evaluate_model_metrics(model_list, formula_dataset)
    
    # Perform likelihood ratio test (LRT) against stationary model
    metrics$LRT_Significance <- NA  # Initialize LRT significance column
    if (!is.null(stationary_model)) {
      for (i in seq_along(model_list)) {
        model <- model_list[[i]]
        if (!is.null(model)) {
          lrt <- tryCatch({
            lr.test(stationary_model, model)
          }, error = function(e) {
            message(sprintf("Error in LRT for model %s: %s", names(model_list)[i], e$message))
            return(NULL)
          })
          
          # Add LRT results to the metrics
          if (!is.null(lrt)) {
            metrics$LRT_Significance[i] <- if (lrt$p.value < 0.01) {
              "99% significant"
            } else if (lrt$p.value < 0.05) {
              "95% significant"
            } else if (lrt$p.value < 0.1) {
              "90% significant"
            } else {
              "Not significant"
            }
          }
        }
      }
    }
    
    # Extract model parameters and add them as separate columns
    parameter_data <- data.frame(matrix(NA, nrow = nrow(metrics), ncol = length(all_params)))
    colnames(parameter_data) <- all_params
    
    for (i in seq_along(model_list)) {
      model <- model_list[[i]]
      if (!is.null(model) && "results" %in% names(model)) {
        param_values <- model$results$par
        for (param in names(param_values)) {
          parameter_data[i, param] <- param_values[param]
        }
      }
    }
    
    # Add parameter data to metrics
    updated_dataset <- cbind(metrics, parameter_data)
    
    # Save to workbook if requested
    if (save_excel) {
      addWorksheet(wb, sheet_name)
      writeData(wb, sheet_name, updated_dataset)
    }
    
    return(updated_dataset)
  })
  
  names(results_list) <- names(datasets)
  
  # Save workbook to file
  if (save_excel) {
    save_path <- gsub("/?$", "/", save_path)
    save_file <- paste0(save_path, save_name)
    saveWorkbook(wb, save_file, overwrite = TRUE)
    message(sprintf("Results saved to '%s'", save_file))
  }
  
  return(results_list)
}


#' Calculate AICc from a fitted GEV model
#'
#' This function extracts the AICc (Corrected Akaike Information Criterion) from 
#' a given `fevd` model object.
#'
#' @param model A fitted GEV model object from the `fevd()` function in the `extRemes` package.
#'
#' @return The AICc value for the given model. Returns `NA` if the calculation fails.
#'
#' @examples
#' best_mod <- fevd(q, type = "GEV", method = "MLE")  # Fit a GEV model
#' aicc_value <- get_AICc(best_mod)  # Extract AICc
#' print(aicc_value)
#'
#' @export
get_AICc <- function(model) {
  # Check if the model is valid
  if (is.null(model) || !"results" %in% names(model)) {
    warning("Invalid model: No results found.")
    return(NA)
  }
  
  tryCatch({
    # Extract AIC without printing the full summary
    model_summary <- suppressMessages(suppressWarnings(summary(model)))
    aic <- model_summary$AIC  # Get AIC value silently
    
    # Get number of parameters (length of estimated parameters)
    n_params <- length(model$parnames)
    
    # Get number of observations (length of q)
    n_obs <- length(model$x)
    
    # Compute AICc (corrected AIC)
    aicc <- if (n_obs > n_params + 1) {
      aic + ((2 * n_params^2 + 2 * n_params) / (n_obs - n_params - 1))
    } else {
      NA
    }
    
    return(aicc)
  }, error = function(e) {
    warning("Error calculating AICc: ", e$message)
    return(NA)
  })
}


#' Run a Likelihood Ratio Test (LRT) for a hydrological station
#'
#' This function prepares standardized covariates for a given hydrological station, 
#' retrieves the corresponding model formulas for climate and ECA models, evaluates them, 
#' and performs a likelihood ratio test (LRT) to compare the two models.
#'
#' @param station A string representing the hydrological station ID.
#' @param data A dataframe containing discharge and covariate information.
#' @param guide_data A dataframe used for guiding the preparation of covariates.
#' @param model_ds A dataframe containing model formulas for different stations, 
#'   including columns `cov_formula_eca` and `cov_formula_climate`.
#'
#' @return A list containing the results of the likelihood ratio test (LRT).
#'
#' @examples
#' station <- "02AB006"
#' LRT_result <- run_lrt_test(station, data, guide_data, model_ds)
#' print(LRT_result)
#'
#' @export
run_lrt_test <- function(station, data, guide_data, cov_output, model_ds) {
  # Extract relevant variables from the covariate output
  q <- cov_output$dataset$Max_discharge
  cov1 <- cov_output$cov1
  cov2 <- cov_output$cov2
  cov3 <- cov_output$cov3
  cov4 <- cov_output$cov4
  cov5 <- cov_output$cov5
  cov6 <- cov_output$cov6
  cov7 <- cov_output$cov7
  
  # Filter the model dataset for the given station
  station_ds <- model_ds[model_ds$ID_hydro_st == station, ]
  
  # Extract model formulas as text from the dataset
  eca_mod_txt <- station_ds$cov_formula_eca
  climate_mod_txt <- station_ds$cov_formula_climate
  
  # Convert the extracted text formulas into R model objects
  eca_mod <- eval(parse(text = eca_mod_txt))
  climate_mod <- eval(parse(text = climate_mod_txt))
  
  # Perform the likelihood ratio test (LRT) to compare the two models
  LRT <- lr.test(climate_mod, eca_mod, alpha = 0.05)
  
  # Return the result of the likelihood ratio test
  return(LRT)
}

# Output has some duplication columns in GEV evaluation so I want to remove all those duplications
clean_excel_files <- function(folder_path) {
  # Get a list of all Excel files
  excel_files <- list.files(folder_path, pattern = "\\.xlsx$", full.names = TRUE)
  
  for (file in excel_files) {
    # Get sheet names
    sheet_names <- excel_sheets(file)
    
    # List to store cleaned sheets
    cleaned_sheets <- list()
    
    for (sheet in sheet_names) {
      # Read the sheet
      df <- read_excel(file, sheet = sheet, .name_repair = "minimal")
      
      # Remove duplicate columns
      df <- df[, !duplicated(colnames(df))]
      
      # Save the cleaned data to the list
      cleaned_sheets[[sheet]] <- df
    }
    
    # Overwrite the original file
    write_xlsx(cleaned_sheets, file)
    
    message(paste("Cleaned file overwritten:", file))
  }
}

# QUANTIFICATION FUNCTIONS ----------------------------------

get_lowest_AICc_for_station <- function(folder_path, station_id, input_type = "eca") {
  # Validate the input type
  if (!input_type %in% c("eca", "climate")) {
    stop("Invalid input type. Use 'eca' or 'climate'.")
  }
  
  # Determine the sheet name based on the input type
  sheet_name <- ifelse(input_type == "eca", "eca_combined", "climate_combined")
  
  # Construct the pattern to find the file with the specified station ID
  file_pattern <- paste0("Station_", station_id, "_model_Evaluation_Results_Unique_Covs\\.xlsx$")
  
  # Get the full path of the matching Excel file
  excel_file <- list.files(folder_path, pattern = file_pattern, full.names = TRUE)
  
  # Check if the file exists
  if (length(excel_file) == 0) {
    stop(paste("No file found for station ID:", station_id))
  }
  
  # Try to read the specified sheet **without renaming columns**
  tryCatch({
    df <- read_excel(excel_file, sheet = sheet_name, .name_repair = "minimal")
    
    # Print column names to check if renaming still happens
    print(colnames(df))  
    
    # Ensure required columns exist
    if (!"AICc" %in% colnames(df) || !"cov_formula" %in% colnames(df)) {
      stop("Required columns 'AICc' or 'cov_formula' are missing in the sheet.")
    }
    
    # Find the row with the lowest AICc
    min_aicc_row <- df %>% filter(AICc == min(AICc, na.rm = TRUE))
    if (nrow(min_aicc_row) > 0) {
      return(data.frame(
        Station_ID = station_id,
        Formula = min_aicc_row$cov_formula[1],
        AICc = min_aicc_row$AICc[1]
      ))
    } else {
      stop("No rows with valid AICc found.")
    }
  }, error = function(e) {
    stop(paste("Error processing file for station ID:", station_id, "-", e))
  })
}


#' Extract Parameters and Covariate Mapping from a Fitted GEV Model
#'
#' This function extracts the estimated parameters from a fitted `fevd` model 
#' and maps each parameter to its corresponding covariate from the model formula.
#'
#' @param model A fitted GEV model object from `fevd()`.
#' @param model_name (Optional) A string to identify the model (e.g., "best_mod02AD010").
#'
#' @return A dataframe with:
#'   - `Model_Name`: The name of the model.
#'   - `Parameter`: The estimated parameter name (`mu0`, `mu1`, `sigma0`, etc.).
#'   - `Covariate`: The covariate associated with each parameter (if applicable).
#'   - `Value`: The estimated parameter value.
#'
#' @examples
#' best_mod <- fevd(q, location.fun = ~cov4 + cov7, scale.fun = ~cov2 + cov5, type = "GEV", method = "MLE")
#' param_df <- extract_gev_parameters_with_covariates(best_mod, model_name = "best_mod02AD010")
#' print(param_df)
#'
#' @export
extract_gev_parameters_with_covariates <- function(model, model_name = "Unnamed_Model") {
  # Check if the model is valid
  if (is.null(model) || !"results" %in% names(model)) {
    warning("Invalid model: No results found.")
    return(NULL)
  }
  
  tryCatch({
    # Extract parameter estimates
    param_values <- model$results$par
    param_names <- names(param_values)
    
    # Extract covariates from model formula
    location_covs <- if (!is.null(model$call$location.fun)) all.vars(model$call$location.fun) else character(0)
    scale_covs <- if (!is.null(model$call$scale.fun)) all.vars(model$call$scale.fun) else character(0)
    
    # Initialize covariate mapping
    covariate_map <- rep(NA, length(param_names))
    
    # Assign covariates to mu and sigma parameters dynamically
    mu_index <- 1
    sigma_index <- 1
    for (i in seq_along(param_names)) {
      param <- param_names[i]
      
      if (grepl("^mu[1-9]+", param)) {  # Location parameter (mu1, mu2, etc.)
        if (mu_index <= length(location_covs)) {
          covariate_map[i] <- location_covs[mu_index]
        }
        mu_index <- mu_index + 1
      }
      
      if (grepl("^sigma[1-9]+", param)) {  # Scale parameter (sigma1, sigma2, etc.)
        if (sigma_index <= length(scale_covs)) {
          covariate_map[i] <- scale_covs[sigma_index]
        }
        sigma_index <- sigma_index + 1
      }
    }
    
    # Create dataframe
    param_df <- data.frame(
      Model_Name = model_name,
      Parameter = param_names,
      Covariate = covariate_map,
      Value = param_values,
      stringsAsFactors = FALSE
    )
    
    return(param_df)
  }, error = function(e) {
    warning("Error extracting parameters: ", e$message)
    return(NULL)
  })
}


#' Compute Average Mu and Sigma Based on Covariate Values
#'
#' This function calculates the average values for covariates (cov1 to cov6), 
#' finds the min/max for cov7 (ECA), and computes mu and sigma based on the best fitted GEV model.
#'
#' @param parameter_table A dataframe from `extract_gev_parameters_with_covariates()`
#' @param cov_output A list containing station-specific covariates and their values.
#'
#' @return A dataframe with:
#'   - `ECA_min`: The minimum value of cov7.
#'   - `ECA_max`: The maximum value of cov7.
#'   - `Average_Mu_ECA_min`: Computed mu for ECA_min.
#'   - `Average_Mu_ECA_max`: Computed mu for ECA_max.
#'   - `Average_Sigma_ECA_min`: Computed sigma for ECA_min.
#'   - `Average_Sigma_ECA_max`: Computed sigma for ECA_max.
#'
#' @examples
#' result <- compute_average_mu_sigma(parameter_table_cov, cov_output)
#' print(result)
#'
#' @export
compute_average_mu_sigma <- function(parameter_table, cov_output) {
  # Compute average for cov1 to cov6
  avg_cov1 <- mean(cov_output$cov1, na.rm = TRUE)
  avg_cov2 <- mean(cov_output$cov2, na.rm = TRUE)
  avg_cov3 <- mean(cov_output$cov3, na.rm = TRUE)
  avg_cov4 <- mean(cov_output$cov4, na.rm = TRUE)
  avg_cov5 <- mean(cov_output$cov5, na.rm = TRUE)
  avg_cov6 <- mean(cov_output$cov6, na.rm = TRUE)
  
  # Compute min and max for cov7 (ECA)
  min_eca <- min(cov_output$cov7, na.rm = TRUE)
  max_eca <- max(cov_output$cov7, na.rm = TRUE)
  
  # Initialize mu and sigma calculations
  mu_min_eca <- 0
  mu_max_eca <- 0
  sigma_min_eca <- 0
  sigma_max_eca <- 0
  
  # Iterate over extracted parameters
  for (i in seq_len(nrow(parameter_table))) {
    param <- parameter_table$Parameter[i]
    covariate <- parameter_table$Covariate[i]
    value <- parameter_table$Value[i]
    
    # Check if parameter is a mu or sigma term
    if (grepl("^mu", param)) {
      # Assign covariate values correctly
      if (is.na(covariate)) { mu_min_eca <- mu_min_eca + value; mu_max_eca <- mu_max_eca + value }
      else if (covariate == "cov1") { mu_min_eca <- mu_min_eca + value * avg_cov1; mu_max_eca <- mu_max_eca + value * avg_cov1 }
      else if (covariate == "cov2") { mu_min_eca <- mu_min_eca + value * avg_cov2; mu_max_eca <- mu_max_eca + value * avg_cov2 }
      else if (covariate == "cov3") { mu_min_eca <- mu_min_eca + value * avg_cov3; mu_max_eca <- mu_max_eca + value * avg_cov3 }
      else if (covariate == "cov4") { mu_min_eca <- mu_min_eca + value * avg_cov4; mu_max_eca <- mu_max_eca + value * avg_cov4 }
      else if (covariate == "cov5") { mu_min_eca <- mu_min_eca + value * avg_cov5; mu_max_eca <- mu_max_eca + value * avg_cov5 }
      else if (covariate == "cov6") { mu_min_eca <- mu_min_eca + value * avg_cov6; mu_max_eca <- mu_max_eca + value * avg_cov6 }
      else if (covariate == "cov7") { mu_min_eca <- mu_min_eca + value * min_eca; mu_max_eca <- mu_max_eca + value * max_eca }
    }
    
    if (grepl("^sigma", param)) {
      # Assign covariate values correctly
      if (is.na(covariate)) { sigma_min_eca <- sigma_min_eca + value; sigma_max_eca <- sigma_max_eca + value }
      else if (covariate == "cov1") { sigma_min_eca <- sigma_min_eca + value * avg_cov1; sigma_max_eca <- sigma_max_eca + value * avg_cov1 }
      else if (covariate == "cov2") { sigma_min_eca <- sigma_min_eca + value * avg_cov2; sigma_max_eca <- sigma_max_eca + value * avg_cov2 }
      else if (covariate == "cov3") { sigma_min_eca <- sigma_min_eca + value * avg_cov3; sigma_max_eca <- sigma_max_eca + value * avg_cov3 }
      else if (covariate == "cov4") { sigma_min_eca <- sigma_min_eca + value * avg_cov4; sigma_max_eca <- sigma_max_eca + value * avg_cov4 }
      else if (covariate == "cov5") { sigma_min_eca <- sigma_min_eca + value * avg_cov5; sigma_max_eca <- sigma_max_eca + value * avg_cov5 }
      else if (covariate == "cov6") { sigma_min_eca <- sigma_min_eca + value * avg_cov6; sigma_max_eca <- sigma_max_eca + value * avg_cov6 }
      else if (covariate == "cov7") { sigma_min_eca <- sigma_min_eca + value * min_eca; sigma_max_eca <- sigma_max_eca + value * max_eca }
    }
  }
  
  # Return results in a dataframe
  return(data.frame(
    ECA_min = min_eca,
    ECA_max = max_eca,
    Average_Mu_ECA_min = mu_min_eca,
    Average_Mu_ECA_max = mu_max_eca,
    Average_Sigma_ECA_min = sigma_min_eca,
    Average_Sigma_ECA_max = sigma_max_eca
  ))
}


#' Quantify Return Period Discharge Under ECA Scenarios
#'
#' This function estimates the discharge levels corresponding to different return periods 
#' under both minimum and maximum Equivalent Clearcut Area (ECA) scenarios. It fits 
#' the best Generalized Extreme Value (GEV) model for a given station, extracts 
#' GEV parameters, and computes discharge quantiles for different return periods.
#' Additionally, it calculates the percentage increase in flood magnitude due to ECA.
#'
#' @param station_id The ID of the hydrological station for which return levels are calculated.
#' @param folder_path Optional. The file path to the directory containing relevant data files. Default is an empty string.
#' @param input_type The type of input used for model selection (default: "eca").
#' @param save_csv Logical. If \code{TRUE}, saves the results as a CSV file. Default is \code{FALSE}.
#' @param file_path Optional. The directory where the CSV file should be saved. Default is the working directory (`"."`).
#' @param file_name Optional. Custom name for the output CSV file (e.g., "station_results.csv"). Default is \code{NULL}, which generates a filename automatically.
#'
#' @return A dataframe containing:
#' \itemize{
#'   \item \code{Station_ID}: The ID of the hydrological station.
#'   \item \code{Return_Period}: The return period (e.g., 2, 5, 10, 20 years).
#'   \item \code{Exceedance_Probability}: The probability of exceeding the discharge level in a given year.
#'   \item \code{ECA_Min_Valid}: The **minimum valid ECA** (i.e., lowest ECA value where `Sigma_ECA` is positive).
#'   \item \code{ECA_Max_Valid}: The **maximum valid ECA** (i.e., highest ECA value where `Sigma_ECA` is positive).
#'   \item \code{Discharge_ECA_Min}: Estimated peak discharge under the valid minimum ECA condition.
#'   \item \code{Discharge_ECA_Max}: Estimated peak discharge under the valid maximum ECA condition.
#'   \item \code{ECA_Flood_Risk_Increase_%}: The percentage increase in flood magnitude due to ECA.
#' }
#'
#' @examples
#' result <- rp_quantification_min_max_eca(station_id = 101, save_csv = TRUE, file_path = "Results", file_name = "station_101_results.csv")
#' print(result)
#'
#' @export
rp_quantification_min_max_eca <- function(station_id, folder_path = "", input_type = "eca", 
                                          save_csv = FALSE, file_path = ".", file_name = NULL, 
                                          clean_formula_input = NULL) {
  # Check if clean formula is provided
  if (is.null(clean_formula_input)) {
    # Get the best model formula
    best_formula_table <- get_lowest_AICc_for_station("Outputs/Detection/GEV_Evaluation", station_id, input_type)
    best_formula <- best_formula_table$Formula
    
    # Print the best formula
    message("Best formula for station ", station_id, ": ", best_formula)
    
    # Clean the formula
    clean_formula <- gsub("^mod[0-9]+ <-\\s*", "", best_formula)
  } else {
    # Use the provided clean formula
    clean_formula <- clean_formula_input
    message("Using provided clean formula for station ", station_id)
  }
  
  # Prepare covariates
  cov_output <- prepare_station_covs_standardized(station_id, data, guide_data)
  q <- cov_output$dataset$Max_discharge
  cov1 <- cov_output$cov1
  cov2 <- cov_output$cov2
  cov3 <- cov_output$cov3
  cov4 <- cov_output$cov4
  cov5 <- cov_output$cov5
  cov6 <- cov_output$cov6
  cov7 <- cov_output$cov7
  
  # Fit the best model
  best_model <- eval(parse(text = clean_formula))
  
  # Extract parameters with covariates
  parameter_table_cov <- extract_gev_parameters_with_covariates(best_model, model_name = paste0("best_mod_", station_id))
  
  # Compute mu and sigma for all ECA values
  mu_sigma_table <- compute_all_mu_sigma_for_ecas(parameter_table_cov, cov_output)
  
  # Ensure only rows with positive Sigma_ECA are used
  valid_mu_sigma_table <- mu_sigma_table[mu_sigma_table$Sigma_ECA > 0, ]
  
  # Identify the min and max ECA values **ensuring Sigma_ECA is positive**
  eca_min_valid <- min(valid_mu_sigma_table$ECA, na.rm = TRUE)
  eca_max_valid <- max(valid_mu_sigma_table$ECA, na.rm = TRUE)
  
  # Extract corresponding Mu and Sigma for the identified ECA values
  mu_eca_min <- valid_mu_sigma_table$Mu_ECA[valid_mu_sigma_table$ECA == eca_min_valid]
  sigma_eca_min <- valid_mu_sigma_table$Sigma_ECA[valid_mu_sigma_table$ECA == eca_min_valid]
  
  mu_eca_max <- valid_mu_sigma_table$Mu_ECA[valid_mu_sigma_table$ECA == eca_max_valid]
  sigma_eca_max <- valid_mu_sigma_table$Sigma_ECA[valid_mu_sigma_table$ECA == eca_max_valid]
  
  # Extract shape parameter (should be constant)
  shape <- unique(parameter_table_cov$Value[parameter_table_cov$Parameter == "shape"])
  
  # Define return periods and exceedance probabilities
  rp <- c(1.5, 2, 5, 7, 10, 15, 20, 30, 35, 50, 100)
  ep <- 1 / rp
  
  # Compute quantiles for **valid** ECA min and max scenarios
  q_quant_eca_min <- qevd(1 - ep, loc = mean(mu_eca_min), scale = mean(sigma_eca_min), shape = shape, type = "GEV")
  q_quant_eca_max <- qevd(1 - ep, loc = mean(mu_eca_max), scale = mean(sigma_eca_max), shape = shape, type = "GEV")
  
  # Compute percentage increase in flood magnitude due to ECA
  eca_flood_risk_increase <- ((q_quant_eca_max - q_quant_eca_min) / q_quant_eca_min) * 100
  
  # Create results dataframe
  results <- data.frame(
    Station_ID = station_id,
    Return_Period = rp,
    Exceedance_Probability = ep,
    Valid_ECA_Min = eca_min_valid,
    Valid_ECA_Max = eca_max_valid,
    Discharge_ECA_Min = q_quant_eca_min,
    Discharge_ECA_Max = q_quant_eca_max,
    ECA_Flood_Risk_Increase_Percentage = eca_flood_risk_increase
  )
  
  # Save to CSV if requested
  if (save_csv) {
    if (is.null(file_name)) {
      file_name <- paste0("Return_Levels_Station_", station_id, ".csv")
    }
    
    full_file_path <- file.path(file_path, file_name)
    
    write.csv(results, file = full_file_path, row.names = FALSE)
    message("Results saved to ", full_file_path)
  }
  
  return(results)
}

#' Quantify Return Period Discharge Under ECA Scenarios
#'
#' This function estimates the discharge levels corresponding to different return periods 
#' under both minimum and maximum Equivalent Clearcut Area (ECA) scenarios. It fits 
#' the best Generalized Extreme Value (GEV) model for a given station, extracts 
#' GEV parameters, and computes discharge quantiles for different return periods.
#' Additionally, it calculates the percentage increase in flood magnitude due to ECA.
#'
#' @param station_id The ID of the hydrological station for which return levels are calculated.
#' @param folder_path Optional. The file path to the directory containing relevant data files. Default is an empty string.
#' @param input_type The type of input used for model selection (default: "eca").
#' @param save_csv Logical. If \code{TRUE}, saves the results as a CSV file. Default is \code{FALSE}.
#' @param file_path Optional. The directory where the CSV file should be saved. Default is the working directory (`"."`).
#' @param file_name Optional. Custom name for the output CSV file (e.g., "station_results.csv"). Default is \code{NULL}, which generates a filename automatically.
#'
#' @return A dataframe containing:
#' \itemize{
#'   \item \code{Station_ID}: The ID of the hydrological station.
#'   \item \code{Return_Period}: The return period (e.g., 2, 5, 10, 20 years).
#'   \item \code{Exceedance_Probability}: The probability of exceeding the discharge level in a given year.
#'   \item \code{Discharge_ECA_Min}: Estimated peak discharge under minimum ECA conditions.
#'   \item \code{Discharge_ECA_Max}: Estimated peak discharge under maximum ECA conditions.
#'   \item \code{ECA_Flood_Risk_Increase_%}: The percentage increase in flood magnitude due to ECA.
#' }
#'
#' @details
#' The function first selects the best GEV model for the station by identifying the model with 
#' the lowest AICc value. It then extracts GEV parameters and computes return period quantiles 
#' using the fitted model under different ECA scenarios. The function outputs a dataframe containing 
#' the computed return levels and the percentage increase in flood risk due to ECA.
#'
#' If \code{save_csv = TRUE}, the results are saved in the specified \code{file_path} with the given \code{file_name}. 
#' If \code{file_name} is not provided, it defaults to \code{"Return_Levels_Station_<station_id>.csv"}.
#'
#' @examples
#' result <- rp_quantification_min_max_eca(station_id = 101, save_csv = TRUE, file_path = "Results", file_name = "station_101_results.csv")
#' print(result)
#'
#' @export
rp_quantification_min_max_eca_not_standard <- function(station_id, folder_path = "", input_type = "eca", 
                                          save_csv = FALSE, file_path = ".", file_name = NULL, 
                                          clean_formula_input = NULL) {
  # Check if clean formula is provided
  if (is.null(clean_formula_input)) {
    # Get the best model formula
    best_formula_table <- get_lowest_AICc_for_station("Outputs/Detection/GEV_Evaluation", station_id, input_type)
    best_formula <- best_formula_table$Formula
    
    # Print the best formula
    message("Best formula for station ", station_id, ": ", best_formula)
    
    # Clean the formula
    clean_formula <- gsub("^mod[0-9]+ <-\\s*", "", best_formula)
  } else {
    # Use the provided clean formula
    clean_formula <- clean_formula_input
    message("Using provided clean formula for station ", station_id)
  }
  
  # Prepare covariates
  cov_output <- prepare_station_covs(station_id, data, guide_data)
  q <- cov_output$dataset$Max_discharge
  cov1 <- cov_output$cov1
  cov2 <- cov_output$cov2
  cov3 <- cov_output$cov3
  cov4 <- cov_output$cov4
  cov5 <- cov_output$cov5
  cov6 <- cov_output$cov6
  cov7 <- cov_output$cov7
  
  # Fit the best model
  best_model <- eval(parse(text = clean_formula))
  
  # Extract parameters with covariates
  parameter_table_cov <- extract_gev_parameters_with_covariates(best_model, model_name = paste0("best_mod_", station_id))
  
  # Compute mu and sigma for all ECA values
  mu_sigma_table <- compute_all_mu_sigma_for_ecas(parameter_table_cov, cov_output)
  
  # Ensure only rows with positive Sigma_ECA are used
  valid_mu_sigma_table <- mu_sigma_table[mu_sigma_table$Sigma_ECA > 0, ]
  
  # Identify the min and max ECA values **ensuring Sigma_ECA is positive**
  eca_min_valid <- min(valid_mu_sigma_table$ECA, na.rm = TRUE)
  eca_max_valid <- max(valid_mu_sigma_table$ECA, na.rm = TRUE)
  
  # Extract corresponding Mu and Sigma for the identified ECA values
  mu_eca_min <- valid_mu_sigma_table$Mu_ECA[valid_mu_sigma_table$ECA == eca_min_valid]
  sigma_eca_min <- valid_mu_sigma_table$Sigma_ECA[valid_mu_sigma_table$ECA == eca_min_valid]
  
  mu_eca_max <- valid_mu_sigma_table$Mu_ECA[valid_mu_sigma_table$ECA == eca_max_valid]
  sigma_eca_max <- valid_mu_sigma_table$Sigma_ECA[valid_mu_sigma_table$ECA == eca_max_valid]
  
  # Extract shape parameter (should be constant)
  shape <- unique(parameter_table_cov$Value[parameter_table_cov$Parameter == "shape"])
  
  # Define return periods and exceedance probabilities
  rp <- c(1.5, 2, 5, 7, 10, 15, 20, 30, 35, 50, 100)
  ep <- 1 / rp
  
  # Compute quantiles for **valid** ECA min and max scenarios
  q_quant_eca_min <- qevd(1 - ep, loc = mean(mu_eca_min), scale = mean(sigma_eca_min), shape = shape, type = "GEV")
  q_quant_eca_max <- qevd(1 - ep, loc = mean(mu_eca_max), scale = mean(sigma_eca_max), shape = shape, type = "GEV")
  
  # Compute percentage increase in flood magnitude due to ECA
  eca_flood_risk_increase <- ((q_quant_eca_max - q_quant_eca_min) / q_quant_eca_min) * 100
  
  # Create results dataframe
  results <- data.frame(
    Station_ID = station_id,
    Return_Period = rp,
    Exceedance_Probability = ep,
    Valid_ECA_Min = eca_min_valid,
    Valid_ECA_Max = eca_max_valid,
    Discharge_ECA_Min = q_quant_eca_min,
    Discharge_ECA_Max = q_quant_eca_max,
    ECA_Flood_Risk_Increase_Percentage = eca_flood_risk_increase
  )
  
  # Save to CSV if requested
  if (save_csv) {
    if (is.null(file_name)) {
      file_name <- paste0("Return_Levels_Station_", station_id, ".csv")
    }
    
    full_file_path <- file.path(file_path, file_name)
    
    write.csv(results, file = full_file_path, row.names = FALSE)
    message("Results saved to ", full_file_path)
  }
  
  return(results)
}

# --------- ATTRIBUTION -----------------
#' Compute Mu, Sigma, and Shape for All ECA Values
#'
#' This function calculates mu and sigma for **all observed ECA values** across the study period.
#' It also includes the **shape parameter as a constant** in the final table.
#'
#' @param parameter_table A dataframe from `extract_gev_parameters_with_covariates()`
#' @param cov_output A list containing station-specific covariates and their values.
#'
#' @return A dataframe with:
#'   - `ECA`: Each observed ECA value from cov7.
#'   - `Mu_ECA`: Computed mu for each ECA.
#'   - `Sigma_ECA`: Computed sigma for each ECA.
#'   - `Shape`: Shape parameter (constant for all rows).
#'
#' @examples
#' result <- compute_all_mu_sigma_for_ecas(parameter_table_cov, cov_output)
#' print(result)
#'
#' @export
compute_all_mu_sigma_for_ecas <- function(parameter_table, cov_output, 
                                          save_csv = FALSE, file_name = "mu_sigma_table.csv", file_path = "./") {
  
  # Unique ECA values
  unique_eca_values <- unique(cov_output$cov7[!is.na(cov_output$cov7)])
  unique_years <- unique(cov_output$dataset$Year)
  
  # Safely compute averages only if covariates exist and are numeric
  safe_mean <- function(x) {
    if (is.null(x) || all(is.na(x))) return(NA_real_)
    return(mean(x, na.rm = TRUE))
  }
  
  avg_cov1 <- safe_mean(cov_output$cov1)
  avg_cov2 <- safe_mean(cov_output$cov2)
  avg_cov3 <- safe_mean(cov_output$cov3)
  avg_cov4 <- safe_mean(cov_output$cov4)
  avg_cov5 <- safe_mean(cov_output$cov5)
  avg_cov6 <- safe_mean(cov_output$cov6)
  
  shape_param <- parameter_table$Value[parameter_table$Parameter == "shape"]
  
  mu_sigma_table <- data.frame(Year = numeric(), ECA = numeric(), Mu_ECA = numeric(), 
                               Sigma_ECA = numeric(), Shape = numeric())
  
  for (i in seq_along(unique_eca_values)) {
    eca_value <- unique_eca_values[i]
    year_value <- ifelse(i <= length(unique_years), unique_years[i], NA)  # Safe year assignment
    
    mu_eca <- 0
    sigma_eca <- 0
    
    for (j in seq_len(nrow(parameter_table))) {
      param <- parameter_table$Parameter[j]
      covariate <- parameter_table$Covariate[j]
      value <- parameter_table$Value[j]
      
      if (grepl("^mu", param)) {
        if (is.na(covariate)) { 
          mu_eca <- mu_eca + value 
        } else if (covariate == "cov1" && !is.na(avg_cov1)) { 
          mu_eca <- mu_eca + value * avg_cov1 
        } else if (covariate == "cov2" && !is.na(avg_cov2)) { 
          mu_eca <- mu_eca + value * avg_cov2 
        } else if (covariate == "cov3" && !is.na(avg_cov3)) { 
          mu_eca <- mu_eca + value * avg_cov3 
        } else if (covariate == "cov4" && !is.na(avg_cov4)) { 
          mu_eca <- mu_eca + value * avg_cov4 
        } else if (covariate == "cov5" && !is.na(avg_cov5)) { 
          mu_eca <- mu_eca + value * avg_cov5 
        } else if (covariate == "cov6" && !is.na(avg_cov6)) { 
          mu_eca <- mu_eca + value * avg_cov6 
        } else if (covariate == "cov7") { 
          mu_eca <- mu_eca + value * eca_value 
        }
      }
      
      if (grepl("^sigma", param)) {
        if (is.na(covariate)) { 
          sigma_eca <- sigma_eca + value 
        } else if (covariate == "cov1" && !is.na(avg_cov1)) { 
          sigma_eca <- sigma_eca + value * avg_cov1 
        } else if (covariate == "cov2" && !is.na(avg_cov2)) { 
          sigma_eca <- sigma_eca + value * avg_cov2 
        } else if (covariate == "cov3" && !is.na(avg_cov3)) { 
          sigma_eca <- sigma_eca + value * avg_cov3 
        } else if (covariate == "cov4" && !is.na(avg_cov4)) { 
          sigma_eca <- sigma_eca + value * avg_cov4 
        } else if (covariate == "cov5" && !is.na(avg_cov5)) { 
          sigma_eca <- sigma_eca + value * avg_cov5 
        } else if (covariate == "cov6" && !is.na(avg_cov6)) { 
          sigma_eca <- sigma_eca + value * avg_cov6 
        } else if (covariate == "cov7") { 
          sigma_eca <- sigma_eca + value * eca_value 
        }
      }
    }
    
    mu_sigma_table <- rbind(mu_sigma_table, data.frame(
      Year = year_value,
      ECA = eca_value,
      Mu_ECA = mu_eca,
      Sigma_ECA = sigma_eca,
      Shape = shape_param
    ))
  }
  
  if (save_csv) {
    write.csv(mu_sigma_table, file = file.path(file_path, file_name), row.names = FALSE)
  }
  
  return(mu_sigma_table)
}


#' Compute Return Levels for Each ECA Value
#'
#' This function computes return levels for different return periods for each ECA value,
#' ensuring that corresponding Mu, Sigma, and Shape values are used **row-by-row**.
#'
#' @param mu_sigma_table A dataframe from `compute_all_mu_sigma_for_ecas()`
#'                       containing Mu, Sigma, and Shape values for each ECA.
#' @param return_periods A numeric vector specifying the return periods (default: common values).
#'
#' @return A dataframe where:
#'   - `ECA`: Each observed ECA value.
#'   - One column per return period, with return levels computed for that ECA.
#'
#' @examples
#' return_levels_table <- compute_return_levels(mu_sigma_table)
#' print(return_levels_table)
#'
#' @export
compute_return_levels <- function(mu_sigma_table, 
                                  return_periods = c(1.5, 2, 5, 7, 10, 15, 20, 30, 35, 50, 100),
                                  save_csv = FALSE, file_name = "return_levels.csv", file_path = "./") {
  
  # Remove rows where Sigma_ECA is negative
  mu_sigma_table <- mu_sigma_table[mu_sigma_table$Sigma_ECA > 0, ]
  
  ep <- 1 / return_periods  
  
  return_levels_table <- data.frame(Year = mu_sigma_table$Year, ECA = mu_sigma_table$ECA)
  
  for (i in seq_len(nrow(mu_sigma_table))) {
    row <- mu_sigma_table[i, ]
    
    return_levels <- sapply(ep, function(ep_val) {
      qevd(1 - ep_val, loc = row$Mu_ECA, scale = row$Sigma_ECA, shape = row$Shape, type = "GEV")
    })
    
    return_levels_table[i, 3:(length(return_periods) + 2)] <- return_levels
  }
  
  colnames(return_levels_table)[3:(length(return_periods) + 2)] <- paste0("Return_Level_", return_periods, "Yrs")
  
  if (save_csv) {
    write.csv(return_levels_table, file = file.path(file_path, file_name), row.names = FALSE)
  }
  
  return(return_levels_table)
}


#' Compute Return Levels from the Best-Fitted Model Without Fixing Climatic Covariates
#'
#' This function extracts return levels from the best-fitted model (`fevd` object) without 
#' fixing climatic covariates, allowing them to vary as they were modeled.
#'
#' @param best_model An `fevd` model object representing the best-fitted model.
#' @param return_periods A numeric vector specifying the return periods (e.g., c(1.5, 2, 5, ...)).
#'
#' @return A `return.level` object containing return levels for the specified return periods.
#'
#' @examples
#' return_periods <- c(1.5, 2, 5, 7, 10, 15, 20, 30, 35, 50, 100)
#' rl_best_model <- get_return_levels(best_mod02AD010, return_periods)
#' print(rl_best_model)
#'
#' @export
get_return_levels_best_model <- function(best_model, return_periods) {
  return.level(best_model, return.period = return_periods)
}


#' Comprehensive Trend Analysis Over Time (Using "Year" as Independent Variable)
#'
#' This function applies Pettitt's test (for change-point detection), Pearson correlation 
#' (for linear trend detection), Mann-Kendall (for monotonic trends), and Spearman correlation 
#' (for rank-based trends) to each return period column over time.
#'
#' @param return_level_table A dataframe containing `Year` and return level columns.
#' @param save_csv Logical, whether to save the results as a CSV file (default: FALSE).
#' @param file_name Name of the output CSV file if `save_csv` is TRUE.
#' @param file_path Path where the CSV file will be saved.
#' 
#' @return A dataframe summarizing trend analysis results for each return period.
#'
#' @examples
#' trend_results <- analyze_trends_comprehensive(return_level_table)
#' print(trend_results)
#'
#' @export
analyze_trends_comprehensive <- function(return_level_table, 
                                         save_csv = FALSE, 
                                         file_name = "trend_analysis.csv", 
                                         file_path = "./") {
  
  # Load necessary libraries
  if (!requireNamespace("trend", quietly = TRUE)) install.packages("trend")
  if (!requireNamespace("Kendall", quietly = TRUE)) install.packages("Kendall")
  if (!requireNamespace("stats", quietly = TRUE)) install.packages("stats")
  
  library(trend)     # Pettitt's Test
  library(Kendall)   # Mann-Kendall Test
  library(stats)     # Pearson & Spearman correlation
  
  # Ensure "Year" is present
  if (!"Year" %in% colnames(return_level_table)) {
    stop("Error: The dataframe must contain 'Year' as the independent variable.")
  }
  
  # Extract independent variable (Year) and ensure it's numeric
  independent_var <- as.numeric(return_level_table$Year)
  
  # Initialize results dataframe
  trend_results <- data.frame(
    Return_Period = character(),
    Pettitt_p = numeric(),
    Pearson_r = numeric(),
    Pearson_p = numeric(),
    MannKendall_tau = numeric(),
    MannKendall_p = numeric(),
    Spearman_rho = numeric(),
    Spearman_p = numeric(),
    Interpretation = character(),
    stringsAsFactors = FALSE
  )
  
  # Loop over each return period column (ignoring "Year")
  for (col_name in setdiff(colnames(return_level_table), "Year")) {
    
    return_levels <- return_level_table[[col_name]]
    
    # Ensure numeric return levels
    return_levels <- as.numeric(return_levels)
    
    # Remove NA values to avoid computation issues
    valid_idx <- !is.na(return_levels) & !is.na(independent_var)
    if (sum(valid_idx) < 5) {
      trend_results <- rbind(trend_results, data.frame(
        Return_Period = col_name,
        Pettitt_p = NA,
        Pearson_r = NA,
        Pearson_p = NA,
        MannKendall_tau = NA,
        MannKendall_p = NA,
        Spearman_rho = NA,
        Spearman_p = NA,
        Interpretation = "Not enough data for analysis.",
        stringsAsFactors = FALSE
      ))
      next
    }
    
    x <- independent_var[valid_idx]
    y <- return_levels[valid_idx]
    
    # Check if all values are the same (constant series)
    if (length(unique(y)) == 1) {
      trend_results <- rbind(trend_results, data.frame(
        Return_Period = col_name,
        Pettitt_p = NA,
        Pearson_r = 0,
        Pearson_p = 1,
        MannKendall_tau = 0,
        MannKendall_p = 1,
        Spearman_rho = 0,
        Spearman_p = 1,
        Interpretation = "No trend: constant values.",
        stringsAsFactors = FALSE
      ))
      next
    }
    
    # Perform trend tests
    pettitt_result <- pettitt.test(y)
    pearson_result <- cor.test(x, y, method = "pearson")
    mann_kendall_result <- MannKendall(y)
    spearman_result <- cor.test(x, y, method = "spearman")
    
    # Extract test results
    pettitt_p <- pettitt_result$p.value
    pearson_r <- pearson_result$estimate
    pearson_p <- pearson_result$p.value
    mann_kendall_tau <- mann_kendall_result$tau
    mann_kendall_p <- mann_kendall_result$sl
    spearman_rho <- spearman_result$estimate
    spearman_p <- spearman_result$p.value
    
    # Interpretation
    interpretation <- c()
    if (pettitt_p < 0.05) interpretation <- c(interpretation, "Abrupt change detected.")
    if (pearson_p < 0.05) interpretation <- c(interpretation, "Strong linear trend detected.")
    if (mann_kendall_p < 0.05) interpretation <- c(interpretation, "Consistent trend over time.")
    if (spearman_p < 0.05) interpretation <- c(interpretation, "Rank-based monotonic trend detected.")
    if (length(interpretation) == 0) interpretation <- "No significant trend detected."
    else interpretation <- paste(interpretation, collapse = " ")
    
    # Store results
    trend_results <- rbind(trend_results, data.frame(
      Return_Period = col_name,
      Pettitt_p = pettitt_p,
      Pearson_r = pearson_r,
      Pearson_p = pearson_p,
      MannKendall_tau = mann_kendall_tau,
      MannKendall_p = mann_kendall_p,
      Spearman_rho = spearman_rho,
      Spearman_p = spearman_p,
      Interpretation = interpretation,
      stringsAsFactors = FALSE
    ))
  }
  
  # Save as CSV if requested
  if (save_csv) {
    write.csv(trend_results, file = file.path(file_path, file_name), row.names = FALSE)
  }
  
  return(trend_results)
}

# this function get return levels with changing CLimatic covariates
save_return_levels <- function(station, data, guide_data, clean_formula,
                               standardized = TRUE, output_path, save = FALSE) {
  # Prepare covariates using standardized or non-standardized function
  if (standardized) {
    cov_output <- prepare_station_covs_standardized(station, data, guide_data)
  } else {
    cov_output <- prepare_station_covs(station, data, guide_data)
  }
  
  # Extract required variables
  q <- cov_output$dataset$Max_discharge
  cov1 <- cov_output$cov1
  cov2 <- cov_output$cov2
  cov3 <- cov_output$cov3
  cov4 <- cov_output$cov4
  cov5 <- cov_output$cov5
  cov6 <- cov_output$cov6
  cov7 <- cov_output$cov7
  year <- cov_output$dataset$Year
  
  # Fit model
  best_model <- eval(parse(text = clean_formula))
  
  # Define return periods
  return_periods <- c(1.5, 2, 5, 7, 10, 15, 20, 30, 35, 50, 100)
  
  # Create an empty dataframe to store results
  result_df <- data.frame(Year = as.numeric(year))  # Ensure 'Year' is numeric
  
  # Compute return levels for each year and return period
  for (rp in return_periods) {
    result_df[[paste0("Return_Level_", rp)]] <- as.numeric(return.level(best_model, rp))  # Ensure numeric
  }
  
  # Add station and formula information
  result_df$Station <- as.character(station)  # Ensure station is stored as character
  result_df$Formula <- as.character(clean_formula)  # Ensure formula is stored as character
  
  # Define filename
  filename <- paste0(output_path, "/Return_Levels_Without_Isolating_ECA_Station_", station, ".xlsx")
  
  # Save to Excel if save is TRUE
  if (save) {
    write.xlsx(result_df, filename, rowNames = FALSE)  # Fixed row.names warning
  }
  
  return(result_df)
}


# COUNTER FACTUAL ATTRIBUTIONS ----------------
# here we put functions for making the counter factural scenarios.

#' Compute Mu, Sigma, and Shape for Fixed ECA Value Over Time
#'
#' This function calculates mu and sigma for a **fixed ECA value** across all years,
#' using real covariate values (`cov1` to `cov5`) for each year if available, and outputs these covariates.
#'
#' @param parameter_table A dataframe from `extract_gev_parameters_with_covariates()`
#' @param cov_output A list containing station-specific covariates and their values.
#' @param fixed_eca_value The ECA value (cov7) to fix across all years.
#' @param save_csv Logical. If TRUE, saves output as CSV.
#' @param file_name Name of the CSV file (default "mu_sigma_table_fixed_eca.csv").
#' @param file_path Path to save the CSV file (default "./").
#'
#' @return A dataframe with:
#'   - `Year`: Year from the dataset.
#'   - `ECA`: Fixed ECA value repeated.
#'   - `cov1`, `cov2`, ..., `cov5`: Real covariate values (if available).
#'   - `Mu_ECA`: Computed mu for each year.
#'   - `Sigma_ECA`: Computed sigma for each year.
#'   - `Shape`: Shape parameter (constant for all rows).
#'
#' @examples
#' result <- compute_mu_sigma_for_fixed_eca(parameter_table_cov, cov_output, fixed_eca_value = 0.5)
#' print(result)
#'
#' @export
compute_mu_sigma_for_fixed_eca <- function(parameter_table, cov_output, fixed_eca_value, 
                                           save_csv = FALSE, file_name = "mu_sigma_table_fixed_eca.csv", file_path = "./") {
  years <- cov_output$dataset$Year
  covariates <- list(
    cov1 = if (!is.null(cov_output$cov1)) cov_output$cov1 else NULL,
    cov2 = if (!is.null(cov_output$cov2)) cov_output$cov2 else NULL,
    cov3 = if (!is.null(cov_output$cov3)) cov_output$cov3 else NULL,
    cov4 = if (!is.null(cov_output$cov4)) cov_output$cov4 else NULL,
    cov5 = if (!is.null(cov_output$cov5)) cov_output$cov5 else NULL
  )
  
  shape_param <- parameter_table$Value[parameter_table$Parameter == "shape"]
  
  mu_sigma_table <- data.frame(Year = years, ECA = fixed_eca_value)
  
  for (cov_name in names(covariates)) {
    if (!is.null(covariates[[cov_name]])) {
      mu_sigma_table[[cov_name]] <- covariates[[cov_name]]
    }
  }
  
  mu_sigma_table$Mu_ECA <- numeric(length(years))
  mu_sigma_table$Sigma_ECA <- numeric(length(years))
  mu_sigma_table$Shape <- shape_param
  
  for (i in seq_along(years)) {
    mu_eca <- 0
    sigma_eca <- 0
    
    for (j in seq_len(nrow(parameter_table))) {
      param <- parameter_table$Parameter[j]
      covariate <- parameter_table$Covariate[j]
      value <- parameter_table$Value[j]
      
      if (grepl("^mu", param)) {
        if (is.na(covariate)) {
          mu_eca <- mu_eca + value
        } else if (covariate %in% names(covariates) && !is.null(covariates[[covariate]])) {
          mu_eca <- mu_eca + value * covariates[[covariate]][i]
        } else if (covariate == "cov7") {
          mu_eca <- mu_eca + value * fixed_eca_value
        }
      }
      
      if (grepl("^sigma", param)) {
        if (is.na(covariate)) {
          sigma_eca <- sigma_eca + value
        } else if (covariate %in% names(covariates) && !is.null(covariates[[covariate]])) {
          sigma_eca <- sigma_eca + value * covariates[[covariate]][i]
        } else if (covariate == "cov7") {
          sigma_eca <- sigma_eca + value * fixed_eca_value
        }
      }
    }
    
    mu_sigma_table$Mu_ECA[i] <- mu_eca
    mu_sigma_table$Sigma_ECA[i] <- sigma_eca
  }
  
  if (save_csv) {
    write.csv(mu_sigma_table, file = file.path(file_path, file_name), row.names = FALSE)
  }
  
  return(mu_sigma_table)
}


# UNCERTAINTY ANALYSIS -------------

#' Get Mu, Sigma, Shape for Median ECA
#'
#' This function reads the Mu-Sigma table for a station, identifies the row corresponding to
#' the median ECA value, and extracts the corresponding Mu, Sigma, and Shape parameters.
#'
#' @param station_id String. Station ID (e.g., "02AB006").
#' @param mu_sigma_folder_path String. Path to the folder containing Mu-Sigma tables. Default is the thesis outputs folder.
#'
#' @return A list with three elements:
#'   - mu: location parameter (Mu) at median ECA
#'   - sigma: scale parameter (Sigma) at median ECA
#'   - shape: shape parameter (xi) (usually constant)
#'
#' @examples
#' get_mu_sigma_shape_for_median_eca("02AB006")

get_mu_sigma_shape_for_median_eca <- function(station_id, 
                                              mu_sigma_folder_path = "/Users/elhamashrafizadeh/Library/CloudStorage/OneDrive-UniversityofToronto/MSc_Thesis/Processing/R Processing/Final_Detection_Attribution_Quantification/Outputs/Attribution/Mu_Sigma_Tables") {
  
  library(readr)
  library(dplyr)
  
  # Construct file path
  file_path <- file.path(mu_sigma_folder_path, paste0("Complete_mu_sigma_table_", station_id, ".csv"))
  
  if (!file.exists(file_path)) {
    stop("Mu-Sigma file not found for station: ", station_id)
  }
  
  # Read table
  mu_sigma_table <- read_csv(file_path, show_col_types = FALSE)
  
  # Find the median ECA value
  median_eca_value <- median(mu_sigma_table$ECA, na.rm = TRUE)
  
  # Find the row closest to the median ECA
  row_median <- mu_sigma_table %>%
    mutate(diff = abs(ECA - median_eca_value)) %>%
    arrange(diff) %>%
    slice(1)
  
  # Extract parameters
  result <- list(
    mu = row_median$Mu_ECA,
    sigma = row_median$Sigma_ECA,
    shape = row_median$Shape
  )
  
  return(result)
}


#' Simulate Random Flood Discharges from GEV Parameters
#' 
#' This function simulates random discharge (q) values from a specified GEV distribution
#' given mu (location), sigma (scale), and shape parameters.
#' 
#' @param n Integer. Number of random samples to generate.
#' @param mu Numeric. Location parameter of the GEV.
#' @param sigma Numeric. Scale parameter of the GEV.
#' @param shape Numeric. Shape parameter of the GEV.
#' 
#' @return A numeric vector of simulated discharge values.
#' 
#' @examples
#' simulate_q_from_gev_parameters(n = 100, mu = 186.3, sigma = 24.4, shape = -0.28)

simulate_q_from_gev_parameters <- function(n, mu, sigma, shape) {
  
  library(extRemes)
  
  # Simulate using revd() function from extRemes
  simulated_q <- revd(n = n, loc = mu, scale = sigma, shape = shape, type = "GEV")
  
  return(simulated_q)
}


#' Quantification at ECA Min and Max (Given Model and Data)
#'
#' This function calculates return levels and percentage increases between
#' minimum and maximum ECA scenarios based on a fitted GEV model and
#' manually supplied covariates and q.
#'
#' @param fitted_model Fitted fevd GEV model object (on resampled q).
#' @param q Numeric vector of discharge values.
#' @param cov1 to cov7 Numeric vectors of covariates (same length as q).
#' @param return_periods Vector of return periods. Default = c(1.5, 2, 5, 7, 10, 15, 20, 30, 35, 50, 100).
#'
#' @return A data.frame containing return levels at ECA min, ECA max, and percentage increase.
#'
#' @export
rp_quantification_min_max_eca_custom <- function(q, fitted_model, cov_output_custom, station_id, return_periods) {
  
  # Step 1: Extract GEV parameters with covariates
  parameter_table_cov <- extract_gev_parameters_with_covariates(
    model = fitted_model,
    model_name = paste0("best_mod_", station_id)
  )
  
  if (is.null(parameter_table_cov) || nrow(parameter_table_cov) == 0) {
    stop("Error: Parameter table is empty or extraction failed.")
  }
  
  # Step 2: Compute Mu and Sigma for all ECA values using the custom covariates
  mu_sigma_table <- compute_all_mu_sigma_for_ecas(
    parameter_table = parameter_table_cov,
    cov_output = cov_output_custom
  )
  
  if (is.null(mu_sigma_table) || nrow(mu_sigma_table) == 0) {
    stop("Error: Mu/Sigma table is empty.")
  }
  
  # Step 3: Keep only rows where Sigma is positive
  valid_mu_sigma_table <- mu_sigma_table[mu_sigma_table$Sigma_ECA > 0, ]
  
  if (nrow(valid_mu_sigma_table) == 0) {
    stop("Error: No valid Mu/Sigma rows with positive Sigma.")
  }
  
  # Step 4: Find valid minimum and maximum ECA
  eca_min_valid <- min(valid_mu_sigma_table$ECA, na.rm = TRUE)
  eca_max_valid <- max(valid_mu_sigma_table$ECA, na.rm = TRUE)
  
  # Step 5: Find corresponding Mu and Sigma
  mu_eca_min <- valid_mu_sigma_table$Mu_ECA[valid_mu_sigma_table$ECA == eca_min_valid]
  sigma_eca_min <- valid_mu_sigma_table$Sigma_ECA[valid_mu_sigma_table$ECA == eca_min_valid]
  
  mu_eca_max <- valid_mu_sigma_table$Mu_ECA[valid_mu_sigma_table$ECA == eca_max_valid]
  sigma_eca_max <- valid_mu_sigma_table$Sigma_ECA[valid_mu_sigma_table$ECA == eca_max_valid]
  
  # Step 6: Extract Shape (constant)
  shape <- unique(parameter_table_cov$Value[parameter_table_cov$Parameter == "shape"])
  
  # Step 7: Compute quantiles at specified return periods
  rp <- return_periods
  ep <- 1 / rp
  
  q_quant_eca_min <- qevd(1 - ep, loc = mean(mu_eca_min), scale = mean(sigma_eca_min), shape = shape, type = "GEV")
  q_quant_eca_max <- qevd(1 - ep, loc = mean(mu_eca_max), scale = mean(sigma_eca_max), shape = shape, type = "GEV")
  
  # Step 8: Compute percentage increase
  eca_flood_risk_increase <- ((q_quant_eca_max - q_quant_eca_min) / q_quant_eca_min) * 100
  
  # Step 9: Create output dataframe
  results <- data.frame(
    Station_ID = station_id,
    Return_Period = rp,
    Exceedance_Probability = ep,
    Valid_ECA_Min = eca_min_valid,
    Valid_ECA_Max = eca_max_valid,
    Discharge_ECA_Min = q_quant_eca_min,
    Discharge_ECA_Max = q_quant_eca_max,
    ECA_Flood_Risk_Increase_Percentage = eca_flood_risk_increase
  )
  
  return(results)
}



plot_flood_risk_distribution_log <- function(
    station,
    uncertainty_results,
    quantification_dir = "/Users/elhamashrafizadeh/Library/CloudStorage/OneDrive-UniversityofToronto/MSc_Thesis/Processing/R Processing/Final_Detection_Attribution_Quantification/Outputs/Quantification",
    save_path = NULL,
    save_prefix = NULL,
    box_color = "#1a3c6b",
    font_family = "Times New Roman",
    base_size = 18,
    max_return_period = 50  # only plot up to this RP
) {
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(tidyr)
  
  # Step 1: Get return periods and reshape bootstrap results
  rp <- uncertainty_results$return_periods
  mat <- as.data.frame(uncertainty_results$quantification_mat)
  colnames(mat) <- paste0(rp, "Yr")
  mat$Sample <- 1:nrow(mat)
  
  long_mat <- mat %>%
    pivot_longer(-Sample, names_to = "Return_Period_Label", values_to = "Percent_Increase") %>%
    mutate(Return_Period = as.numeric(gsub("Yr", "", Return_Period_Label))) %>%
    filter(Return_Period <= max_return_period)
  
  # Step 2: Dynamic range without artificial padding
  y_values <- long_mat$Percent_Increase
  y_min_raw <- min(y_values[y_values > 0], na.rm = TRUE)
  y_max_raw <- max(y_values, na.rm = TRUE)
  
  pretty_ticks <- c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000)
  y_min <- max(pretty_ticks[pretty_ticks <= y_min_raw], 0.1)
  y_max <- min(pretty_ticks[pretty_ticks >= y_max_raw], 1000)
  
  visible_ticks <- pretty_ticks[pretty_ticks >= y_min & pretty_ticks <= y_max]
  
  # Step 3: Plot boxplot only (no scatter)
  p <- ggplot(long_mat, aes(x = factor(Return_Period), y = Percent_Increase)) +
    geom_boxplot(fill = box_color, alpha = 0.3, color = box_color, outlier.shape = NA) +
    scale_y_log10(
      limits = c(y_min, y_max),
      breaks = visible_ticks,
      labels = function(x) paste0(x, "%"),
      expand = c(0, 0)
    ) +
    labs(
      title = paste("", station),
      x = "Return Period (years)",
      y = "Hazard Probability (Increased %)"
    ) +
    theme_minimal(base_family = font_family, base_size = base_size)
  
  # Step 4: Save if needed
  if (!is.null(save_path) && !is.null(save_prefix)) {
    ggsave(file.path(save_path, paste0(save_prefix, "_Flood_Risk_Boxplot_LogScale.png")),
           plot = p, dpi = 400, width = 9, height = 6)
  }
  
  return(p)
}

plot_flood_risk_distribution_log_negative <- function(
    station,
    uncertainty_results,
    quantification_dir = "/Users/elhamashrafizadeh/Library/CloudStorage/OneDrive-UniversityofToronto/MSc_Thesis/Processing/R Processing/Final_Detection_Attribution_Quantification/Outputs/Quantification",
    save_path = NULL,
    save_prefix = NULL,
    box_color = "#1a3c6b",
    font_family = "Times New Roman",
    base_size = 18,
    max_return_period = 50  # only plot up to this RP
) {
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(tidyr)
  
  # Step 1: Get return periods and reshape bootstrap results
  rp <- uncertainty_results$return_periods
  mat <- as.data.frame(uncertainty_results$quantification_mat)
  colnames(mat) <- paste0(rp, "Yr")
  mat$Sample <- 1:nrow(mat)
  
  long_mat <- mat %>%
    pivot_longer(-Sample, names_to = "Return_Period_Label", values_to = "Percent_Increase") %>%
    mutate(Return_Period = as.numeric(gsub("Yr", "", Return_Period_Label))) %>%
    filter(Return_Period <= max_return_period)
  
  # Step 2: Plot boxplot with linear scale (supports negative values)
  p <- ggplot(long_mat, aes(x = factor(Return_Period), y = Percent_Increase)) +
    geom_boxplot(fill = box_color, alpha = 0.3, color = box_color, outlier.shape = NA) +
    scale_y_continuous(  # linear scale to allow negative values
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = paste("", station),
      x = "Return Period (years)",
      y = "Hazard Probability (Change %)"
    ) +
    theme_minimal(base_family = font_family, base_size = base_size)
  
  # Step 3: Save if needed
  if (!is.null(save_path) && !is.null(save_prefix)) {
    ggsave(file.path(save_path, paste0(save_prefix, "_Flood_Risk_Boxplot_LinearScale_WithNegatives.png")),
           plot = p, dpi = 400, width = 9, height = 6)
  }
  
  return(p)
}
