
# PACKAGES
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(purrr)

# TREND PLOTTING ---------------------------
# LOADING THE DATA

directory <- '/Users/elhamashrafizadeh/Library/CloudStorage/OneDrive-UniversityofToronto/MSc_Thesis/Processing/R Processing/Final_Detection_Attribution_Quantification/Outputs/Attribution/Return_Levels_Fixed_Climatic_Changing_ECA'

stations <- c(
  "02AB006",
  "02AC002",
  "02AD010",
  "02CC005",
  "02CE002",
  "02JC008",
  "04JD005",
  "04LM001",
  "04MF001",
  "05QD016",
  "05QE009"
)

# STATION LABELS
station_labels <- tibble(
  Station = c(
    "02AB006", "02AC002", "02AD010", "02CC005", "02CE002",
    "02JC008", "04JD005", "04LM001", "04MF001", "05QD016", "05QE009"
  ),
  Station_Label = c(
    "D1", "D2", "D3", "D4", "D5",
    "D6", "D7", "D8", "D9", "D10", "D11"
  )
)

all_data <- data.frame()
for (station in stations) {
  
  file_name <- paste0("Complete_Return_Levels_table_", station, ".csv")
  file_path <- file.path(directory, file_name)
  
  df <- read_csv(file_path, show_col_types = FALSE)
  
  df_subset <- df[, c("Year", "Return_Level_5Yrs", "Return_Level_20Yrs", "Return_Level_35Yrs")]
  
  df_subset$Station <- station
  
  all_data <- rbind(all_data, df_subset)
  
}

plot_data <- pivot_longer(
  all_data,
  cols = c("Return_Level_5Yrs", "Return_Level_20Yrs", "Return_Level_35Yrs"),
  names_to = "Return_Period",
  values_to = "Discharge"
)

plot_data$Return_Period[plot_data$Return_Period == "Return_Level_5Yrs"] <- "5-Year"
plot_data$Return_Period[plot_data$Return_Period == "Return_Level_20Yrs"] <- "20-Year"
plot_data$Return_Period[plot_data$Return_Period == "Return_Level_35Yrs"] <- "35-Year"
station_labels <- data.frame(
  Station = stations,
  Label = paste0("D", seq_along(stations))
)

plot_data <- merge(plot_data, station_labels, by = "Station")

# NOW PLOTTING BEGINS

plot_data$Label <- factor(
  plot_data$Label,
  levels = paste0("D", 1:11)
)
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

ggplot(plot_data, aes(x = Year, y = Discharge, color = Return_Period)) +
  
  geom_line(linewidth = 1) +
  
  facet_wrap(~Label, ncol = 4, scales = "free_y") +
  
  scale_color_manual(values = c(
    "5-Year" = "#102542",
    "20-Year" = "#b4b8c1",
    "35-Year" = "#E69F00"
  )) +
  
  labs(
    x = "Year",
    y = expression(Discharge~(m^3/s)),
    color = "Return Period"
  ) +
  
  theme_classic(base_family = "Times New Roman", base_size = 16) +
  
  theme(
    strip.text = element_text(face = "bold"),
    #axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

# QUANTIFICATION PLOTS -----------------


# 1. adding the change in frequencies as well!
add_frequency_change <- function(df){
  
  # interpolate log(return period) as function of discharge
  future_curve <- approxfun(
    x = df$Discharge_ECA_Max,
    y = log(df$Return_Period),
    rule = 2
  )
  
  # find new return period
  df$New_Return_Period <- exp(future_curve(df$Discharge_ECA_Min))
  
  # frequency amplification factor
  df$Frequency_Increase <- df$Return_Period / df$New_Return_Period
  
  return(df)
}

directory <- '/Users/elhamashrafizadeh/Library/CloudStorage/OneDrive-UniversityofToronto/MSc_Thesis/Processing/R Processing/Final_Detection_Attribution_Quantification/Outputs/Quantification'

stations <- c(
  "02AB006",
  "02AC002",
  "02AD010",
  "02CC005",
  "02CE002",
  "02JC008",
  "04JD005",
  "04LM001",
  "04MF001",
  "05QD016",
  "05QE009"
)

# STATION LABELS
station_labels <- tibble(
  Station = c(
    "02AB006", "02AC002", "02AD010", "02CC005", "02CE002",
    "02JC008", "04JD005", "04LM001", "04MF001", "05QD016", "05QE009"
  ),
  Station_Label = c(
    "D1", "D2", "D3", "D4", "D5",
    "D6", "D7", "D8", "D9", "D10", "D11"
  )
)


# 5. EMPTY OBJECT TO STORE ALL RESULTS
all_results <- data.frame()

# 6. LOOP OVER STATIONS
for (station in stations) {
  
  file_name <- paste0("Quantification_Results_For_Min_Max_ECA_Scenrios_", station, ".csv")
  file_path <- file.path(directory, file_name)
  
  df <- read_csv(file_path, show_col_types = FALSE)
  
  df <- add_frequency_change(df)
  
  station_label <- station_labels$Station_Label[station_labels$Station == station]
  df$Station_Label <- station_label
  
  all_results <- rbind(all_results, df)
}

# write_csv(
#   all_results,
#   file.path(directory, "All_Stations_Frequency_Change_Table.csv")
# )

changes <- all_results[, c(
  "Station_ID",
  "Station_Label",
  "Return_Period",
  "ECA_Flood_Risk_Increase_Percentage",
  "Frequency_Increase"
)]

changes <- changes[!changes$Return_Period %in% c(1.5,2, 50, 100),]

ffc_long <- pivot_longer(
  all_results,
  cols = c("Discharge_ECA_Min", "Discharge_ECA_Max"),
  names_to = "Scenario",
  values_to = "Discharge"
)

# clean scenario names
ffc_long$Scenario[ffc_long$Scenario == "Discharge_ECA_Min"] <- "Min ECA"
ffc_long$Scenario[ffc_long$Scenario == "Discharge_ECA_Max"] <- "Max ECA"

# fix order
ffc_long$Scenario <- factor(
  ffc_long$Scenario,
  levels = c("Min ECA", "Max ECA")
)

# fix D order if needed
ffc_long$Station_Label <- factor(
  ffc_long$Station_Label,
  levels = paste0("D", 1:11)
)

ffc_long <- ffc_long[!ffc_long$Return_Period %in% c(1.5, 50, 100),]

# PLOTTING
ggplot(ffc_long, aes(x = Return_Period, y = Discharge, color = Scenario)) +
  
  geom_line(linewidth = 1) +
  geom_point(size = 1.2) +
  
  facet_wrap(~ Station_Label, ncol = 3, scales = "free_y") +
  
  # scale_x_log10(
  #   breaks = c(2, 5, 7, 10, 15, 20, 30, 35),
  #   labels = c(2, 5, 7, 10, 15, 20, 30, 35)
  # ) +
  scale_x_continuous(
    breaks = c(2, 5, 7, 10, 15, 20, 30, 35),
    labels = c(2, 5, 7, 10, 15, 20, 30, 35)
  ) +
  
  scale_color_manual(values = c(
    "Min ECA" = "#102542",
    "Max ECA" = "#f87060"
  )) +
  
  labs(
    x = "Return Period (years)",
    y = expression(Discharge~(m^3/s)),
    color = NULL
  ) +
  
  theme_classic(base_family = "Times New Roman", base_size = 15) +
  
  theme(
    strip.text = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    legend.position = "bottom",
    
    # grid lines
    # panel.grid.major = element_line(color = "grey80", linewidth = 0.4),
    # panel.grid.minor = element_line(color = "grey90", linewidth = 0.2)
  ) 

ggplot(ffc_long, aes(x = Return_Period, y = Discharge, color = Scenario)) +
  
  geom_line(linewidth = 1) +
  geom_point(size = 1.2) +
  
  facet_wrap(~ Station_Label, ncol = 5, scales = "free_y") +
  
  # scale_x_log10(
  #   breaks = c(2, 5, 7, 10, 15, 20, 30, 35),
  #   labels = c(2, 5, 7, 10, 15, 20, 30, 35)
  # ) +
  scale_x_continuous(
    breaks = c(2, 5, 7, 10, 15, 20, 30, 35),
    labels = c(2, 5, 7, 10, 15, 20, 30, 35)
  ) +
  
  scale_color_manual(values = c(
    "Min ECA" = "#102542",
    "Max ECA" = "#f87060"
  )) +
  
  labs(
    x = "Return Period (years)",
    y = expression(Discharge~(m^3/s)),
    color = NULL
  ) +
  
  theme_classic(base_family = "Times New Roman", base_size = 15) +
  
  theme(
    strip.text = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    legend.position = "bottom",
    
    # grid lines
    # panel.grid.major = element_line(color = "grey80", linewidth = 0.4),
    # panel.grid.minor = element_line(color = "grey90", linewidth = 0.2)
  ) 
# /////////////////////////////

ggplot(changes, aes(x = factor(Return_Period), y = ECA_Flood_Risk_Increase_Percentage)) +
  
  geom_boxplot(fill = "grey85", color = "black", linewidth = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.12, height = 0, size = 1.6, alpha = 0.8) +
  
  geom_hline(
    yintercept = 0,
    color = "grey50",
    linewidth = 0.4,
    linetype = "dashed"
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0.1, 0.1))
  ) +
  
  labs(
    x = "Return Period (years)",
    y = "Change in magnitude (%)"
  ) +
  
  theme_classic(base_family = "Times New Roman", base_size = 16) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 10)
  )

ggplot(changes, aes(x = factor(Return_Period), y = Frequency_Increase)) +
  
  geom_boxplot(fill = "grey85", color = "black", linewidth = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.12, height = 0, size = 1.6, alpha = 0.8) +
  
  geom_hline(
    yintercept = 1,
    color = "grey50",
    linewidth = 0.4,
    linetype = "dashed"
  ) +
  
  scale_y_continuous(
    limits = c(0, NA),
    breaks = scales::pretty_breaks(n = 10),
    expand = c(0, 0)
  ) +
  
  labs(
    x = "Return Period (years)",
    y = "Increase in frequency (times)"
  ) +
  
  theme_classic(base_family = "Times New Roman", base_size = 16) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  ) +
  scale_y_continuous(limits = c(0, 25))



ggplot(changes, aes(x = factor(Return_Period), y = ECA_Flood_Risk_Increase_Percentage)) +
  
  stat_summary(
    fun = median,
    geom = "point",
    size = 3
  ) +
  
  stat_summary(
    fun.min = min,
    fun.max = max,
    geom = "errorbar",
    width = 0.25,
    linewidth = 0.6
  ) +
  
  # geom_jitter(width = 0.12, height = 0, size = 1.5, alpha = 0.6) +
  # 
  # geom_hline(
  #   yintercept = 0,
  #   color = "grey50",
  #   linewidth = 0.4,
  #   linetype = "dashed"
  # ) +
  
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 10),
    expand = expansion(mult = c(0.1, 0.1))
  ) +
  
  labs(
    x = "Return Period (years)",
    y = "Change in magnitude (%)"
  ) +
  
  theme_classic(base_family = "Times New Roman", base_size = 16) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  ) + geom_hline(
    yintercept = 0,
    color = "grey50",
    linewidth = 0.4,
    linetype = "dashed"
  ) 

ggplot(changes, aes(x = factor(Return_Period), y = Frequency_Increase)) +
  
  stat_summary(
    fun = median,
    geom = "point",
    size = 3
  ) +
  
  stat_summary(
    fun.min = min,
    fun.max = max,
    geom = "errorbar",
    width = 0.25,
    linewidth = 0.6
  ) +
  
  # geom_jitter(width = 0.12, height = 0, size = 1.5, alpha = 0.6) +
  # 
  # geom_hline(
  #   yintercept = 0,
  #   color = "grey50",
  #   linewidth = 0.4,
  #   linetype = "dashed"
  # ) +
  
  scale_y_continuous(
    limits = c(0, NA),
    breaks = scales::pretty_breaks(n = 10)
  ) +
  
  labs(
    x = "Return Period (years)",
    y = "Increase in frequency (times)"
  ) +
  
  theme_classic(base_family = "Times New Roman", base_size = 16) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  ) +
 geom_hline(
    yintercept = 1,
    color = "grey50",
    linewidth = 0.4,
    linetype = "dashed"
  ) 

# scale factor to align the two variables
scale_factor <- max(changes$ECA_Flood_Risk_Increase_Percentage, na.rm = TRUE) /
  max(changes$Frequency_Increase, na.rm = TRUE)

# scale factor
scale_factor <- max(changes$ECA_Flood_Risk_Increase_Percentage, na.rm = TRUE) /
  max(changes$Frequency_Increase, na.rm = TRUE)

offset <- 0.18   # controls spacing between the two summaries

ggplot() +
  
  # Magnitude change
  stat_summary(
    data = changes,
    aes(
      x = as.numeric(factor(Return_Period)) - offset,
      y = ECA_Flood_Risk_Increase_Percentage
    ),
    fun = median,
    geom = "point",
    size = 3,
    color = "black"
  ) +
  
  stat_summary(
    data = changes,
    aes(
      x = as.numeric(factor(Return_Period)) - offset,
      y = ECA_Flood_Risk_Increase_Percentage
    ),
    fun.min = min,
    fun.max = max,
    geom = "errorbar",
    width = 0.2,
    linewidth = 0.6,
    color = "black"
  ) +
  
  # Frequency increase
  stat_summary(
    data = changes,
    aes(
      x = as.numeric(factor(Return_Period)) + offset,
      y = Frequency_Increase * scale_factor
    ),
    fun = median,
    geom = "point",
    size = 3,
    color = "#b22222"
  ) +
  
  stat_summary(
    data = changes,
    aes(
      x = as.numeric(factor(Return_Period)) + offset,
      y = Frequency_Increase * scale_factor
    ),
    fun.min = min,
    fun.max = max,
    geom = "errorbar",
    width = 0.2,
    linewidth = 0.6,
    color = "#b22222"
  ) +
  
  scale_x_continuous(
    breaks = seq_along(unique(changes$Return_Period)),
    labels = sort(unique(changes$Return_Period))
  ) +
  
  scale_y_continuous(
    name = "Changes in Flood Magnitude (%)",
    breaks = scales::pretty_breaks(n = 10),
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "Changes in Flood Frequency (times)"
    )
  ) +
  
  geom_hline(
    yintercept = 0,
    color = "grey50",
    linewidth = 0.4,
    linetype = "dashed"
  ) +
  
  theme_classic(base_family = "Times New Roman", base_size = 16) +
  theme(
    axis.title.y = element_text(face = "bold", color = "black"),
    axis.title.y.right = element_text(face = "bold", color = "#b22222", angle = 90),
    axis.text = element_text(color = "black")
  ) +
  
  labs(x = "Return Period (years)")

# ALL TOGETHER ----
# 1. keep only the return periods you want to annotate
annot_data <- all_results[all_results$Return_Period %in% c(5, 20, 35), ]

# 2. make sure station label order is correct
ffc_long$Station_Label <- factor(ffc_long$Station_Label, levels = paste0("D", 1:11))
annot_data$Station_Label <- factor(annot_data$Station_Label, levels = paste0("D", 1:11))

# 3. create text labels
annot_data$label <- sprintf(
  "%+.0f%%\n%.1fx",
  annot_data$ECA_Flood_Risk_Increase_Percentage,
  annot_data$Frequency_Increase
)

# 4. create a small y-offset for labels so they sit above the Max ECA line
station_max <- aggregate(Discharge_ECA_Max ~ Station_Label, data = all_results, max)
station_min <- aggregate(Discharge_ECA_Max ~ Station_Label, data = all_results, min)

station_span <- merge(station_max, station_min, by = "Station_Label")
station_span$offset <- 0.08 * (station_span$Discharge_ECA_Max.x - station_span$Discharge_ECA_Max.y)

annot_data <- merge(
  annot_data,
  station_span[, c("Station_Label", "offset")],
  by = "Station_Label"
)

annot_data$y_lab <- annot_data$Discharge_ECA_Max + annot_data$offset

ggplot(ffc_long, aes(x = Return_Period, y = Discharge, color = Scenario)) +
  
  geom_line(linewidth = 1) +
  geom_point(size = 1.2) +
  
  geom_text(
    data = annot_data,
    aes(x = Return_Period, y = y_lab, label = label),
    inherit.aes = FALSE,
    size = 4.5,
    family = "Times New Roman",
    color = "black",
    lineheight = 0.9
  ) +
  
  facet_wrap(~ Station_Label, ncol = 3, scales = "free_y") +
  
  scale_x_log10(
    breaks = c(2, 5, 7, 10, 15, 20, 30, 35),
    labels = c(2, 5, 7, 10, 15, 20, 30, 35)
  ) +
  
  scale_color_manual(values = c(
    "Min ECA" = "#102542",
    "Max ECA" = "#f87060"
  )) +
  
  labs(
    x = "Return Period (years)",
    y = expression(Discharge~(m^3/s)),
    color = NULL
  ) +
  
  theme_classic(base_family = "Times New Roman", base_size = 16) +
  theme(
    strip.text = element_text(face = "bold", margin = margin(t = 1, b = 1)),
    strip.background = element_blank(),
    axis.text = element_text(color = "black"),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey80", linewidth = 0.4),
    panel.grid.minor = element_line(color = "grey90", linewidth = 0.2),
    panel.spacing = grid::unit(1.1, "lines")
  )

# RELATIVE CHANGES --------

# colors
station_colors <- c(
  "Baseline" = "black",
  "D1"  = "#8a8a8a",
  "D2"  = "#969696",
  "D3"  = "#a2a2a2",
  "D4"  = "#aeaeae",
  "D5"  = "#bababa",
  "D6"  = "#c6c6c6",
  "D7"  = "#d2d2d2",
  "D8"  = "#4774a1",
  "D9"  = "#d9d9d9",
  "D10" = "#ee8577",
  "D11" = "#e0e0e0"
)

# keep needed columns
relative_df <- all_results[, c(
  "Station_Label",
  "Return_Period",
  "Discharge_ECA_Min",
  "Discharge_ECA_Max"
)]

# remove unwanted return periods
relative_df <- relative_df[!relative_df$Return_Period %in% c(1.5, 50, 100), ]

# compute relative change from Min ECA to Max ECA
relative_df$change_ratio <- (
  relative_df$Discharge_ECA_Max - relative_df$Discharge_ECA_Min
) / relative_df$Discharge_ECA_Min

# extract D4 Min ECA baseline
d4_baseline <- relative_df[
  relative_df$Station_Label == "D4",
  c("Return_Period", "Discharge_ECA_Min")
]

colnames(d4_baseline)[2] <- "Baseline"

# join baseline to all stations by return period
relative_df <- merge(relative_df, d4_baseline, by = "Return_Period")

# calculate standardized max based on D4 baseline
relative_df$Standardized_Max <- relative_df$Baseline * (1 + relative_df$change_ratio)

# station curves for plotting
stations_df <- relative_df[, c(
  "Station_Label",
  "Return_Period",
  "Standardized_Max"
)]

colnames(stations_df)[3] <- "Discharge"

# remove only D10 at return period 2
stations_df <- stations_df[!(
  stations_df$Station_Label == "D10" &
    stations_df$Return_Period == 2
), ]

# baseline curve as separate "station"
baseline_df <- unique(relative_df[, c("Return_Period", "Baseline")])
colnames(baseline_df)[2] <- "Discharge"
baseline_df$Station_Label <- "Baseline"

# combine baseline + station curves
plot_df <- rbind(stations_df, baseline_df)

# set legend/plot order
plot_df$Station_Label <- factor(
  plot_df$Station_Label,
  levels = c("Baseline", paste0("D", 1:11))
)

# plot
ggplot(plot_df, aes(x = Return_Period, y = Discharge, color = Station_Label, group = Station_Label)) +
  
  geom_line(linewidth = 1) +
  geom_point(size = 1) +
  
  scale_color_manual(values = station_colors) +
  
  scale_x_continuous(
    breaks = c(2, 5, 7, 10, 15, 20, 30, 35),
    labels = c(2, 5, 7, 10, 15, 20, 30, 35)
  ) +
  
  labs(
    x = "Return Period (years)",
    y = expression(Standardized~Flood~Flow~(m^3/s)),
    color = NULL
  ) +
  
  theme_classic(base_family = "Times New Roman", base_size = 16) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    legend.position = "right"
  )

