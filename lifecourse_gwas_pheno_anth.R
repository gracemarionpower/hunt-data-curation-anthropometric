library(readr)

# Set working directory
setwd("/home/grace.power/scratch/grace/data/lifecourse_gwas")

# Read data with PID as character
data <- read_csv(
  "/home/grace.power/scratch/grace/data/2021_03_22_Brumpton_eksport_av_ny_bestilling_paa_nytt_utvalg_110556/YH_and_HNT_subset.csv",
  col_types = cols(`PID.110556` = col_character())
)

# Combine value + age per wave
make_long_measure <- function(data, var_prefix) {
  all_visits <- paste0(rep(c("YH", "NT"), each = 4), rep(1:4, 2))
  long_data <- list()
  
  for (visit in all_visits) {
    value_col <- paste0(var_prefix, ".", visit, "BLM")
    age_col <- paste0("PartAg.", visit, ifelse(grepl("^YH", visit), "BLQ", "BLQ1"))
    
    if (value_col %in% names(data) & age_col %in% names(data)) {
      df <- data.frame(
        FID = data$PID.110556,
        IID = data$PID.110556,
        wave = visit,
        value = as.numeric(data[[value_col]]),
        age = as.numeric(data[[age_col]])
      )
      long_data[[visit]] <- df
    }
  }
  do.call(rbind, long_data)
}

# Save function
save_versions <- function(df, base_name) {
  write.table(df, paste0(base_name, "_full.txt"), sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE, na = ".")
  complete_df <- df[!is.na(df$value) & !is.na(df$age), ]
  write.table(complete_df, paste0(base_name, ".txt"), sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE, na = ".")
}

# HEIGHT
height_data <- make_long_measure(data, "Hei")
save_versions(height_data, "height")

# WAIST CIRCUMFERENCE
waist_data <- make_long_measure(data, "WaistCirc")
save_versions(waist_data, "wc")

# HIP
hip_data <- make_long_measure(data, "HipCirc")

# WHR
whr_data <- merge(waist_data, hip_data, by = c("FID", "IID", "wave", "age"), suffixes = c("_waist", "_hip"))
whr_data$value <- whr_data$value_waist / whr_data$value_hip
whr_data <- whr_data[, c("FID", "IID", "wave", "value", "age")]
save_versions(whr_data, "whr")

# WEIGHT
weight_data <- make_long_measure(data, "Wei")
save_versions(weight_data, "weight")

# BMI: combine existing NT BMI data + calculated YH BMI
bmi_nt_data <- make_long_measure(data, "Bmi")

# Calculate BMI manually for YH visits
height_yh <- make_long_measure(data, "Hei")
weight_yh <- make_long_measure(data, "Wei")

# Keep only YH visits
height_yh <- height_yh[grepl("^YH", height_yh$wave), ]
weight_yh <- weight_yh[grepl("^YH", weight_yh$wave), ]

# Merge height and weight by ID and wave
bmi_yh_data <- merge(height_yh, weight_yh, by = c("FID", "IID", "wave"), suffixes = c("_height", "_weight"))

# Calculate BMI
bmi_yh_data$value <- bmi_yh_data$value_weight / (bmi_yh_data$value_height / 100)^2
bmi_yh_data$age <- bmi_yh_data$age_weight  # assume same age
bmi_yh_data <- bmi_yh_data[, c("FID", "IID", "wave", "value", "age")]

# Combine NT + YH BMI
bmi_data <- rbind(bmi_nt_data, bmi_yh_data)
save_versions(bmi_data, "bmi")

