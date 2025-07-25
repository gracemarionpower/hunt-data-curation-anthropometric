library(readr)

# Set working directory
setwd("/home/grace.power/scratch/grace/data/lifecourse_gwas")

# Read data with PID as character
data <- read_csv(
  "/home/grace.power/scratch/grace/data/2021_03_22_Brumpton_eksport_av_ny_bestilling_paa_nytt_utvalg_110556/YH_and_HNT_combined.csv",
  col_types = cols(`PID.110556` = col_character())
)

make_long_measure <- function(data, var_prefix, age_prefix = "PartAg") {
  visits <- c(paste0("YH", 1:4), paste0("NT", 1:4))
  
  # Construct correct age variable names depending on visit type
  age_vars <- ifelse(
    grepl("^YH", visits),
    paste0(age_prefix, ".", visits, "BLQ"),
    paste0(age_prefix, ".", visits, "BLQ1")
  )
  
  value_vars <- paste0(var_prefix, ".", visits, "BLM")

  # Filter to only existing variables
  valid <- value_vars %in% names(data) & age_vars %in% names(data)
  value_vars <- value_vars[valid]
  age_vars <- age_vars[valid]
  visits <- visits[valid]

  # Build long format data
  long_df <- do.call(rbind, lapply(seq_along(visits), function(i) {
    pid <- data$PID.110556
    data.frame(
      FID = pid,
      IID = pid,
      visit = visits[i],
      value = data[[value_vars[i]]],
      age = data[[age_vars[i]]],
      stringsAsFactors = FALSE
    )
  }))

  return(long_df)
}

save_versions <- function(df, base_name) {
  # Save all data (with missing values)
  write.table(df, paste0(base_name, "_full.txt"), sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE, na = ".")
  
  # Save only complete cases
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
whr_data <- waist_data
whr_data$value <- waist_data$value / hip_data$value
whr_data$age <- waist_data$age  # ensure age is aligned
save_versions(whr_data, "whr")

# WEIGHT
weight_data <- make_long_measure(data, "Wei")

# BMI: combine existing NT BMI data + calculated YH BMI

# Get existing BMI for NT visits
bmi_nt_data <- make_long_measure(data, "Bmi")

# Calculate BMI manually for YH visits
height_yh <- make_long_measure(data, "Hei")
weight_yh <- make_long_measure(data, "Wei")

# Keep only YH visits
is_yh <- grepl("^YH", height_yh$visit)

bmi_yh_data <- height_yh[is_yh, ]
bmi_yh_data$value <- weight_yh$value[is_yh] / (height_yh$value[is_yh] / 100)^2

# Combine
bmi_data <- rbind(bmi_nt_data, bmi_yh_data)

save_versions(bmi_data, "bmi")
