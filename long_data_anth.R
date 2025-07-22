
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
  value_vars <- paste0(var_prefix, ".", visits, "BLM")
  age_vars <- paste0(age_prefix, ".", visits, "BLQ1")
  age_vars <- ifelse(grepl("^YH", visits), paste0(age_prefix, ".", visits, "BLQ"), age_vars)
  
  valid <- value_vars %in% names(data) & age_vars %in% names(data)
  value_vars <- value_vars[valid]
  age_vars <- age_vars[valid]
  visits <- visits[valid]
  
  long_df <- do.call(rbind, lapply(seq_along(visits), function(i) {
    pid <- data$PID.110556
    data.frame(
      FID = pid,
      IID = pid,
      value = data[[value_vars[i]]],
      age = data[[age_vars[i]]],
      stringsAsFactors = FALSE
    )
  }))
  
  return(long_df)
}

height_data <- make_long_measure(data, "Hei")
write.table(height_data, "height.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE, na = ".")

waist_data <- make_long_measure(data, "WaistCirc")
write.table(waist_data, "waist_circumference.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE, na = ".")

hip_data <- make_long_measure(data, "HipCirc")

whr_data <- waist_data
whr_data$value <- waist_data$value / hip_data$value
write.table(whr_data, "waist_hip_ratio.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE, na = ".")

weight_data <- make_long_measure(data, "Wei")

bmi_data <- weight_data
bmi_data$value <- weight_data$value / (height_data$value / 100)^2
write.table(bmi_data, "bmi.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE, na = ".")

