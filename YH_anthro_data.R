# Load required package
library(haven)
library(readr)

# Set working directory (modify this path if needed)
setwd("/home/grace.power/scratch/grace/data/2021_03_22_Brumpton_eksport_av_ny_bestilling_paa_nytt_utvalg_110556")

# Load data as character
data <- read_tsv("2021-03-22_110556_Data_ny_bestilling_paa_nytt_utvalg_crpfixed.tsv",                            
                 col_types = cols(.default = col_character()))

# Original list with @ symbols
vars_raw <- c(
  "PID.110556", "Sex", "BirthYear",
  "Wei@YH1BLM", "Wei@YH2BLM", "Wei@YH3BLM", "WeiDev@YH3BLM", "Wei@YH4BLM1", "WeiDev@YH4BLM1",
  "Hei@YH1BLM", "HeiCorr@YH1BLM", "Hei@YH2BLM", "Hei@YH3BLM", "Hei@YH4BLM1",
  "HeiSit@YH1BLM", "HeiSit@YH2BLM",
  "WaistCirc@YH1BLM", "WaistCirc@YH2BLM", "WaistCirc@YH3BLM", "WaistCircDev@YH3BLM",
  "WaistCirc@YH4BLM1", "WaistCircDev@YH4BLM1",
  "HipCircDev@YH4BLM1", "HipCirc@YH1BLM", "HipCirc@YH2BLM", "HipCirc@YH3BLM", "HipCirc@YH4BLM1", "HipCircDev@YH3BLM",
  "PartAg@YH1BLQ", "PartAg@YH1Lu3MI", "PartAg@YH2BLQ", "PartAg@YH3BLQ", "PartAg@YH4BLM1", "PartAg@YH4BLQ"
)

# Adjust for column name formatting (replace "@" with ".")
vars_fixed <- gsub("@", ".", vars_raw)

# Identify which of these variables exist in the dataset
existing_vars <- vars_fixed[vars_fixed %in% colnames(data)]

# Extract a new dataset with only those columns
subset_data <- data[, existing_vars]

# Save available subset to CSV
write.csv(subset_data, "available_YH_data.csv", row.names = FALSE)

# Print missing variables
missing_vars <- vars_raw[!(vars_fixed %in% colnames(data))]
cat("Missing variables:\n")
print(missing_vars)

# Core variables stay at the front
core_vars <- c("PID.110556", "Sex", "BirthYear")

# Function to extract and sort column names by wave suffix
get_wave_columns <- function(data, wave) {
  grep(paste0("\\.", wave), colnames(data), value = TRUE)
}

# Build reordered column list
ordered_vars <- c(
  core_vars,
  get_wave_columns(subset_data, "YH1"),
  get_wave_columns(subset_data, "YH2"),
  get_wave_columns(subset_data, "YH3"),
  get_wave_columns(subset_data, "YH4")
)

# Apply reordering and rename final dataset
YH1to4_data_ordered <- subset_data[, ordered_vars]

# View result
head(YH1to4_data_ordered)
write.csv(YH1to4_data_ordered, "YH1to4_data_ordered.csv", row.names = FALSE)




