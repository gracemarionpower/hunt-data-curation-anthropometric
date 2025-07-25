# Load required packages
library(haven)
library(readr)

# Set working directory
setwd("/home/grace.power/scratch/grace/data/2021_03_22_Brumpton_eksport_av_ny_bestilling_paa_nytt_utvalg_110556")

# Load data as character
data <- read_tsv("2021-03-22_110556_Data_ny_bestilling_paa_nytt_utvalg_crpfixed.tsv", 
                 col_types = cols(.default = col_character()))

# Define raw variable list using @ format
vars_raw <- c(
  "PID.110556", "Sex", "BirthYear",
  "Wei@YH1BLM", "Wei@YH2BLM", "Wei@YH3BLM", "WeiDev@YH3BLM", "Wei@YH4BLM1", "WeiDev@YH4BLM1",
  "Hei@YH1BLM", "HeiCorr@YH1BLM", "Hei@YH2BLM", "Hei@YH3BLM", "Hei@YH4BLM1",
  "HeiSit@YH1BLM", "HeiSit@YH2BLM",
  "WaistCirc@YH1BLM", "WaistCirc@YH2BLM", "WaistCirc@YH3BLM", "WaistCircDev@YH3BLM",
  "WaistCirc@YH4BLM1", "WaistCircDev@YH4BLM1",
  "HipCircDev@YH4BLM1", "HipCirc@YH1BLM", "HipCirc@YH2BLM", "HipCirc@YH3BLM", "HipCirc@YH4BLM1", "HipCircDev@YH3BLM",
  "PartAg@YH1BLQ", "PartAg@YH1Lu3MI", "PartAg@YH2BLQ", "PartAg@YH3BLQ", "PartAg@YH4BLM1", "PartAg@YH4BLQ",
  "Pbf@YH4BLM1", "Slm@YH4BLM1", "Smm@YH4BLM1", "Vfa@YH4BLM1", "Vfl@YH4BLM1"
)

# Convert "@" to "." for column matching
vars_fixed <- gsub("@", ".", vars_raw)
existing_vars <- vars_fixed[vars_fixed %in% colnames(data)]
subset_data <- data[, existing_vars]
write.csv(subset_data, "available_YH_data.csv", row.names = FALSE)

# Print missing variables
missing_vars <- vars_raw[!(vars_fixed %in% colnames(data))]
cat("Missing variables:\n")
print(missing_vars)

# Reorder columns
core_vars <- c("PID.110556", "Sex", "BirthYear")

get_wave_columns <- function(data, wave) {
  grep(paste0("\\.", wave), colnames(data), value = TRUE)
}

ordered_vars <- c(
  core_vars,
  get_wave_columns(subset_data, "YH1"),
  get_wave_columns(subset_data, "YH2"),
  get_wave_columns(subset_data, "YH3"),
  get_wave_columns(subset_data, "YH4")
)

YH1to4_data_ordered <- subset_data[, ordered_vars]
write.csv(YH1to4_data_ordered, "YH1to4_data_ordered.csv", row.names = FALSE)
YH1to4_data_ordered <- read.csv("YH1to4_data_ordered.csv", stringsAsFactors = FALSE)

# Count complete cases per wave
count_complete_wave <- function(data, wave_suffix) {
  wave_cols <- grep(paste0("\\.", wave_suffix, "$"), colnames(data), value = TRUE)
  complete_cases <- complete.cases(data[, wave_cols])
  sum(complete_cases)
}

n_YH1 <- count_complete_wave(YH1to4_data_ordered, "YH1BLM")
n_YH2 <- count_complete_wave(YH1to4_data_ordered, "YH2BLM")
n_YH3 <- count_complete_wave(YH1to4_data_ordered, "YH3BLM")
n_YH4 <- count_complete_wave(YH1to4_data_ordered, "YH4BLQ")

cat("People with full data:\n")
cat("YH1:", n_YH1, "\n")
cat("YH2:", n_YH2, "\n")
cat("YH3:", n_YH3, "\n")
cat("YH4:", n_YH4, "\n")

# Age variable summaries
age_vars <- c("PartAg.YH1BLQ", "PartAg.YH2BLQ", "PartAg.YH3BLQ", "PartAg.YH4BLQ")

for (var in age_vars) {
  cat("\nSummary for", var, ":\n")
  print(summary(as.numeric(YH1to4_data_ordered[[var]])))
  cat("SD:", sd(as.numeric(YH1to4_data_ordered[[var]]), na.rm = TRUE), "\n")
}

# Adult HUNT (NT) data
anthro_roots <- c("Wei", "Hei", "WaistCirc", "HipCirc", "Bmi", "ArmCirc", "LegCirc", "NeckCirc", 
                  "Pbf", "Slm", "Smm", "Vfa", "Vfl")

# Include all relevant age variables (YH and NT)
age_vars <- c(
  "PartAg.YH1BLQ", "PartAg.YH2BLQ", "PartAg.YH3BLQ", "PartAg.YH4BLQ",
  "PartAg.NT1BLQ1", "PartAg.NT2BLQ1", "PartAg.NT3BLQ1", "PartAg.NT4BLQ1"
)

# Identify NT anthropometric variable names
pattern <- paste0("^(", paste(anthro_roots, collapse = "|"), ")\\.NT[1-4].*")
nt_anthro_vars <- grep(pattern, colnames(data), value = TRUE)

cat("Matched NT1–NT4 anthropometric variables:\n")
print(nt_anthro_vars)

# Also include NT age variables if present
nt_age_vars <- age_vars[grepl("NT", age_vars) & age_vars %in% colnames(data)]

# Create NT dataset
core_vars <- c("PID.110556", "Sex", "BirthYear")
nt_anthro_data <- data[, c(core_vars, nt_anthro_vars, nt_age_vars)]

# Helper to group variables by NT wave
get_nt_wave_vars <- function(data, nt_wave) {
  grep(paste0("\\.NT", nt_wave), colnames(data), value = TRUE)
}

# Reorder columns by wave
ordered_nt_vars <- c(
  core_vars,
  get_nt_wave_vars(nt_anthro_data, "1"),
  get_nt_wave_vars(nt_anthro_data, "2"),
  get_nt_wave_vars(nt_anthro_data, "3"),
  get_nt_wave_vars(nt_anthro_data, "4")
)

# Export ordered NT data
nt_anthro_data_ordered <- nt_anthro_data[, ordered_nt_vars]
write.csv(nt_anthro_data_ordered, "H1to4_data_ordered.csv", row.names = FALSE)

# Merge with Young-HUNT data
combined_data <- merge(YH1to4_data_ordered, nt_anthro_data_ordered, 
                       by = c("PID.110556", "Sex", "BirthYear"), 
                       all = TRUE)

combined_data[combined_data == "<NA>"] <- NA
write.csv(combined_data, "YH_and_HNT_combined.csv", row.names = FALSE)

# Search for possible pubertal development variables
cat("\nPossible puberty-related variable names:\n")
print(grep("Pub|Tanner|Menarche|Dev|Hair|Breast|Genital", colnames(data), value = TRUE))





