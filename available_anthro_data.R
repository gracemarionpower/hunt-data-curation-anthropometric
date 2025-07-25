library(haven)
library(readr)

# Set working directory
setwd("/home/grace.power/scratch/grace/data/2021_03_22_Brumpton_eksport_av_ny_bestilling_paa_nytt_utvalg_110556")

# Load data as character
data <- read_tsv("2021-03-22_110556_Data_ny_bestilling_paa_nytt_utvalg_crpfixed.tsv", 
                 col_types = cols(.default = col_character()))

# Standardize YH4 variable naming (e.g., BLM1 → BLM)
colnames(data) <- gsub("YH4BLM1", "YH4BLM", colnames(data))
colnames(data) <- gsub("YH4BLQ1", "YH4BLQ", colnames(data))

# Define variable stems of interest
anthro_vars <- c("Wei", "Hei", "WaistCirc", "HipCirc", "Bmi", "ArmCirc", "LegCirc", "NeckCirc", 
                 "Pbf", "Slm", "Smm", "Vfa", "Vfl")

# Define age variables to search for
age_var_patterns <- paste0("PartAg\\.(YH|NT)[1-4]BLQ[1]*")

# Pull all anthropometric + age vars that exist in dataset
value_vars <- grep(paste0("^(", paste(anthro_vars, collapse = "|"), ")\\.(YH|NT)[1-4]BLM$"),
                   colnames(data), value = TRUE)

age_vars <- grep(age_var_patterns, colnames(data), value = TRUE)

# Identify matching waves by value/age availability
extract_wave <- function(x) gsub("^.*\\.(YH|NT)[1-4]BL.*$", "\\1", x)
value_waves <- extract_wave(value_vars)
age_waves <- extract_wave(age_vars)
common_waves <- intersect(value_waves, age_waves)

# Prepare output subset
core_vars <- c("PID.110556", "Sex", "BirthYear")
selected_vars <- unique(c(core_vars, value_vars, age_vars))
subset_data <- data[, selected_vars]

# Write all available variables
write.csv(subset_data, "YH_and_HNT_subset.csv", row.names = FALSE)

# Search for possible pubertal development variables
cat("\nPossible puberty-related variable names:\n")
print(grep("Pub|Tanner|Menarche|Dev|Hair|Breast|Genital", colnames(data), value = TRUE))





