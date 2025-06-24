# Load required packages
library(readr)
library(dplyr)

# Read in your data (assuming it's in a CSV format)
data <- read_csv("YH_and_HNT_combined.csv")

# Create a new dataframe with required columns
subset_data <- data %>%
  transmute(
    FID = PID.110556,
    IID = PID.110556,
    yob = BirthYear,
    sex = Sex
  )

# Write to file
write.table(subset_data, "fid_iid_yob_sex.txt", 
            row.names = FALSE, col.names = TRUE, quote = FALSE, sep = " ")
