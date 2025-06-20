# Load required package
library(haven)

# Set working directory (modify this path if needed)
setwd("/home/grace.power/scratch/grace/data")

# Read the SPSS file
data <- read_sav("2022-01-27_110556_Data.sav")

# Convert ObsEndDat to Date (if it's not already)
data$ObsEndDat <- as.Date(data$ObsEndDat)

# Extract year from ObsEndDat
data$ObsYear <- as.numeric(format(data$ObsEndDat, "%Y"))

# Calculate age at observation
data$AgeAtObsEnd <- data$ObsYear - data$BirthYear

# Filter out invalid or missing ages (NA or <= 0)
data_clean <- subset(data, !is.na(AgeAtObsEnd) & AgeAtObsEnd > 0)

# (Optional) Check summary of cleaned age variable
summary(data_clean$AgeAtObsEnd)





