##Bryans first script to initialize for Slay the Spire project

## Initialize_StS_Project_Script.R - Initialization script for Slay The Spire project

# This script sets up the project environment by:
# 1. Installing and loading required packages.
# 2. Loading the main data set from an RDS file for efficient data access.

# IMPORTANT:
# If the data source (JSON file) is updated or used for the first time, you must
# convert the JSON file to an RDS file. This is necessary for optimal performance,
# as RDS files load much faster in R than JSON files.

# To update the RDS file from the JSON file, run the following commands in R:
# data <- jsonlite::fromJSON("data/november.json")  # Replace with your JSON file path
# saveRDS(data, file = "data/november.rds")         # Save the data as an RDS file

# Once the RDS file is generated, you can use this initialization script
# to set up the environment and load the data quickly.


# Function to check if packages are installed and install them if they are not
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
}

# List of required packages
required_packages <- c("dplyr", "ggplot2", "googledrive", "googlesheets4", 
                       "jsonlite", "lubridate", "tidyr", "tidyverse", "progress", "scales")

# Install any missing packages
install_if_missing(required_packages)

# Load the packages, does the same thing as library(each package individually) as a separate call for each and all of the packages in Required_packages
lapply(required_packages, library, character.only = TRUE)


# Load data from the RDS file
message("Starting to load data from RDS...")
data <- tryCatch({
  readRDS("/Users/bryan/Documents/R_Projects/Slay_The_Spire/data/november.rds")
}, error = function(e) {
  message("Error loading data: ", e$message)
  NULL
})

if (!is.null(data)) {
  message("Data loaded successfully from RDS.")
  head(data)
} else {
  message("Data could not be loaded.")
}
 

