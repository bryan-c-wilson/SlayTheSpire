## Initialize_StS_Project_Script.R - Initialization script for Slay The Spire project
## Project by Bryan Wilson
# This script sets up the project environment by:
# 1. Installing and loading required packages.
# 2. Loading the main data sets from RDS files or creating them from raw data (.run files or JSON).
# 3. Combining and saving .run files as an RDS file if needed.

# IMPORTANT:
# This script automatically converts raw data (.run files or JSON files) to RDS files
# if they do not exist. This ensures optimal performance and a seamless setup process.

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

# Load the packages
lapply(required_packages, library, character.only = TRUE)

# Function to combine .run files from multiple directories and save as RDS
combine_and_save_runs <- function(directories, output_path) {
  all_runs <- list()
  
  for (dir in directories) {
    run_files <- list.files(path = dir, pattern = "*.run", full.names = TRUE)
    
    for (file in run_files) {
      run_data <- jsonlite::fromJSON(file)
      all_runs <- append(all_runs, list(run_data))
    }
  }
  
  events <- data.frame(event = I(all_runs))
  saveRDS(events, file = output_path)
  message("Runs have been successfully combined and saved as RDS.")
}

# Check if bryan.rds exists, if not, create it
bryan_rds_path <- "Slay_The_Spire/data/bryan.rds"
if (!file.exists(bryan_rds_path)) {
  directories <- c("Slay_The_Spire/Bryan's Run Data/runs/DEFECT",
                   "Slay_The_Spire/Bryan's Run Data/runs/IRONCLAD",
                   "Slay_The_Spire/Bryan's Run Data/runs/SILENT",
                   "Slay_The_Spire/Bryan's Run Data/runs/WATCHER")
  combine_and_save_runs(directories, bryan_rds_path)
}

# Check if november.rds exists, if not, create it from JSON
november_rds_path <- "Slay_The_Spire/data/november.rds"
if (!file.exists(november_rds_path)) {
  message("november.rds not found. Converting from JSON...")
  data <- jsonlite::fromJSON("data/november.json")
  saveRDS(data, file = november_rds_path)
  message("november.rds created successfully.")
}

# Load data from the RDS files
message("Starting to load data from RDS...")
data <- tryCatch({
  readRDS(november_rds_path)
}, error = function(e) {
  message("Error loading data: ", e$message)
  NULL
})

bryan_data <- tryCatch({
  readRDS(bryan_rds_path)
}, error = function(e) {
  message("Error loading bryan.rds data: ", e$message)
  NULL
})

if (!is.null(data)) {
  message("Data from november.rds loaded successfully.")
  head(data)
} else {
  message("november.rds data could not be loaded.")
}

if (!is.null(bryan_data)) {
  message("Data from bryan.rds loaded successfully.")
  head(bryan_data)
} else {
  message("bryan.rds data could not be loaded.")
}
