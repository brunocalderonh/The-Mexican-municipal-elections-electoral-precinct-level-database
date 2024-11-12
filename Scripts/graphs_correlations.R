rm(list = ls())

library(tidyr)
library(dplyr)
library(haven)
library(openxlsx)
library(purrr)
library(readxl)
library(rstudioapi)

# Get the path of the current script
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the root of the repository
# Assuming your script is in 'Scripts/Script States/', go two levels up
setwd(file.path(script_dir, "../"))

# Path to the .zip file
zip_file <- "Final Data/all_states_final.zip"

# Unzip the file to a temporary directory
temp_dir <- tempdir()  # Create a temporary directory
unzip(zip_file, exdir = temp_dir)  # Extract the contents to the temp directory

# Find the .dta file within the temp directory
unzipped_file <- file.path(temp_dir, "all_states_final.csv")

# Now read the unzipped .dta file from the temporary directory
db <- read_dta(unzipped_file)

