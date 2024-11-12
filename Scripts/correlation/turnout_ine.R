# Load necessary libraries
rm(list = ls())

library(tidyr)
library(dplyr)
library(readr)
library(openxlsx)
library(purrr)
library(readxl)
library(rstudioapi)

# Get the path of the current script
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the root of the repository
setwd(file.path(script_dir, "../../"))
getwd()

# Define the years and codes of interest
years <- c("2015", "2016", "2017", "2018")
codes <- c("AGS", "BCS", "BC", "CAMP", "CHIS", "CHIH", "CDMX", "COL", "COAH", "DGO", 
           "DF", "GTO", "GRO", "HGO", "JAL", "MEX", "MICH", "MOR", "NL", "NAY", 
           "OAX", "PUE", "QRO", "QROO", "SIN", "SLP", "SON", "TAB", "TAMPS", "TLAX", 
           "VER", "YUC", "ZAC")

# Define a function to load CSV files and add the year variable
load_csv_files <- function(year, code) {
  # Construct the full folder path including "AYUNTAMIENTOS_csv"
  folder_path <- file.path("Correlation Data", "turnout_inegi", year, paste0(code, "_PEL_", year), "AYUNTAMIENTOS_csv")
  
  # Create the CSV file name for the 'SEC' file
  csv_file <- file.path(folder_path, paste0(year, "_SEE_AYUN_", code, "_SEC.csv"))
  
  # Check if the CSV file exists
  if (file.exists(csv_file)) {
    # Load the CSV file
    data <- read.csv(csv_file)
    # Add the year variable to the data frame
    data$year <- year
    return(data)
  } else {
    message(paste("CSV file not found:", csv_file))
    return(NULL)
  }
}

# Example usage: Load all data for all years and codes
all_data <- map_df(years, function(year) {
  map_df(codes, function(code) {
    load_csv_files(year, code)
  })
})

# Print a message after loading all data
message("Data loading completed.")


all_data <- all_data %>% 
  select(ID_ESTADO, NOMBRE_ESTADO, ID_MUNICIPIO, MUNICIPIO, SECCION, CASILLAS, NUM_VOTOS_CAN_NREG, NUM_VOTOS_VALIDOS, NUM_VOTOS_NULOS, TOTAL_VOTOS, LISTA_NOMINAL, year) %>% 
  mutate(turnout_ine = TOTAL_VOTOS / LISTA_NOMINAL) %>% 
  mutate(
    uniqueid = paste0 (
      sprintf("%02d", as.numeric(ID_ESTADO)), # format state ID with 2 digits
      sprintf("%03d", as.numeric(ID_MUNICIPIO)) # format municipality ID with 3 digits
    )) %>% 
  rename("state" = "NOMBRE_ESTADO",
         "mun" = "MUNICIPIO",
         "total" = "TOTAL_VOTOS",
         "valid" = "NUM_VOTOS_VALIDOS",
         "listanominal" = "LISTA_NOMINAL",
         "section" = "SECCION") %>% 
  select(uniqueid, year, state, mun, section, listanominal, valid, total, turnout_ine) #%>% 
  # filter(turnout_ine <= 1) %>% 
  # filter(turnout_ine != 0)

# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Correlation Data/generated_data")
output_path <- file.path(output_dir, "ine_turnout.csv")

# Use write_csv to save the file
write_csv(all_data, output_path)

# Confirm file saved correctly
cat("File saved at:", output_path)
