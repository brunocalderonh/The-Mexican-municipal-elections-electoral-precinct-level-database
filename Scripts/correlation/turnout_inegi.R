# Clear environment
rm(list = ls())

# Load necessary packages
library(tidyverse)

# Set the working directory to where the year folders are located
setwd("/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/correlation/turnout_inegi")

# Define the years you are interested in
years <- c("2015", "2016", "2017", "2018")

# Define all the codes (even if some years might not have all of them)
codes <- c("AGS", "BCS", "BC", "CAMP", "CHIS", "CHIH", "CDMX", "COL", "COAH", "DGO", 
           "DF", "GTO", "GRO", "HGO", "JAL", "MEX", "MICH", "MOR", "NL", "NAY", 
           "OAX", "PUE", "QRO", "QROO", "SIN", "SLP", "SON", "TAB", "TAMPS", "TLAX", 
           "VER", "YUC", "ZAC")

# Function to load CSV files and add the year variable
load_csv_files <- function(year, code) {
  # Construct the folder path
  folder_path <- file.path(year, paste0(code, "_PEL_", year), "AYUNTAMIENTOS_csv")
  
  # Check if the AYUNTAMIENTOS_csv folder exists
  if (dir.exists(folder_path)) {
    # Create the CSV file name based on the year and code
    csv_file <- file.path(folder_path, paste0(year, "_SEE_AYUN_", code, "_SEC.csv"))
    
    # Check if the CSV file exists
    if (file.exists(csv_file)) {
      # Load the CSV file into a data frame
      data <- read.csv(csv_file)
      # Add the year variable
      data$year <- year
      return(data)
    } else {
      message(paste("CSV file not found:", csv_file))
      return(NULL)
    }
  } else {
    message(paste("Folder not found:", folder_path))
    return(NULL)
  }
}

# Create separate lists to store data for each year
data_2015 <- list()
data_2016 <- list()
data_2017 <- list()
data_2018 <- list()

# Iterate over the codes and load data for each year individually
for (code in codes) {
  data_2015[[code]] <- load_csv_files("2015", code)
  data_2016[[code]] <- load_csv_files("2016", code)
  data_2017[[code]] <- load_csv_files("2017", code)
  data_2018[[code]] <- load_csv_files("2018", code)
}

# Combine the data for each year into a single data frame
combined_2015 <- bind_rows(data_2015)
combined_2016 <- bind_rows(data_2016)
combined_2017 <- bind_rows(data_2017)
combined_2018 <- bind_rows(data_2018)

# Combine all years into a single data frame
# Combine all years into a single data frame
turnout_inegi <- bind_rows(combined_2015, combined_2016, combined_2017, combined_2018) %>% 
  select(ID_ESTADO, NOMBRE_ESTADO, ID_MUNICIPIO, MUNICIPIO, SECCION, CASILLAS, NUM_VOTOS_CAN_NREG, NUM_VOTOS_VALIDOS, NUM_VOTOS_NULOS, TOTAL_VOTOS, LISTA_NOMINAL, year) %>% 
  mutate(turnout = TOTAL_VOTOS / LISTA_NOMINAL) %>% 
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
  select(uniqueid, year, state, mun, section, listanominal, valid, total, turnout)


write.csv(turnout_inegi, "/Users/brunocalderon/Library/CloudStorage/OneDrive-Personal/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/correlation/generated_data/ine_turnout_sec.csv")
# `combined_all_years` is now your final data frame with all years merged and a `year` column.