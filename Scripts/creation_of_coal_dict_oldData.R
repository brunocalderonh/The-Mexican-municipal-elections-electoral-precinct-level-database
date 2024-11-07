# Clear the environment
rm(list = ls())
library(tidyverse)

###### APPEND 32 Individual final state files to create a single file

# Define the base path
base_path <- "~/Documents/OneDrive/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/states/"

# Define the state names
states <- c("aguascalientes", "baja", "bajasur", "campeche", "chiapas", "chihuahua", 
            "coahuila", "colima", "durango", "guanajuato", "guerrero", "hidalgo", 
            "jalisco", "mexico", "michoacan", "morelos", "nayarit", 
            "nuevoleon", "oaxaca", "puebla", "queretaro", "quintanaroo", 
            "sanluispotosi", "sinaloa", "sonora", "tabasco", "tamaulipas", 
            "tlaxcala","veracruz", "yucatan", "zacatecas")

# Initialize an empty list to store the data frames
df_list <- list()

# Loop through each state, read the data, and append the relevant variables
for (state in states) {
  # Construct the file path with the correct naming convention
  file_path <- paste0(base_path, state, "/", state, "_FINALV-Bruno’s MacBook Pro.csv")
  
  # Check if the file exists before attempting to read it
  if (file.exists(file_path)) {
    # Try to read the CSV file and catch any errors
    data <- tryCatch({
      read.csv(file_path)
    }, error = function(e) {
      warning(paste("Could not read file:", file_path, "-", e$message))
      NULL
    })
    
    # Proceed only if data was successfully read and has content
    if (!is.null(data) && nrow(data) > 0) {
      # Select columns uniqueid, year, section, and those with an underscore, excluding specific terms
      data_selected <- data %>%
        select(matches(".*_.*")) %>% # Select columns with uniqueid, year, section, and an underscore
        select(-matches("vote|party|runnerup|state|incumbent|mutually_exclusive|Partido_Cardenista|Alianza_Just|CI_")) # Exclude specific terms
      
      # Add the data to the list
      df_list[[state]] <- data_selected
    } else {
      warning(paste("File is empty or could not be processed:", file_path))
    }
  } else {
    warning(paste("File does not exist:", file_path))
  }
}

# Combine all data frames into one
combined_df <- bind_rows(df_list)


list_coalitions <- as.data.frame(names(combined_df)) %>% 
  rename(coalitions = "names(combined_df)") %>% 
  mutate(components = str_split(coalitions, "_")) %>% 
  unnest_wider(components, names_sep = "_")

# This will create new columns named component_1, component_2, etc.


# Write the combined data frame to a CSV file
write.csv(list_coalitions, "~/Documents/OneDrive/Documents/ITAM/RA - Horacio/Monitoring Brokers/Data/coalition_dic.csv", row.names = FALSE)