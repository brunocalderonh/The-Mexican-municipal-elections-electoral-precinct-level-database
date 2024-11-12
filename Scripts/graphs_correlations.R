rm(list = ls())

library(tidyr)
library(dplyr)
library(readr)  # Use readr for read_csv
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

# Find the CSV file within the temp directory
unzipped_file <- file.path(temp_dir, "all_states_final.csv")

# Now read the unzipped CSV file from the temporary directory
db <- read_csv(unzipped_file)  # Use read_csv for CSV files


##### CORRELATION - INE #####
#### INE
ine_db <- read.csv("correlation/generated_data/ine_turnout_sec.csv") %>% 
  mutate(uniqueid = as.numeric(uniqueid)) %>% 
  select(-X) %>% 
  rename (turnout_ine = turnout)


# Filter out rows in final_db and ine_db that don't have matching 'section' values
final_db_filtered <- final_db %>% filter(section %in% ine_db$section)
ine_db_filtered <- ine_db %>% filter(section %in% final_db$section)
# Perform the left join after filtering
correlation_ine <- final_db_filtered %>%
  left_join(ine_db_filtered %>% select(uniqueid, year, section, turnout_ine), 
            by = c("uniqueid", "year", "section"))

# Calculate the correlation between 'turnout_magar' and 'turnout'
correlation_result <- cor(correlation_ine$turnout_ine, correlation_ine$turnout, use = "complete.obs")
# Print the correlation result
correlation_result

##### Correlation Magar Municipal #####
final_db_mun <- read.csv("States/final/mun_aggregated.csv") 
magar_db <- read.csv("correlation/generated_data/magar_vote_turnout_mun.csv") %>% 
  mutate(magar_listanominal_voteShare_PRI =ifelse(listanominal> 0, pri / listanominal, NA)) %>% 
  mutate(magar_listanominal_voteShare_PAN =ifelse(listanominal> 0, pan / listanominal, NA)) %>% 
  mutate(magar_listanominal_voteShare_PRD =ifelse(listanominal> 0, prd / listanominal, NA)) %>% 
  mutate(magar_listanominal_voteShare_MORENA =ifelse(listanominal> 0, morena / listanominal, NA)) %>% 
  rename(turnout_magar = turnout)

correlation_magar_mun <- final_db_mun %>%
  left_join(magar_db %>% select(uniqueid, year, turnout_magar, magar_listanominal_voteShare_PRI, magar_listanominal_voteShare_PAN,magar_listanominal_voteShare_PRD,magar_listanominal_voteShare_MORENA), 
            by = c("uniqueid", "year"))

# Calculate the correlation between 'turnout_magar' and 'turnout'
correlation_result <- cor(correlation_magar_mun$turnout_magar, correlation_magar_mun$turnout, use = "complete.obs")
# Print the correlation result
correlation_result

# Calculate the correlation between 'turnout_magar' and 'turnout'
correlation_result <- cor(correlation_magar_mun$magar_listanominal_voteShare_PRI, correlation_magar_mun$listanominal_voteShare_PRI, use = "complete.obs")
# Print the correlation result
correlation_result

# Calculate the correlation between 'turnout_magar' and 'turnout'
correlation_result <- cor(correlation_magar_mun$magar_listanominal_voteShare_PAN, correlation_magar_mun$listanominal_voteShare_PAN, use = "complete.obs")
# Print the correlation result
correlation_result

# Calculate the correlation between 'turnout_magar' and 'turnout'
correlation_result <- cor(correlation_magar_mun$magar_listanominal_voteShare_PRD, correlation_magar_mun$listanominal_voteShare_PRD, use = "complete.obs")
# Print the correlation result
correlation_result

# Calculate the correlation between 'turnout_magar' and 'turnout'
correlation_result <- cor(correlation_magar_mun$magar_listanominal_voteShare_MORENA, correlation_magar_mun$listanominal_voteShare_MORENA, use = "complete.obs")
# Print the correlation result
correlation_result


##### GRAPHS #####

##### OPCION 2  #####
# Step 1: Flag rows where incumbent_party contains a coalition (i.e., has '_')
# and calculate number of elections at the municipal level
coalition_trend <- final_db %>%
  # Create a flag for coalitions (1 if coalition, 0 otherwise)
  mutate(coalition = ifelse(grepl("_", incumbent_party), 1, 0)) %>%
  
  # Step 2: Summarize the number of coalitions and elections per year
  group_by(year) %>%
  summarise(
    # Count the total number of coalitions
    num_coalitions = sum(coalition),
    
    # Count the unique number of municipal elections (use 'mun' for this)
    num_elections = n_distinct(uniqueid)
  ) %>%
  
  # Calculate the number of coalitions per election
  mutate(coalitions_per_election = num_coalitions / num_elections)

# Step 1: Calculate Z-score for coalitions per election
coalition_trend <- coalition_trend %>%
  mutate(z_score_coalitions = (coalitions_per_election - mean(coalitions_per_election)) / sd(coalitions_per_election))

# Step 2a: Plot the trend of Z-score normalized coalitions per election over time (Line Chart)
ggplot(coalition_trend, aes(x = year, y = z_score_coalitions)) +
  geom_line(color = "blue") +
  geom_point(size = 3) +
  labs(title = "Z-Score of Coalitions per Election Over Time",
       x = "Year",
       y = "Z-Score (Coalitions per Election)") +
  theme_minimal()

# Step 2b: Create a histogram to show the Z-score of coalitions per election over time
ggplot(coalition_trend, aes(x = year, y = z_score_coalitions)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Z-Score of Coalitions per Election Over Time",
       x = "Year",
       y = "Z-Score (Coalitions per Election)") +
  theme_minimal()

##### OPCION 2  #####


# Step 1: Add state_code based on uniqueid length
finaldb <- finaldb %>%
  mutate(state_code = case_when(
    nchar(as.character(uniqueid)) == 4 ~ substr(uniqueid, 1, 1),  # First digit for 4-character uniqueid
    nchar(as.character(uniqueid)) == 5 ~ substr(uniqueid, 1, 2),  # First two digits for 5-character uniqueid
    TRUE ~ NA_character_  # Handle unexpected cases
  ))

# Step 2: Create a mapping for state names
state_names <- c(
  "1" = "Aguascalientes", "2" = "Baja California", "3" = "Baja California Sur",
  "4" = "Campeche", "5" = "Coahuila", "6" = "Colima", "7" = "Chiapas", 
  "8" = "Chihuahua", "9" = "Ciudad de México", "10" = "Durango", "11" = "Guanajuato", 
  "12" = "Guerrero", "13" = "Hidalgo", "14" = "Jalisco", "15" = "México (State of)", 
  "16" = "Michoacán", "17" = "Morelos", "18" = "Nayarit", "19" = "Nuevo León", 
  "20" = "Oaxaca", "21" = "Puebla", "22" = "Querétaro", "23" = "Quintana Roo", 
  "24" = "San Luis Potosí", "25" = "Sinaloa", "26" = "Sonora", "27" = "Tabasco", 
  "28" = "Tamaulipas", "29" = "Tlaxcala", "30" = "Veracruz", "31" = "Yucatán", 
  "32" = "Zacatecas"
)

# Step 3: Summarize to check for elections held
held_elections_data <- final_df  %>%
  group_by(state_code, year) %>%
  summarize(elections_held = any(!is.na(incumbent_vote) & incumbent_vote > 0), .groups = 'drop')

# Step 4: Convert elections_held to numeric (0 or 1)
held_elections_data$elections_held <- as.numeric(held_elections_data$elections_held)

# Step 5: Replace state codes with state names
held_elections_data <- held_elections_data %>%
  mutate(state_code = state_names[as.character(state_code)])

# Step 6: Plot the data as a binary heatmap with discrete colors (black/white)
# Step 6: Plot the data as a binary heatmap with discrete colors (black/white) and a black border around the white legend box
ggplot(held_elections_data, aes(x = year, y = state_code, fill = factor(elections_held))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "white", "1" = "black"), 
                    name = "Elections Held", 
                    labels = c("No Elections", "Elections Held"),
                    guide = guide_legend(override.aes = list(color = c("black", NA)))) +  # Black border around the white legend box
  scale_x_continuous(breaks = seq(min(held_elections_data$year), max(held_elections_data$year), by = 1)) +  # Display all years
  labs(title = "Elections Held by State and Year",
       x = "Year", 
       y = "State") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))



##
# Calculate the number of unique sections
unique_sections <- length(unique(finaldb$section))

# Calculate the number of unique uniqueids (municipality codes)
unique_uniqueids <- length(unique(finaldb$uniqueid))

cat("Number of unique sections:", unique_sections, "\n")
cat("Number of unique uniqueids:", unique_uniqueids, "\n")


total_elections <- n_distinct(finaldb$uniqueid, finaldb$year)

cat("Total number of elections:", total_elections, "\n")






###

#######

Db_all_yearly_1 <- final_df  %>% 
  dplyr::group_by(year,uniqueid) %>% 
  dplyr::summarise(underscore_count = str_count(party_component, "_")/n()) %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(Elections = sum(underscore_count, na.rm = TRUE))

ggplot(Db_all_yearly_1, aes(x = year, y = Elections)) +
  geom_line() +
  geom_point() +
  labs(title = "Trend of proportion of electoral coalitions Per Year", x = "Year", y = "Proportion of Coalitions") +
  theme_minimal()




Db_all_yearly <-final_df  %>% 
  dplyr::group_by(year,uniqueid) %>% 
  dplyr::mutate(underscore_count = ifelse(str_count(party_component, "_")>0,1,0)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(year,uniqueid,section,party_component,underscore_count) %>% 
  dplyr::group_by(year,uniqueid) %>% 
  dplyr::summarise(Elections = n(),
                   Sum_coallitions = sum(underscore_count, na.rm = TRUE)) %>% 
  dplyr::mutate(Ratio_coalitions = Sum_coallitions/Elections)
table(Db_all_yearly$Ratio_coalitions)


#
Db_all_yearly <- final_df %>% 
  dplyr::group_by(year,uniqueid) %>% 
  dplyr::summarise(underscore_count = ifelse(str_count(party_component, "_")>0,1,0)) %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(Elections = n(),
                   Sum_coallitions = sum(underscore_count, na.rm = TRUE)) %>% 
  dplyr::mutate(Ratio_coalitions = Sum_coallitions/Elections)

#
# Plot the trend per year
ggplot(Db_all_yearly, aes(x = year, y = Ratio_coalitions)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())


# Function to calculate coalitions by party
calc_coalitions_by_party <- function(data, party_component_col) {
  data %>%
    group_by(year, uniqueid) %>%
    summarise(underscore_count = ifelse(str_count(!!sym(party_component_col), "_") > 0, 1, 0)) %>%
    group_by(year) %>%
    summarise(Elections = n(),
              Sum_coalitions = sum(underscore_count, na.rm = TRUE)) %>%
    mutate(Party = party_component_col,
           Ratio_coalitions = Sum_coalitions / Elections)
}

# Apply the function for each party component
party_list <- c("PRD_vote_party_component", "PRI_vote_party_component", "MORENA_vote_party_component", "PAN_vote_party_component")

# Bind the data for all parties
Db_all_parties <- bind_rows(lapply(party_list, function(party) calc_coalitions_by_party(final_df, party)))

ggplot(Db_all_parties, aes(x = year, y = Ratio_coalitions, color = Party, group = Party)) +
  geom_line() +
  geom_point() +
  scale_color_discrete(labels = c("MORENA", "PAN", "PRD", "PRI")) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())



# hasta 2018

Db_all_yearly <- final_df %>%
  dplyr::group_by(year, uniqueid) %>%
  dplyr::summarise(underscore_count = ifelse(str_count(party_component, "_") > 0, 1, 0)) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(Elections = n(),
                   Sum_coallitions = sum(underscore_count, na.rm = TRUE)) %>%
  dplyr::mutate(Ratio_coalitions = Sum_coallitions / Elections) %>%
  dplyr::filter(year <= 2018)

# Plot the trend per year up to 2018
ggplot(Db_all_yearly, aes(x = year, y = Ratio_coalitions)) +
  geom_line() +
  geom_point() +
  labs(title = "Proportion of Elections with Coalitions by Year", 
       x = "Year", 
       y = "Proportion of Elections with Coalitions") +
  theme_minimal()



# Function to calculate coalitions by party
calc_coalitions_by_party <- function(data, party_component_col) {
  data %>%
    group_by(year, uniqueid) %>%
    summarise(underscore_count = ifelse(str_count(!!sym(party_component_col), "_") > 0, 1, 0)) %>%
    group_by(year) %>%
    summarise(Elections = n(),
              Sum_coalitions = sum(underscore_count, na.rm = TRUE)) %>%
    mutate(Party = party_component_col,
           Ratio_coalitions = Sum_coalitions / Elections)
}

# Apply the function for each party component
party_list <- c("PRD_vote_party_component", "PRI_vote_party_component", "MORENA_vote_party_component", "PAN_vote_party_component")

# Bind the data for all parties and filter up to 2018
Db_all_parties <- bind_rows(lapply(party_list, function(party) calc_coalitions_by_party(final_df, party))) %>%
  dplyr::filter(year <= 2018)

# Plot the trends for all parties up to 2018
ggplot(Db_all_parties, aes(x = year, y = Ratio_coalitions, color = Party, group = Party)) +
  geom_line() +
  geom_point() +
  labs(title = "Trend of Proportion of Coalitions Per Year for Each Party",
       x = "Year",
       y = "Proportion of Coalitions") +
  scale_color_discrete(labels = c("MORENA", "PAN", "PRD", "PRI")) +
  theme_minimal()


##### OPCION 3 #####
names(final_df)

validation <- final_df %>% 
  group_by(year,uniqueid) %>% 
  mutate(distinct_party_component = n_distinct(party_component))  %>% 
  group_by(year,uniqueid) %>% 
  summarise(Count_party_component = sum(Count_party_component),
            Count = n(),
            Ratio =Count_party_component/Count)

table(final_df$year)


Db_all_yearly <-final_df  %>% 
  dplyr::group_by(year,uniqueid) %>% 
  dplyr::mutate(distinct_party_component = n_distinct(party_component))  %>% 
  dplyr::filter(distinct_party_component == 1) %>% 
  dplyr::mutate(underscore_count = ifelse(str_count(party_component, "_")>0,1,0)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(year,uniqueid,section,party_component,underscore_count) %>% 
  dplyr::group_by(year,uniqueid) %>% 
  dplyr::summarise(Coallitions_dich = max(underscore_count)) %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(Count_elections = n(),
                Coalitions = sum(Coallitions_dich, na.rm = T),
                Ratio_coalitions = Coalitions/Count_elections)

# Add the standard deviation
Db_all_yearly <- final_df %>%
  dplyr::group_by(year, uniqueid) %>%
  dplyr::mutate(distinct_party_component = n_distinct(party_component)) %>%
  dplyr::filter(distinct_party_component == 1) %>%
  dplyr::mutate(underscore_sum = str_count(party_component, "_") + 1) %>%
  dplyr::ungroup() %>%
  dplyr::filter(underscore_sum > 1) %>%
  dplyr::select(year, uniqueid, section, party_component, underscore_sum) %>%
  dplyr::group_by(year, uniqueid) %>%
  dplyr::summarise(Coallitions = max(underscore_sum)) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(Coallitions_mean = mean(Coallitions, na.rm = TRUE),
                Coallitions_sd = sd(Coallitions, na.rm = TRUE))

# Now plot with error bars and sexenio shading
ggplot(Db_all_yearly, aes(x = year, y = Coallitions_mean)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = Coallitions_mean - Coallitions_sd, ymax = Coallitions_mean + Coallitions_sd), width = 0.2) +
  
  # Shade regions for different presidential periods (sexenios)
  annotate("rect", xmin = 2000, xmax = 2006, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "blue") +  # Fox (2000-2006)
  annotate("rect", xmin = 2006, xmax = 2012, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "blue") +   # Calderon (2006-2012)
  annotate("rect", xmin = 2012, xmax = 2018, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "green") + # Peña Nieto (2012-2018)
  annotate("rect", xmin = 2018, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "red") + # López Obrador (2018-2024)
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
