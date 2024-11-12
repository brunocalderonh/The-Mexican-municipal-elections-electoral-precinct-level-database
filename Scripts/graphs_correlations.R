rm(list = ls())

library(tidyr)
library(dplyr)
library(readr)  # Use readr for read_csv
library(openxlsx)
library(purrr)
library(readxl)
library(rstudioapi)
library(readr)
library(stringr)
library(ggplot2)


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

#final db
db <- read_csv(unzipped_file)  # Use read_csv for CSV files


#ine
ine_db <- read.csv("Correlation Data/generated_data/ine_turnout.csv") %>% 
  mutate(uniqueid = as.numeric(uniqueid))  %>% 
   filter(turnout_ine != 0)

#Magar 
magar_db <- read.csv("Correlation Data/generated_data/magar_turnout.csv") %>% 
  mutate(magar_listanominal_voteShare_PRI =ifelse(listanominal> 0, pri / listanominal, NA)) %>% 
  mutate(magar_listanominal_voteShare_PAN =ifelse(listanominal> 0, pan / listanominal, NA)) %>% 
  mutate(magar_listanominal_voteShare_PRD =ifelse(listanominal> 0, prd / listanominal, NA)) %>% 
  mutate(magar_listanominal_voteShare_MORENA =ifelse(listanominal> 0, morena / listanominal, NA)) 

#aggregate to mun level for corr with magar 
# Aggregating at the municipal level
db_mun <- db %>%
  group_by(uniqueid, year) %>%
  summarise(
    # Calculations with listanominal
    listanominal_voteShare_incumbent = ifelse(sum(listanominal, na.rm = TRUE) > 0, 
                                              sum(incumbent_vote, na.rm = TRUE) / sum(listanominal, na.rm = TRUE), NA),
    listanominal_voteShare_state_incumbent = ifelse(sum(listanominal, na.rm = TRUE) > 0, 
                                                    sum(state_incumbent_vote, na.rm = TRUE) / sum(listanominal, na.rm = TRUE), NA),
    listanominal_voteShare_PRI = ifelse(sum(listanominal, na.rm = TRUE) > 0, 
                                        sum(PRI_vote, na.rm = TRUE) / sum(listanominal, na.rm = TRUE), NA),
    listanominal_voteShare_PRD = ifelse(sum(listanominal, na.rm = TRUE) > 0, 
                                        sum(PRD_vote, na.rm = TRUE) / sum(listanominal, na.rm = TRUE), NA),
    listanominal_voteShare_PAN = ifelse(sum(listanominal, na.rm = TRUE) > 0, 
                                        sum(PAN_vote, na.rm = TRUE) / sum(listanominal, na.rm = TRUE), NA),
    listanominal_voteShare_MORENA = ifelse(sum(listanominal, na.rm = TRUE) > 0, 
                                           sum(MORENA_vote, na.rm = TRUE) / sum(listanominal, na.rm = TRUE), NA),
    listanominal_voteShare_runnerup = ifelse(sum(listanominal, na.rm = TRUE) > 0, 
                                             sum(runnerup_vote, na.rm = TRUE) / sum(listanominal, na.rm = TRUE), NA),
    
    # New Calculations with valid
    valid_voteShare_incumbent = ifelse(sum(valid, na.rm = TRUE) > 0, 
                                       sum(incumbent_vote, na.rm = TRUE) / sum(valid, na.rm = TRUE), NA),
    valid_voteShare_state_incumbent = ifelse(sum(valid, na.rm = TRUE) > 0, 
                                             sum(state_incumbent_vote, na.rm = TRUE) / sum(valid, na.rm = TRUE), NA),
    valid_voteShare_PRI = ifelse(sum(valid, na.rm = TRUE) > 0, 
                                 sum(PRI_vote, na.rm = TRUE) / sum(valid, na.rm = TRUE), NA),
    valid_voteShare_PRD = ifelse(sum(valid, na.rm = TRUE) > 0, 
                                 sum(PRD_vote, na.rm = TRUE) / sum(valid, na.rm = TRUE), NA),
    valid_voteShare_PAN = ifelse(sum(valid, na.rm = TRUE) > 0, 
                                 sum(PAN_vote, na.rm = TRUE) / sum(valid, na.rm = TRUE), NA),
    valid_voteShare_MORENA = ifelse(sum(valid, na.rm = TRUE) > 0, 
                                    sum(MORENA_vote, na.rm = TRUE) / sum(valid, na.rm = TRUE), NA),
    valid_voteShare_runnerup = ifelse(sum(valid, na.rm = TRUE) > 0, 
                                      sum(runnerup_vote, na.rm = TRUE) / sum(valid, na.rm = TRUE), NA),
    
    # Turnout at municipal level
    turnout = ifelse(sum(listanominal, na.rm = TRUE) > 0, 
                     sum(total, na.rm = TRUE) / sum(listanominal, na.rm = TRUE), NA)
  ) %>%
  ungroup()



##### CORRELATION - INE #####

# Left join `ine_db` to `db` on `year`, `uniqueid`, and `section`
corr_test_ine <- db %>%
  left_join(ine_db %>% select(year, uniqueid, section, turnout_ine), by = c("year", "uniqueid", "section")) 

# Run a linear regression with `turnout` as the dependent variable and `turnout_ine` as the independent variable
regression_model <- lm(turnout ~ turnout_ine, data = corr_test_ine)
stargazer::stargazer(regression_model, type = "text")
correlation <- cor(corr_test_ine$turnout, corr_test_ine$turnout_ine, use = "complete.obs")
correlation




##### Correlation Magar Municipal #####


corr_test_magar_mun <- db_mun%>%
  left_join(magar_db %>% select(uniqueid, year, magar_turnout, magar_listanominal_voteShare_PRI, magar_listanominal_voteShare_PAN,magar_listanominal_voteShare_PRD,magar_listanominal_voteShare_MORENA), 
            by = c("uniqueid", "year"))

# Calculate the correlation between 'turnout_magar' and 'turnout'
correlation_result <- cor(corr_test_magar_mun$magar_turnout, corr_test_magar_mun$turnout, use = "complete.obs")
# Print the correlation result
correlation_result

# Calculate the correlation between 'turnout_magar' and 'turnout'
correlation_result <- cor(corr_test_magar_mun$magar_listanominal_voteShare_PRI, corr_test_magar_mun$listanominal_voteShare_PRI, use = "complete.obs")
# Print the correlation result
correlation_result

# Calculate the correlation between 'turnout_magar' and 'turnout'
correlation_result <- cor(corr_test_magar_mun$magar_listanominal_voteShare_PAN, corr_test_magar_mun$listanominal_voteShare_PAN, use = "complete.obs")
# Print the correlation result
correlation_result

# Calculate the correlation between 'turnout_magar' and 'turnout'
correlation_result <- cor(corr_test_magar_mun$magar_listanominal_voteShare_PRD, corr_test_magar_mun$listanominal_voteShare_PRD, use = "complete.obs")
# Print the correlation result
correlation_result

# Calculate the correlation between 'turnout_magar' and 'turnout'
correlation_result <- cor(corr_test_magar_mun$magar_listanominal_voteShare_MORENA, corr_test_magar_mun$listanominal_voteShare_MORENA, use = "complete.obs")
# Print the correlation result
correlation_result

# Load the required package
library(stargazer)

# Create linear models to represent the correlations
model_turnout <- lm(turnout ~ magar_turnout, data = corr_test_magar_mun)
model_PRI <- lm(listanominal_voteShare_PRI ~ magar_listanominal_voteShare_PRI, data = corr_test_magar_mun)
model_PAN <- lm(listanominal_voteShare_PAN ~ magar_listanominal_voteShare_PAN, data = corr_test_magar_mun)
model_PRD <- lm(listanominal_voteShare_PRD ~ magar_listanominal_voteShare_PRD, data = corr_test_magar_mun)
model_MORENA <- lm(listanominal_voteShare_MORENA ~ magar_listanominal_voteShare_MORENA, data = corr_test_magar_mun)

# Use stargazer to display the correlations in a regression table format
stargazer(model_turnout, model_PRI, model_PAN, model_PRD, model_MORENA, type = "text",
          column.labels = c("Turnout", "PRI", "PAN", "PRD", "MORENA"),
          dep.var.labels = "Correlation")



##### GRAPHS #####
####Proportion of Municipalities with at Least Electoral Coalition Over time#####
# Calculate proportion of municipalities with at least one coalition per year
graph1 <- db %>%
  # Identify coalition presence by uniqueid and year
  group_by(year, uniqueid) %>%
  mutate(underscore_count = ifelse(str_count(party_component, "_") > 0 & !str_starts(party_component, "CI_1"), 1, 0)) %>%
  summarise(has_coalition = ifelse(sum(underscore_count, na.rm = TRUE) > 0, 1, 0)) %>%
  ungroup() %>%
  # Calculate proportion of municipalities with coalitions each year
  group_by(year) %>%
  summarise(proportion_with_coalition = mean(has_coalition, na.rm = TRUE))


ggplot(graph1, aes(x = year, y = proportion_with_coalition)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Proportion of Municipalities with Coalitions",
       title = "Proportion of Municipalities with at Least One Electoral Coalition Over Time") #+
  # theme_minimal()  +
  # theme(axis.title.x = element_blank(),
  #       axis.title.y = element_blank())



####Trend of Avergae Number of Parties in electoral coalitions over time#####
# Calculate average coalition size per year
graph2 <- db %>%
  # Identify and count coalition size based on underscores in party_component
  mutate(coalition_size = ifelse(str_count(party_component, "_") > 0 & !str_starts(party_component, "CI_1"),
                                 str_count(party_component, "_") + 1, NA)) %>%
  # Keep only rows with coalition sizes
  filter(!is.na(coalition_size)) %>%
  # Group by year to get the average coalition size per year
  group_by(year) %>%
  summarise(average_coalition_size = mean(coalition_size, na.rm = TRUE))

# Plot the average coalition size over time
ggplot(graph2, aes(x = year, y = average_coalition_size)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Average Number of Parties in Coalition",
       title = "Trend in the Average Number of Parties in Electoral Coalitions Over Time") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

##### Heatmap #####

# Step 1: Add state_code based on uniqueid length
db <- db %>%
  mutate(state_code = case_when(
    nchar(as.character(uniqueid)) == 4 ~ substr(uniqueid, 1, 1),  # First digit for 4-character uniqueid
    nchar(as.character(uniqueid)) == 5 ~ substr(uniqueid, 1, 2),  # First two digits for 5-character uniqueid
    TRUE ~ NA_character_  # Handle unexpected cases
  ))

# Step 2: Create a mapping for state names
state_names <- c(
  "1" = "Aguascalientes", "2" = "Baja California", "3" = "Baja California Sur",
  "4" = "Campeche", "5" = "Coahuila", "6" = "Colima", "7" = "Chiapas", 
  "8" = "Chihuahua", "10" = "Durango", "11" = "Guanajuato", 
  "12" = "Guerrero", "13" = "Hidalgo", "14" = "Jalisco", "15" = "México (State of)", 
  "16" = "Michoacán", "17" = "Morelos", "18" = "Nayarit", "19" = "Nuevo León", 
  "20" = "Oaxaca", "21" = "Puebla", "22" = "Querétaro", "23" = "Quintana Roo", 
  "24" = "San Luis Potosí", "25" = "Sinaloa", "26" = "Sonora", "27" = "Tabasco", 
  "28" = "Tamaulipas", "29" = "Tlaxcala", "30" = "Veracruz", "31" = "Yucatán", 
  "32" = "Zacatecas"
)

# Step 3: Summarize to check for elections held
held_elections_data <- db %>%
  group_by(state_code, year) %>%
  summarize(elections_held = any(!is.na(incumbent_vote) & incumbent_vote > 0), .groups = 'drop')

# Step 4: Convert elections_held to numeric (0 or 1)
held_elections_data$elections_held <- as.numeric(held_elections_data$elections_held)

# Step 5: Replace state codes with state names
held_elections_data <- held_elections_data %>%
  mutate(state_name = state_names[as.character(state_code)]) %>%
  filter(!is.na(state_name))  # Remove any unmatched state codes

# Step 6: Plot the data as a binary heatmap with discrete colors (black/white) and a black border around the white legend box
ggplot(held_elections_data, aes(x = year, y = reorder(state_name, as.numeric(state_code)), fill = factor(elections_held))) +
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
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.text.y = element_text(size = 8))  # Adjust y-axis label size if needed


######Stacked Area Chart of Party Vote Shares Over Time #####

# Normalize vote shares for each year
party_data <- db %>%
  group_by(year, party_component) %>%
  summarise(total_vote = sum(incumbent_vote, na.rm = TRUE), .groups = 'drop') %>%
  group_by(year) %>%
  mutate(vote_share = total_vote / sum(total_vote, na.rm = TRUE)) %>%
  ungroup()

# Plot the stacked area chart
ggplot(party_data, aes(x = year, y = vote_share, fill = party_component)) +
  geom_area(position = "stack") +
  labs(x = "Year", y = "Vote Share",
       title = "Vote Share by Party Over Time") +
  theme_minimal()

#Using valid voteshare
party_data1 <- db %>%
  group_by(year, party_component) %>%
  summarise(total_vote = sum(incumbent_vote, na.rm = TRUE), 
            vote_share = sum(valid_voteShare_incumbent_vote, na.rm = TRUE), 
            .groups = 'drop') %>%
  ungroup()

# Plot the stacked area chart
ggplot(party_data1, aes(x = year, y = vote_share, fill = party_component)) +
  geom_area(position = "stack") +
  labs(x = "Year", y = "Vote Share",
       title = "Vote Share by Party Over Time") +
  theme_minimal()


