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
library(stargazer)



# Get the path of the current script
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the root of the repository
# Assuming your script is in 'Scripts/Script States/', go two levels up
setwd(file.path(script_dir, "../../"))

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


#COMPUTE BASIC STATS OF ELECTIONS AND SUCH
# Number of unique municipal elections (unique combinations of `mun_code` and `year`)
num_municipal_elections <- nrow(unique(db[c("mun_code", "year")]))
cat("Number of unique municipal elections:", num_municipal_elections, "\n")

# Number of unique municipalities (`mun_code`)
num_unique_municipalities <- length(unique(db$mun_code))
cat("Number of unique municipalities:", num_unique_municipalities, "\n")

# Number of unique electoral precincts, considering both `uniqueid` and `section`
num_unique_precincts <- nrow(unique(db[c("state_code", "precinct")]))
cat("Number of unique electoral precincts:", num_unique_precincts, "\n")

#COMPUTE STATS OF DIFFERENT CASES OF INCUMBENT CALCULATIONS
# Assuming your data frame is named 'db'

# Case 1: incumbent_party is a single party and incumbent_party_component is either a single party or a coalition (no _ in incumbent_party)
case1_df <- db %>%
  filter(
    !grepl("_", incumbent_party) & # Single party in incumbent_party
      (grepl("_", incumbent_party_component) | !grepl("_", incumbent_party_component)) & # Single party or coalition in incumbent_party_component
      is.na(researched_incumbent) # Exclude observations with a value in researched_incumbent
  )

num_municipal_elections_case1 <- case1_df %>%
  distinct(mun_code, year) %>%
  nrow()

percent_precincts_affected_case1 <- (nrow(case1_df) / nrow(db)) * 100

cat("Case 1:\n")
cat("Number of municipal elections:", num_municipal_elections_case1, "\n")
cat("Percentage of precincts affected:", percent_precincts_affected_case1, "%\n\n")

# Case 2: incumbent_party is a coalition (contains "_") and incumbent_party_component is the same or a bigger coalition
case2_df <- db %>%
  filter(
    grepl("_", incumbent_party) & # Coalition in incumbent_party
      grepl(incumbent_party, incumbent_party_component, fixed = TRUE) & # Same or bigger coalition in incumbent_party_component
      is.na(researched_incumbent) # Exclude observations with a value in researched_incumbent
  )

num_municipal_elections_case2 <- case2_df %>%
  distinct(mun_code, year) %>%
  nrow()

percent_precincts_affected_case2 <- (nrow(case2_df) / nrow(db)) * 100

cat("Case 2:\n")
cat("Number of municipal elections:", num_municipal_elections_case2, "\n")
cat("Percentage of precincts affected:", percent_precincts_affected_case2, "%\n\n")



# Case 3: Valid value in researched_incumbent and incumbent_vote (manually researched cases with vote values found)
case3_df <- db %>%
  filter(
    !is.na(researched_incumbent) &
      !is.na(incumbent_vote)
  )

num_municipal_elections_case3 <- case3_df %>%
  distinct(mun_code, year) %>%
  nrow()

percent_precincts_affected_case3 <- (nrow(case3_df) / nrow(db)) * 100

cat("Case 3:\n")
cat("Number of municipal elections:", num_municipal_elections_case3, "\n")
cat("Percentage of precincts affected:", percent_precincts_affected_case3, "%\n\n")



# Case 4: Valid value in researched_incumbent and no value in incumbent_vote (manual cases for which we did not find a valid match)
case4_df <- db %>%
  filter(
    !is.na(researched_incumbent) &
      is.na(incumbent_vote)
  )

num_municipal_elections_case4 <- case4_df %>%
  distinct(mun_code, year) %>%
  nrow()

percent_precincts_affected_case4 <- (nrow(case4_df) / nrow(db)) * 100

cat("Case 4:\n")
cat("Number of municipal elections:", num_municipal_elections_case4, "\n")
cat("Percentage of precincts affected:", percent_precincts_affected_case4, "%\n")


# Case 5: NA or 0 in incumbent_vote and no valid value in researched_incumbent
case5_df <- db %>%
  filter(
    (is.na(incumbent_vote) | incumbent_vote == 0)
  )

num_municipal_elections_case5 <- case5_df %>%
  distinct(mun_code, year) %>%
  nrow()

percent_precincts_affected_case5 <- (nrow(case5_df) / nrow(db)) * 100

cat("Case 5:\n")
cat("Number of municipal elections:", num_municipal_elections_case5, "\n")
cat("Percentage of precincts affected:", percent_precincts_affected_case5, "%\n")



#LOAD DATABASES FOR CORRELATION CALCULATIONS

#Magar 
magar_db <- read.csv("Correlation Data/generated_data/magar_turnout.csv") %>% 
  rename("mun_code" = "uniqueid",
         "registered_voters"="listanominal") %>% 
  mutate(magar_share_PRI_registered_voters =ifelse(registered_voters> 0, pri / registered_voters, NA)) %>% 
  mutate(magar_share_PAN_registered_voters =ifelse(registered_voters> 0, pan / registered_voters, NA)) %>% 
  mutate(magar_share_PRD_registered_voters =ifelse(registered_voters> 0, prd / registered_voters, NA)) %>% 
  mutate(magar_share_MORENA_registered_voters =ifelse(registered_voters> 0, morena / registered_voters, NA)) 
  

#aggregate to mun level for corr with magar 
# Aggregating at the municipal level
db_mun <- db %>%
  group_by(mun_code, year) %>%
  summarise(
    # Calculations with registered_voters
    share_incumbent_registered_voters = ifelse(sum(registered_voters, na.rm = TRUE) > 0, 
                                              sum(incumbent_vote, na.rm = TRUE) / sum(registered_voters, na.rm = TRUE), NA),
    share_state_incumbent_registered_voters = ifelse(sum(registered_voters, na.rm = TRUE) > 0, 
                                                    sum(state_incumbent_vote, na.rm = TRUE) / sum(registered_voters, na.rm = TRUE), NA),
    share_PRI_registered_voters = ifelse(sum(registered_voters, na.rm = TRUE) > 0, 
                                        sum(PRI_vote, na.rm = TRUE) / sum(registered_voters, na.rm = TRUE), NA),
    share_PRD_registered_voters = ifelse(sum(registered_voters, na.rm = TRUE) > 0, 
                                        sum(PRD_vote, na.rm = TRUE) / sum(registered_voters, na.rm = TRUE), NA),
    share_PAN_registered_voters = ifelse(sum(registered_voters, na.rm = TRUE) > 0, 
                                        sum(PAN_vote, na.rm = TRUE) / sum(registered_voters, na.rm = TRUE), NA),
    share_MORENA_registered_voters = ifelse(sum(registered_voters, na.rm = TRUE) > 0, 
                                           sum(MORENA_vote, na.rm = TRUE) / sum(registered_voters, na.rm = TRUE), NA),
    share_runnerup_registered_voters = ifelse(sum(registered_voters, na.rm = TRUE) > 0, 
                                             sum(runnerup_vote, na.rm = TRUE) / sum(registered_voters, na.rm = TRUE), NA),
    
    # New Calculations with valid
    share_incumbent_valid_vote  = ifelse(sum(valid, na.rm = TRUE) > 0, 
                                       sum(incumbent_vote, na.rm = TRUE) / sum(valid, na.rm = TRUE), NA),
    share_state_incumbent_valid_vote = ifelse(sum(valid, na.rm = TRUE) > 0, 
                                             sum(state_incumbent_vote, na.rm = TRUE) / sum(valid, na.rm = TRUE), NA),
    share_PRI_valid_vote = ifelse(sum(valid, na.rm = TRUE) > 0, 
                                 sum(PRI_vote, na.rm = TRUE) / sum(valid, na.rm = TRUE), NA),
    share_PRD_valid_vote = ifelse(sum(valid, na.rm = TRUE) > 0, 
                                 sum(PRD_vote, na.rm = TRUE) / sum(valid, na.rm = TRUE), NA),
    share_PAN_valid_vote = ifelse(sum(valid, na.rm = TRUE) > 0, 
                                 sum(PAN_vote, na.rm = TRUE) / sum(valid, na.rm = TRUE), NA),
    share_MORENA_valid_vote = ifelse(sum(valid, na.rm = TRUE) > 0, 
                                    sum(MORENA_vote, na.rm = TRUE) / sum(valid, na.rm = TRUE), NA),
    share_runnerup_valid_vote = ifelse(sum(valid, na.rm = TRUE) > 0, 
                                      sum(runnerup_vote, na.rm = TRUE) / sum(valid, na.rm = TRUE), NA),
    # Turnout at municipal level
    turnout = ifelse(sum(registered_voters, na.rm = TRUE) > 0, 
                     sum(total, na.rm = TRUE) / sum(registered_voters, na.rm = TRUE), NA)
  ) %>%
  ungroup()




##### Correlation Magar Municipal #####


corr_test_magar_mun <- db_mun%>%
  left_join(magar_db %>% select(mun_code, year, magar_turnout, magar_share_PRI_registered_voters, magar_share_PAN_registered_voters,magar_share_PRD_registered_voters,magar_share_MORENA_registered_voters), 
            by = c("mun_code", "year"))

# Calculate the correlation between 'turnout_magar' and 'turnout'
correlation_result <- cor(corr_test_magar_mun$magar_turnout, corr_test_magar_mun$turnout, use = "complete.obs")
# Print the correlation result
correlation_result

# Calculate the correlation between 'turnout_magar' and 'turnout'
correlation_result <- cor(corr_test_magar_mun$magar_share_PRI_registered_voters, corr_test_magar_mun$share_PRI_registered_voters, use = "complete.obs")
# Print the correlation result
correlation_result

# Calculate the correlation between 'turnout_magar' and 'turnout'
correlation_result <- cor(corr_test_magar_mun$magar_share_PAN_registered_voters, corr_test_magar_mun$share_PAN_registered_voters, use = "complete.obs")
# Print the correlation result
correlation_result

# Calculate the correlation between 'turnout_magar' and 'turnout'
correlation_result <- cor(corr_test_magar_mun$magar_share_PRD_registered_voters, corr_test_magar_mun$share_PRD_registered_voters, use = "complete.obs")
# Print the correlation result
correlation_result

# Calculate the correlation between 'turnout_magar' and 'turnout'
correlation_result <- cor(corr_test_magar_mun$magar_share_MORENA_registered_voters, corr_test_magar_mun$share_MORENA_registered_voters, use = "complete.obs")
# Print the correlation result
correlation_result

# Create linear models to represent the correlations
model_turnout <- lm(turnout ~ magar_turnout, data = corr_test_magar_mun)
model_PRI <- lm(share_PRI_registered_voters ~ magar_share_PRI_registered_voters, data = corr_test_magar_mun)
model_PAN <- lm(share_PAN_registered_voters ~ magar_share_PAN_registered_voters, data = corr_test_magar_mun)
model_PRD <- lm(share_PRD_registered_voters ~ magar_share_PRD_registered_voters, data = corr_test_magar_mun)
model_MORENA <- lm(share_MORENA_registered_voters ~ magar_share_MORENA_registered_voters, data = corr_test_magar_mun)

# Use stargazer to display the correlations in a regression table format
stargazer(model_turnout, model_PRI, model_PAN, model_PRD, model_MORENA, type = "text",
          column.labels = c("Turnout", "PRI", "PAN", "PRD", "MORENA"),
          dep.var.labels = "Correlation")




##### GRAPHS #####

####Proportion of Municipalities with at Least Electoral Coalition Over time####
graph <- db %>%
  # Identify coalition presence by mun_code and year
  group_by(year, mun_code) %>%
  mutate(underscore_count = ifelse(str_count(incumbent_party_component, "_") > 0 & !str_starts(incumbent_party_component, "CI_1"), 1, 0)) %>%
  summarise(has_coalition = ifelse(sum(underscore_count, na.rm = TRUE) > 0, 1, 0)) %>%
  ungroup() %>%
  # Calculate the total number of municipalities per year
  group_by(year) %>%
  summarise(
    total_municipalities = n(),
    coalition_municipalities = sum(has_coalition, na.rm = TRUE),
    proportion_with_coalition = coalition_municipalities / total_municipalities
  )

# Plot with x-axis showing every 5 years
ggplot(graph, aes(x = year, y = proportion_with_coalition)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(min(graph$year, na.rm = TRUE), max(graph$year, na.rm = TRUE), by = 5)) +  # Show only every 5 years
  labs(x = "", y = "") +
  theme_bw() +
  theme(axis.text.x = element_text( face = "bold", size = 15),  # Make x-axis (years) bold
        axis.text.y = element_text(size = 15, face = "bold")) # Increase font size of legend labels  # Make y-axis (state names) bold



##### Heatmap #####


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
  group_by(state_code, mun, year) %>%
  summarize(elections_held = any(!is.na(incumbent_vote) & incumbent_vote > 0), .groups = 'drop')

# Step 4: Convert elections_held to numeric (0 or 1)
held_elections_data$elections_held <- as.numeric(held_elections_data$elections_held)

# Step 5: Replace state codes with state names
held_elections_data <- held_elections_data %>%
  mutate(state_name = state_names[as.character(state_code)]) %>%
  filter(!is.na(state_name))  # Remove any unmatched state codes


# Step 6: Add a condition for "EXTRAORDINARIO" in the `mun` column
held_elections_data <- held_elections_data %>%
  mutate(
    fill_category = case_when(
      str_detect(mun, "EXTRAORDINARIO") ~ "Extraordinary Elections",
      elections_held == 1 ~ "Elections Held",
      TRUE ~ "No Elections"
    )
  )

# Step 7: Update the ggplot to use the new `fill_category` column
ggplot(held_elections_data, aes(x = year, y = reorder(state_name, desc(state_name)), fill = fill_category)) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c(
      "No Elections" = "white",
      "Elections Held" = "black",
      "Extraordinary Elections" = "red"
    ),
    name = "Elections Held"
  ) +
  scale_x_continuous(breaks = seq(min(held_elections_data$year), max(held_elections_data$year), by = 1)) +  # Display all years
  labs(x = "", y = "") +  # Remove axis labels
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, face = "bold", size = 13),  # Make x-axis (years) bold
    axis.text.y = element_text(size = 13, face = "bold"),  # Increase y-axis text size and make it bold
    legend.title = element_text(size = 13, face = "bold"),  # Make legend title bold and larger
    legend.text = element_text(size = 13)  # Increase font size of legend labels
  )

