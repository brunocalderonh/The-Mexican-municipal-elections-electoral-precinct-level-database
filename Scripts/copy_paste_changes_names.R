#CHANGES



#1.process raw elec data

pacman::p_load (dplyr
                , haven
                , readstata13
                , readxl
                , tidyverse
                , tidyr
                , openxlsx
                , data.table)

...



data.table::fwrite(zacatecas_all,"../../../Processed Data/zacatecas/zacatecas_process_raw_data.csv")


#2.Vote manipulation

db <- read_csv("Processed Data/zacatecas/zacatecas_process_raw_data.csv")

...

# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Processed Data/zacatecas")
output_path <- file.path(output_dir, "zacatecas_vote_manipulation.csv")

# Use write_csv to save the file
write_csv(db, output_path)

# Confirm file saved correctly
cat("File saved at:", output_path)

#3. incumbent_manipulator

vote_db <- read_csv("Processed Data/zacatecas/zacatecas_vote_manipulation.csv")


….


# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Processed Data/zacatecas")
output_path <- file.path(output_dir, "zacatecas_incumbent_manipulator.csv")

# Use write_csv to save the file
write_csv(final_merged_data, output_path)

# Confirm file saved correctly
cat("File saved at:", output_path)


#4. Vote calculator

finaldb <- read_csv("Processed Data/zacatecas/zacatecas_incumbent_manipulator.csv")


…..
…..



# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Processed Data/zacatecas")
output_path <- file.path(output_dir, "zacatecas_vote_calculator.csv")

# Use write_csv to save the file
write_csv(finaldb, output_path)

# Confirm file saved correctly
cat("File saved at:", output_path)


#5. Final

# Read the Excel files
state <- read.csv("Data/incumbent data/incumbent JL/incumbent_state_JL.csv")
db <- read_excel("Data/collapsed database manual cases/zacatecas_collapsed_edited.xlsx")
og <- read.csv("Processed Data/zacatecas/zacatecas_vote_calculator.csv")
#create state ID for merging
og <- og %>%
  mutate(stateid = if_else(nchar(as.character(uniqueid)) == 4, 
                           as.numeric(substr(uniqueid, 1, 1)), 
                           as.numeric(substr(uniqueid, 1, 2)))) %>% 
  select(stateid, everything())

# Select the relevant columns from the collapsed database
db_subset <- db %>%
  select(uniqueid, year, PRI_vote, researched_incumbent, source_researched_incumbent)

#db_subset <- db_subset[!duplicated(db_subset), ]

# Merge the datasets based on uniqueid and year
merged_data <- og %>%
  left_join(db_subset, by = c("uniqueid", "year"))

# Select the relevant columns from the collapsed database
state_subset <- state %>%
  select(CVE_ENTIDAD, YEAR, PARTIDO_GOBERNADOR) %>% 
  rename(
    stateid = CVE_ENTIDAD,
    year = YEAR,
    state_incumbent_party = PARTIDO_GOBERNADOR
  ) %>% 
  group_by(stateid, year) %>%
  summarize(state_incumbent_party = first(state_incumbent_party), .groups = "drop")

merged_data <- merged_data %>%
  left_join(state_subset, by = c("stateid", "year")) %>% 
  select(-stateid)
….

# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Processed Data/zacatecas")
output_path <- file.path(output_dir, "zacatecas_final.csv")

# Use write_csv to save the file
write_csv(merged_data, output_path)

# Confirm file saved correctly
cat("File saved at:", output_path)

