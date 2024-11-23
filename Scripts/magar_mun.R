rm(list = ls())

library(tidyverse)
library(readxl)

# Get the path of the current script
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the root of the repository
# Assuming your script is in 'Scripts/Script States/', go two levels up
setwd(file.path(script_dir, "../"))

db70 <- read_excel("Data/municipal magar data splitcoal/mu-coalSplit1970s.xlsx")
db80 <- read_excel("Data/municipal magar data splitcoal/mu-coalSplit1980s.xlsx")
db90 <- read_excel("Data/municipal magar data splitcoal/mu-coalSplit1990s.xlsx")
db00 <- read_excel("Data/municipal magar data splitcoal/mu-coalSplit2000s.xlsx")
db10 <- read_excel("Data/municipal magar data splitcoal/mu-coalSplit2010s.xlsx")
db20 <- read_excel("Data/municipal magar data splitcoal/mu-coalSplit2020s.xlsx")

db <- rbind(db70,db80)
db <- rbind(db, db90)
db <- rbind(db,db00)
db <- rbind(db,db10)
db <- rbind(db,db20)

db <- db %>% 
 select(-emm,-edon,-ife,-date,-ncand,-ncoal)

# Define identifier variables
id_vars <- c("yr", "mun", "inegi")

# Process each pair manually
pair_01 <- db %>%
  select(all_of(c(id_vars, "l01", "v01"))) %>%
  filter(!is.na(l01)) %>%
  pivot_wider(
    names_from = l01,
    values_from = v01,
    values_fn = ~ paste(.x, collapse = ", ")
  )

pair_02 <- db %>%
  select(all_of(c(id_vars, "l02", "v02"))) %>%
  filter(!is.na(l02)) %>%
  pivot_wider(
    names_from = l02,
    values_from = v02,
    values_fn = ~ paste(.x, collapse = ", ")
  )

pair_03 <- db %>%
  select(all_of(c(id_vars, "l03", "v03"))) %>%
  filter(!is.na(l03)) %>%
  pivot_wider(
    names_from = l03,
    values_from = v03,
    values_fn = ~ paste(.x, collapse = ", ")
  )

pair_04 <- db %>%
  select(all_of(c(id_vars, "l04", "v04"))) %>%
  filter(!is.na(l04)) %>%
  pivot_wider(
    names_from = l04,
    values_from = v04,
    values_fn = ~ paste(.x, collapse = ", ")
  )

pair_05 <- db %>%
  select(all_of(c(id_vars, "l05", "v05"))) %>%
  filter(!is.na(l05)) %>%
  pivot_wider(
    names_from = l05,
    values_from = v05,
    values_fn = ~ paste(.x, collapse = ", ")
  )

pair_06 <- db %>%
  select(all_of(c(id_vars, "l06", "v06"))) %>%
  filter(!is.na(l06)) %>%
  pivot_wider(
    names_from = l06,
    values_from = v06,
    values_fn = ~ paste(.x, collapse = ", ")
  )

pair_07 <- db %>%
  select(all_of(c(id_vars, "l07", "v07"))) %>%
  filter(!is.na(l07)) %>%
  pivot_wider(
    names_from = l07,
    values_from = v07,
    values_fn = ~ paste(.x, collapse = ", ")
  )

pair_08 <- db %>%
  select(all_of(c(id_vars, "l08", "v08"))) %>%
  filter(!is.na(l08)) %>%
  pivot_wider(
    names_from = l08,
    values_from = v08,
    values_fn = ~ paste(.x, collapse = ", ")
  )

pair_09 <- db %>%
  select(all_of(c(id_vars, "l09", "v09"))) %>%
  filter(!is.na(l09)) %>%
  pivot_wider(
    names_from = l09,
    values_from = v09,
    values_fn = ~ paste(.x, collapse = ", ")
  )

pair_10 <- db %>%
  select(all_of(c(id_vars, "l10", "v10"))) %>%
  filter(!is.na(l10)) %>%
  pivot_wider(
    names_from = l10,
    values_from = v10,
    values_fn = ~ paste(.x, collapse = ", ")
  )

# Continue similarly for all pairs (l11/v11 through l23/v23)...

# Step 2: Merge pairs one by one
merged_data <- full_join(pair_01, pair_02, by = id_vars)
merged_data <- full_join(merged_data, pair_03, by = id_vars)
merged_data <- full_join(merged_data, pair_04, by = id_vars)
merged_data <- full_join(merged_data, pair_05, by = id_vars)
merged_data <- full_join(merged_data, pair_06, by = id_vars)
merged_data <- full_join(merged_data, pair_07, by = id_vars)
merged_data <- full_join(merged_data, pair_08, by = id_vars)
merged_data <- full_join(merged_data, pair_09, by = id_vars)
merged_data <- full_join(merged_data, pair_10, by = id_vars)
# Continue merging for remaining pairs...

# Step 3: Combine `.x` and `.y` columns dynamically
for (col in names(merged_data)) {
  if (grepl("\\.x$", col)) {
    base_col <- sub("\\.x$", "", col)  # Get the base column name (e.g., 'pri')
    merged_data[[base_col]] <- coalesce(merged_data[[col]], merged_data[[paste0(base_col, ".y")]])
  }
}

# Step 4: Remove `.x` and `.y` columns
merged_data <- merged_data %>%
  select(-ends_with(".x"), -ends_with(".y"))

# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Data/municipal magar data splitcoal")
output_path <- file.path(output_dir, "magar_mun_votes.csv")
 
# Use write_csv to save the file
write_csv(merged_data, output_path)
