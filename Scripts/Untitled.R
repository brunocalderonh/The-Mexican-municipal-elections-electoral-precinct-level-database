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

db_01 <- db %>% 
 select(-emm,-edon,-ife,-date,-ncand,-ncoal)

test_pivot <- db_01 %>%
  pivot_wider(
    names_from = starts_with("l"),       # Dynamically use all `l#` columns for unique column names
    values_from = starts_with("v"),     # Use all `v#` columns for values
    names_sep = "_",                    # Add a separator for clarity
    values_fn = ~ paste(.x, collapse = ", ")  # Combine repeated values
  )
