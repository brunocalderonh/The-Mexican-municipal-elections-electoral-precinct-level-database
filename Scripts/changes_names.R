CHANGES



#1.process raw elec data

data.table::fwrite(bajasur_all,"../../../Processed Data/bajasur/bajasur_process_raw_data.csv")


#2.Vote manipulation

db <- read_csv("Processed Data/bajasur/bajasur_process_raw_data.csv")

…..

# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Processed Data/bajasur")
output_path <- file.path(output_dir, "bajasur_vote_manipulation.csv")

# Use write_csv to save the file
write_csv(db, output_path)

# Confirm file saved correctly
cat("File saved at:", output_path)

#3. incumbent_manipulator

vote_db <- read_csv("Processed Data/bajasur/bajasur_vote_manipulation.csv")


….


# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Processed Data/bajasur")
output_path <- file.path(output_dir, "bajasur_incumbent_manipulator.csv")

# Use write_csv to save the file
write_csv(final_merged_data, output_path)

# Confirm file saved correctly
cat("File saved at:", output_path)


#4. Vote calculator

finaldb <- read_csv("Processed Data/bajasur/bajasur_incumbent_manipulator.csv")


…..
…..



# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Processed Data/bajasur")
output_path <- file.path(output_dir, "bajasur_vote_calculator.csv")

# Use write_csv to save the file
write_csv(finaldb, output_path)

# Confirm file saved correctly
cat("File saved at:", output_path)


#5. Final

og <- read.csv("Processed Data/bajasur/bajasur_vote_calculator.csv")


….


# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Processed Data/bajasur")
output_path <- file.path(output_dir, "bajasur_final.csv")

# Use write_csv to save the file
write_csv(merged_data, output_path)

# Confirm file saved correctly
cat("File saved at:", output_path)

