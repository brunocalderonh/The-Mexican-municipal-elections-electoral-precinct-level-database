


For each of the 32 states, execute the R script files in the following order:

This folder includes a series of R scripts, organized sequentially, that perform data cleaning, transformation, and analysis. The key scripts are:

1. **`state_precinct_manipulation.R`**  
   This script processes electoral data from 2004 to 2019 by appending election results across different years and ensuring consistency in the structure of the datasets. For each year, the data is imported and variable names are standardized to maintain uniformity across datasets. The script aggregates election results at the municipality level by summing votes for all political parties and coalitions. It also calculates turnout rates by dividing total votes by the number of registered voters in each municipality. The results are collapsed by municipality, ensuring that valid vote counts are captured for further analysis. Each municipality is identified by a unique ID to ensure consistency when appending data from multiple election years, enabling the creation of a harmonized dataset.

2. **`vote_manipulation.R`**  
   From the previously created vote database, this script selects and organizes the desired variables for further cleaning. It retains key variables such as municipality name, state name, unique ID, year, total votes, eligible voters, and valid votes, while excluding unnecessary data. The script ensures that all party and coalition variables are preserved, which are essential for subsequent incumbent vote calculations.

3. **`incumbent_manipulation.R`**  
   This script merges incumbent party and candidate information with the vote data. It uses datasets from various sources to standardize incumbent information (party and candidate fields) and merges them at the municipal level using the unique ID. The resulting dataset integrates incumbent details and vote data, enabling analysis of the influence of incumbency on electoral outcomes.

4. **`incumbent_vote_calculator.R`**  
   This script calculates the incumbent and runner-up vote totals by linking incumbent party information to the vote data. It standardizes party names and handles both individual party and coalition votes. The script ensures that the vote totals for incumbents and runners-up are accurately calculated and linked to their respective parties, allowing for further electoral analysis.

5. **`final.R`**  
   This script generates the final version of the electoral data for each state by merging vote data with detailed incumbent and runner-up party information. It calculates the votes for key political parties—PRI, PAN, MORENA, and PRD—as well as the state-level incumbent votes. It also processes final vote counts at both the municipal and precinct levels, incorporating data from previously processed files. **Important**: Make sure to run the corresponding `state_collapsed_edited` file to prepare the collapsed data at the municipal level before running this script.


**                                                                                                                       **
   **Only after executing the files from steps 1 through 5 for each of the 32 states, then execute the R script in step 6 below:
**                                                                                                                       **
6. **`FINAL_db_formation.R`**  
   This is the final step that consolidates the electoral data for all 32 states into a single unified dataset. The script appends the data for each state, cleans it, and calculates key metrics such as voter turnout (the ratio of total votes to registered voters) and vote shares for the incumbent, runner-up, and major political parties (PRI, PAN, MORENA, PRD). The resulting dataset contains all necessary electoral information for further analysis at both the state and municipal levels across Mexico.
