# Mexican Electoral Database: Precinct-Level Analysis (1994-2019)

This repository provides a complete workflow for processing, cleaning, and analyzing precinct-level electoral data for 31 Mexican states (excluding Mexico City) between 1994 and 2019. The data processing pipeline is implemented in R, with each state’s data processed independently before merging into a consolidated dataset.

## Folder Structure

### `Data`
This folder contains all the raw and auxiliary data required to process and generate the intermediate datasets as well as the final processed databases.

#### Subfolders:
1. **`extraordinary_elections`**  
   - `correct_extra_elec_final.csv`: Lists extraordinary and normal elections that are removed during processing.  
   - `diff_year_extra_elec.csv` & `diff_year_extra_elec_flag.csv`: Files necessary for processing extraordinary elections held in different years.  

2. **`collapsed_database_manual_cases`**  
   - `state_collapsed_edited.csv`: Manually sourced information about incumbents whose new electoral coalitions differ from, or are not a superset of, their coalitions in previous elections.
   - 
3. **`Raw_Electoral_Data.csv`**  
   - Contains the raw electoral data used to initiate state-level processing via the `process_raw_electoral_data.R` script.

4. **`incumbent_data`**  
   - Contains data on incumbents, used during the construction of state-level databases.

5. **`municipal_magar_data_splitcoal`**  
   - Contains municipal-level Magar data [1], used for correlation analysis and to construct municipal-level incumbent and runner-up data in `FINAL_db_formation.R`.

---

### `Processed Data`
This folder organizes the processed data for each of Mexico's 31 states, produced by sequential execution of the script files. 

#### Subfolders and Key Files:
1. **State Subfolders:**  
   - Each state's folder contains outputs from the scripts, saved sequentially:
     - `state_process_raw_data.csv`: Produced by `process_raw_electoral_data.R`.
     - `state_vote_manipulation.csv`: Produced by `vote_manipulation.R`.
     - `state_incumbent_manipulator.csv`: Produced by `incumbent_manipulation.R`.
     - `state_vote_calculator.csv`: Produced by `incumbent_vote_calculator.R`.
     - `state_final.csv`: Produced by `final.R`.

2. **Shared Files:**
   - `duplicate_cases.csv`: Used to address minor inconsistencies during the final merge.
   - `coalition_dic.csv`: Contains all coalitions present in the dataset.

---

### `Final Data`
This folder contains the consolidated dataset merging all 31 states:
- `all_states_final.zip`: The final dataset after processing all states, compressed for distribution.

---

### `Correlation Data`
This folder contains data and scripts used for the correlation analysis during technical validation.

#### Subfolders:
1. **`turnout_magar`**  
   - Contains Magar data [1], processed by `magar_mun.R` for correlation.

2. **`turnout_ine`**  
   - Contains INE data processed by `ine_mun.R` for correlation.

3. **`generated_data`**  
   - Includes the outputs `ine_turnout.csv` and `magar_turnout.csv`, used in `graphs_correlation.R`.

---

### `Scripts`
This folder contains all R scripts used for data processing and analysis.

#### State-Specific Scripts:
1. **`process_raw_electoral_data.R`**  
   - Imports precinct-level electoral data, standardizes variables, and aggregates polling booth-level data to the precinct level. Produces `state_process_raw_data.csv`. The municipal electoral precinct-level data for 31 states of Mexico (excluding the Federal District) was originally sourced by  Larreguy (2012) [3], Marshall (2023) [4], and Enríquez et al. (2024) [5]. 

2. **`vote_manipulation.R`**  
   - Cleans and selects relevant variables (e.g., municipality/state names, codes, votes). For some states, incorporates `correct_extra_elec_final.csv`, `diff_year_extra_elec.csv`, and `diff_year_extra_elec_flag.csv`. Produces `state_vote_manipulation.csv`.

3. **`incumbent_manipulation.R`**  
   - Processes incumbent and runner-up data from multiple sources, including Magar [1] and SNIM [2], and merges it into the precinct-level dataset. Produces `state_incumbent_manipulator.csv`.

4. **`incumbent_vote_calculator.R`**  
   - Calculates precinct-level votes for incumbents and runner-ups, standardizing party names and handling coalitions. Produces `state_vote_calculator.csv`.

5. **`final.R`**  
   - Computes votes for major parties (PRI, PAN, MORENA, PRD) and state incumbents. Integrates manually researched data from `state_collapsed_edited.csv`. Produces `state_final.csv`.

6. **`FINAL_db_formation.R`**  
   - Merges all 31 state datasets into a single dataset. Computes vote shares for incumbents, runner-ups, and major parties, relative to valid votes and registered voters. Outputs `all_states_final.zip`.

---

### `correlation`
Contains scripts for technical validation:
1. **`ine_turnout.R`**  
   - Generates `ine_turnout.csv` using INE data.
2. **`magar_turnout.R`**  
   - Generates `magar_turnout.csv` using Magar data [1].
3. **`graphs_correlations.R`**  
   - Produces correlation graphs and tests using `ine_turnout.csv` and `magar_turnout.csv`.

---

### `README.md`
This file provides detailed instructions for replicating the data processing workflow.

---

## Execution Order

For each of the 31 states:
1. `process_raw_electoral_data.R`
2. `vote_manipulation.R`
3. `incumbent_manipulation.R`
4. `incumbent_vote_calculator.R`
5. `final.R`

After all states:
6. `FINAL_db_formation.R`

---

## References
1. Magar, E. (2018). *Recent Mexican Election Vote Returns*. Retrieved from [https://github.com/emagar/elecRetrns](https://github.com/emagar/elecRetrns). Last revised on March 10, 2023.
2. SNIM (2024). *Sistema Nacional de Información Municipal*. Retrieved from [http://www.snim.rami.gob.mx/](http://www.snim.rami.gob.mx/).
3. Larreguy, H. A. (2012). *Monitoring Political Brokers: Evidence from Clientelistic Networks in Mexico*. Available at SSRN: [https://ssrn.com/abstract=2225027](https://ssrn.com/abstract=2225027).
4. Marshall, J. (2023). *Political Information Cycles: When Do Voters Sanction Incumbent Parties for High Homicide Rates?* [Working Paper](https://john-l-marshall.github.io/files/tuning_in_voting_out.pdf).
5. Enríquez, J. R., Larreguy, H., Marshall, J., & Simpser, A. (2024). *Mass Political Information on Social Media: Facebook Ads, Electorate Saturation, and Electoral Accountability in Mexico*. *Journal of the European Economic Association*.
