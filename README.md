


For each of the 31 Mexican states (we exclude Mexico City), execute the R script files in the following order:

This folder includes a series of R scripts, organized sequentially, that perform data cleaning, transformation, and analysis. The key scripts are:

1. **`state_precinct_manipulation.R`**  
This script uses municipal electoral precinct-level data for 31 states of Mexico (excluding the Federal District) for which precinct-level data is available between 1994 and 2019, which was originally sourced by  Larreguy (2012) [3], Marshall (2023) [4], and Enríquez et al. (2024) [5]. The script imports each state-election file and standardizes their variable names. Whenever the electoral data was reported at the polling station level, the script aggregates the information at the electoral precinct level.
2. **`vote_manipulation.R`**  
This script cleans the data originating from manipulation.R by selecting the relevant variables. Specifically, it retains the following variables: municipality and state names (2-digit code for uniquely identifying municipalities), state code, municipality code (5-digit code for uniquely identifying municipalities), electoral precinct code (4-digit code for uniquely electoral precinct within each Mexican state), election year, valid votes cast, total votes cast, and registered voters. Furthermore, all votes cast for specific parties and electoral coalitions are kept. The script systematically excludes variables such as those related to state and municipality aggregated vote, winner identity, and rank-related metrics (first, second, third-place vote counts). These omitted variables are either redundant or not essential for this phase of data cleaning, where the focus is on retaining raw vote data for further processing. For some states this sript will include the execution of *correct\_extra\_elec\_final.csv* explained below, in order to handle extrarodinary elections.
  
   
    2.1. **`correct_extra_elec_final.csv`**  
  This file contains the curated cases of extraordinary elections we want to remove, and for some states the execution of this file is needed when running the above *vote_manipulation.R* script.
  
4. **`incumbent_manipulation.R`**  
This script processes and merges information at the municipal level about the identity and party or electoral coalition of the incumbents and runner-ups from several sources, including Magar (2023) [1] and the Sistema Nacional de Información Municipal (SNIM) [2]. Each dataset provides information about incumbents across various election years and states. The script keeps the relevant variables, standardizes the variable names across data sets, and then appends all the incumbent information into a single dataset. The resulting dataset is then merged into the electoral precinct-level dataset resulting from *vote_manipulation.R*.

5. **`incumbent_vote_calculator.R`**  
This script computes the electoral precinct-level votes for incumbent and runner-up parties by linking the identity of such parties or their electoral coalitions to the precinct-level vote data.  The script first standardizes party names for variables capturing both individual party and coalition votes.  It then merges a dataset that includes detailed, manually sourced incumbent and runner-up party information for the subset of elections where the new incumbent or runner-up electoral coalitions were not a superset of their coalitions in the previous elections. It then creates a variable capturing the votes cast for the incumbent party or its new electoral coalition and an analogous variable for the runner-up party.

7. **`final.R`**  
This script generates the final version of the electoral dataset for each state. It computes the votes for the main political parties—PRI, PAN, MORENA, and PRD—or its electoral coalitions. The script also computes the votes for the state incumbent party or its municipal electoral coalitions. Furthermore, the script integrates manually researched incumbent information through the *state\_collapsed\_edited.csv* file explained below.
  
   
   5.1 **`state_collapsed_edited.csv`**
   This directory contains manually curated files for cases where certain electoral coalitions to which the incumbent party belonged were not a superset of the coalitions it belonged to in the previous election. In these instances, manual research was conducted to identify the incumbent party in the incumbent coalition. Each manually researched case is substantiated with at least one reliable source.


**                                                                                                                       **
   **Only after executing the files from steps 1 through 5 for each of the 31 states, then execute the R script in step 6 below:**
**                                                                                                                       **

6. **`FINAL_db_formation.R`**  
This script compiles and merges the final datasets for the 31 Mexican states into a single dataset, and it determines a final incumbent by cross-referencing multiple data sources to ensure the proper identification of incumbent and runner-up parties. The script also computes the vote shares for the incumbent and runner-up coalitions, as well as for the major political parties (PRI, PAN, MORENA, PRD), both relative to valid votes and the voter registry. The dataset is then exported for analysis.

## References

1. Magar, E. (2018). Recent Mexican Election Vote Returns. Retrieved from [https://github.com/emagar/elecRetrns](https://github.com/emagar/elecRetrns). Last revised on March 10, 2023. ORCID: [https://orcid.org/0000-0002-6766-1677](https://orcid.org/0000-0002-6766-1677).

2. Sistema Nacional de Información Municipal. (2024). Sistema Nacional de Información Municipal (SNIM). Retrieved from [http://www.snim.rami.gob.mx/](http://www.snim.rami.gob.mx/). Data retrieved from multiple official sources including INEGI, CONAPO, and PNUD.

3. Larreguy, H. A. (2012). Monitoring Political Brokers: Evidence from Clientelistic Networks in Mexico. EPSA 2013 Annual General Conference Paper 655. Available at SSRN: [https://ssrn.com/abstract=2225027](https://ssrn.com/abstract=2225027).

4. Marshall, J. (2023). Political information cycles: When do voters sanction incumbent parties for high homicide rates? Working paper. Retrieved from [https://john-l-marshall.github.io/files/tuning_in_voting_out.pdf](https://john-l-marshall.github.io/files/tuning_in_voting_out.pdf).

5. Enríquez, J. R., Larreguy, H., Marshall, J., & Simpser, A. (2024). Mass Political Information on Social Media: Facebook Ads, Electorate Saturation, and Electoral Accountability in Mexico. *Journal of the European Economic Association*, 22(4), 1678-1722. doi: [10.1093/jeea/jvae011](https://doi.org/10.1093/jeea/jvae011).
