

Script File Dictionary


For each of the 32 states the following are the steps to follow:

1. state_precint_manipulation.R
   Takes elections from 2004 up until 2019 and manipulates and generates vote data in absolute terms.
   It identifies with a unique id each municipality from the state, aggregating and appending all elections.

3. vote_manipulation.R
   From the previously created vote database, this script organizes and selects the desired variables for further cleaning.

4. incumbent_manipulation.R
   This file then manipulates and runs the incumbent candidate information merging the incumbent party and candidate data into the vote data.
   
6. incumbent_vote_calculator.R
   Given the final values of incumbent parties and the corresponding vote values, the script proceeds to calculate incumbent vote.

7. final.R
   This script generates the final version of the incumbent vote data for each state. Its important to run the state_collapsed_edited file to be correctly execute the tasks in the script.


* After having done steps 1 through 7 for all 32 states then the final script comes at hand:
  
9. final_db_formation.R
    
    created a single dataframe  from the 32 individual state files that result from the above steps.
   
