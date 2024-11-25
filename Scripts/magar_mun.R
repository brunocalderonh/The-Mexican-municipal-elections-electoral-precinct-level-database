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
  select(-ends_with(".x"), -ends_with(".y")) %>% 
  rename(
    "year" = "yr",
    "mun_code" = "inegi",
  ) %>% 
  mutate(mun_code = as.numeric(mun_code))%>%
  rename_with(
    ~ ifelse(. %in% c("year", "mun_code", "mun"), ., 
             str_replace_all(toupper(.), "-", "_"))
  )

# Set the path to save the CSV file relative to the repository's root
output_dir <- file.path(getwd(), "Data/municipal magar data splitcoal")
output_path <- file.path(output_dir, "magar_mun_votes.csv")
 
# Use write_csv to save the file
write_csv(merged_data, output_path)


##### merge with the final db incumbent and runnerup parties

# Path to the .zip file
zip_file <- "Final Data/all_states_final.zip"

# Unzip the file to a temporary directory
temp_dir <- tempdir()  # Create a temporary directory
unzip(zip_file, exdir = temp_dir)  # Extract the contents to the temp directory

# Find the CSV file within the temp directory
unzipped_file <- file.path(temp_dir, "all_states_final.csv")

# Now read the unzipped CSV file from the temporary directory

#final db
finaldb <- read_csv(unzipped_file) %>% 
  select(
    state_code,                             
    mun_code,                              
    year,                                   
    state,                                  
    mun,                                    
    incumbent_party,
    runnerup_party) # Use read_csv for CSV files

finaldb <- finaldb %>%
  group_by(mun_code, state_code, year, state, mun) %>%
  summarise(
    incumbent_party = first(incumbent_party), # Assuming incumbent_party doesn't vary within mun_code
    runnerup_party = first(runnerup_party),   # Assuming runnerup_party doesn't vary within mun_code
    .groups = "drop" # Ensures the result is not grouped
  )


merged_data <- merged_data %>%
  select(-mun)

mun_db <- finaldb %>% 
  left_join(merged_data, by = c("mun_code", "year"))


####


# Define replacements
replacements <- c("PNA" = "PANAL", 
                  "FUERCIUD" = "PFC", 
                  "CONVE" = "PC", 
                  "PJ1" = "PJ"
                  )

# Replace occurrences in column names
colnames(mun_db) <- colnames(mun_db) %>%
  str_replace_all(replacements)

indep_columns <- c(
  "INDEP_IVONNE", "INDEP_ROSA_M", "JOSE_RODRIGO_ROBINSON_BOURS_CASTELO", 
  "JOSE_RAMON_GUTIERREZ_MORALES", "JOSE_CORTES_PEREZ", "ARMANDO_FLORES_LOPEZ", 
  "INDEP_TAVO", "LUIS_FERNANDO_SERRANO_GARCIA", "AGUSTIN_TOLEDANO_AMARO", 
  "PEDRO_ANTONIO_MONTENEGRO_MORGADO", "CI_LUIS", "JOSE_RODRIGO_ROBINSON_BOURS",
  "INDEP_ELISA_PATRICIA_QUINTANILLA", "INDEP_HIRAM_PEÑA_GOMEZ", 
  "INDEP_CARLOS_LARA_MACIAS", "OMAR_GARCIA_ARAMBULA", "INDEP_DE_LA_TORRE", 
  "INDEP_CARLOS_TENA_NEVAREZ", "INDEP_LUIS_E_TERRAZAS_SEYFFERT", 
  "INDEP_ROBERTO_A_GONZALEZ_GARCIA", "INDEP_DIAZ", "INDEP_LOPEZ", 
  "INDEP_ORTIZ", "INDEP_AVILA", "INDEP_GABRIEL_DEL_MONTER", 
  "RODOLFO_VIDALES", "INDEP_JOSE_ROMERO_MERCADO", "INDEP_JOSE_ALFREDO_CASTRO", 
  "INDEP_JOSE_ZEPEDA_CONTRERAS", "INDEP_CBS", "INDEP_JLUS", "INDEP_DRH", 
  "INDEP_JOEL_A", "INDEP_BARUCH", "INDEP_RAYMUNDO", "INDEP_JOSE_D", 
  "INDEP_HUGO_AMADO_MUNOZ_FLORES", "INDEP_HIPOLITO_RIGOBERTO_PEREZ_MONTES", 
  "JOSE_FELIX_LOPEZ_MENDOZA", "RAYMUNDO_ARIAS_GALINDO", 
  "MIGUEL_OVED_ROBINSON_BOURS_DEL_CASTILLO", "DAVID_SANCHEZ_RINCON", 
  "INDEP_CSR", "INDEP_SEBASTIAN", "INDEP_JOSE_LUIS", "FELIPE_SALAZAR_CORREA", 
  "INDEP_FLORENTINO", "VICTOR_HUGO_RIVERA_MUÑOZ", "REYNALDO_LUJAN_ALVAREZ", 
  "RAFAEL_ANGEL_GARCIA_CANO", "CI_ESAU", "CI_AMELIO", "CI_IVAN", 
  "CI_JOSE_M", "INDEP_MIGUEL_RODRIGUEZ_SALAZAR", "YAIREMMANUELHERRERAFLORES", 
  "ALFREDO_MORENO_CARREÑO", "JUAN_CARLOS_MOLINA_TORRES", 
  "KEVIN_FERNANDO_PERAZA_ESTRADA", "FERNANDO_HOYOS_AGUILAR", 
  "INDEP_OSAIAS_OVILLA", "INDEP_GONZALEZ", "INDEP_RIVERA", "INDEP_REYES", 
  "INDEP_GUSTAVO_GARCIA_ARIAS", "HECTOR_FABELA", "INDEP_RAUL_JESUS_ROBLES_MEDINA", 
  "INDEP_VICTOR_MANUEL_PEREZ", "INDEP_MARIA_GUADALUPE_BECERRA", 
  "INDEP_JOSE_LUIS_GONZALEZ_GONZALEZ", "INDEP_MA_DE_LA_LUZ_RUIZ_RUBIO", 
  "INDEP_JOSE_FRANCISCO_SANCHEZ_PEREZ", "EDGAR_RENE_RUELAS_GIL", 
  "INDEP_MARIA_DEL_CARMEN_GALLEGOS", "INDEP_ALBERTO_ALFARO_GARCIA", 
  "INDEP_OMAR_CEBALLOS_MORENO", "INDEP_AFT", "INDEP_PAMM", "INDEP_RAE", 
  "INDEP_CABO", "INDEP_EIQB", "INDEP_PEDRO", "INDEP_ARTEMIO", 
  "INDEP_JUAN_DE_DIOS", "INDEP_IVAN", "INDEP_CARLOS_R", 
  "INDEP_FEDERICOMONTEROCASTILLO", "INDEP_CMR", "INDEP_JMS", 
  "INDEP_IZL", "ERNESTO_URIBE_CORONA", "GERMAN_AGUAYO_VALENZUELA", 
  "MARCO_ANTONIO_LUNA_ESPINDOLA", "RAMON_HUITRON_RAMIREZ", 
  "GUSTAVO_JIMENEZ_ROMERO", "ANGEL_COCOLETZI_COCOLETZI", "INDEP_MUCHARRAZ", 
  "INDEP_CACHIS", "INDEP_JUST", "LUIS_ENRIQUE", "DAVID_HDZ", 
  "ARTURO_BLANCO", "INDEP_LUPITA", "AURELIO_RUIZ", "INDEP_ALEXIS", 
  "INDEP_GUSTIN", "RAUL_ULLOA_GUZMAN", "MARIO_ADRIAN_REYES_SANTANA", 
  "SERAFIN_BERMUDEZ_VIRAMONTES", "J_JESUS_MAQUIR_ENRIQUEZ_RODRIGUEZ", 
  "WALTER_VALDES_GAMON", "GERARDO_GLEZ", "VICTOR_GUERRERO", 
  "ANA_EMILIA_PESCI_MARTINEZ", "CELSO_ARTURO_FIGUEROA_MENDEL", 
  "CI_JOEL", "CI_GEONATAN", "CI_LAURO", "PAS_MORENA", "CLEMENTE_NEYOY_YOCUPICIO", 
  "INDEP_ARNOLDO_JAVIER_RODRIGUEZ", "INDEP_PATRICIO_GARZA_TAPIA", 
  "JUAN_GABRIEL_ROMO_MURILLO", "GUSTAVO_FLORES_BETANZOS", 
  "GASTON_LUKEN_GARZA", "INDEP_RUBIO", "JESUS_MARISCAL_VARGAS", 
  "MANUEL_CANO_VILLALOBOS", "INDEP_MEDINA", "JOSE_GALLARDO_MOLINA", 
  "INDEP_ALEJANDRO_VINAY_MELGAR", "INDEP_MANUEL_HERNANDEZ_GONZALEZ_DRFOX", 
  "OSCAR_ANTONIO_VALDES_JIMENEZ", "INDEP_ARNE_AUSDEN_RUTHEN_HAAG", 
  "JESUS_BALDERAS", "JOSE_MENDOZA", "INDEP_JUAN_DIEGO_CASTRO_MORALES", 
  "INDEP_IGNACIO_TELLEZ_GONZALEZ", "MARIA_DEL_SOCORRO_VALDEZ", 
  "INDEP_CARLOS_ALBERTO_BLANCO", "INDEP_JUAN_CARLOS_BUSTAMANTE", 
  "INDEP_JESUS_OSWALDO_SILVA_MAGANA", "INDEP_ANTONIO_HORACIO_CRUZ", 
  "INDEP_JVJR", "INDEP_LAFG", "INDEP_MES", "INDEP_JABE", "INDEP_RAUL", 
  "INDEP_VICTOR", "INDEP_ISRAEL_GUERRERO_BOCANEGRA", "INDEP_CIZM", 
  "RUBEN_ARTURO_CHAVEZ_GARCIA", "NORBERTO_BARRAZA_ALMAZAN", 
  "GUSTAVO_COYOTZI_RODRIGUEZ", "ADAN_LIMA_GONZALEZ", "JOSE_ARMENTA_RAMOS", 
  "HECTOR_ZEPEDA", "INDEP_JONATHAN", "INDEP_CHRISTOPHER", "INDEP_EVM", 
  "INDEP_EMILIO", "INDEP_ALMANZA", "INDEP_COSSY", "JOSE_EZEQUIEL", 
  "INDEP_CID", "EVERARDO_CABAÑAS_SALCEDO", "HONESTTRABTRANSPAR", 
  "HILDE_SOSA", "RAUL_AVILA_GUILLEN", "MARCO_ANTONIO_VIZCARRA_CALDERON", 
  "CONCEPCION_TREJO_CARDOZA", "INDEP_HAAC", "CI_VICTOR", "CI_MANUEL", 
  "HECTOR_JUAN_SALCIDO_ALVAREZ", "CARLOS_ALBERTO_QUIROZ_ROMO", 
  "INDEP_CARLOS_ALBERTO_GUERRERO", "INDEP_MARGGID_ANTONIO_RODRIGUEZ"
)

# Make column names temporarily unique
names(mun_db) <- make.unique(names(mun_db))

mun_db <- mun_db %>%
  mutate(
    PFC = coalesce(PFC, `PFC.1`), # Combine the "PFC" columns
    PJ = coalesce(PJ, `PJ.1`)     # Combine the "PJ" columns
  ) %>%
  select(-`PFC.1`, -`PJ.1`) # Remove the duplicate columns

any(duplicated(names(mun_db))) # Should return FALSE

mun_db <- mun_db %>%
  mutate(
    INDEP = coalesce(!!!syms(indep_columns))
  ) %>%
  select(-all_of(indep_columns))






assign_incumbent_vote <- function(data) {
  
  # Initialize columns
  data <- data %>%
    mutate(mun_incumbent_vote = NA,
           mun_party_component = NA)
  
  # Loop through each row of the data
  for (I in 1:nrow(data)) {
    incumbent_party <- data$incumbent_party[I]
    
    # Skip if incumbent_party is NA or empty
    if (is.na(incumbent_party) || incumbent_party == "") next
    
    # Check if it is a coalition
    if (str_detect(incumbent_party, "_")) {
      parties <- unlist(str_split(incumbent_party, "_"))
      
      # Check if any individual party within the coalition is present in other columns
      for (party in parties) {
        # Differentiating between PAN and PANAL using word boundaries
        if (party == "PAN") {
          party_vars <- names(data)[str_detect(names(data), "\\bPAN\\b") & !str_detect(names(data), "PANAL")]
        } else if (party == "PANAL") {
          party_vars <- names(data)[str_detect(names(data), "\\bPANAL\\b")]
        } else {
          party_vars <- names(data)[str_detect(names(data), paste0("\\b", party, "\\b"))]
        }
        
        for (party_var in party_vars) {
          if (!is.na(data[[party_var]][I]) && data[[party_var]][I] != 0) {
            data$mun_incumbent_vote[I] <- data[[party_var]][I]
            data$mun_party_component[I] <- party_var
            break
          }
        }
        if (!is.na(data$mun_incumbent_vote[I])) break
      }
      
      # If no individual party is found, proceed with coalition logic
      if (is.na(data$mun_incumbent_vote[I])) {
        coalition_vars <- names(data)[sapply(names(data), function(x) all(parties %in% str_split(x, "_")[[1]]))]
        
        for (coalition_var in coalition_vars) {
          if (!is.na(data[[coalition_var]][I]) && data[[coalition_var]][I] != 0) {
            data$mun_incumbent_vote[I] <- data[[coalition_var]][I]
            data$mun_party_component[I] <- coalition_var
            break
          }
        }
      }
    } else {
      # Handle single parties
      if (incumbent_party == "PAN") {
        party_vars <- names(data)[str_detect(names(data), "\\bPAN\\b") & !str_detect(names(data), "PANAL")]
      } else if (incumbent_party == "PANAL") {
        party_vars <- names(data)[str_detect(names(data), "\\bPANAL\\b")]
      } else {
        party_vars <- names(data)[str_detect(names(data), paste0("\\b", incumbent_party, "\\b"))]
      }
      
      for (party_var in party_vars) {
        if (!is.na(data[[party_var]][I]) && data[[party_var]][I] != 0) {
          data$mun_incumbent_vote[I] <- data[[party_var]][I]
          data$mun_party_component[I] <- party_var
          break
        }
      }
      
      # If no single party found, check coalitions containing the single party
      if (is.na(data$mun_incumbent_vote[I])) {
        coalition_vars <- names(data)[sapply(names(data), function(x) incumbent_party %in% str_split(x, "_")[[1]])]
        
        for (coalition_var in coalition_vars) {
          if (!is.na(data[[coalition_var]][I]) && data[[coalition_var]][I] != 0) {
            data$mun_incumbent_vote[I] <- data[[coalition_var]][I]
            data$mun_party_component[I] <- coalition_var
            break
          }
        }
      }
    }
  }
  
  return(data)
}



correct_runnerup_vote <- function(data) {
  
  # Initialize columns for storing results
  data <- data %>%
    mutate(mun_runnerup_vote = NA,
           mun_runnerup_party_component = NA)
  
  # Loop through each row of the data
  for (I in 1:nrow(data)) {
    runnerup_party <- data$runnerup_party[I]
    
    # Skip processing for NA or empty runnerup_party
    if (is.na(runnerup_party) || runnerup_party == "") next
    
    # Check if it is a coalition
    if (str_detect(runnerup_party, "_")) {
      parties <- unlist(str_split(runnerup_party, "_"))
      
      # Check if any individual party within the coalition is present in other columns
      for (party in parties) {
        # Differentiating between PAN and PANAL using word boundaries
        if (party == "PAN") {
          party_vars <- names(data)[str_detect(names(data), "\\bPAN\\b") & !str_detect(names(data), "PANAL")]
        } else if (party == "PANAL") {
          party_vars <- names(data)[str_detect(names(data), "\\bPANAL\\b")]
        } else {
          party_vars <- names(data)[str_detect(names(data), paste0("\\b", party, "\\b"))]
        }
        
        for (party_var in party_vars) {
          if (!is.na(data[[party_var]][I]) && data[[party_var]][I] != 0) {
            data$mun_runnerup_vote[I] <- data[[party_var]][I]
            data$mun_runnerup_party_component[I] <- party_var
            break
          }
        }
        if (!is.na(data$mun_runnerup_vote[I])) break
      }
      
      # If no individual party is found, proceed with coalition logic
      if (is.na(data$mun_runnerup_vote[I])) {
        coalition_vars <- names(data)[sapply(names(data), function(x) all(parties %in% str_split(x, "_")[[1]]))]
        
        for (coalition_var in coalition_vars) {
          if (!is.na(data[[coalition_var]][I]) && data[[coalition_var]][I] != 0) {
            data$mun_runnerup_vote[I] <- data[[coalition_var]][I]
            data$mun_runnerup_party_component[I] <- coalition_var
            break
          }
        }
      }
    } else {
      # Handle single parties
      if (runnerup_party == "PAN") {
        party_vars <- names(data)[str_detect(names(data), "\\bPAN\\b") & !str_detect(names(data), "PANAL")]
      } else if (runnerup_party == "PANAL") {
        party_vars <- names(data)[str_detect(names(data), "\\bPANAL\\b")]
      } else {
        party_vars <- names(data)[str_detect(names(data), paste0("\\b", runnerup_party, "\\b"))]
      }
      
      for (party_var in party_vars) {
        if (!is.na(data[[party_var]][I]) && data[[party_var]][I] != 0) {
          data$mun_runnerup_vote[I] <- data[[party_var]][I]
          data$mun_runnerup_party_component[I] <- party_var
          break
        }
      }
      
      # If no single party found, check coalitions containing the single party
      if (is.na(data$mun_runnerup_vote[I])) {
        coalition_vars <- names(data)[sapply(names(data), function(x) runnerup_party %in% str_split(x, "_")[[1]])]
        
        for (coalition_var in coalition_vars) {
          if (!is.na(data[[coalition_var]][I]) && data[[coalition_var]][I] != 0) {
            data$mun_runnerup_vote[I] <- data[[coalition_var]][I]
            data$mun_runnerup_party_component[I] <- coalition_var
            break
          }
        }
      }
    }
  }
  
  return(data)
}




mun_db <- assign_incumbent_vote(mun_db)
mun_db <- correct_runnerup_vote(mun_db)
mun_db <- mun_db %>% 
  select( state_code,                             
          mun_code,                              
          year,                                   
          state,                                  
          mun,                                    
          incumbent_party,
          mun_incumbent_vote,
          mun_party_component,
          runnerup_party,
          mun_runnerup_vote,
          mun_runnerup_party_component
          )
