# About ------------------------------------------------------------------------

# Aggregating billing APRNs from Physician Compare
# Author:         Shirley Cai 
# Date created:   06/10/2024  
# Last edited:    02/28/2026

# Import and merge data --------------------------------------------------------

years <- formatC(14:23, width = 2, flag = "0")
varname_xw <- read_csv('data/input/physician-compare/ndf-xw.csv', n_max = 37) 

read_compare <- function(yr){
  if(yr == "16"){
    raw <- read_csv(paste0("data/input/physician-compare/National_Downloadable_File_20", yr, ".csv"), 
                    col_types = list(.default = col_character()), 
                    col_names = FALSE) %>% 
      clean_names()
  } else{
    raw <- read_csv(paste0("data/input/physician-compare/National_Downloadable_File_20", yr, ".csv"), 
                    col_types = list(.default = col_character())) %>% 
      clean_names()
  }
  
  # Fix parsing issues 
  if(yr == '14') {
    raw[,21] <- case_when(raw[,21] == '.' ~ NA, 
                          TRUE ~ raw[,21])
    raw[raw$npi == "1487638854",] <- c(raw[raw$npi == "1487638854",1:18], raw[raw$npi == "1487638854",20:43], NA)
    raw[raw$npi == "1487638854",42] <- NA
  } else if (yr == '17') {
    raw[,27] <- gsub(" ", "", raw[,27])
  }
  
  # Rename variables 
  if(yr == "14" | yr == "15") {
    raw <- raw %>% 
      renamefrom(cw_file = varname_xw, raw = colnames_2015, clean = clean)
  } else if(yr == "16"){
    raw <- raw %>% 
      renamefrom(cw_file = varname_xw, raw = colnames_2016, clean = clean)
  } else if (yr == "17"){
    raw <- raw %>% 
      renamefrom(cw_file = varname_xw, raw = colnames_2017, clean = clean)
  } else {
    raw <- raw %>% 
      renamefrom(cw_file = varname_xw, raw = colnames_2018, clean = clean)
  }
  
  # Add year variable
  raw <- raw %>%
    mutate(year = as.numeric(paste0("20", yr)))
  return(raw)
}

raw <- lapply(years, read_compare)
df <- do.call(bind_rows, raw)

# Free up memory
rm(raw)
gc()

# Cast to numeric, change indicator variables, get short zip
df <- df %>% 
  mutate(
    npi = as.numeric(npi), 
    Dfemale = case_when(gender == "M" ~ 0, 
                        gender == "F" ~ 1, 
                        gender == "U" ~ NA, 
                        TRUE ~ NA), 
    graduation_year = as.numeric(graduation_year), 
    n_group_members = as.numeric(n_group_members), 
    Daddress_line_2_suppressed = case_when(Daddress_line_2_suppressed == "Y" ~ 1, 
                                           Daddress_line_2_suppressed == "N" ~ 0, 
                                           TRUE ~ NA), 
    zip_full = zip_code, 
    zip_code = substr(zip_code, 1, 5)
  )

# Include only APRNs -----------------------------------------------------------

# APRNs include NPs, CRNAs, CNMs, and CNS
# Requires a Master's (MSN) or Doctor of Nursing Practice (DNP)

# Limit to APRN using specialty
include_specialty <- c("NURSE PRACTITIONER", 
                       "CERTIFIED REGISTERED NURSE ANESTHETIST", "CERTIFIED REGISTERED NURSE ANESTHETIST (CRNA)", 
                       "CERTIFIED NURSE MIDWIFE", "CERTIFIED NURSE MIDWIFE (CNM)",
                       "CLINICAL NURSE SPECIALIST", "CERTIFIED CLINICAL NURSE SPECIALIST (CNS)")

message("Number of APRNs billing for Medicare ----------")
message(paste0("Total NPI-year obs.: ", nrow(df)))
message(paste0("ARPN NPI-year obs.: ", sum(df$primary_specialty %in% include_specialty)))
message(paste0("Unique APRN: ", length(unique(df$npi[df$primary_specialty %in% include_specialty]))))
df <- df %>% filter(primary_specialty %in% include_specialty)
message("Removed non-APRN observations")

# Assign one ZIP to each NPI-year observation ----------------------------------

df <- df %>% group_by(npi, year) %>% mutate(n_zip = length(unique(zip_code))) %>% ungroup()

message("Multiple practice ZIPs ----------")
message(paste0("Total physician-year obs.: ", nrow(df)))
message(paste0("Multiple ZIP physician-year obs.: ", 
               sum(df$n_zip > 1)))
message(paste0("Multiple ZIP physicians: ", 
               length(unique(df$npi[df$n_zip > 1]))))

# Merge in CBSA code
cbsa_xw <- read_tsv('data/output/zipcbsa-xw.txt')
df <- df %>% left_join(cbsa_xw, by = c('zip_code' = 'zip'))
df <- df %>% group_by(npi, year) %>% mutate(n_cbsa = length(unique(cbsa))) %>% ungroup()

message("Multiple practice CBSAs ----------")
message(paste0("Total physician-year obs.: ", nrow(df)))
message(paste0("Multiple CBSA physician-year obs.: ", 
               sum(df$n_cbsa > 1)))
message(paste0("Multiple CBSA physicians: ", 
               length(unique(df$npi[df$n_cbsa > 1]))))

rm(cbsa_xw)
gc()

df <- df %>% group_by(npi, year) %>% mutate(n_state = length(unique(state))) %>% ungroup()

message("Multiple practice states ----------")
message(paste0("Total physician-year obs.: ", nrow(df)))
message(paste0("Multiple state physician-year obs.: ", 
               sum(df$n_state > 1)))
message(paste0("Multiple state physicians: ", 
               length(unique(df$npi[df$n_state > 1]))))

# TODO: Figure out a better way to determine multiple practice location: CBSA likely not broad enough

df <- df %>% filter(n_cbsa == 1)
message("Removed physicians practicing in multiple CBSA codes")

df <- df %>% 
  distinct(npi, year, pac_id, enroll_id, medical_school_name, zip_code, 
           organization_legal_name, address_line_1, .keep_all = TRUE)
message("Removed approximately duplicate rows")

# Merge in county --------------------------------------------------------------

# xw ZIP to county 
zip_xw <- read_tsv('data/output/zipcounty-xw.txt') 
df <- df %>% left_join(zip_xw, by = c('zip_code' = 'zip'))

df <- df %>% distinct(npi, year, county)

# Break county ties
set.seed(1234)
df <- df %>% 
  group_by(npi, year) %>% 
  slice_sample(n = 1) %>%
  ungroup()

# Aggregate to the county-year level 
agg_df <- df %>% 
  group_by(county, year) %>% 
  summarise(
    aprn_medicare = n()
  )

# TODO: Figure out why 2023 is all missing zip 

agg_df <- agg_df %>% filter(!is.na(county))

# Split up state and county FIPS 
agg_df <- agg_df %>% 
  rename(full_fips = county) %>% 
  mutate(
    state_fips = substr(full_fips, 1, 2), 
    county_fips = substr(full_fips, 3, 5)
  ) %>% 
  ungroup() %>% 
  select(-c(full_fips))

# Export -----------------------------------------------------------------------

gc()

write_csv(agg_df, 'data/output/phys-compare-aprn.csv')
rm(list = ls())
