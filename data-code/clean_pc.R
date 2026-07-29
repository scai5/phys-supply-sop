# About ------------------------------------------------------------------------

# Cleaning physician and APRN location from Physician Compare
# Author:         Shirley Cai 
# Date created:   06/10/2024  
# Last edited:    07/16/2026
#                 Updated to pull speciality from NPPES

# Import and merge data --------------------------------------------------------

ptax <- read_csv('data/output/nppes_taxonomy.csv') %>% 
  mutate(D_ptax = TRUE)

years <- formatC(14:23, width = 2, flag = "0")
varname_xw <- read_csv('data/input/physician-compare/ndf-xw.csv', n_max = 26) 

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
  } else if (yr == "23"){
    raw <- raw %>% 
      renamefrom(cw_file = varname_xw, raw = colnames_2023, clean = clean)
  } else {    
    raw <- raw %>% 
      renamefrom(cw_file = varname_xw, raw = colnames_2018, clean = clean)
  }
  
  print(yr)
  
  # Trim down
  raw <- raw %>% select(npi, gender, credential, zip_code,
                        medical_school_name, graduation_year)
  raw <- raw %>% distinct_all()
  
  # Add year variable
  raw <- raw %>%
    mutate(year = as.numeric(paste0("20", yr)), 
           npi = as.numeric(npi))
  
  # Filter by specialty using NPPES taxonomy code for that year
  raw <- raw %>% 
    left_join(ptax, by = c('npi', 'year')) %>% 
    filter(D_ptax == TRUE)
  
  raw <- raw %>% select(-c(D_ptax))
  
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
    Dfemale = case_when(gender == "M" ~ 0, 
                        gender == "F" ~ 1, 
                        gender == "U" ~ NA, 
                        TRUE ~ NA), 
    graduation_year = as.numeric(graduation_year), 
    zip_full = zip_code, 
    zip_code = substr(zip_code, 1, 5)
  )

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

# TODO: Figure out a better way to determine multiple practice location: CBSA likely not broad enough

df <- df %>% filter(n_cbsa == 1)
message("Removed physicians practicing in multiple CBSA codes")

df <- df %>% 
  distinct(npi, year, medical_school_name, graduation_year, zip_code, .keep_all = TRUE)
message("Removed approximately duplicate rows")

# Prepare for export -----------------------------------------------------------

## Get static characteristics --------------------------------------------------

# Observation = NPI
# Medical school and graduation year should not change yearly

phys_char <- df %>% 
  filter(D_aprn == 0) %>% 
  select(npi, year, medical_school_name, graduation_year) %>% 
  distinct_at(vars(-year), .keep_all = TRUE)

single <- phys_char %>% group_by(npi) %>% filter(n() == 1) %>% ungroup() %>% 
  select(-c(year))
dupe <- phys_char %>% group_by(npi) %>% filter(n() > 1) %>% ungroup() %>% 
  mutate(medical_school_name = replace(medical_school_name, medical_school_name == "OTHER", NA))
dupe <- dupe %>% 
  group_by(npi) %>% 
  arrange(npi, desc(year)) %>% 
  summarise(
    medical_school_name = first(na.omit(medical_school_name)),
    graduation_year = first(na.omit(graduation_year))
  )

static_char <- bind_rows(single, dupe)
rm(phys_char)
rm(single)
rm(dupe)
gc()

## Get dynamic characteristics -------------------------------------------------

# Observation = NPI-year
# Location can change each year 

phys_char <- df %>% 
  select(npi, year, zip_code, cbsa, Dfemale, D_pcp, D_spec, D_aprn)

message(paste0("Distinct physician-year obs.: ", nrow(phys_char %>% distinct(npi, year))))

# For physicians with more than one ZIP, randomly choose a ZIP
set.seed(1234)
phys_char <- phys_char %>% 
  group_by(npi, year) %>% 
  slice_sample(n = 1) %>%
  ungroup()

# Export -----------------------------------------------------------------------

gc()

write_tsv(static_char,'data/output/phys-compare-static.txt')
write_tsv(phys_char, 'data/output/phys-compare-dynamic.txt')
rm(list = ls())
