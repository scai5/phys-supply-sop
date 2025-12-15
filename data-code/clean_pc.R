# About ------------------------------------------------------------------------

# Cleaning Physician Compare
# Author:         Shirley Cai 
# Date created:   06/10/2024  
# Last edited:    12/09/2025 

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

# Sample restrictions ----------------------------------------------------------

## Removing non-MDs ------------------------------------------------------------

# TODO: Get out DOs 

message("Missing Compare credentials ----------")
message(paste0("Total physician-year obs.: ", nrow(df)))
message(paste0("Unknown credentials physician-year obs.: ", 
               sum(is.na(df$credential))))
message(paste0("Unknown credentials physicians: ", 
               length(unique(df$npi[is.na(df$credential)]))))

message("Non-MD Compare credentials ----------")
# There are no dual credentials
message(paste0("Total physician-year obs.: ", nrow(df)))
message(paste0("Non-MD credentials physician-year obs.: ", 
               sum(df$credential != "MD" %>% tidyr::replace_na(FALSE), na.rm = TRUE)))
message(paste0("Non-MD credentials physicians: ", 
               length(unique(df$npi[(df$credential != "MD") %>% tidyr::replace_na(FALSE)]))))
df <- df %>% filter((credential == "MD") %>% tidyr::replace_na(TRUE))
message("Removed non-MD Compare observations, keeps NA credential observations")

# Attempt to limit to MD using specialty
exclude_specialty <- c("CHIROPRACTIC", "PODIATRY", "OPTOMETRY", "DENTIST",
                       "CLINICAL SOCIAL WORKER",
                       "NURSE PRACTITIONER", "PHYSICIAN ASSISTANT", 
                       "CERTIFIED REGISTERED NURSE ANESTHETIST", "CERTIFIED REGISTERED NURSE ANESTHETIST (CRNA)", 
                       "CERTIFIED NURSE MIDWIFE", "CERTIFIED NURSE MIDWIFE (CNM)",
                       "CLINICAL NURSE SPECIALIST", "CERTIFIED CLINICAL NURSE SPECIALIST (CNS)",
                       "UNDEFINED NON-PHYSICIAN TYPE (SPECIFY)")

message("Non-MD Compare specialty ----------")
message(paste0("Total physician-year obs.: ", nrow(df)))
message(paste0("Non-MD Compare specialty worker-year obs.: ", 
               sum(df$primary_specialty %in% exclude_specialty)))
message(paste0("Non-MD Compare specialty workers: ", 
               length(unique(df$npi[df$primary_specialty %in% exclude_specialty]))))
df <- df %>% 
  filter(!(primary_specialty %in% exclude_specialty %>% tidyr::replace_na(FALSE)))
message("Removed non-physician specialty Compare observations")

## Restrict on location --------------------------------------------------------

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
message("Removed physicians practicing in multiple ZIP codes")

df <- df %>% 
  distinct(npi, year, pac_id, enroll_id, medical_school_name, zip_code, 
           organization_legal_name, address_line_1, .keep_all = TRUE)
message("Removed approximately duplicate rows")

## df for static physician characteristics -------------------------------------

phys_char <- df %>% 
  select(npi, year, medical_school_name, graduation_year) %>% 
  distinct_at(vars(-year), .keep_all = TRUE)

# Get only one medical school for each npi 
single <- phys_char %>% group_by(npi) %>% filter(n() == 1) %>% ungroup() %>% 
  select(-c(year))
dupe <- phys_char %>% group_by(npi) %>% filter(n() > 1) %>% ungroup() %>% 
  mutate(medical_school_name = replace(medical_school_name, medical_school_name == "OTHER", NA))
dupe <- dupe %>% 
  group_by(npi) %>% 
  arrange(npi, desc(year)) %>% 
  summarise(
    medical_school_name = first(na.omit(medical_school_name)),
    graduation_year = first(graduation_year)
  )

static_char <- bind_rows(single, dupe)
rm(phys_char)
rm(single)
rm(dupe)
gc()

## df for dynamic physician characteristics ------------------------------------

phys_char <- df %>% 
  select(npi, year, primary_specialty, all_secondary_specialties,
         organization_legal_name, group_practice_pac_id, n_group_members,
         address_line_1, zip_code, cbsa, Dfemale)

# Export -----------------------------------------------------------------------

rm(df)
gc()

write_tsv(static_char,'data/output/phys-compare-static.txt')
write_tsv(phys_char, 'data/output/phys-compare-dynamic.txt')
rm(list = ls())
