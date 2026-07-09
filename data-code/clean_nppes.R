# About ------------------------------------------------------------------------

# Clean NPPES
# Author:         Shirley Cai 
# Date created:   02/03/2026
# Last edited:    07/08/2026

# Goal: Export out MD vs DO credentials and specialty

# Import NPPES -----------------------------------------------------------------

## Taxonomy codes --------------------------------------------------------------

# NBER collects 2007 through 2020
# This will need to be merged in using closest()
# TODO remove n_max
ptax <- read_dta('data/input/nppes/collection/ptaxcode.dta', n_max = 1000) %>% 
  mutate(
    npi = as.double(npi), 
    year = substr(yyyymm, 1, 4)
  ) %>% 
  select(-c(yyyymm))

# TODO: check on seq
dupe <- ptax %>% group_by(npi, year) %>% filter(n() > 1) %>% ungroup()

ptax_23 <- read_dta('data/input/nppes/ptaxcode_20235.dta') %>% clean_names()
ptax_23 <- ptax_23 %>% 
  filter(pprimtax == 'Y') %>% 
  select(-c(seq, pprimtax)) %>% 
  mutate(npi = as.double(npi))

# TODO: Combine ptax and ptax_23 

# Taxonomy description
tax_dict <- read_csv('data/input/provider-taxonomy/Medicare_Provider_and_Supplier_Taxonomy_Crosswalk_JAN__2025.csv') %>% 
  clean_names()
tax_dict <- tax_dict %>% 
  select(provider_taxonomy_code, provider_taxonomy_description_type_classification_specialization) %>% 
  rename(provider_taxonomy_description = provider_taxonomy_description_type_classification_specialization) %>% 
  distinct_all() 
tax_dict <- tax_dict %>% group_by(provider_taxonomy_code) %>% summarize(provider_taxonomy_description = first(provider_taxonomy_description))

# Filter to physicians 
ptax <- ptax %>% left_join(tax_dict, by = c('ptaxcode' = 'provider_taxonomy_code'))
ptax <- ptax %>% filter(grepl("Allopathic & Osteopathic Physicians", provider_taxonomy_description, ignore.case = TRUE) %>% replace_na(TRUE))

## Core ------------------------------------------------------------------------

core <- read_csv('data/input/nppes/core_202509.csv') %>% 
  clean_names() %>% 
  filter(entity == 'Individual') 
core <- core %>% select(npi, pcredential, pcredentialoth)

# Merge core and taxonomy
df <- ptax %>% left_join(core, by = 'npi')

rm(core, ptax, tax_dict)
gc()

# Assign credential and specialty indicators -----------------------------------

message(paste0('Total obs: ', nrow(df)))
message(paste0('Missing credentials: ', sum(is.na(df$pcredential))))

# Assign MD or DO using NPPES credentials 
df <- df %>% 
  mutate(pcredential = gsub("\\.| ", "", pcredential)) %>% 
  filter(!((grepl("DMD|DDS", pcredential) %>% replace_na(FALSE))))

df <- df %>%
  mutate(
    D_missing_cred = is.na(pcredential), 
    D_md = (grepl("MD", pcredential) | grepl("MEDICALDOCTOR", pcredential)), 
    D_do = grepl("DO", pcredential)
  )

# Drop if not MD or DO or not missing
df <- df %>% filter(D_md | D_do | D_missing_cred)

# Assign PCP or specialist based on NPPES taxonomy
#   PCP:  Internal medicine   207R00000X, 207RA0000X
#         General practice    208D00000X
#         Family medicine     207Q00000X 
#         General pediatrics  208000000X, 2080A0000X
#   Specialist: Everyone else 
df <- df %>%
  mutate(
    D_missing_spec = is.na(ptaxcode), 
    D_pcp = (ptaxcode == "207R00000X" | ptaxcode == "207RA0000X" | 
               ptaxcode == "208D00000X" | ptaxcode == "207Q00000X" | 
               ptaxcode == "208000000X" | ptaxcode == "2080A0000X"), 
    D_spec = !D_pcp
  )

# Export -----------------------------------------------------------------------

write_csv(df, 'data/output/nppes.csv')
rm(list = ls())
