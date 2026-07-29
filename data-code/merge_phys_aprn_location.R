# About ------------------------------------------------------------------------

# Aggregating physician location
# Author:         Shirley Cai 
# Date created:   01/21/2026  
# Last edited:    07/09/2026 
#                 Moved ZIP -> state and county FIPS here

# OUT: df with NPI, year, ZIP, state and county FIPS

# Import and merge -------------------------------------------------------------

# MDPPAS and Physician Compare assign 1 ZIP code to each NPI-year 
mdppas <- read_csv('data/output/mdppas.csv') %>% 
  select(npi, year, phy_zip_pos1)
pc <- read_tsv('data/output/phys-compare-dynamic.txt') %>% 
  select(npi, year, zip_code)

pc <- pc %>% mutate(zip_code = as.numeric(zip_code))

# Merge MDPPAS and Physician Compare together 
# In cases where MDPPAS and PC have different ZIP, defer to MDPPAS 
df <- bind_rows(mdppas, pc)
df <- as.data.table(df)
df <- df[, .(zip = fcoalesce(phy_zip_pos1[1], zip_code[1])), by = .(npi, year)]

rm(mdppas, pc)
gc()

# Get state and county FIPS by ZIP ---------------------------------------------

zip_xw <- read_tsv('data/output/zipcounty-xw.txt') %>% 
  mutate(zip = as.double(zip))
df <- df %>% left_join(zip_xw, by = 'zip')

# Mutate state and county FIPS
df <- df %>% 
  rename(full_fips = county) %>% 
  mutate(
    state_fips = substr(full_fips, 1, 2), 
    county_fips = substr(full_fips, 3, 5)
  )

# Remove Puerto Rico and Virgin Islands
df <- df %>% 
  filter(state_fips != 72, state_fips != 78)

message(paste0('Percent practitioner location with no FIPs match: ', round(sum(is.na(df$full_fips)) / nrow(df), 3)))
df <- df %>% filter(!is.na(full_fips))

# Add APRN indicator -----------------------------------------------------------

ptax <- read_csv('data/output/nppes_taxonomy.csv') %>% select(npi, year, D_aprn)
df <- df %>% left_join(ptax, by = c('npi', 'year'))

# Export -----------------------------------------------------------------------

write_csv(df, 'data/output/phys_aprn_location.csv')
rm(list = ls())
