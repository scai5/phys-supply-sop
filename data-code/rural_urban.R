# About ------------------------------------------------------------------------

# Rural / urban indicators
# Author:         Shirley Cai 
# Date created:   06/16/2026 
# Last edited:    06/16/2026 

# Read-in ----------------------------------------------------------------------

rural_codes <- read_excel('data/input/rural-urban/ruralurbancodes2013.xls') %>% 
  clean_names()

# Clean and export -------------------------------------------------------------

rural_codes <- rural_codes %>% select(fips, rucc_2013, description)

# Create rural county indicator 
rural_codes <- rural_codes %>% 
  mutate(description = ifelse(description == "Not Applicable", NA, description)) %>% 
  separate_wider_delim(description, delim = " - ", names = c("metro", "description"))

rural_codes <- rural_codes %>% 
  mutate(D_rural = ifelse(metro == "Nonmetro", 1, 0))

# Separate state and county FIPS 
rural_codes <- rural_codes %>% 
  mutate(
    state_fips = as.numeric(substr(fips, 1, 2)), 
    county_fips = as.numeric(substr(fips, 3, 5))
  )

# Export 
rural_codes <- rural_codes %>% select(state_fips, county_fips, rucc_2013, D_rural)
write_csv(rural_codes, 'data/output/rural_counties.csv')
rm(list = ls())
