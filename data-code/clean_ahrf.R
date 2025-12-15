# About ------------------------------------------------------------------------

# Cleaning AHRF data
# Author:         Shirley Cai 
# Date created:   07/28/2025 
# Last edited:    11/11/2025 

# Import and extract data ------------------------------------------------------

# Unit of observation: County-year 
# Outcomes: Total active MDs (non-federal), total active DOs (non-federal)
#           PCPs vs specialists
# Controls: Population, ???

## Read 2009-2010 release ------------------------------------------------------

vars_2009 <- read_excel('data/input/ahrf_2009_vars.xlsx') %>% 
  mutate(var_name_full = ifelse(is.na(year), var_name, paste(var_name, year, sep = "_")))

raw_2009 <- read_fwf(
  'data/input/area-health-resource-files/ahrf2009.asc',
  fwf_positions(
    start = vars_2009$start_col, 
    end = vars_2009$end_col, 
    col_names = vars_2009$var_name_full
  )
)

raw_2009 <- raw_2009 %>% mutate(across(!state & !county, as.numeric))

# Remove State of Alaska observation 
raw_2009 <- raw_2009 %>% filter(!grepl("State of", county))

# Pivot to long format
raw_2009 <- raw_2009 %>% 
  pivot_longer(
    cols = -c(state, county, state_fips, county_fips),
    names_pattern =  "(.*)_(\\d+)",
    names_to = c(".value", "year"),
    values_drop_na = TRUE
  )

# Compute PCP count = GP + gen FM + gen IM + gen peds, excl. residents
raw_2009 <- raw_2009 %>% 
  mutate(
    tot_md_pcp = tot_md_gp - md_gp_pc_hospital_res + 
                 tot_md_gen_fm - md_gen_fm_pc_hospital_res + 
                 tot_md_gen_im - md_gen_im_pc_hospital_res + 
                 tot_md_gen_peds - md_gen_peds_pc_hospital_res, 
    tot_do_pcp = do_gp + do_gen_im + do_gen_peds - do_pc_hospital_res, 
    tot_pcp = tot_md_pcp + tot_do_pcp
  )

raw_2009 <- raw_2009 %>% mutate(across(!state & !county, function(x) ifelse(x <= 0, NA, x)))

## Read 2019-2020 release ------------------------------------------------------

vars_2019 <- read_excel('data/input/ahrf_2019_vars.xlsx') %>% 
  mutate(var_name_full = ifelse(is.na(year), var_name, paste(var_name, year, sep = "_")))

# Read in as wide format
raw_2019 <- read_fwf(
  'data/input/area-health-resource-files/AHRF2020.asc',
  fwf_positions(
    start = vars_2019$start_col, 
    end = vars_2019$end_col, 
    col_names = vars_2019$var_name_full
  )
)

# Remove State of Alaska observation 
raw_2019 <- raw_2019 %>% filter(!grepl("State of", county))

raw_2019 <- raw_2019 %>% mutate(across(!state & !county, as.numeric))

# Pivot to long format
raw_2019 <- raw_2019 %>% 
  pivot_longer(
    cols = -c(state, county, state_fips, county_fips),
    names_pattern =  "(.*)_(\\d+)",
    names_to = c(".value", "year"),
    values_drop_na = TRUE
  )

## Merge and clean up ----------------------------------------------------------

out <- bind_rows(raw_2009, raw_2019)

# Missing values in AHRF labeled as 0
out <- out %>% mutate(across(!state & !county, function(x) ifelse(x == 0, NA, x)))

# Compute number of specialists
out <- out %>% 
  mutate(
    tot_md_spec = md_pc - md_pc_hospital_res - tot_md_pcp, 
    tot_do_spec = do_pc - do_pc_hospital_res - tot_do_pcp
  )

# Export data ------------------------------------------------------------------

write_csv(out, 'data/output/ahrf.csv')
rm(list = ls())
