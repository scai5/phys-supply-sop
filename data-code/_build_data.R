# About ------------------------------------------------------------------------

# Building main dataset
# Author:         Shirley Cai 
# Date created:   08/01/2025 
# Last edited:    12/12/2025 

# Preliminary ------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, janitor, modelsummary, gt, lubridate, stringr,
               fixest, ggfixest, did, crosswalkr)

# Cleaning raw data ------------------------------------------------------------

source('data-code/clean_ahrf.R')
source('data-code/clean_bls.R')

# Read in data -----------------------------------------------------------------

ahrf <- read_csv('data/output/ahrf.csv')
sop <- read_csv('data/input/nurse-sop/nurse-sop.csv')
bls <- read_csv('data/output/annual_wage.csv')

# Merge SOP and AHRF -----------------------------------------------------------

# FPA = 1 the first FULL year with FPA
sop <- sop %>% 
  select(stateFIPS, year, FPA, effective_date) %>% 
  distinct(stateFIPS, year, FPA, .keep_all = TRUE)
sop <- sop %>% 
  mutate(
    effective_year = as.numeric(str_extract(effective_date, "^\\d{4}")), 
    effective_month = as.numeric(str_extract(effective_date, "(?<=m)\\d+")),
    FPA = case_when(
      year == effective_year & effective_month < 7 ~ 1, 
      year == effective_year & effective_month >= 7 ~ 0, 
      TRUE ~ FPA
    )
  ) %>% 
  distinct(stateFIPS, year, FPA, .keep_all = TRUE)

sop <- sop %>% 
  group_by(stateFIPS) %>% 
  mutate(
    effective_year = case_when(
      effective_month < 7 ~ effective_year, 
      effective_month >= 7 ~ effective_year + 1, 
      TRUE ~ effective_year
    )
  ) %>% 
  ungroup() %>% 
  select(stateFIPS, year, FPA, effective_date, effective_year) 

# Drop territories 
ahrf <- ahrf %>% filter(state != "GU", state != "PR", state != "VI")

# Aggregate to state level 
state_ahrf <- ahrf %>% 
  filter(year <= 2018) %>%
  group_by(state, year) %>% 
  summarize(
    state_fips = median(state_fips), 
    tot_md = sum(tot_md, na.rm = TRUE), 
    tot_do = sum(tot_do, na.rm = TRUE), 
    tot_md_pcp = sum(tot_md_pcp, na.rm = TRUE), 
    tot_do_pcp = sum(tot_do_pcp, na.rm = TRUE), 
    tot_md_spec = sum(tot_md_spec, na.rm = TRUE), 
    tot_do_spec = sum(tot_do_spec, na.rm = TRUE),
    population = sum(population, na.rm = TRUE)
  )
state_ahrf <- state_ahrf %>% 
  mutate(across((starts_with("tot_")), function(x) ifelse(year == 2009, NA, x))) %>% 
  mutate(across((ends_with("_pcp")), function(x) ifelse(year < 2005 | year == 2006, NA, x))) %>% 
  mutate(across((ends_with("_spec")), function(x) ifelse(year < 2005 | year == 2006, NA, x)))

# Calculate MD per pop ratio
state_ahrf <- state_ahrf %>% 
  mutate(
    md_per_10k = tot_md / population * 10000, 
    md_pcp_per_10k = tot_md_pcp / population * 10000, 
    md_spec_per_10k = tot_md_spec / population * 10000, 
    do_per_10k = tot_do / population * 10000, 
    do_pcp_per_10k = tot_do_pcp / population * 10000, 
    do_spec_per_10k = tot_do_spec / population * 10000
  )
ahrf <- ahrf %>% 
  mutate(
    md_per_10k = tot_md / population * 10000, 
    md_pcp_per_10k = tot_md_pcp / population * 10000, 
    md_spec_per_10k = tot_md_spec / population * 10000,
    do_per_10k = tot_do / population * 10000, 
    do_pcp_per_10k = tot_do_pcp / population * 10000, 
    do_spec_per_10k = tot_do_spec / population * 10000
  )

county_df <- ahrf  %>% 
  left_join(sop, by = c('state_fips' = 'stateFIPS',
                        'year' = 'year')) %>% 
  filter(!is.na(FPA))
  
state_df <- state_ahrf %>% 
  left_join(sop, by = c('state_fips' = 'stateFIPS',
                        'year' = 'year')) %>% 
  filter(!is.na(FPA))

# Merge with BLS wage data -----------------------------------------------------

bls <- bls %>% 
  select(-c(st)) %>% 
  mutate(st_fips = as.double(st_fips))

# BLS is state only 
state_df <- state_df %>% 
  left_join(bls, by = c('state_fips' = 'st_fips',
                        'year' = 'year'))

# Export -----------------------------------------------------------------------

write_csv(county_df, 'data/output/final_df_county.csv')
write_csv(state_df, 'data/output/final_df_state.csv')
rm(list = ls())
