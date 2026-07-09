# About ------------------------------------------------------------------------

# Building main dataset
# Author:         Shirley Cai 
# Date created:   08/01/2025 
# Last edited:    07/08/2026 

# Preliminary ------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, janitor, modelsummary, gt, lubridate, stringr,
               fixest, ggfixest, did, crosswalkr, broom, data.table, haven)

# Cleaning raw data ------------------------------------------------------------

# ZIP code crosswalks
source('data-code/zipcbsa_xw.R')
source('data-code/zipcounty_xw.R')

# Rural codes
source('data-code/rural_urban.R')

# Nurse SOP (treatment)
source('data-code/clean_sop.R')

# AHRF: Aggregate phs / APRN labor supply, controls
source('data-code/clean_ahrf.R')

# Location data 
source('data-code/clean_nppes.R')
source('data-code/clean_mdppas.R')
source('data-code/clean_pc.R')
source('data-code/clean_pc_aprn.R') # TODO: Phase out
source('data-code/merge_phys_aprn_location.R')

# Newly practicing physicians
source('data-code/aggregate_new_phys.R')

# Physician-level code mix
source('data-code/clean_pfs_rvu.R')
source('data-code/clean_puf.R')

# Wage
source('data-code/clean_bls.R')

# Read in data -----------------------------------------------------------------

ahrf <- read_csv('data/output/ahrf.csv')
sop <- read_csv('data/output/sop.csv')
new_phys <- read_csv('data/output/new-phys.csv')
aprn <- read_csv('data/output/phys-compare-aprn.csv')
bls <- read_csv('data/output/annual_wage.csv')
rural_codes <- read_csv('data/output/rural_counties.csv')
puf_util <- read_tsv('data/output/puf_util.txt')

# Merge AHRF and aggregated Physician Compare ----------------------------------

# Drop territories
ahrf <- ahrf %>% filter(state != "GU", state != "PR", state != "VI")

# New physicians
new_phys <- new_phys %>% 
  mutate(
    state_fips = as.double(state_fips), 
    county_fips = as.double(county_fips)
  )
new_phys <- new_phys %>% 
  filter(state_fips != 72, state_fips != 78)

# APRNs billing for Medicare
aprn <- aprn %>% 
  mutate(
    state_fips = as.double(state_fips), 
    county_fips = as.double(county_fips)
  )
aprn <- aprn %>% 
  filter(state_fips != 72, state_fips != 78)

ahrf <- ahrf %>% full_join(new_phys, by = c('state_fips', 'county_fips', 'year'))
ahrf <- ahrf %>% full_join(aprn, by = c('state_fips', 'county_fips', 'year'))

ahrf <- ahrf %>% 
  mutate(
    across((starts_with("new_")), ~if_else(year>=2014 & year<=2022 & is.na(.), 0, .)), 
    aprn_medicare = if_else(year>=2014 & year<= 2022 & is.na(aprn_medicare), 0, aprn_medicare)
  )

# Aggregate to state level -----------------------------------------------------

# Aggregate to state level 
state_ahrf <- ahrf %>% 
  group_by(state_fips, year) %>% 
  summarize(
    state_fips = median(state_fips), 
    tot_md = sum(tot_md, na.rm = TRUE), 
    tot_do = sum(tot_do, na.rm = TRUE), 
    tot_pcp = sum(tot_pcp, na.rm = TRUE), 
    tot_md_pcp = sum(tot_md_pcp, na.rm = TRUE), 
    tot_do_pcp = sum(tot_do_pcp, na.rm = TRUE), 
    tot_md_spec = sum(tot_md_spec, na.rm = TRUE), 
    tot_do_spec = sum(tot_do_spec, na.rm = TRUE),
    tot_aprn = sum(tot_aprn, na.rm = TRUE), 
    population = sum(population, na.rm = TRUE), 
    new_md = sum(new_md, na.rm = TRUE), 
    new_do = sum(new_do, na.rm = TRUE),
    new_md_pcp = sum(new_md_pcp, na.rm = TRUE),
    new_do_pcp = sum(new_do_pcp, na.rm = TRUE),
    new_md_spec = sum(new_md_spec, na.rm = TRUE),
    new_do_spec = sum(new_do_spec, na.rm = TRUE)
  )
state_ahrf <- state_ahrf %>% 
  mutate(across((starts_with("tot_")), function(x) ifelse(year == 2009, NA, x))) %>% 
  mutate(across((ends_with("_pcp") & !starts_with("new")), function(x) ifelse(year < 2005 | year == 2006, NA, x))) %>% 
  mutate(across((ends_with("_spec") & !starts_with("new")), function(x) ifelse(year < 2005 | year == 2006, NA, x)))

# Replace 0's with NA 
state_ahrf <- state_ahrf %>% 
  mutate(
    across(starts_with('tot_'), ~replace(., . == 0, NA)), 
    population = ifelse(population == 0, NA, population), 
    across(starts_with('new_'), ~if_else(year < 2014 | year > 2022, NA, .))
  )

# Calculate MD per pop ratio
state_ahrf <- state_ahrf %>% 
  mutate(
    md_per_10k = tot_md / population * 10000, 
    md_pcp_per_10k = tot_md_pcp / population * 10000, 
    md_spec_per_10k = tot_md_spec / population * 10000, 
    do_per_10k = tot_do / population * 10000, 
    do_pcp_per_10k = tot_do_pcp / population * 10000, 
    do_spec_per_10k = tot_do_spec / population * 10000, 
    new_md_per_10k = new_md / population * 10000, 
    new_md_pcp_per_10k = new_md_pcp / population * 10000, 
    new_md_spec_per_10k = new_md_spec / population * 10000, 
    new_do_per_10k = new_do / population * 10000, 
    new_do_pcp_per_10k = new_do_pcp / population * 10000, 
    new_do_spec_per_10k = new_do_spec / population * 10000,
    new_md_per_cap = new_md / population, 
    new_md_pcp_per_cap = new_md_pcp / population, 
    new_md_spec_per_cap = new_md_spec / population, 
    new_do_per_cap = new_do / population, 
    new_do_pcp_per_cap = new_do_pcp / population, 
    new_do_spec_per_cap = new_do_spec / population, 
    aprn_per_10k = tot_aprn / population * 10000
  )
ahrf <- ahrf %>% 
  mutate(
    md_per_10k = tot_md / population * 10000, 
    md_pcp_per_10k = tot_md_pcp / population * 10000, 
    md_spec_per_10k = tot_md_spec / population * 10000,
    do_per_10k = tot_do / population * 10000, 
    do_pcp_per_10k = tot_do_pcp / population * 10000, 
    do_spec_per_10k = tot_do_spec / population * 10000,
    new_md_per_10k = new_md / population * 10000, 
    new_md_pcp_per_10k = new_md_pcp / population * 10000, 
    new_md_spec_per_10k = new_md_spec / population * 10000, 
    new_do_per_10k = new_do / population * 10000, 
    new_do_pcp_per_10k = new_do_pcp / population * 10000, 
    new_do_spec_per_10k = new_do_spec / population * 10000,
    new_md_per_cap = new_md / population, 
    new_md_pcp_per_cap = new_md_pcp / population, 
    new_md_spec_per_cap = new_md_spec / population, 
    new_do_per_cap = new_do / population, 
    new_do_pcp_per_cap = new_do_pcp / population, 
    new_do_spec_per_cap = new_do_spec / population, 
    aprn_per_10k = tot_aprn / population * 10000, 
    aprn_medicare_per_10k = aprn_medicare / population * 10000
  )

# Merge SOP and AHRF -----------------------------------------------------------

# Filter to 2000 to 2022
ahrf <- ahrf %>% filter(year >= 2000, year <= 2022)
state_ahrf <- state_ahrf %>% filter(year >= 2000, year <= 2022)

county_df <- ahrf  %>% 
  left_join(sop, by = c('state_fips' = 'stateFIPS',
                        'year' = 'year')) 
state_df <- state_ahrf %>% 
  left_join(sop, by = c('state_fips' = 'stateFIPS',
                        'year' = 'year')) 

# Merge with BLS wage data -----------------------------------------------------

bls <- bls %>% 
  select(-c(st)) %>% 
  mutate(st_fips = as.double(st_fips))

# BLS is state only 
state_df <- state_df %>% 
  left_join(bls, by = c('state_fips' = 'st_fips',
                        'year' = 'year'))

# Create treatment variables ---------------------------------------------------

# Create time to treat variable 
state_df <- state_df %>% 
  mutate(
    time_to_treat = ifelse(treat == 1, year - effective_year, 0)
  )
county_df <- county_df %>% 
  mutate(
    time_to_treat = ifelse(treat == 1, year - effective_year, 0)
  )

# Other variable creation ------------------------------------------------------

state_df <- state_df %>% 
  mutate(
    tot_pcp = ifelse(year < 2010, tot_md_pcp + tot_do_pcp, tot_pcp), 
    pcp_per_10k = tot_pcp / population * 10000, 
    tot_spec = tot_md_spec + tot_do_spec, 
    spec_per_10k = md_spec_per_10k + do_spec_per_10k,
    new_pcp = new_md_pcp + new_do_pcp, 
    new_pcp_per_10k = new_md_pcp_per_10k + new_do_pcp_per_10k, 
    Dmissing_md = is.na(tot_md), 
    Dmissing_do = is.na(tot_do), 
    Dmissing_md_pcp = is.na(tot_md_pcp), 
    Dmissing_do_pcp = is.na(tot_do_pcp),
    Dmissing_aprn = is.na(tot_aprn), 
    Dmissing_pop = is.na(population), 
    treat_year = ifelse(treat == 0, Inf, effective_year)
  )

county_df <- county_df %>% 
  mutate(
    tot_pcp = ifelse(year < 2010, tot_md_pcp + tot_do_pcp, tot_pcp), 
    pcp_per_10k = tot_pcp / population * 10000,
    tot_spec = tot_md_spec + tot_do_spec, 
    spec_per_10k = md_spec_per_10k + do_spec_per_10k, 
    new_pcp = new_md_pcp + new_do_pcp, 
    new_pcp_per_10k = new_md_pcp_per_10k + new_do_pcp_per_10k, 
    Dmissing_md = is.na(tot_md), 
    Dmissing_do = is.na(tot_do), 
    Dmissing_md_pcp = is.na(tot_md_pcp), 
    Dmissing_do_pcp = is.na(tot_do_pcp),
    Dmissing_aprn = is.na(tot_aprn), 
    Dmissing_pop = is.na(population),
    prop_female = pop_female / population, 
    prop_black = pop_black / population, 
    prop_aian = pop_aian / population, 
    prop_asian = pop_asian / population, 
    prop_hisp = pop_hisp / population, 
    prop_medicare = medicare_aged_tot / population, 
    treat_year = ifelse(treat == 0, Inf, effective_year)
  )

# Merge in urban rural codes for counties 
county_df <- county_df %>% left_join(rural_codes, by = c('state_fips', 'county_fips'))

# Service mix dataset ----------------------------------------------------------

puf_util <- puf_util %>% 
  mutate(
    state_fips = as.double(state_fips), 
    county_fips = as.double(county_fips)
  )

puf_util <- puf_util %>% 
  left_join(sop, by = c('state_fips' = 'stateFIPS',
                        'year' = 'year')) 

# Create treatment variables
puf_util <- puf_util %>% 
  mutate(
    time_to_treat = ifelse(treat == 1, year - effective_year, 0)
  )

# Export -----------------------------------------------------------------------

write_csv(county_df, 'data/output/final_df_county.csv')
write_csv(state_df, 'data/output/final_df_state.csv')
write_csv(puf_util, 'data/output/final_service_mix.csv')
rm(list = ls())
