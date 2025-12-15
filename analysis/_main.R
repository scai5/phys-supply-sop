# About ------------------------------------------------------------------------

# Main analysis file 
# Author:         Shirley Cai 
# Date created:   08/22/2025 
# Last edited:    12/12/2025 

# Preliminary ------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, janitor, modelsummary, gt, lubridate, stringr,
               fixest, ggfixest, did, crosswalkr)

# Read in data -----------------------------------------------------------------

# Read in data 
df <- read_csv('data/output/final_df_state.csv')
df_c <- read_csv('data/output/final_df_county.csv')

# TODO: Move to _build_data.R
df <- df %>% 
  group_by(state) %>% 
  mutate(
    treat = ifelse(sum(FPA)>0, 1, 0)
  ) %>% 
  ungroup()
df <- df %>% 
  mutate(
    time_to_treat = ifelse(treat == 1, year - effective_year, 0)
  )

df_c <- df_c %>% 
  group_by(state) %>% 
  mutate(
    treat = ifelse(sum(FPA)>0, 1, 0)
  ) %>% 
  ungroup()
df_c <- df_c %>% 
  mutate(
    time_to_treat = ifelse(treat == 1, year - effective_year, 0)
  )

# Create missing indicators 
df <- df %>% 
  mutate(
    Dmissing_md = is.na(tot_md), 
    Dmissing_do = is.na(tot_do), 
    Dmissing_md_pcp = is.na(tot_md_pcp), 
    Dmissing_do_pcp = is.na(tot_do_pcp),
    Dmissing_pop = is.na(population)
  )

df_c <- df_c %>% 
  mutate(
    Dmissing_md = is.na(tot_md), 
    Dmissing_do = is.na(tot_do), 
    Dmissing_md_pcp = is.na(tot_md_pcp), 
    Dmissing_do_pcp = is.na(tot_do_pcp),
    Dmissing_pop = is.na(population)
  )

# Create time to treat variable 
df <- df %>% 
  mutate(
    time_to_treat = ifelse(treat == 1, year - effective_year, 0)
  )
df_c <- df_c %>% 
  mutate(
    time_to_treat = ifelse(treat == 1, year - effective_year, 0)
  )

# Analysis files ---------------------------------------------------------------

source('analysis/1-descriptives.R')
source('analysis/2-event-study.R')
source('analysis/3-staggered-did.R')