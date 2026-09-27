# About ------------------------------------------------------------------------

# Main analysis file 
# Author:         Shirley Cai 
# Date created:   08/22/2025 
# Last edited:    09/26/2026 

# Preliminary ------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, janitor, modelsummary, gt, lubridate, stringr,
               fixest, ggfixest, did, crosswalkr, broom, data.table, haven, arrow,
               tigris, ivreg)

# Read in data -----------------------------------------------------------------

# Read in data 
df <- read_csv('data/output/final_df_state.csv')
df_c <- read_csv('data/output/final_df_county.csv')

# Analysis files ---------------------------------------------------------------

source('analysis/0-helpers.R')

# TODO: Edit this
source('analysis/1-descriptives.R')

source('analysis/2-agg-labor-supply.R')
source('analysis/3-new-pcps.R')
source('analysis/4-service-volume.R')
source('analysis/5-service-mix.R')
source('analysis/6-by-aprn-density.R')
source('analysis/7-aprn-density-ssiv.R')
source('analysis/8-by-urban-rural.R')
