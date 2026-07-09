# About ------------------------------------------------------------------------

# Cleaning AHRF data
# Author:         Shirley Cai 
# Date created:   07/28/2025 
# Last edited:    02/19/2026 

# Clean county level data ------------------------------------------------------

# Unit of observation: County-year 
# Outcomes: Total active MDs (non-federal), total active DOs (non-federal)
#           PCPs vs specialists
#           APRNs (2010-2019 ONLY)
# Controls: Population

## Read 2009-2010 release ------------------------------------------------------

vars_2009 <- read_excel('data/input/ahrf_2009_vars.xlsx') %>% 
  mutate(var_name_full = ifelse(is.na(year), var_name, paste(var_name, year, sep = "_")))

raw_2009 <- read_fwf(
  'data/input/area-health-resource-files/county/ahrf2009.asc',
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

## Read 2011-2012 release ------------------------------------------------------

raw_2011 <- read_fwf(
  'data/input/area-health-resource-files/county/ahrf2011.asc',
  fwf_cols(
    state = c(00065, 00066), 
    county = c(00092, 00121), 
    state_fips = c(00122, 00123), 
    county_fips = c(00124, 00126), 
    year = c(00032, 00035), 
    fte_rn_st_gen_2008 = c(15243, 15248),
    fte_rn_stng_lt_2008 = c(15261, 15266),
    fte_rn_va_2008 = c(15279, 15284)
  )
)

# Remove State of Alaska observation 
raw_2011 <- raw_2011 %>% filter(!grepl("State of", county))
raw_2011 <- raw_2011 %>% mutate(across(!state & !county, as.numeric))

## Read 2012-2013 release ------------------------------------------------------

vars_2012 <- read_excel('data/input/ahrf_2012_vars.xlsx') %>% 
  mutate(var_name_full = ifelse(is.na(year), var_name, paste(var_name, year, sep = "_")))

# Read in as wide format
raw_2012 <- read_fwf(
  'data/input/area-health-resource-files/county/ahrf2012.asc',
  fwf_positions(
    start = vars_2012$start_col, 
    end = vars_2012$end_col, 
    col_names = vars_2012$var_name_full
  )
)

# Remove State of Alaska observation 
raw_2012 <- raw_2012 %>% filter(!grepl("State of", county))
raw_2012 <- raw_2012 %>% mutate(across(!state & !county, as.numeric))

## Read 2013-2014 release ------------------------------------------------------

raw_2013 <- read_fwf(
  'data/input/area-health-resource-files/county/ahrf2014.asc',
  fwf_cols(
    state = c(00065, 00066), 
    county = c(00092, 00121), 
    state_fips = c(00122, 00123), 
    county_fips = c(00124, 00126), 
    year = c(00032, 00035), 
    fte_rn_st_gen_2011 = c( 13268, 13273 ),
    fte_rn_stng_lt_2011 = c( 13286, 13291 ),
    fte_rn_va_2011 = c( 13304, 13309 )
  )
)

# Remove State of Alaska observation 
raw_2013 <- raw_2013 %>% filter(!grepl("State of", county))
raw_2013 <- raw_2013 %>% mutate(across(!state & !county, as.numeric))

## Read 2014-2015 release ------------------------------------------------------

raw_2014 <- read_fwf(
  'data/input/area-health-resource-files/county/ahrf2015.asc',
  fwf_cols(
    state = c(00065, 00066), 
    county = c(00092, 00121), 
    state_fips = c(00122, 00123), 
    county_fips = c(00124, 00126), 
    year = c(00032, 00035), 
    fte_rn_st_gen_2012 = c( 13562, 13567 ),
    fte_rn_stng_lt_2012 = c( 13580, 13585 ),
    fte_rn_va_2012 = c( 13598, 13603 )
  )
)

# Remove State of Alaska observation 
raw_2014 <- raw_2014 %>% filter(!grepl("State of", county))
raw_2014 <- raw_2014 %>% mutate(across(!state & !county, as.numeric))

## Read 2015-2016 release ------------------------------------------------------

vars_2015 <- read_excel('data/input/ahrf_2015_vars.xlsx') %>% 
  mutate(var_name_full = ifelse(is.na(year), var_name, paste(var_name, year, sep = "_")))

# Read in as wide format
raw_2015 <- read_fwf(
  'data/input/area-health-resource-files/county/ahrf2016.asc',
  fwf_positions(
    start = vars_2015$start_col, 
    end = vars_2015$end_col, 
    col_names = vars_2015$var_name_full
  )
)

# Remove State of Alaska observation 
raw_2015 <- raw_2015 %>% filter(!grepl("State of", county))
raw_2015 <- raw_2015 %>% mutate(across(!state & !county, as.numeric))

## Read 2016-2017 release ------------------------------------------------------

raw_2016 <- read_fwf(
  'data/input/area-health-resource-files/county/ahrf2017.asc',
  fwf_cols(
    state = c(00065, 00066), 
    county = c(00092, 00121), 
    state_fips = c(00122, 00123), 
    county_fips = c(00124, 00126), 
    year = c(00032, 00035), 
    fte_rn_st_gen_2014 = c( 13160, 13165 ),
    fte_rn_stng_lt_2014 = c( 13178, 13183 ),
    fte_rn_va_2014 = c( 13196, 13201 )
  )
)

# Remove State of Alaska observation 
raw_2016 <- raw_2016 %>% filter(!grepl("State of", county))
raw_2016 <- raw_2016 %>% mutate(across(!state & !county, as.numeric))

## Read 2017-2018 release ------------------------------------------------------

raw_2017 <- read_fwf(
  'data/input/area-health-resource-files/county/ahrf2018.asc',
  fwf_cols(
    state = c(00065, 00066), 
    county = c(00092, 00121), 
    state_fips = c(00122, 00123), 
    county_fips = c(00124, 00126), 
    year = c(00032, 00035), 
    fte_rn_st_gen_2016 = c( 12512, 12517 ),
    fte_rn_stng_lt_2016 = c( 12530, 12535 ),
    fte_rn_va_2016 = c( 12548, 12553 ),
    fte_rn_st_gen_2015 = c( 12518, 12523 ),
    fte_rn_stng_lt_2015 = c( 12536, 12541 )
    # Missing VA 2015
  )
)

# Remove State of Alaska observation 
raw_2017 <- raw_2017 %>% filter(!grepl("State of", county))
raw_2017 <- raw_2017 %>% mutate(across(!state & !county, as.numeric))

## Read 2018-2019 release ------------------------------------------------------

raw_2018 <- read_fwf(
  'data/input/area-health-resource-files/county/ahrf2019.asc',
  fwf_cols(
    state = c(00065, 00066), 
    county = c(00092, 00121), 
    state_fips = c(00122, 00123), 
    county_fips = c(00124, 00126), 
    year = c(00032, 00035), 
    fte_rn_st_gen_2017 = c( 12809, 12814 ),
    fte_rn_stng_lt_2017 = c( 12827, 12832 ),
    fte_rn_va_2017 = c( 12845, 12850 )
  )
)

# Remove State of Alaska observation 
raw_2018 <- raw_2018 %>% filter(!grepl("State of", county))
raw_2018 <- raw_2018 %>% mutate(across(!state & !county, as.numeric))

## Read 2019-2020 release ------------------------------------------------------

vars_2019 <- read_excel('data/input/ahrf_2019_vars.xlsx') %>% 
  mutate(var_name_full = ifelse(is.na(year), var_name, paste(var_name, year, sep = "_")))

# Read in as wide format
raw_2019 <- read_fwf(
  'data/input/area-health-resource-files/county/AHRF2020.asc',
  fwf_positions(
    start = vars_2019$start_col, 
    end = vars_2019$end_col, 
    col_names = vars_2019$var_name_full
  )
)

# Remove State of Alaska observation 
raw_2019 <- raw_2019 %>% filter(!grepl("State of", county))
raw_2019 <- raw_2019 %>% mutate(across(!state & !county, as.numeric))

## Read 2020-2021 release ------------------------------------------------------

raw_2021 <- read_fwf(
  'data/input/area-health-resource-files/county/ahrf2021.asc',
  fwf_cols(
    state = c(00065, 00066), 
    county = c(00092, 00121), 
    state_fips = c(00122, 00123), 
    county_fips = c(00124, 00126), 
    year = c(00032, 00035), 
    fte_rn_st_gen_2019 = c( 13337,13342 ),
    fte_rn_stng_lt_2019 = c( 13355,13360 ),
    fte_rn_va_2019 = c( 13373,13378 )
  )
)

# Remove State of Alaska observation 
raw_2021 <- raw_2021 %>% filter(!grepl("State of", county))
raw_2021 <- raw_2021 %>% mutate(across(!state & !county, as.numeric))

## Read 2021-2022 release ------------------------------------------------------

vars_2022 <- read_excel('data/input/ahrf_2022_vars.xlsx') %>% 
  mutate(var_name_full = ifelse(is.na(year), var_name, paste(var_name, year, sep = "_")))

raw_2022 <- read_fwf(
  'data/input/area-health-resource-files/county/ahrf2022.asc', 
  fwf_positions(
    start = vars_2022$start_col, 
    end = vars_2022$end_col, 
    col_names = vars_2022$var_name_full
  )
)

# Remove State of Alaska observation 
raw_2022 <- raw_2022 %>% filter(county != "Alaska")
raw_2022 <- raw_2022 %>% mutate(across(!state & !county, as.numeric))

## Read 2023-2024 release ------------------------------------------------------

raw_2024 <- read_csv('data/input/area-health-resource-files/county/AHRF 2023-2024 CSV/ahrf2024_Feb2025.csv')

raw_2024 <- raw_2024 %>%
  select(
    state = st_name_abbrev, 
    county = cnty_name_st_abbrev, 
    state_fips = fips_st, 
    county_fips = fips_cnty, 
    year = date_file, 
    population_2023 = popn_est_23, 
    population_2022 = popn_est_22, 
    tot_pcp_2022 = phys_nf_prim_care_pc_exc_rsdt_22,
    tot_pcp_2021 = phys_nf_prim_care_pc_exc_rsdt_21,
    tot_md_pcp_2022 = md_nf_prim_care_pc_excl_rsdnt_22,
    tot_md_pcp_2021 = md_nf_prim_care_pc_excl_rsdnt_21,
    tot_do_pcp_2022 = do_nf_prim_care_pc_excl_rsdnt_22,
    tot_do_pcp_2021 = do_nf_prim_care_pc_excl_rsdnt_21, 
    tot_aprn_2023 = aprn_npi_23,                     
    tot_aprn_2022 = aprn_npi_22,                     
    tot_np_2023 = np_npi_23,                       
    tot_np_2022 = np_npi_22,
    tot_cns_2023 = clin_nurse_spec_npi_23,          
    tot_cns_2022 = clin_nurse_spec_npi_22,          
    tot_crna_2023 = crna_npi_23,                     
    tot_crna_2022 = crna_npi_22,                     
    tot_cnm_2023 = apn_midwvs_npi_23,               
    tot_cnm_2022 = apn_midwvs_npi_22, 
    unemp_rate_2023 = unemply_rate_ge16_23, 
    unemp_rate_2022 = unemply_rate_ge16_22,
    income_pc_2022 = per_cap_persnl_incom_22, 
    income_pc_2021 = per_cap_persnl_incom_21,
    medicare_aged_tot_2022 = medcr_aged_enrolld_med_22,
    medicare_aged_tot_2021 = medcr_aged_enrolld_med_21,
    medicare_disabled_tot_2022 = medcr_disbld_enrolld_med_22, 
    medicare_disabled_tot_2021 = medcr_disbld_enrolld_med_21,
    pop_female_2022 = popn_fem_22,
    pop_female_2021 = popn_fem_21,
    pop_hisp_male_2022 = popn_mal_hsp_22, 
    pop_hisp_male_2021 = popn_mal_hsp_21, 
    pop_hisp_female_2022 = popn_fem_hsp_22, 
    pop_hisp_female_2021 = popn_fem_hsp_21, 
    pop_black_male_2022 = popn_mal_bl_22,
    pop_black_male_2021 = popn_mal_bl_21,
    pop_black_female_2022 = popn_fem_bl_22,
    pop_black_female_2021 = popn_fem_bl_21,
    pop_aian_male_2022 = popn_mal_aian_22,
    pop_aian_male_2021 = popn_mal_aian_21,
    pop_aian_female_2022 = popn_fem_aian_22,
    pop_aian_female_2021 = popn_fem_aian_21,
    pop_asian_male_2022 = popn_mal_asn_22,
    pop_asian_male_2021 = popn_mal_asn_21, 
    pop_hpi_male_2022 = popn_mal_nhpi_22,
    pop_hpi_male_2021 = popn_mal_nhpi_21, 
    pop_asian_female_2022 = popn_fem_asn_22, 
    pop_asian_female_2021 = popn_fem_asn_21, 
    pop_hpi_female_2022 = popn_fem_nhpi_22, 
    pop_hpi_female_2021 = popn_fem_nhpi_21, 
    fte_rn_st_gen_2022 = stgh_fte_rn_incl_nh_22,          
    fte_rn_st_gen_2021 = stgh_fte_rn_incl_nh_21,          
    fte_rn_stng_lt_2022 = stnglth_fte_rn_incl_nh_22,       
    fte_rn_stng_lt_2021 = stnglth_fte_rn_incl_nh_21,       
    fte_rn_va_2022 = vetn_hosp_fte_rn_incl_nh_22,     
    fte_rn_va_2021 = vetn_hosp_fte_rn_incl_nh_21     
  )

raw_2024 <- raw_2024 %>% mutate(across(!state & !county, as.numeric))

## Merge and clean up ----------------------------------------------------------

# Join singleton releases 
raw_other <- raw_2011 %>% 
  full_join(raw_2012 %>% select(-c(state, county)), by = c('state_fips', 'county_fips')) %>% 
  full_join(raw_2013 %>% select(-c(state, county, year)), by = c('state_fips', 'county_fips')) %>% 
  full_join(raw_2014 %>% select(-c(state, county, year)), by = c('state_fips', 'county_fips')) %>% 
  full_join(raw_2015 %>% select(-c(state, county)), by = c('state_fips', 'county_fips')) %>% 
  full_join(raw_2016 %>% select(-c(state, county, year)), by = c('state_fips', 'county_fips')) %>% 
  full_join(raw_2017 %>% select(-c(state, county, year)), by = c('state_fips', 'county_fips')) %>% 
  full_join(raw_2018 %>% select(-c(state, county, year)), by = c('state_fips', 'county_fips')) %>% 
  full_join(raw_2021 %>% select(-c(state, county, year)), by = c('state_fips', 'county_fips'))

# Join big releases 2019, 2022, 2024
raw_other <- raw_other %>% 
  full_join(raw_2019 %>% select(-c(state, county)), by = c('state_fips', 'county_fips')) %>% 
  full_join(raw_2022 %>% select(-c(state, county)), by = c('state_fips', 'county_fips')) %>% 
  full_join(raw_2024 %>% select(-c(state, county)), by = c('state_fips', 'county_fips'))

# Pivot to longer 
raw_other <- raw_other %>% 
  pivot_longer(
    cols = -c(state, county, state_fips, county_fips),
    names_pattern =  "(.*)_(\\d+)",
    names_to = c(".value", "year"),
    values_drop_na = TRUE
  )

# Join with 2009 release
out <- bind_rows(raw_2009, raw_other)

# Missing values in AHRF labeled as 0
out <- out %>% mutate(across(!state & !county, function(x) ifelse(x == 0, NA, x)))

# Compute additional variables
out <- out %>% 
  mutate(
    # Number of specialists
    tot_md_spec = md_pc - tot_md_pcp, 
    tot_do_spec = do_pc - tot_do_pcp,
    
    # FTE RN (excl VA)
    fte_rn = fte_rn_st_gen + fte_rn_stng_lt, 
    
    # Population demographics
    pop_hisp = ifelse(year != 2010, pop_hisp_female + pop_hisp_male, pop_hisp),
    pop_black = ifelse(year != 2010, pop_black_female + pop_black_male, pop_black), 
    pop_aian = ifelse(year != 2010, pop_aian_female + pop_aian_male, pop_aian), 
    pop_asian = ifelse(year != 2010, pop_asian_female + pop_asian_male, pop_asian), 
    pop_hpi = ifelse(year != 2010, pop_hpi_female + pop_hpi_male, pop_hpi), 
    pop_female = ifelse(year == 2010, pop_female_0_5 + pop_female_5_9 + pop_female_10_14 + pop_female_ge_15, pop_female)
  )

out <- out %>% select(-c(pop_female_0_5, pop_female_5_9, pop_female_10_14, pop_female_ge_15))


## Export data -----------------------------------------------------------------

write_csv(out, 'data/output/ahrf.csv')
rm(list = ls())
