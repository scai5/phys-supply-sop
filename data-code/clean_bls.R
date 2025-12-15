# About ------------------------------------------------------------------------

# Cleaning BLS wage data
# Author:         Shirley Cai 
# Date created:   09/09/2025 
# Last edited:    12/09/2025 

# Import and merge data --------------------------------------------------------

years <- c(1999:2019)

# Function to read in raw data for each year
read_bls <- function(yr){
  if(yr <= 2002){
    file_name <- paste0('data/input/bls-oews/state/state_', yr, '_dl')
  } else if(yr <= 2007) {
    file_name <- paste0('data/input/bls-oews/state/state_may', yr, '_dl')
  } else {
    file_name <- paste0('data/input/bls-oews/state/state_M', yr, '_dl')
  }
  
  extension <- ifelse(yr <= 2013, '.xls', '.xlsx')
  file_name <- paste0(file_name, extension)
  
  if(yr == 1999){
    raw <- read_excel(file_name, skip = 42)
  } else if(yr == 2000){
    raw <- read_excel(file_name, skip = 41)
  } else{
    raw <- read_excel(file_name)
  }
  
  raw <- raw %>% 
    clean_names() %>% 
    mutate(
      year = yr
    )
  
  # Cast all to character
  raw <- raw %>% mutate_all(as.character)
}

raw <- lapply(years, read_bls)
bls_agg <- do.call(bind_rows, raw)  

rm(raw)
gc()

# Clean up data ----------------------------------------------------------------

# Combine columns with different names
bls_agg <- bls_agg %>% 
  mutate(
    occ_title = ifelse(is.na(occ_title), occ_titl, occ_title)
  ) %>% 
  rename(
    st_fips = area
  )

bls_agg <- bls_agg %>% select(-c(occ_titl))

# Remove territories
st_codes <- bls_agg %>% filter(year == 2018) %>% distinct(st_fips, st, state)
bls_agg <- bls_agg %>%
  select(-c(st, state)) %>% 
  left_join(st_codes, by = c("st_fips"))
bls_agg <- bls_agg %>% 
  filter(state != "Guam", state != "Puerto Rico", state != "Virgin Islands")

# Cast to numeric 
bls_agg <- bls_agg %>%
  mutate(across(starts_with("tot_"), as.numeric)) %>% 
  mutate(across(starts_with("emp_"), as.numeric)) %>% 
  mutate(across(starts_with("pct_"), as.numeric)) %>% 
  mutate(across(starts_with("a_"), as.numeric)) %>% 
  mutate(across(starts_with("h_"), as.numeric)) %>% 
  mutate(across(starts_with("jobs_"), as.numeric))
bls_agg <- bls_agg %>% mutate(year = as.numeric(year))

# Select doctor occupation codes 
bls_agg <- bls_agg %>% filter(grepl("29-106", occ_code) | grepl("29-121", occ_code) | 
                                grepl("29-122", occ_code) | grepl("29-124", occ_code))
bls_agg <- bls_agg %>% mutate(occ_title = toupper(occ_title))

# xw 2018 codes to 2010 codes 
code_xw <- read_excel('data/input/bls-oews/soc_2010_to_2018_crosswalk.xlsx', skip = 8) %>% 
  clean_names() %>% 
  rename(
    soc_2010  = x2010_soc_code, 
    soc_2010_title = x2010_soc_title,
    soc_2018 = x2018_soc_code,
    soc_2018_title = x2018_soc_title
  )

# TODO: Deal with 2019 -- need to average things up I think 
#bls_agg <- bls_agg %>% 
#  left_join(code_xw, by = c('occ_code' = 'soc_2018')) %>% 
#  mutate(
#    occ_std = ifelse(year > 2018, soc_2010, occ_code) 
#  )

# Up to 2018 for now 
bls_agg <- bls_agg %>% 
  filter(year <= 2018) %>% 
  mutate(
    occ_abbr = case_when(occ_code == "29-1061" ~ "anes", 
                             occ_code == "29-1062" ~ "fm",
                             occ_code == "29-1063" ~ "im",
                             occ_code == "29-1064" ~ "obgyn",
                             occ_code == "29-1065" ~ "peds",
                             occ_code == "29-1066" ~ "psych",
                             occ_code == "29-1067" ~ "surgeon",
                             occ_code == "29-1069" ~ "other")
  )

# Pivot to wide
bls_agg <- bls_agg %>% 
  select(st_fips, st, year, occ_abbr, tot_emp, h_mean, a_mean, h_median, a_median) %>% 
  pivot_wider(
    names_from = occ_abbr, 
    values_from = c(tot_emp, h_mean, a_mean, h_median, a_median), 
    names_glue = "{occ_abbr}_wage_{.value}"
  )

# Diagnostics ------------------------------------------------------------------

na_summary <- bls_agg %>%
  group_by(year) %>%
  summarise(across(everything(), ~sum(is.na(.))))

# Median vars have too many missing, so use mean 
bls_agg <- bls_agg %>% 
  select(st_fips, st, year, ends_with("a_mean"))

# Which states have missing data?
missing_details <- bls_agg %>%
  group_by(year, st) %>%
  summarise(across(everything(), ~any(is.na(.))), .groups = "drop") %>%
  filter(if_any(-c(year, st), ~.))
missing_sum <- missing_details %>% 
  group_by(st) %>% 
  summarise(
    missing_fm = sum(fm_wage_a_mean), 
    missing_im = sum(im_wage_a_mean), 
    missing_peds = sum(peds_wage_a_mean), 
    missing_anes = sum(anes_wage_a_mean), 
    missing_psych = sum(psych_wage_a_mean), 
    missing_surgeon = sum(surgeon_wage_a_mean), 
    missing_obgyn = sum(obgyn_wage_a_mean), 
    missing_other = sum(other_wage_a_mean)
  )

# Adjust for inflation ---------------------------------------------------------

cpi <- read_excel('data/input/bls-oews/phys_services_cpi_w.xlsx', skip = 11) %>% 
  clean_names()

cpi_2015 <- cpi$annual[cpi$year == 2015]

# Reindex to 2015 dollars
cpi <- cpi %>% mutate(cpi = annual / cpi_2015) %>% select(-c(annual))

bls_agg <- bls_agg %>% 
  left_join(cpi, by = c('year')) 

# Calculate real wage 
bls_agg <- bls_agg %>% 
  mutate(
    anes_wage_real = anes_wage_a_mean / cpi, 
    fm_wage_real = fm_wage_a_mean / cpi, 
    im_wage_real = im_wage_a_mean / cpi,
    peds_wage_real = peds_wage_a_mean / cpi, 
    psych_wage_real = psych_wage_a_mean / cpi, 
    surgeon_wage_real = surgeon_wage_a_mean / cpi, 
    obgyn_wage_real = obgyn_wage_a_mean / cpi, 
    other_wage_real = other_wage_a_mean / cpi
  )

# Export -----------------------------------------------------------------------

write_csv(bls_agg, 'data/output/annual_wage.csv')
rm(list = ls())
