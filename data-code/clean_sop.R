# About ------------------------------------------------------------------------

# Reformatting SOP dates
# Author:         Shirley Cai 
# Date created:   02/27/2026 
# Last edited:    02/27/2026

# Import and reformat ----------------------------------------------------------

sop <- read_csv('data/input/nurse-sop/nurse-sop.csv')

sop <- sop %>% 
  select(stateFIPS, year, FPA, effective_date) %>% 
  distinct(stateFIPS, year, FPA, .keep_all = TRUE)

sop <- sop %>% 
  mutate(
    effective_year = as.numeric(str_extract(effective_date, "^\\d{4}")), 
    effective_month = as.numeric(str_extract(effective_date, "(?<=m)\\d+"))
  ) %>% 
  distinct(stateFIPS, year, FPA, .keep_all = TRUE)

# FPA = 1 the first FULL year with FPA
sop <- sop %>% 
  group_by(stateFIPS) %>% 
  mutate(
    effective_year = case_when(
      effective_month == 1 ~ effective_year, 
      TRUE ~ effective_year + 1
    ), 
    FPA = ifelse(year >= effective_year, 1, 0), 
    FPA = ifelse(is.na(FPA), 0, FPA)
  ) %>% 
  ungroup() %>% 
  select(stateFIPS, year, FPA, effective_date, effective_year) 
sop <- sop %>% distinct(stateFIPS, year, FPA, .keep_all = TRUE)

# Define treatment 
sop <- sop %>% 
  group_by(stateFIPS) %>% 
  mutate(treat = ifelse(sum(FPA) > 0, 1, 0)) %>% 
  ungroup()

# Export -----------------------------------------------------------------------

write_csv(sop, 'data/output/sop.csv')
rm(list = ls())
