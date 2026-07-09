# About ------------------------------------------------------------------------

# Identifying and aggregating new physician location 
# Author:         Shirley Cai 
# Date created:   01/21/2026  
# Last edited:    07/08/2026 

# Goal: Identify obs that represent a physician's "first" location choice 
#       Aggregate number of "first" location choices to the county-year level
#         - Total new phys, new MDs, new DOs 
#         - New PCPs, new MD PCPs, new DO PCPs
#         - New specialists, new MD specialists, new DO specialists

# Keep physician's "first" location choice -------------------------------------

# Read in location data for physicians only 
df <- read_csv('data/output/phys_aprn_location.csv') %>% filter(D_aprn = 0)

# Merge in graduation year
pc_static <- read_tsv('data/output/phys-compare-static.txt')
df <- df %>% left_join(pc_static, by = 'npi')

message(paste0('Total observations: ', nrow(df)))
message(paste0('Obs missing graduation year: ', sum(is.na(df$graduation_year))))
df <- df %>% filter(!is.na(graduation_year))
message('Removed obs missing graduation year')

# Drop observations unreasonably close or far from graduation year
message(paste0('Total observations: ', nrow(df)))
message(paste0('Obs < 3 yrs from grad: ', sum(df$year - df$graduation_year < 3)))
message(paste0('Obs > 10 yrs from grad: ', sum(df$year - df$graduation_year > 10)))
df <- df %>% filter(year - graduation_year > 3, year - graduation_year < 10)
message('Removed obs too close or far from grad year')

# Within NPI group, keep the oldest observation 
df <- df %>% 
  group_by(npi) %>% 
  filter(year == min(year)) %>% 
  slice(1)

# First time I see you after the first year = "first" location choice 
df <- df %>% filter(year > 2013)

# Merge in NPPES information ---------------------------------------------------

# Merge in NPPES
nppes <- read_csv('data/output/nppes.csv') %>% 
  mutate(D_nppes = 1)
df <- df %>% left_join(nppes, by = 'npi')

message(paste0('Total observations: ', nrow(df)))
message(paste0('No NPPES match: ', sum(is.na(df$D_nppes))))

# Aggregate to county-year level -----------------------------------------------

agg_df <- df %>% 
  group_by(county, year) %>% 
  summarise(
    new_md = sum(D_md, na.rm = TRUE),
    new_do = sum(D_do, na.rm = TRUE),
    new_md_pcp = sum(D_md & D_pcp, na.rm = TRUE),
    new_do_pcp = sum(D_do & D_pcp, na.rm = TRUE),
    new_md_spec = sum(D_md & D_spec, na.rm = TRUE),
    new_do_spec = sum(D_do & D_spec, na.rm = TRUE)
  )

# TODO: Figure out why 2023 is all missing zip 

# Export -----------------------------------------------------------------------

write_csv(agg_df, 'data/output/new-phys.csv')
rm(list = ls())
