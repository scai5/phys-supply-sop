# About ------------------------------------------------------------------------

# Cleaning physician location from MDPPAS
# Author:         Shirley Cai 
# Date created:   06/12/2024  
# Last edited:    07/16/2026
#                 Updated to include APRNs, indicated with APRN flag

# Import and merge data --------------------------------------------------------

# Place of service zip only populated for 2013-2018
years <- formatC(13:18, width = 2, flag = "0")

read_mdppas <- function(yr){
  raw <- read_csv(paste0("data/input/mdppas/PhysicianData_20", yr, ".csv"))
  return(raw)
}

# Assume parsing issues are not large issues
raw <- lapply(years, read_mdppas)
mdppas <- do.call(rbind.data.frame, raw) %>% 
  clean_names()

# Free up memory
rm(raw)
gc()

# Filter to only physicians and APRN by NPPES specialty
# Remove non-physicians
ptax <- read_csv('data/output/nppes_taxonomy.csv') %>% 
  mutate(D_ptax = TRUE)
mdppas <- mdppas %>% 
  left_join(ptax, by = c('npi', 'year')) %>% 
  filter(D_ptax == TRUE)
mdppas <- mdppas %>% select(-c(D_ptax))

message('MDPPAS missing zip location -----')
message(paste0('Phys-year obs missing ZIP: ', sum(is.na(mdppas$phy_zip_pos1))))
mdppas <- mdppas %>% filter(!is.na(phy_zip_pos1))
message('Removed obs missing ZIP')

mdppas <- mdppas %>% select(npi, year, sex, phy_zip_pos1, D_aprn)

# Export -----------------------------------------------------------------------

write_csv(mdppas, 'data/output/mdppas.csv')
rm(list = ls())
