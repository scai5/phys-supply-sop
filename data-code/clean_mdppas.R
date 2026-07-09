# About ------------------------------------------------------------------------

# Cleaning physician location from MDPPAS
# Author:         Shirley Cai 
# Date created:   06/12/2024  
# Last edited:    07/08/2026
#                 Updated to include APRNs, indicated with APRN flag

# Import and merge data --------------------------------------------------------

years <- formatC(13:20, width = 2, flag = "0")

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

# Remove non-physicians
mdppas <- mdppas %>% 
  mutate(zip = str_pad(phy_zip_pos1, 5, pad = "0")) %>% 
  filter(spec_broad <= 8)

mdppas <- mdppas %>% mutate(D_aprn = ifelse(spec_broad <= 6, 0, 1))

# Place of service zip only populated for 2013-2018
mdppas <- mdppas %>% filter(year <= 2018)

message('MDPPAS missing zip location -----')
message(paste0('Phys-year obs missing ZIP: ', sum(is.na(mdppas$phy_zip_pos1))))
mdppas <- mdppas %>% filter(!is.na(phy_zip_pos1))
message('Removed obs missing ZIP')

mdppas <- mdppas %>% select(npi, year, sex, spec_broad, spec_prim_1, spec_prim_1_name, phy_zip_pos1)

# Export -----------------------------------------------------------------------

write_csv(mdppas, 'data/output/mdppas.csv')
rm(list = ls())
