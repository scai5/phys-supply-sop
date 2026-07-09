# About ------------------------------------------------------------------------

# ZIP-county crosswalk
# Author:         Shirley Cai 
# Date created:   12/11/2024 
# Last edited:    02/03/2026

# Import and merge data --------------------------------------------------------

xw <- read_excel('data/input/zip2county/ZIP_COUNTY_122010.xlsx') %>% 
  clean_names()

# Map each ZIP to county with the highest ratio of business addresses 
set.seed(1234)
xw <- xw %>% 
  group_by(zip) %>%
  slice(which.max(rank(bus_ratio, ties.method = "random"))) %>% 
  select(c(zip, county))

# Export -----------------------------------------------------------------------

write_tsv(xw,'data/output/zipcounty-xw.txt')
rm(list = ls())
