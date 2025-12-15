# About ------------------------------------------------------------------------

# ZIP-CBSA crosswalk
# Author:         Shirley Cai 
# Date created:   12/11/2024 
# Last edited:    12/11/2024

# Import and merge data --------------------------------------------------------

xw <- read_excel('data/input/zip2cbsa_xw/ZIP_CBSA_122010.xlsx') %>% 
  clean_names()

# Map each ZIP to county with the highest ratio of business addresses 
set.seed(1234)
xw <- xw %>% 
  group_by(zip) %>%
  slice(which.max(rank(bus_ratio, ties.method = "random"))) %>% 
  select(c(zip, cbsa))

# Export -----------------------------------------------------------------------

write_tsv(xw,'data/output/zipcbsa-xw.txt')
rm(list = ls())
