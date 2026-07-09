# About ------------------------------------------------------------------------

# Aggregate CMS Physician Fee Schedule RVUs
# Author:         Shirley Cai 
# Date created:   07/07/2026  
# Last edited:    07/07/2026

# Import data ------------------------------------------------------------------

years <- formatC(13:22, width = 2, flag = "0")

read_rvu <- function(yr){
  
  raw <- read_csv(paste0('data/input/pfs-rvu/RVU', yr, "A/PPRRVU", yr, "_JAN.csv"), 
                  skip = 9, na = c("")) %>% 
    clean_names() %>% 
    rename(
      hcpcs_cd = hcpcs, 
      status_code = code, 
      wrvu = rvu_6, 
      non_fac_pe_rvu = pe_rvu_7, 
      D_non_fac_rare = indicator_8, 
      fac_pe_rvu = pe_rvu_9, 
      D_fac_rare = indicator_10, 
      mp_rvu = rvu_11, 
      tot_non_fac_rvu = total_12, 
      tot_fac_rvu = total_13, 
      pctc_indicator = ind
    ) 
  
  raw <- raw %>% select(hcpcs_cd, mod, status_code, wrvu, non_fac_pe_rvu, D_non_fac_rare, 
                        fac_pe_rvu, D_fac_rare, mp_rvu, tot_fac_rvu, tot_non_fac_rvu, pctc_indicator)
  
  raw <- raw %>%
    filter(
      status_code == "A",                        # Active codes with payment
      (pctc_indicator < 3 | pctc_indicator > 5), # Remove TC only (3), global test only (4), incident-to (5)
      is.na(mod)                                 # Only global codes, no discontinued procedures
    )
  
  raw <- raw %>% mutate(year = as.numeric(paste0("20", yr)))
}
  
raw <- lapply(years, read_rvu)
rvu <- do.call(rbind.data.frame, raw)  
  
# Free up memory
rm(raw)
gc()
  
rvu <- rvu %>% 
  mutate(
    D_non_fac_rare = ifelse(is.na(D_non_fac_rare), 0, D_non_fac_rare),
    D_non_fac_rare = ifelse(D_non_fac_rare == "NA", 1, 0), 
    D_fac_rare = ifelse(is.na(D_fac_rare), 0, D_fac_rare),
    D_fac_rare = ifelse(D_fac_rare == "NA", 1, 0)
  )

rvu <- rvu %>% select(!c(mod, status_code))

# Export -----------------------------------------------------------------------

write_csv(rvu, 'data/output/rvu.csv')
rm(list = ls())
gc()
