# About ------------------------------------------------------------------------

# Aggregate Medicare utilization PUF
# Author:         Shirley Cai 
# Date created:   06/18/2024  
# Last edited:    07/09/2026

# Import PUF by provider and service -------------------------------------------

years <- formatC(13:22, width = 2, flag = "0")

read_puf <- function(yr){
  raw <- read_csv(paste0("data/input/part-b-puf/by-provider-service/Medicare_Physician_Other_Practitioners_by_Provider_and_Service_20", yr, ".csv"))
  
  # Clean variable names 
  raw <- raw %>% 
    clean_names() %>% 
    filter(rndrng_prvdr_ent_cd == "I") %>% 
    select(
      npi = rndrng_npi, 
      credentials = rndrng_prvdr_crdntls, 
      gender = rndrng_prvdr_gndr, 
      specialty = rndrng_prvdr_type, 
      Dmedicare = rndrng_prvdr_mdcr_prtcptg_ind, 
      hcpcs_cd, 
      hcpcs_desc, 
      hcpcs_drug_ind, 
      place_of_service = place_of_srvc, 
      tot_benes, 
      tot_services = tot_srvcs, 
      tot_bene_day_services = tot_bene_day_srvcs, 
      avg_submitted_charge = avg_sbmtd_chrg, 
      avg_medicare_allowed_amt = avg_mdcr_alowd_amt, 
      avg_medicare_payment = avg_mdcr_pymt_amt
    ) %>% 
    mutate(
      year = as.numeric(paste0("20", yr)),
      credentials = str_replace_all(credentials, " ", ""), 
      credentials = str_replace_all(credentials, "\\.", "")
    )
  
  return(raw)
}

raw <- lapply(years, read_puf)
puf <- do.call(rbind.data.frame, raw)  

# Free up memory
rm(raw)
gc()

# Sample restriction -----------------------------------------------------------

# TODO: Better specialty filtering using NPPES

pcp_specialty <- c("Family Practice", "General Practice", "Internal Medicine", "Pediatric Medicine")
aprn_specialty <- c("Nurse Practitioner",
                    "Certified Registered Nurse Anesthetist (CRNA)", "CRNA", 
                    "Certified Nurse Midwife",
                    "Certified Clinical Nurse Specialist")
specialty_list <- c(pcp_specialty, aprn_specialty)

puf <- puf %>% 
  filter(((grepl("MD|DO", credentials) | is.na(credentials)) & specialty %in% pcp_specialty) | specialty %in% aprn_specialty)

puf <- puf %>% 
  mutate(
    D_aprn = ifelse(specialty %in% aprn_specialty, 1, 0), 
    D_np = ifelse(specialty == "Nurse Practitioner", 1, 0),
    D_office = ifelse(place_of_service == "O", 1, 0),
    D_female = ifelse(gender == "F", 1, 0)
  )

gc()

# Calculating service mix ------------------------------------------------------

# Merge in RVUs
rvu <- read_csv('data/output/rvu.csv') %>% select(hcpcs_cd, year, wrvu)
rvu_2013 <- rvu %>% filter(year == 2013) %>% select(!c(year)) %>% rename(wrvu_2013 = wrvu)
puf <- puf %>% 
  left_join(rvu, by = c('hcpcs_cd', 'year')) %>% 
  left_join(rvu_2013, by = c('hcpcs_cd'))

# Summarize RVUs per service at practitioner-year level
puf_util <- puf %>% 
  group_by(npi, year) %>% 
  summarise(
    # wRVUs
    tot_wrvu = sum(wrvu * tot_services, na.rm = TRUE), 
    tot_wrvu_office = sum(wrvu * tot_services * D_office, na.rm = TRUE), 
    tot_wrvu_2013 = sum(wrvu_2013 * tot_services, na.rm = TRUE),
    tot_wrvu_2013_office = sum(wrvu_2013 * tot_services * D_office, na.rm = TRUE),
    
    # Total services
    tot_services_office = sum(tot_services * D_office, na.rm = TRUE), 
    tot_services = sum(tot_services), 
    
    # Practitioner characteristics
    D_aprn = first(D_aprn), 
    D_female = first(D_female)
  )

puf_util <- puf_util %>% 
  mutate(
    wrvu_per_service = tot_wrvu / tot_services, 
    wrvu_2013_per_service = tot_wrvu_2013 / tot_services, 
    wrvu_office_per_service = tot_wrvu_office / tot_services_office, 
    wrvu_2013_office_per_service = tot_wrvu_2013_office / tot_services_office, 
    share_office_services = tot_services_office / tot_services
  )

# Merge with location data -----------------------------------------------------

prac_loc <- read_csv('data/output/phys_aprn_location.csv')
puf_util <- puf_util %>% 
  left_join(prac_loc, by = c('npi', 'year'))

puf_util <- puf_util %>% filter(!is.na(full_fips))

# Export -----------------------------------------------------------------------

write_tsv(puf,'data/output/puf.txt')
write_tsv(puf_util, 'data/output/puf_util.txt')

rm(list = ls())
gc()
