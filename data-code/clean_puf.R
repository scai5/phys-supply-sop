# About ------------------------------------------------------------------------

# Aggregate Medicare utilization PUF
# Author:         Shirley Cai 
# Date created:   06/18/2024  
# Last edited:    07/27/2026
#                 Specialty filtering using NPPES

# Import PUF by provider -------------------------------------------------------

years <- formatC(13:22, width = 2, flag = "0")

read_agg <- function(yr){
  if(yr < 17){
    raw <- read_csv(paste0("data/input/part-b-puf/by-provider/20", yr, "/MUP_PHY_R24_P05_V10_D", yr, "_Prov.csv"))
  } else {
    raw <- read_csv(paste0("data/input/part-b-puf/by-provider/20", yr, "/MUP_PHY_R24_P07_V10_D", yr, "_Prov.csv"))
  }
  
  # Clean variable names 
  raw <- raw %>% 
    clean_names() %>%  
    filter(rndrng_prvdr_ent_cd == "I") %>% 
    select(
      npi = rndrng_npi, 
      tot_benes, 
      tot_services = tot_srvcs,
      tot_charges = tot_sbmtd_chrg, 
      tot_allowed_amt = tot_mdcr_alowd_amt, 
      tot_payment = tot_mdcr_pymt_amt, 
      tot_std_amt = tot_mdcr_stdzd_amt, 
      bene_avg_age, 
      bene_female = bene_feml_cnt, 
      bene_male = bene_male_cnt, 
      bene_white = bene_race_wht_cnt, 
      bene_black = bene_race_black_cnt, 
      bene_asian = bene_race_api_cnt, 
      bene_hispanic = bene_race_hspnc_cnt, 
      bene_aian = bene_race_nat_ind_cnt, 
      bene_other_race = bene_race_othr_cnt, 
      bene_dual_enroll = bene_dual_cnt, 
      bene_medicare_only = bene_ndual_cnt, 
      bene_avg_risk = bene_avg_risk_scre
    ) %>% 
    mutate(
      year = as.numeric(paste0("20", yr))
    )
}

raw <- lapply(years, read_agg)
puf_agg <- do.call(rbind.data.frame, raw)  

# Free up memory
rm(raw)
gc()

# Create provider level variables
puf_agg <- puf_agg %>% 
  mutate(
    prop_female = bene_female / tot_benes, 
    prop_black = bene_black / tot_benes, 
    prop_asian = bene_asian / tot_benes, 
    prop_hispanic = bene_hispanic / tot_benes, 
    prop_dual_enroll = bene_dual_enroll / tot_benes
  )

puf_agg <- puf_agg %>% 
  select(npi, year, tot_benes, 
         bene_avg_age, bene_avg_risk,
         prop_female, prop_black, prop_asian, prop_hispanic, prop_dual_enroll)

write_tsv(puf_agg,'data/temp/puf-agg.txt')
rm(puf_agg)
gc()

# Import PUF by provider and service -------------------------------------------

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
      year = as.numeric(paste0("20", yr))
    )
  
  return(raw)
}

raw <- lapply(years, read_puf)
puf <- do.call(rbind.data.frame, raw)  

# Free up memory
rm(raw)
gc()

# Sample restriction -----------------------------------------------------------

nppes <- read_csv('data/output/nppes.csv') %>% 
  mutate(D_nppes = 1) 
puf <- puf %>% left_join(nppes, by = c('npi', 'year'))

rm(nppes)
gc()

message(paste0('Total observations: ', nrow(puf)))
message(paste0('No NPPES match: ', sum(is.na(puf$D_nppes))))

puf <- puf %>% filter(D_pcp == 1 | D_aprn == 1)
puf <- puf %>% select(-c(D_nppes))

puf <- puf %>% 
  mutate(
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

prac_loc <- read_csv('data/output/phys_aprn_location.csv') %>% 
  select(npi, year, state_fips, county_fips, full_fips, zip)
puf_util <- puf_util %>% 
  left_join(prac_loc, by = c('npi', 'year'))

puf_util <- puf_util %>% filter(!is.na(full_fips))

# Merge with provider level PUF ------------------------------------------------

puf_agg <- read_tsv('data/temp/puf-agg.txt')

puf_util <- puf_util %>% left_join(puf_agg, by = c('npi', 'year'))

# Export -----------------------------------------------------------------------

write_tsv(puf,'data/output/puf.txt')
write_tsv(puf_util, 'data/output/puf_util.txt')

rm(list = ls())
gc()
