# About ------------------------------------------------------------------------

# "High impact" counties DiD
# Author:         Shirley Cai 
# Date created:   02/27/2026 
# Last edited:    09/26/2026 

# Idea: Leverage within state variation using pre-policy APRN presence @ county
# Pre-policy APRN presence = APRNs / (APRN + PCP) in 2010

# TODO: Revising the structure of this... 
# Specifications 
#   1. DiD within counties with the highest quartile of APRN presence 
#   2. DiD within counties with the lowest quartile of APRN presence 
#   3. DiD within counties with the highest quartile within state 
#   4. DiD within counties with the lowest quartile within state
#   5. Triple difference

# Specifications
# Across national quartiles and within state quartiles of APRN presence
#   1. 

# Remaining endogeneity issue: Counties with high and low APRN presence are going 
# to have differences in productivity, etc., so we cannot compare them directly
#       -> SSIV, instrumenting for # APRNs billing 

# Data preparation -------------------------------------------------------------

df_c <- df_c %>%
  mutate(
    full_fips = paste0(formatC(state_fips, width = 2, flag = "0"), 
                       formatC(county_fips, width = 3, flag = "0")),
    full_fips = as.numeric(full_fips)
  )
df_c <- df_c %>% mutate(treat_year = ifelse(treat == 0, 0, effective_year))
df_ss <- df_c %>% filter(year >= 2010, year <= 2022)

df_util <- read_csv('data/output/final_service_mix.csv')
df_util_untrim <- df_util 
df_util <- df_util_untrim %>% filter(wrvu_per_service < 5)
df_util <- df_util %>% mutate(treat_year = ifelse(treat == 0, 0, effective_year))

# Restrict sample to certain years pre and post policy 
pre_periods <- 5
post_periods <- 5
balance_e <- NULL

# National quartiles -----------------------------------------------------------

## Full ------------------------------------------------------------------------

df_q <- df_ss
df_md_q <- df_util %>% filter(D_aprn == 0)
df_aprn_q <- df_util %>% filter(D_aprn == 1)

# Physician supply
c_pcp_pc <- get_cs("pcp_per_10k", "PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/full/", file_ext = "_county", idname = "full_fips")
c_new_pcp_pc <- get_cs("new_pcp_per_10k", "New PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/full/", file_ext = "_county", idname = "full_fips")

# Service volume
pcp_services <- get_cs("tot_services", "Services per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/full/", file_ext = "_pcp", idname = "npi")
pcp_wrvu <- get_cs("tot_wrvu", "wRVUs per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/full/", file_ext = "_pcp", idname = "npi")
pcp_bene <- get_cs("tot_benes", "Beneficiaries per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/full/", file_ext = "_pcp", idname = "npi")
aprn_services <- get_cs("tot_services", "Services per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/full/", file_ext = "_aprn", idname = "npi")
aprn_wrvu <- get_cs("tot_wrvu", "wRVUs per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/full/", file_ext = "_aprn", idname = "npi")
aprn_bene <- get_cs("tot_benes", "Beneficiaries per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/full/", file_ext = "_aprn", idname = "npi")

# Service mix 
pcp_wrvu_service <- get_cs("wrvu_per_service", "wRVUs per service", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/full/", file_ext = "_pcp", idname = "npi")
pcp_new_sick_visits <- get_cs("new_sick_visits", "New E/M visits", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/full/", file_ext = "_pcp", idname = "npi")
pcp_est_sick_visits <- get_cs("est_sick_visits", "Est. E/M visits", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/full/", file_ext = "_pcp", idname = "npi")
aprn_wrvu_service <- get_cs("wrvu_per_service", "wRVUs per service", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/full/", file_ext = "_aprn", idname = "npi")
aprn_new_sick_visits <- get_cs("new_sick_visits", "New E/M visits", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/full/", file_ext = "_aprn", idname = "npi")
aprn_est_sick_visits <- get_cs("est_sick_visits", "Est. E/M visits", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/full/", file_ext = "_aprn", idname = "npi")

# Service mix (county)
c_pcp_est_sick_visits <- get_cs("pcp_est_em_visits", "PCP est. E/M visits (county)", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/full/", file_ext = "_county", idname = "full_fips")
c_pcp_share_est_sick_visits <- get_cs("pcp_share_est_em_visits", "Share PCP est. E/M visits (county)", df_q, pre_periods, post_periods, balance_e,file_pre = "by-aprn-density/national-quartiles/full/", file_ext = "_county", idname = "full_fips")

## Q1 (lowest) -----------------------------------------------------------------

df_q <- df_ss %>% filter(share_quartile == 1)
df_md_q <- df_util %>% filter(D_aprn == 0, share_quartile == 1)
df_aprn_q <- df_util %>% filter(D_aprn == 1, share_quartile == 1)

# Physician supply
c_pcp_pc_q1 <- get_cs("pcp_per_10k", "PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q1/", file_ext = "_county", idname = "full_fips")
c_new_pcp_pc_q1 <- get_cs("new_pcp_per_10k", "New PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q1/", file_ext = "_county", idname = "full_fips")

# Service volume
pcp_services_q1 <- get_cs("tot_services", "Services per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q1/", file_ext = "_pcp", idname = "npi")
pcp_wrvu_q1 <- get_cs("tot_wrvu", "wRVUs per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q1/", file_ext = "_pcp", idname = "npi")
pcp_bene_q1 <- get_cs("tot_benes", "Beneficiaries per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q1/", file_ext = "_pcp", idname = "npi")
aprn_services_q1 <- get_cs("tot_services", "Services per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q1/", file_ext = "_aprn", idname = "npi")
aprn_wrvu_q1 <- get_cs("tot_wrvu", "wRVUs per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q1/", file_ext = "_aprn", idname = "npi")
aprn_bene_q1 <- get_cs("tot_benes", "Beneficiaries per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q1/", file_ext = "_aprn", idname = "npi")

# Service mix 
pcp_wrvu_service_q1 <- get_cs("wrvu_per_service", "wRVUs per service", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q1/", file_ext = "_pcp", idname = "npi")
pcp_new_sick_visits_q1 <- get_cs("new_sick_visits", "New E/M visits", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q1/", file_ext = "_pcp", idname = "npi")
pcp_est_sick_visits_q1 <- get_cs("est_sick_visits", "Est. E/M visits", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q1/", file_ext = "_pcp", idname = "npi")
aprn_wrvu_service_q1 <- get_cs("wrvu_per_service", "wRVUs per service", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q1/", file_ext = "_aprn", idname = "npi")
aprn_new_sick_visits_q1 <- get_cs("new_sick_visits", "New E/M visits", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q1/", file_ext = "_aprn", idname = "npi")
aprn_est_sick_visits_q1 <- get_cs("est_sick_visits", "Est. E/M visits", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q1/", file_ext = "_aprn", idname = "npi")

# Service mix (county)
c_pcp_est_sick_visits_q1 <- get_cs("pcp_est_em_visits", "PCP est. E/M visits (county)", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q1/", file_ext = "_county", idname = "full_fips")
c_pcp_share_est_sick_visits_q1 <- get_cs("pcp_share_est_em_visits", "Share PCP est. E/M visits (county)", df_q, pre_periods, post_periods, balance_e,file_pre = "by-aprn-density/national-quartiles/q1/", file_ext = "_county", idname = "full_fips")

## Q2 --------------------------------------------------------------------------

df_q <- df_ss %>% filter(share_quartile == 2)
df_md_q <- df_util %>% filter(D_aprn == 0, share_quartile == 2)
df_aprn_q <- df_util %>% filter(D_aprn == 1, share_quartile == 2)

# Physician supply
c_pcp_pc_q2 <- get_cs("pcp_per_10k", "PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q2/", file_ext = "_county", idname = "full_fips")
c_new_pcp_pc_q2 <- get_cs("new_pcp_per_10k", "New PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q2/", file_ext = "_county", idname = "full_fips")

# Service volume
pcp_services_q2 <- get_cs("tot_services", "Services per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q2/", file_ext = "_pcp", idname = "npi")
pcp_wrvu_q2 <- get_cs("tot_wrvu", "wRVUs per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q2/", file_ext = "_pcp", idname = "npi")
pcp_bene_q2 <- get_cs("tot_benes", "Beneficiaries per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q2/", file_ext = "_pcp", idname = "npi")
aprn_services_q2 <- get_cs("tot_services", "Services per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q2/", file_ext = "_aprn", idname = "npi")
aprn_wrvu_q2 <- get_cs("tot_wrvu", "wRVUs per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q2/", file_ext = "_aprn", idname = "npi")
aprn_bene_q2 <- get_cs("tot_benes", "Beneficiaries per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q2/", file_ext = "_aprn", idname = "npi")

# Service mix 
pcp_wrvu_service_q2 <- get_cs("wrvu_per_service", "wRVUs per service", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q2/", file_ext = "_pcp", idname = "npi")
pcp_new_sick_visits_q2 <- get_cs("new_sick_visits", "New E/M visits", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q2/", file_ext = "_pcp", idname = "npi")
pcp_est_sick_visits_q2 <- get_cs("est_sick_visits", "Est. E/M visits", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q2/", file_ext = "_pcp", idname = "npi")
aprn_wrvu_service_q2 <- get_cs("wrvu_per_service", "wRVUs per service", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q2/", file_ext = "_aprn", idname = "npi")
aprn_new_sick_visits_q2 <- get_cs("new_sick_visits", "New E/M visits", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q2/", file_ext = "_aprn", idname = "npi")
aprn_est_sick_visits_q2 <- get_cs("est_sick_visits", "Est. E/M visits", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q2/", file_ext = "_aprn", idname = "npi")

# Service mix (county)
c_pcp_est_sick_visits_q2 <- get_cs("pcp_est_em_visits", "PCP est. E/M visits (county)", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q2/", file_ext = "_county", idname = "full_fips")
c_pcp_share_est_sick_visits_q2 <- get_cs("pcp_share_est_em_visits", "Share PCP est. E/M visits (county)", df_q, pre_periods, post_periods, balance_e,file_pre = "by-aprn-density/national-quartiles/q2/", file_ext = "_county", idname = "full_fips")

## Q3 --------------------------------------------------------------------------

df_q <- df_ss %>% filter(share_quartile == 3)
df_md_q <- df_util %>% filter(D_aprn == 0, share_quartile == 3)
df_aprn_q <- df_util %>% filter(D_aprn == 1, share_quartile == 3)

# Physician supply
c_pcp_pc_q3 <- get_cs("pcp_per_10k", "PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q3/", file_ext = "_county", idname = "full_fips")
c_new_pcp_pc_q3 <- get_cs("new_pcp_per_10k", "New PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q3/", file_ext = "_county", idname = "full_fips")

# Service volume
pcp_services_q3 <- get_cs("tot_services", "Services per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q3/", file_ext = "_pcp", idname = "npi")
pcp_wrvu_q3 <- get_cs("tot_wrvu", "wRVUs per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q3/", file_ext = "_pcp", idname = "npi")
pcp_bene_q3 <- get_cs("tot_benes", "Beneficiaries per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q3/", file_ext = "_pcp", idname = "npi")
aprn_services_q3 <- get_cs("tot_services", "Services per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q3/", file_ext = "_aprn", idname = "npi")
aprn_wrvu_q3 <- get_cs("tot_wrvu", "wRVUs per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q3/", file_ext = "_aprn", idname = "npi")
aprn_bene_q3 <- get_cs("tot_benes", "Beneficiaries per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q3/", file_ext = "_aprn", idname = "npi")

# Service mix 
pcp_wrvu_service_q3 <- get_cs("wrvu_per_service", "wRVUs per service", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q3/", file_ext = "_pcp", idname = "npi")
pcp_new_sick_visits_q3 <- get_cs("new_sick_visits", "New E/M visits", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q3/", file_ext = "_pcp", idname = "npi")
pcp_est_sick_visits_q3 <- get_cs("est_sick_visits", "Est. E/M visits", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q3/", file_ext = "_pcp", idname = "npi")
aprn_wrvu_service_q3 <- get_cs("wrvu_per_service", "wRVUs per service", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q3/", file_ext = "_aprn", idname = "npi")
aprn_new_sick_visits_q3 <- get_cs("new_sick_visits", "New E/M visits", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q3/", file_ext = "_aprn", idname = "npi")
aprn_est_sick_visits_q3 <- get_cs("est_sick_visits", "Est. E/M visits", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q3/", file_ext = "_aprn", idname = "npi")

# Service mix (county)
c_pcp_est_sick_visits_q3 <- get_cs("pcp_est_em_visits", "PCP est. E/M visits (county)", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q3/", file_ext = "_county", idname = "full_fips")
c_pcp_share_est_sick_visits_q3 <- get_cs("pcp_share_est_em_visits", "Share PCP est. E/M visits (county)", df_q, pre_periods, post_periods, balance_e,file_pre = "by-aprn-density/national-quartiles/q3/", file_ext = "_county", idname = "full_fips")

## Q4 (highest) ----------------------------------------------------------------

df_q <- df_ss %>% filter(share_quartile == 4)
df_md_q <- df_util %>% filter(D_aprn == 0, share_quartile == 4)
df_aprn_q <- df_util %>% filter(D_aprn == 1, share_quartile == 4)

# Physician supply
c_pcp_pc_q4 <- get_cs("pcp_per_10k", "PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q4/", file_ext = "_county", idname = "full_fips")
c_new_pcp_pc_q4 <- get_cs("new_pcp_per_10k", "New PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q4/", file_ext = "_county", idname = "full_fips")

# Service volume
pcp_services_q4 <- get_cs("tot_services", "Services per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q4/", file_ext = "_pcp", idname = "npi")
pcp_wrvu_q4 <- get_cs("tot_wrvu", "wRVUs per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q4/", file_ext = "_pcp", idname = "npi")
pcp_bene_q4 <- get_cs("tot_benes", "Beneficiaries per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q4/", file_ext = "_pcp", idname = "npi")
aprn_services_q4 <- get_cs("tot_services", "Services per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q4/", file_ext = "_aprn", idname = "npi")
aprn_wrvu_q4 <- get_cs("tot_wrvu", "wRVUs per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q4/", file_ext = "_aprn", idname = "npi")
aprn_bene_q4 <- get_cs("tot_benes", "Beneficiaries per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q4/", file_ext = "_aprn", idname = "npi")

# Service mix 
pcp_wrvu_service_q4 <- get_cs("wrvu_per_service", "wRVUs per service", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q4/", file_ext = "_pcp", idname = "npi")
pcp_new_sick_visits_q4 <- get_cs("new_sick_visits", "New E/M visits", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q4/", file_ext = "_pcp", idname = "npi")
pcp_est_sick_visits_q4 <- get_cs("est_sick_visits", "Est. E/M visits", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q4/", file_ext = "_pcp", idname = "npi")
aprn_wrvu_service_q4 <- get_cs("wrvu_per_service", "wRVUs per service", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q4/", file_ext = "_aprn", idname = "npi")
aprn_new_sick_visits_q4 <- get_cs("new_sick_visits", "New E/M visits", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q4/", file_ext = "_aprn", idname = "npi")
aprn_est_sick_visits_q4 <- get_cs("est_sick_visits", "Est. E/M visits", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q4/", file_ext = "_aprn", idname = "npi")

# Service mix (county)
c_pcp_est_sick_visits_q4 <- get_cs("pcp_est_em_visits", "PCP est. E/M visits (county)", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/national-quartiles/q4/", file_ext = "_county", idname = "full_fips")
c_pcp_share_est_sick_visits_q4 <- get_cs("pcp_share_est_em_visits", "Share PCP est. E/M visits (county)", df_q, pre_periods, post_periods, balance_e,file_pre = "by-aprn-density/national-quartiles/q4/", file_ext = "_county", idname = "full_fips")

## Export tables ---------------------------------------------------------------

# Aggregate supply
models_a <- list('Q1' = c_pcp_pc_q1, 
                 'Q2' = c_pcp_pc_q2, 
                 'Q3' = c_pcp_pc_q3,
                 'Q4' = c_pcp_pc_q4,
                 'Full' = c_pcp_pc)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
get_table(models_a,
          control_means_a = control_means_a, 
          title = "Table : Aggregate supply by APRN density", source_note = "Note: SE clustered at state, quartiles defined at national level", 
          file_name = "by-aprn-density/national-quartiles/agg-supply")

# New PCPs
models_a <- list('Q1' = c_new_pcp_pc_q1, 
                 'Q2' = c_new_pcp_pc_q2, 
                 'Q3' = c_new_pcp_pc_q3,
                 'Q4' = c_new_pcp_pc_q4,
                 'Full' = c_new_pcp_pc)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
get_table(models_a,
          control_means_a = control_means_a, 
          title = "Table : New PCPs by APRN density", source_note = "Note: SE clustered at state, quartiles defined at national level", 
          file_name = "by-aprn-density/national-quartiles/new-pcps")

# Services
models_a <- list('Q1' = pcp_services_q1, 
                 'Q2' = pcp_services_q2, 
                 'Q3' = pcp_services_q3,
                 'Q4' = pcp_services_q4,
                 'Full' = pcp_services)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
models_b <- list('Q1' = aprn_services_q1, 
                 'Q2' = aprn_services_q2, 
                 'Q3' = aprn_services_q3,
                 'Q4' = aprn_services_q4,
                 'Full' = aprn_services)
control_means_b <- sapply(models_b, get_control_mean, control_group = "notyettreated")
get_table(models_a, models_b,
          control_means_a, control_means_b,
          title = "Table : Total services by APRN density", source_note = "Note: SE clustered at state, quartiles defined at national level", 
          file_name = "by-aprn-density/national-quartiles/total-services", 
          panel_a_label = "Panel A: PCP", panel_b_label = "Panel B: APRN")

# wRVUs 
models_a <- list('Q1' = pcp_wrvu_q1, 
                 'Q2' = pcp_wrvu_q2, 
                 'Q3' = pcp_wrvu_q3,
                 'Q4' = pcp_wrvu_q4,
                 'Full' = pcp_wrvu)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
models_b <- list('Q1' = aprn_wrvu_q1, 
                 'Q2' = aprn_wrvu_q2, 
                 'Q3' = aprn_wrvu_q3,
                 'Q4' = aprn_wrvu_q4,
                 'Full' = aprn_wrvu)
control_means_b <- sapply(models_b, get_control_mean, control_group = "notyettreated")
get_table(models_a, models_b,
          control_means_a, control_means_b,
          title = "Table : Total wRVUs by APRN density", source_note = "Note: SE clustered at state, quartiles defined at national level", 
          file_name = "by-aprn-density/national-quartiles/total-wrvu", 
          panel_a_label = "Panel A: PCP", panel_b_label = "Panel B: APRN")

# Benes
models_a <- list('Q1' = pcp_bene_q1, 
                 'Q2' = pcp_bene_q2, 
                 'Q3' = pcp_bene_q3,
                 'Q4' = pcp_bene_q4,
                 'Full' = pcp_bene)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
models_b <- list('Q1' = aprn_bene_q1, 
                 'Q2' = aprn_bene_q2, 
                 'Q3' = aprn_bene_q3,
                 'Q4' = aprn_bene_q4,
                 'Full' = aprn_bene)
control_means_b <- sapply(models_b, get_control_mean, control_group = "notyettreated")
get_table(models_a, models_b,
          control_means_a, control_means_b,
          title = "Table : Total beneficiaries by APRN density", source_note = "Note: SE clustered at state, quartiles defined at national level", 
          file_name = "by-aprn-density/national-quartiles/total-bene", 
          panel_a_label = "Panel A: PCP", panel_b_label = "Panel B: APRN")

# wRVU per service 
models_a <- list('Q1' = pcp_wrvu_service_q1, 
                 'Q2' = pcp_wrvu_service_q2, 
                 'Q3' = pcp_wrvu_service_q3,
                 'Q4' = pcp_wrvu_service_q4,
                 'Full' = pcp_wrvu_service)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
models_b <- list('Q1' = aprn_wrvu_service_q1, 
                 'Q2' = aprn_wrvu_service_q2, 
                 'Q3' = aprn_wrvu_service_q3,
                 'Q4' = aprn_wrvu_service_q4,
                 'Full' = aprn_wrvu_service)
control_means_b <- sapply(models_b, get_control_mean, control_group = "notyettreated")
get_table(models_a, models_b,
          control_means_a, control_means_b,
          title = "Table : wRVUs per service by APRN density", source_note = "Note: SE clustered at state, quartiles defined at national level", 
          file_name = "by-aprn-density/national-quartiles/wrvu-per-service", 
          panel_a_label = "Panel A: PCP", panel_b_label = "Panel B: APRN")

# New E/M visits 
models_a <- list('Q1' = pcp_new_sick_visits_q1, 
                 'Q2' = pcp_new_sick_visits_q2, 
                 'Q3' = pcp_new_sick_visits_q3,
                 'Q4' = pcp_new_sick_visits_q4,
                 'Full' = pcp_new_sick_visits)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
models_b <- list('Q1' = aprn_new_sick_visits_q1, 
                 'Q2' = aprn_new_sick_visits_q2, 
                 'Q3' = aprn_new_sick_visits_q3,
                 'Q4' = aprn_new_sick_visits_q4,
                 'Full' = aprn_new_sick_visits)
control_means_b <- sapply(models_b, get_control_mean, control_group = "notyettreated")
get_table(models_a, models_b,
          control_means_a, control_means_b,
          title = "Table : New E/M visits by APRN density", source_note = "Note: SE clustered at state, quartiles defined at national level", 
          file_name = "by-aprn-density/national-quartiles/new-em-visits", 
          panel_a_label = "Panel A: PCP", panel_b_label = "Panel B: APRN")

# Established E/M visits 
models_a <- list('Q1' = pcp_est_sick_visits_q1, 
                 'Q2' = pcp_est_sick_visits_q2, 
                 'Q3' = pcp_est_sick_visits_q3,
                 'Q4' = pcp_est_sick_visits_q4,
                 'Full' = pcp_est_sick_visits)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
models_b <- list('Q1' = aprn_est_sick_visits_q1, 
                 'Q2' = aprn_est_sick_visits_q2, 
                 'Q3' = aprn_est_sick_visits_q3,
                 'Q4' = aprn_est_sick_visits_q4,
                 'Full' = aprn_est_sick_visits)
control_means_b <- sapply(models_b, get_control_mean, control_group = "notyettreated")
get_table(models_a, models_b,
          control_means_a, control_means_b,
          title = "Table : Established E/M visits by APRN density", source_note = "Note: SE clustered at state, quartiles defined at national level", 
          file_name = "by-aprn-density/national-quartiles/est-em-visits", 
          panel_a_label = "Panel A: PCP", panel_b_label = "Panel B: APRN")

# County PCP established E/M visits
models_a <- list('Q1' = c_pcp_est_sick_visits_q1, 
                 'Q2' = c_pcp_est_sick_visits_q2, 
                 'Q3' = c_pcp_est_sick_visits_q3,
                 'Q4' = c_pcp_est_sick_visits_q4,
                 'Full' = c_pcp_est_sick_visits)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
get_table(models_a,
          control_means_a = control_means_a, 
          title = "Table : Market PCP established E/M visits by APRN density", source_note = "Note: SE clustered at state, quartiles defined at national level", 
          file_name = "by-aprn-density/national-quartiles/market-pcp-est-em-visits")

# County PCP share established E/M visits
models_a <- list('Q1' = c_pcp_share_est_sick_visits_q1, 
                 'Q2' = c_pcp_share_est_sick_visits_q2, 
                 'Q3' = c_pcp_share_est_sick_visits_q3,
                 'Q4' = c_pcp_share_est_sick_visits_q4,
                 'Full' = c_pcp_share_est_sick_visits)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
get_table(models_a,
          control_means_a = control_means_a, 
          title = "Table : PCP market share of established E/M visits", source_note = "Note: SE clustered at state, quartiles defined at national level", 
          file_name = "by-aprn-density/national-quartiles/market-share-pcp-est-em-visits")

# State quartiles --------------------------------------------------------------

## Full ------------------------------------------------------------------------

df_q <- df_ss
df_util_q <- df_util

# Physician supply
c_pcp_pc <- get_cs("pcp_per_10k", "PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/full/", file_ext = "_county", idname = "full_fips")
c_new_pcp_pc <- get_cs("new_pcp_per_10k", "New PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/full/", file_ext = "_county", idname = "full_fips")

# Service volume
pcp_services <- get_cs("tot_services", "Services per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/full/", file_ext = "_pcp", idname = "npi")
pcp_wrvu <- get_cs("tot_wrvu", "wRVUs per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/full/", file_ext = "_pcp", idname = "npi")
pcp_bene <- get_cs("tot_benes", "Beneficiaries per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/full/", file_ext = "_pcp", idname = "npi")
aprn_services <- get_cs("tot_services", "Services per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/full/", file_ext = "_aprn", idname = "npi")
aprn_wrvu <- get_cs("tot_wrvu", "wRVUs per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/full/", file_ext = "_aprn", idname = "npi")
aprn_bene <- get_cs("tot_benes", "Beneficiaries per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/full/", file_ext = "_aprn", idname = "npi")

# Service mix 
pcp_wrvu_service <- get_cs("wrvu_per_service", "wRVUs per service", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/full/", file_ext = "_pcp", idname = "npi")
pcp_new_sick_visits <- get_cs("new_sick_visits", "New E/M visits", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/full/", file_ext = "_pcp", idname = "npi")
pcp_est_sick_visits <- get_cs("est_sick_visits", "Est. E/M visits", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/full/", file_ext = "_pcp", idname = "npi")
aprn_wrvu_service <- get_cs("wrvu_per_service", "wRVUs per service", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/full/", file_ext = "_aprn", idname = "npi")
aprn_new_sick_visits <- get_cs("new_sick_visits", "New E/M visits", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/full/", file_ext = "_aprn", idname = "npi")
aprn_est_sick_visits <- get_cs("est_sick_visits", "Est. E/M visits", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/full/", file_ext = "_aprn", idname = "npi")

# Service mix (county)
c_pcp_est_sick_visits <- get_cs("pcp_est_em_visits", "PCP est. E/M visits (county)", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/full/", file_ext = "_county", idname = "full_fips")
c_pcp_share_est_sick_visits <- get_cs("pcp_share_est_em_visits", "Share PCP est. E/M visits (county)", df_q, pre_periods, post_periods, balance_e,file_pre = "by-aprn-density/state-quartiles/full/", file_ext = "_county", idname = "full_fips")

## Q1 (lowest) -----------------------------------------------------------------

df_q <- df_ss %>% filter(share_quartile_state == 1)
df_md_q <- df_util %>% filter(D_aprn == 0, share_quartile_state == 1)
df_aprn_q <- df_util %>% filter(D_aprn == 1, share_quartile_state == 1)

# Physician supply
c_pcp_pc_q1 <- get_cs("pcp_per_10k", "PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q1/", file_ext = "_county", idname = "full_fips")
c_new_pcp_pc_q1 <- get_cs("new_pcp_per_10k", "New PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q1/", file_ext = "_county", idname = "full_fips")

# Service volume
pcp_services_q1 <- get_cs("tot_services", "Services per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q1/", file_ext = "_pcp", idname = "npi")
pcp_wrvu_q1 <- get_cs("tot_wrvu", "wRVUs per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q1/", file_ext = "_pcp", idname = "npi")
pcp_bene_q1 <- get_cs("tot_benes", "Beneficiaries per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q1/", file_ext = "_pcp", idname = "npi")
aprn_services_q1 <- get_cs("tot_services", "Services per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q1/", file_ext = "_aprn", idname = "npi")
aprn_wrvu_q1 <- get_cs("tot_wrvu", "wRVUs per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q1/", file_ext = "_aprn", idname = "npi")
aprn_bene_q1 <- get_cs("tot_benes", "Beneficiaries per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q1/", file_ext = "_aprn", idname = "npi")

# Service mix 
pcp_wrvu_service_q1 <- get_cs("wrvu_per_service", "wRVUs per service", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q1/", file_ext = "_pcp", idname = "npi")
pcp_new_sick_visits_q1 <- get_cs("new_sick_visits", "New E/M visits", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q1/", file_ext = "_pcp", idname = "npi")
pcp_est_sick_visits_q1 <- get_cs("est_sick_visits", "Est. E/M visits", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q1/", file_ext = "_pcp", idname = "npi")
aprn_wrvu_service_q1 <- get_cs("wrvu_per_service", "wRVUs per service", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q1/", file_ext = "_aprn", idname = "npi")
aprn_new_sick_visits_q1 <- get_cs("new_sick_visits", "New E/M visits", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q1/", file_ext = "_aprn", idname = "npi")
aprn_est_sick_visits_q1 <- get_cs("est_sick_visits", "Est. E/M visits", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q1/", file_ext = "_aprn", idname = "npi")

# Service mix (county)
c_pcp_est_sick_visits_q1 <- get_cs("pcp_est_em_visits", "PCP est. E/M visits (county)", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q1/", file_ext = "_county", idname = "full_fips")
c_pcp_share_est_sick_visits_q1 <- get_cs("pcp_share_est_em_visits", "Share PCP est. E/M visits (county)", df_q, pre_periods, post_periods, balance_e,file_pre = "by-aprn-density/state-quartiles/q1/", file_ext = "_county", idname = "full_fips")

## Q2 --------------------------------------------------------------------------

df_q <- df_ss %>% filter(share_quartile_state == 2)
df_md_q <- df_util %>% filter(D_aprn == 0, share_quartile_state == 2)
df_aprn_q <- df_util %>% filter(D_aprn == 1, share_quartile_state == 2)

# Physician supply
c_pcp_pc_q2 <- get_cs("pcp_per_10k", "PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q2/", file_ext = "_county", idname = "full_fips")
c_new_pcp_pc_q2 <- get_cs("new_pcp_per_10k", "New PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q2/", file_ext = "_county", idname = "full_fips")

# Service volume
pcp_services_q2 <- get_cs("tot_services", "Services per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q2/", file_ext = "_pcp", idname = "npi")
pcp_wrvu_q2 <- get_cs("tot_wrvu", "wRVUs per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q2/", file_ext = "_pcp", idname = "npi")
pcp_bene_q2 <- get_cs("tot_benes", "Beneficiaries per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q2/", file_ext = "_pcp", idname = "npi")
aprn_services_q2 <- get_cs("tot_services", "Services per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q2/", file_ext = "_aprn", idname = "npi")
aprn_wrvu_q2 <- get_cs("tot_wrvu", "wRVUs per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q2/", file_ext = "_aprn", idname = "npi")
aprn_bene_q2 <- get_cs("tot_benes", "Beneficiaries per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q2/", file_ext = "_aprn", idname = "npi")

# Service mix 
pcp_wrvu_service_q2 <- get_cs("wrvu_per_service", "wRVUs per service", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q2/", file_ext = "_pcp", idname = "npi")
pcp_new_sick_visits_q2 <- get_cs("new_sick_visits", "New E/M visits", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q2/", file_ext = "_pcp", idname = "npi")
pcp_est_sick_visits_q2 <- get_cs("est_sick_visits", "Est. E/M visits", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q2/", file_ext = "_pcp", idname = "npi")
aprn_wrvu_service_q2 <- get_cs("wrvu_per_service", "wRVUs per service", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q2/", file_ext = "_aprn", idname = "npi")
aprn_new_sick_visits_q2 <- get_cs("new_sick_visits", "New E/M visits", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q2/", file_ext = "_aprn", idname = "npi")
aprn_est_sick_visits_q2 <- get_cs("est_sick_visits", "Est. E/M visits", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q2/", file_ext = "_aprn", idname = "npi")

# Service mix (county)
c_pcp_est_sick_visits_q2 <- get_cs("pcp_est_em_visits", "PCP est. E/M visits (county)", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q2/", file_ext = "_county", idname = "full_fips")
c_pcp_share_est_sick_visits_q2 <- get_cs("pcp_share_est_em_visits", "Share PCP est. E/M visits (county)", df_q, pre_periods, post_periods, balance_e,file_pre = "by-aprn-density/state-quartiles/q2/", file_ext = "_county", idname = "full_fips")

## Q3 --------------------------------------------------------------------------

df_q <- df_ss %>% filter(share_quartile_state == 3)
df_md_q <- df_util %>% filter(D_aprn == 0, share_quartile_state == 3)
df_aprn_q <- df_util %>% filter(D_aprn == 1, share_quartile_state == 3)

# Physician supply
c_pcp_pc_q3 <- get_cs("pcp_per_10k", "PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q3/", file_ext = "_county", idname = "full_fips")
c_new_pcp_pc_q3 <- get_cs("new_pcp_per_10k", "New PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q3/", file_ext = "_county", idname = "full_fips")

# Service volume
pcp_services_q3 <- get_cs("tot_services", "Services per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q3/", file_ext = "_pcp", idname = "npi")
pcp_wrvu_q3 <- get_cs("tot_wrvu", "wRVUs per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q3/", file_ext = "_pcp", idname = "npi")
pcp_bene_q3 <- get_cs("tot_benes", "Beneficiaries per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q3/", file_ext = "_pcp", idname = "npi")
aprn_services_q3 <- get_cs("tot_services", "Services per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q3/", file_ext = "_aprn", idname = "npi")
aprn_wrvu_q3 <- get_cs("tot_wrvu", "wRVUs per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q3/", file_ext = "_aprn", idname = "npi")
aprn_bene_q3 <- get_cs("tot_benes", "Beneficiaries per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q3/", file_ext = "_aprn", idname = "npi")

# Service mix 
pcp_wrvu_service_q3 <- get_cs("wrvu_per_service", "wRVUs per service", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q3/", file_ext = "_pcp", idname = "npi")
pcp_new_sick_visits_q3 <- get_cs("new_sick_visits", "New E/M visits", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q3/", file_ext = "_pcp", idname = "npi")
pcp_est_sick_visits_q3 <- get_cs("est_sick_visits", "Est. E/M visits", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q3/", file_ext = "_pcp", idname = "npi")
aprn_wrvu_service_q3 <- get_cs("wrvu_per_service", "wRVUs per service", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q3/", file_ext = "_aprn", idname = "npi")
aprn_new_sick_visits_q3 <- get_cs("new_sick_visits", "New E/M visits", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q3/", file_ext = "_aprn", idname = "npi")
aprn_est_sick_visits_q3 <- get_cs("est_sick_visits", "Est. E/M visits", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q3/", file_ext = "_aprn", idname = "npi")

# Service mix (county)
c_pcp_est_sick_visits_q3 <- get_cs("pcp_est_em_visits", "PCP est. E/M visits (county)", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q3/", file_ext = "_county", idname = "full_fips")
c_pcp_share_est_sick_visits_q3 <- get_cs("pcp_share_est_em_visits", "Share PCP est. E/M visits (county)", df_q, pre_periods, post_periods, balance_e,file_pre = "by-aprn-density/state-quartiles/q3/", file_ext = "_county", idname = "full_fips")

## Q4 (highest) ----------------------------------------------------------------

df_q <- df_ss %>% filter(share_quartile_state == 4)
df_md_q <- df_util %>% filter(D_aprn == 0, share_quartile_state == 4)
df_aprn_q <- df_util %>% filter(D_aprn == 1, share_quartile_state == 4)

# Physician supply
c_pcp_pc_q4 <- get_cs("pcp_per_10k", "PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q4/", file_ext = "_county", idname = "full_fips")
c_new_pcp_pc_q4 <- get_cs("new_pcp_per_10k", "New PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q4/", file_ext = "_county", idname = "full_fips")

# Service volume
pcp_services_q4 <- get_cs("tot_services", "Services per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q4/", file_ext = "_pcp", idname = "npi")
pcp_wrvu_q4 <- get_cs("tot_wrvu", "wRVUs per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q4/", file_ext = "_pcp", idname = "npi")
pcp_bene_q4 <- get_cs("tot_benes", "Beneficiaries per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q4/", file_ext = "_pcp", idname = "npi")
aprn_services_q4 <- get_cs("tot_services", "Services per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q4/", file_ext = "_aprn", idname = "npi")
aprn_wrvu_q4 <- get_cs("tot_wrvu", "wRVUs per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q4/", file_ext = "_aprn", idname = "npi")
aprn_bene_q4 <- get_cs("tot_benes", "Beneficiaries per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q4/", file_ext = "_aprn", idname = "npi")

# Service mix 
pcp_wrvu_service_q4 <- get_cs("wrvu_per_service", "wRVUs per service", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q4/", file_ext = "_pcp", idname = "npi")
pcp_new_sick_visits_q4 <- get_cs("new_sick_visits", "New E/M visits", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q4/", file_ext = "_pcp", idname = "npi")
pcp_est_sick_visits_q4 <- get_cs("est_sick_visits", "Est. E/M visits", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q4/", file_ext = "_pcp", idname = "npi")
aprn_wrvu_service_q4 <- get_cs("wrvu_per_service", "wRVUs per service", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q4/", file_ext = "_aprn", idname = "npi")
aprn_new_sick_visits_q4 <- get_cs("new_sick_visits", "New E/M visits", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q4/", file_ext = "_aprn", idname = "npi")
aprn_est_sick_visits_q4 <- get_cs("est_sick_visits", "Est. E/M visits", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q4/", file_ext = "_aprn", idname = "npi")

# Service mix (county)
c_pcp_est_sick_visits_q4 <- get_cs("pcp_est_em_visits", "PCP est. E/M visits (county)", df_q, pre_periods, post_periods, balance_e, file_pre = "by-aprn-density/state-quartiles/q4/", file_ext = "_county", idname = "full_fips")
c_pcp_share_est_sick_visits_q4 <- get_cs("pcp_share_est_em_visits", "Share PCP est. E/M visits (county)", df_q, pre_periods, post_periods, balance_e,file_pre = "by-aprn-density/state-quartiles/q4/", file_ext = "_county", idname = "full_fips")

## Export tables ---------------------------------------------------------------

# Aggregate supply
models_a <- list('Q1' = c_pcp_pc_q1, 
                 'Q2' = c_pcp_pc_q2, 
                 'Q3' = c_pcp_pc_q3,
                 'Q4' = c_pcp_pc_q4,
                 'Full' = c_pcp_pc)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
get_table(models_a,
          control_means_a = control_means_a, 
          title = "Table : Aggregate supply by APRN density", source_note = "Note: SE clustered at state, quartiles defined at state level", 
          file_name = "by-aprn-density/state-quartiles/agg-supply")

# New PCPs
models_a <- list('Q1' = c_new_pcp_pc_q1, 
                 'Q2' = c_new_pcp_pc_q2, 
                 'Q3' = c_new_pcp_pc_q3,
                 'Q4' = c_new_pcp_pc_q4,
                 'Full' = c_new_pcp_pc)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
get_table(models_a,
          control_means_a = control_means_a, 
          title = "Table : New PCPs by APRN density", source_note = "Note: SE clustered at state, quartiles defined at state level", 
          file_name = "by-aprn-density/state-quartiles/new-pcps")

# Services
models_a <- list('Q1' = pcp_services_q1, 
                 'Q2' = pcp_services_q2, 
                 'Q3' = pcp_services_q3,
                 'Q4' = pcp_services_q4,
                 'Full' = pcp_services)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
models_b <- list('Q1' = aprn_services_q1, 
                 'Q2' = aprn_services_q2, 
                 'Q3' = aprn_services_q3,
                 'Q4' = aprn_services_q4,
                 'Full' = aprn_services)
control_means_b <- sapply(models_b, get_control_mean, control_group = "notyettreated")
get_table(models_a, models_b,
          control_means_a, control_means_b,
          title = "Table : Total services by APRN density", source_note = "Note: SE clustered at state, quartiles defined at state level", 
          file_name = "by-aprn-density/state-quartiles/total-services", 
          panel_a_label = "Panel A: PCP", panel_b_label = "Panel B: APRN")

# wRVUs 
models_a <- list('Q1' = pcp_wrvu_q1, 
                 'Q2' = pcp_wrvu_q2, 
                 'Q3' = pcp_wrvu_q3,
                 'Q4' = pcp_wrvu_q4,
                 'Full' = pcp_wrvu)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
models_b <- list('Q1' = aprn_wrvu_q1, 
                 'Q2' = aprn_wrvu_q2, 
                 'Q3' = aprn_wrvu_q3,
                 'Q4' = aprn_wrvu_q4,
                 'Full' = aprn_wrvu)
control_means_b <- sapply(models_b, get_control_mean, control_group = "notyettreated")
get_table(models_a, models_b,
          control_means_a, control_means_b,
          title = "Table : Total wRVUs by APRN density", source_note = "Note: SE clustered at state, quartiles defined at state level", 
          file_name = "by-aprn-density/state-quartiles/total-wrvu", 
          panel_a_label = "Panel A: PCP", panel_b_label = "Panel B: APRN")

# Benes
models_a <- list('Q1' = pcp_bene_q1, 
                 'Q2' = pcp_bene_q2, 
                 'Q3' = pcp_bene_q3,
                 'Q4' = pcp_bene_q4,
                 'Full' = pcp_bene)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
models_b <- list('Q1' = aprn_bene_q1, 
                 'Q2' = aprn_bene_q2, 
                 'Q3' = aprn_bene_q3,
                 'Q4' = aprn_bene_q4,
                 'Full' = aprn_bene)
control_means_b <- sapply(models_b, get_control_mean, control_group = "notyettreated")
get_table(models_a, models_b,
          control_means_a, control_means_b,
          title = "Table : Total beneficiaries by APRN density", source_note = "Note: SE clustered at state, quartiles defined at state level", 
          file_name = "by-aprn-density/state-quartiles/total-bene", 
          panel_a_label = "Panel A: PCP", panel_b_label = "Panel B: APRN")

# wRVU per service 
models_a <- list('Q1' = pcp_wrvu_service_q1, 
                 'Q2' = pcp_wrvu_service_q2, 
                 'Q3' = pcp_wrvu_service_q3,
                 'Q4' = pcp_wrvu_service_q4,
                 'Full' = pcp_wrvu_service)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
models_b <- list('Q1' = aprn_wrvu_service_q1, 
                 'Q2' = aprn_wrvu_service_q2, 
                 'Q3' = aprn_wrvu_service_q3,
                 'Q4' = aprn_wrvu_service_q4,
                 'Full' = aprn_wrvu_service)
control_means_b <- sapply(models_b, get_control_mean, control_group = "notyettreated")
get_table(models_a, models_b,
          control_means_a, control_means_b,
          title = "Table : wRVUs per service by APRN density", source_note = "Note: SE clustered at state, quartiles defined at state level", 
          file_name = "by-aprn-density/state-quartiles/wrvu-per-service", 
          panel_a_label = "Panel A: PCP", panel_b_label = "Panel B: APRN")

# New E/M visits 
models_a <- list('Q1' = pcp_new_sick_visits_q1, 
                 'Q2' = pcp_new_sick_visits_q2, 
                 'Q3' = pcp_new_sick_visits_q3,
                 'Q4' = pcp_new_sick_visits_q4,
                 'Full' = pcp_new_sick_visits)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
models_b <- list('Q1' = aprn_new_sick_visits_q1, 
                 'Q2' = aprn_new_sick_visits_q2, 
                 'Q3' = aprn_new_sick_visits_q3,
                 'Q4' = aprn_new_sick_visits_q4,
                 'Full' = aprn_new_sick_visits)
control_means_b <- sapply(models_b, get_control_mean, control_group = "notyettreated")
get_table(models_a, models_b,
          control_means_a, control_means_b,
          title = "Table : New E/M visits by APRN density", source_note = "Note: SE clustered at state, quartiles defined at state level", 
          file_name = "by-aprn-density/state-quartiles/new-em-visits", 
          panel_a_label = "Panel A: PCP", panel_b_label = "Panel B: APRN")

# Established E/M visits 
models_a <- list('Q1' = pcp_est_sick_visits_q1, 
                 'Q2' = pcp_est_sick_visits_q2, 
                 'Q3' = pcp_est_sick_visits_q3,
                 'Q4' = pcp_est_sick_visits_q4,
                 'Full' = pcp_est_sick_visits)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
models_b <- list('Q1' = aprn_est_sick_visits_q1, 
                 'Q2' = aprn_est_sick_visits_q2, 
                 'Q3' = aprn_est_sick_visits_q3,
                 'Q4' = aprn_est_sick_visits_q4,
                 'Full' = aprn_est_sick_visits)
control_means_b <- sapply(models_b, get_control_mean, control_group = "notyettreated")
get_table(models_a, models_b,
          control_means_a, control_means_b,
          title = "Table : Established E/M visits by APRN density", source_note = "Note: SE clustered at state, quartiles defined at state level", 
          file_name = "by-aprn-density/state-quartiles/est-em-visits", 
          panel_a_label = "Panel A: PCP", panel_b_label = "Panel B: APRN")

# County PCP established E/M visits
models_a <- list('Q1' = c_pcp_est_sick_visits_q1, 
                 'Q2' = c_pcp_est_sick_visits_q2, 
                 'Q3' = c_pcp_est_sick_visits_q3,
                 'Q4' = c_pcp_est_sick_visits_q4,
                 'Full' = c_pcp_est_sick_visits)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
get_table(models_a,
          control_means_a = control_means_a, 
          title = "Table : Market PCP established E/M visits by APRN density", source_note = "Note: SE clustered at state, quartiles defined at state level", 
          file_name = "by-aprn-density/state-quartiles/market-pcp-est-em-visits")

# County PCP share established E/M visits
models_a <- list('Q1' = c_pcp_share_est_sick_visits_q1, 
                 'Q2' = c_pcp_share_est_sick_visits_q2, 
                 'Q3' = c_pcp_share_est_sick_visits_q3,
                 'Q4' = c_pcp_share_est_sick_visits_q4,
                 'Full' = c_pcp_share_est_sick_visits)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
get_table(models_a,
          control_means_a = control_means_a, 
          title = "Table : PCP market share of established E/M visits", source_note = "Note: SE clustered at state, quartiles defined at state level", 
          file_name = "by-aprn-density/state-quartiles/market-share-pcp-est-em-visits")

# Clean up ---------------------------------------------------------------------

rm(list = ls())

df <- read_csv('data/output/final_df_state.csv')
df_c <- read_csv('data/output/final_df_county.csv')
source('analysis/0-helpers.R')
