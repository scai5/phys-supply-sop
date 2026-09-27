# About ------------------------------------------------------------------------

# Stratify by urban / rural
# Author:         Shirley Cai 
# Date created:   06/22/2026 
# Last edited:    09/26/2026

# Preparation ------------------------------------------------------------------

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

# Full -------------------------------------------------------------------------

df_q <- df_ss
df_md_q <- df_util %>% filter(D_aprn == 0)
df_aprn_q <- df_util %>% filter(D_aprn == 1)

# Physician supply
c_pcp_pc <- get_cs("pcp_per_10k", "PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/full/", file_ext = "_county", idname = "full_fips")
c_new_pcp_pc <- get_cs("new_pcp_per_10k", "New PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/full/", file_ext = "_county", idname = "full_fips")

# Service volume
pcp_services <- get_cs("tot_services", "Services per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/full/", file_ext = "_pcp", idname = "npi")
pcp_wrvu <- get_cs("tot_wrvu", "wRVUs per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/full/", file_ext = "_pcp", idname = "npi")
pcp_bene <- get_cs("tot_benes", "Beneficiaries per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/full/", file_ext = "_pcp", idname = "npi")
aprn_services <- get_cs("tot_services", "Services per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/full/", file_ext = "_aprn", idname = "npi")
aprn_wrvu <- get_cs("tot_wrvu", "wRVUs per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/full/", file_ext = "_aprn", idname = "npi")
aprn_bene <- get_cs("tot_benes", "Beneficiaries per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/full/", file_ext = "_aprn", idname = "npi")

# Service mix 
pcp_wrvu_service <- get_cs("wrvu_per_service", "wRVUs per service", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/full/", file_ext = "_pcp", idname = "npi")
pcp_new_sick_visits <- get_cs("new_sick_visits", "New E/M visits", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/full/", file_ext = "_pcp", idname = "npi")
pcp_est_sick_visits <- get_cs("est_sick_visits", "Est. E/M visits", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/full/", file_ext = "_pcp", idname = "npi")
aprn_wrvu_service <- get_cs("wrvu_per_service", "wRVUs per service", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/full/", file_ext = "_aprn", idname = "npi")
aprn_new_sick_visits <- get_cs("new_sick_visits", "New E/M visits", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/full/", file_ext = "_aprn", idname = "npi")
aprn_est_sick_visits <- get_cs("est_sick_visits", "Est. E/M visits", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/full/", file_ext = "_aprn", idname = "npi")

# Service mix (county)
c_pcp_est_sick_visits <- get_cs("pcp_est_em_visits", "PCP est. E/M visits (county)", df_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/full/", file_ext = "_county", idname = "full_fips")
c_pcp_share_est_sick_visits <- get_cs("pcp_share_est_em_visits", "Share PCP est. E/M visits (county)", df_q, pre_periods, post_periods, balance_e,file_pre = "by-urban-rural/full/", file_ext = "_county", idname = "full_fips")

# Urban ------------------------------------------------------------------------

df_q <- df_ss %>% filter(D_rural == 0)
df_md_q <- df_util %>% filter(D_aprn == 0, D_rural == 0)
df_aprn_q <- df_util %>% filter(D_aprn == 1, D_rural == 0)

# Physician supply
c_pcp_pc_urban <- get_cs("pcp_per_10k", "PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/urban/", file_ext = "_county", idname = "full_fips")
c_new_pcp_pc_urban <- get_cs("new_pcp_per_10k", "New PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/urban/", file_ext = "_county", idname = "full_fips")

# Service volume
pcp_services_urban <- get_cs("tot_services", "Services per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/urban/", file_ext = "_pcp", idname = "npi")
pcp_wrvu_urban <- get_cs("tot_wrvu", "wRVUs per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/urban/", file_ext = "_pcp", idname = "npi")
pcp_bene_urban <- get_cs("tot_benes", "Beneficiaries per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/urban/", file_ext = "_pcp", idname = "npi")
aprn_services_urban <- get_cs("tot_services", "Services per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/urban/", file_ext = "_aprn", idname = "npi")
aprn_wrvu_urban <- get_cs("tot_wrvu", "wRVUs per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/urban/", file_ext = "_aprn", idname = "npi")
aprn_bene_urban <- get_cs("tot_benes", "Beneficiaries per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/urban/", file_ext = "_aprn", idname = "npi")

# Service mix 
pcp_wrvu_service_urban <- get_cs("wrvu_per_service", "wRVUs per service", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/urban/", file_ext = "_pcp", idname = "npi")
pcp_new_sick_visits_urban <- get_cs("new_sick_visits", "New E/M visits", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/urban/", file_ext = "_pcp", idname = "npi")
pcp_est_sick_visits_urban <- get_cs("est_sick_visits", "Est. E/M visits", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/urban/", file_ext = "_pcp", idname = "npi")
aprn_wrvu_service_urban <- get_cs("wrvu_per_service", "wRVUs per service", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/urban/", file_ext = "_aprn", idname = "npi")
aprn_new_sick_visits_urban <- get_cs("new_sick_visits", "New E/M visits", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/urban/", file_ext = "_aprn", idname = "npi")
aprn_est_sick_visits_urban <- get_cs("est_sick_visits", "Est. E/M visits", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/urban/", file_ext = "_aprn", idname = "npi")

# Service mix (county)
c_pcp_est_sick_visits_urban <- get_cs("pcp_est_em_visits", "PCP est. E/M visits (county)", df_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/urban/", file_ext = "_county", idname = "full_fips")
c_pcp_share_est_sick_visits_urban <- get_cs("pcp_share_est_em_visits", "Share PCP est. E/M visits (county)", df_q, pre_periods, post_periods, balance_e,file_pre = "by-urban-rural/urban/", file_ext = "_county", idname = "full_fips")

# Rural ------------------------------------------------------------------------

df_q <- df_ss %>% filter(D_rural == 1)
df_md_q <- df_util %>% filter(D_aprn == 0, D_rural == 1)
df_aprn_q <- df_util %>% filter(D_aprn == 1, D_rural == 1)

# Physician supply
c_pcp_pc_rural <- get_cs("pcp_per_10k", "PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/rural/", file_ext = "_county", idname = "full_fips")
c_new_pcp_pc_rural <- get_cs("new_pcp_per_10k", "New PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/rural/", file_ext = "_county", idname = "full_fips")

# Service volume
pcp_services_rural <- get_cs("tot_services", "Services per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/rural/", file_ext = "_pcp", idname = "npi")
pcp_wrvu_rural <- get_cs("tot_wrvu", "wRVUs per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/rural/", file_ext = "_pcp", idname = "npi")
pcp_bene_rural <- get_cs("tot_benes", "Beneficiaries per PCP", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/rural/", file_ext = "_pcp", idname = "npi")
aprn_services_rural <- get_cs("tot_services", "Services per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/rural/", file_ext = "_aprn", idname = "npi")
aprn_wrvu_rural <- get_cs("tot_wrvu", "wRVUs per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/rural/", file_ext = "_aprn", idname = "npi")
aprn_bene_rural <- get_cs("tot_benes", "Beneficiaries per APRN", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/rural/", file_ext = "_aprn", idname = "npi")

# Service mix 
pcp_wrvu_service_rural <- get_cs("wrvu_per_service", "wRVUs per service", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/rural/", file_ext = "_pcp", idname = "npi")
pcp_new_sick_visits_rural <- get_cs("new_sick_visits", "New E/M visits", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/rural/", file_ext = "_pcp", idname = "npi")
pcp_est_sick_visits_rural <- get_cs("est_sick_visits", "Est. E/M visits", df_md_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/rural/", file_ext = "_pcp", idname = "npi")
aprn_wrvu_service_rural <- get_cs("wrvu_per_service", "wRVUs per service", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/rural/", file_ext = "_aprn", idname = "npi")
aprn_new_sick_visits_rural <- get_cs("new_sick_visits", "New E/M visits", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/rural/", file_ext = "_aprn", idname = "npi")
aprn_est_sick_visits_rural <- get_cs("est_sick_visits", "Est. E/M visits", df_aprn_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/rural/", file_ext = "_aprn", idname = "npi")

# Service mix (county)
c_pcp_est_sick_visits_rural <- get_cs("pcp_est_em_visits", "PCP est. E/M visits (county)", df_q, pre_periods, post_periods, balance_e, file_pre = "by-urban-rural/rural/", file_ext = "_county", idname = "full_fips")
c_pcp_share_est_sick_visits_rural <- get_cs("pcp_share_est_em_visits", "Share PCP est. E/M visits (county)", df_q, pre_periods, post_periods, balance_e,file_pre = "by-urban-rural/rural/", file_ext = "_county", idname = "full_fips")

# Export tables ----------------------------------------------------------------

# Aggregate supply
models_a <- list('Urban' = c_pcp_pc_urban, 
                 'Rural' = c_pcp_pc_rural, 
                 'Full' = c_pcp_pc)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
get_table(models_a,
          control_means_a = control_means_a, 
          title = "Table : Aggregate supply by rurality", source_note = "Note: SE clustered at state", 
          file_name = "by-urban-rural/agg-supply")

# New PCPs
models_a <- list('Urban' = c_new_pcp_pc_urban, 
                 'Rural' = c_new_pcp_pc_rural, 
                 'Full' = c_new_pcp_pc)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
get_table(models_a,
          control_means_a = control_means_a, 
          title = "Table : New PCPs by rurality", source_note = "Note: SE clustered at state", 
          file_name = "by-urban-rural/new-pcps")

# Services
models_a <- list('Urban' = pcp_services_urban, 
                 'Rural' = pcp_services_rural, 
                 'Full' = pcp_services)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
models_b <- list('Urban' = aprn_services_urban, 
                 'Rural' = aprn_services_rural, 
                 'Full' = aprn_services)
control_means_b <- sapply(models_b, get_control_mean, control_group = "notyettreated")
get_table(models_a, models_b,
          control_means_a, control_means_b,
          title = "Table : Total services by rurality", source_note = "Note: SE clustered at state", 
          file_name = "by-urban-rural/total-services", 
          panel_a_label = "Panel A: PCP", panel_b_label = "Panel B: APRN")

# wRVUs 
models_a <- list('Urban' = pcp_wrvu_urban, 
                 'Rural' = pcp_wrvu_rural, 
                 'Full' = pcp_wrvu)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
models_b <- list('Urban' = aprn_wrvu_urban, 
                 'Rural' = aprn_wrvu_rural, 
                 'Full' = aprn_wrvu)
control_means_b <- sapply(models_b, get_control_mean, control_group = "notyettreated")
get_table(models_a, models_b,
          control_means_a, control_means_b,
          title = "Table : Total wRVUs by rurality", source_note = "Note: SE clustered at state", 
          file_name = "by-urban-rural/total-wrvu", 
          panel_a_label = "Panel A: PCP", panel_b_label = "Panel B: APRN")

# Benes
models_a <- list('Urban' = pcp_bene_urban, 
                 'Rural' = pcp_bene_rural, 
                 'Full' = pcp_bene)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
models_b <- list('Urban' = aprn_bene_urban, 
                 'Rural' = aprn_bene_rural, 
                 'Full' = aprn_bene)
control_means_b <- sapply(models_b, get_control_mean, control_group = "notyettreated")
get_table(models_a, models_b,
          control_means_a, control_means_b,
          title = "Table : Total beneficiaries by rurality", source_note = "Note: SE clustered at state", 
          file_name = "by-urban-rural/total-bene", 
          panel_a_label = "Panel A: PCP", panel_b_label = "Panel B: APRN")

# wRVU per service 
models_a <- list('Urban' = pcp_wrvu_service_urban, 
                 'Rural' = pcp_wrvu_service_rural, 
                 'Full' = pcp_wrvu_service)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
models_b <- list('Urban' = aprn_wrvu_service_urban, 
                 'Rural' = aprn_wrvu_service_rural, 
                 'Full' = aprn_wrvu_service)
control_means_b <- sapply(models_b, get_control_mean, control_group = "notyettreated")
get_table(models_a, models_b,
          control_means_a, control_means_b,
          title = "Table : wRVUs per service by rurality", source_note = "Note: SE clustered at state", 
          file_name = "by-urban-rural/wrvu-per-service", 
          panel_a_label = "Panel A: PCP", panel_b_label = "Panel B: APRN")

# New E/M visits 
models_a <- list('Urban' = pcp_new_sick_visits_urban, 
                 'Rural' = pcp_new_sick_visits_rural, 
                 'Full' = pcp_new_sick_visits)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
models_b <- list('Urban' = aprn_new_sick_visits_urban, 
                 'Rural' = aprn_new_sick_visits_rural, 
                 'Full' = aprn_new_sick_visits)
control_means_b <- sapply(models_b, get_control_mean, control_group = "notyettreated")
get_table(models_a, models_b,
          control_means_a, control_means_b,
          title = "Table : New E/M visits by rurality", source_note = "Note: SE clustered at state", 
          file_name = "by-urban-rural/new-em-visits", 
          panel_a_label = "Panel A: PCP", panel_b_label = "Panel B: APRN")

# Established E/M visits 
models_a <- list('Urban' = pcp_est_sick_visits_urban, 
                 'Rural' = pcp_est_sick_visits_rural, 
                 'Full' = pcp_est_sick_visits)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
models_b <- list('Urban' = aprn_est_sick_visits_urban, 
                 'Rural' = aprn_est_sick_visits_rural, 
                 'Full' = aprn_est_sick_visits)
control_means_b <- sapply(models_b, get_control_mean, control_group = "notyettreated")
get_table(models_a, models_b,
          control_means_a, control_means_b,
          title = "Table : Established E/M visits by rurality", source_note = "Note: SE clustered at state", 
          file_name = "by-urban-rural/est-em-visits", 
          panel_a_label = "Panel A: PCP", panel_b_label = "Panel B: APRN")

# County PCP established E/M visits
models_a <- list('Urban' = c_pcp_est_sick_visits_urban, 
                 'Rural' = c_pcp_est_sick_visits_rural, 
                 'Full' = c_pcp_est_sick_visits)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
get_table(models_a,
          control_means_a = control_means_a, 
          title = "Table : Market PCP established E/M visits by rurality", source_note = "Note: SE clustered at state", 
          file_name = "by-urban-rural/market-pcp-est-em-visits")

# County PCP share established E/M visits
models_a <- list('Urban' = c_pcp_share_est_sick_visits_urban, 
                 'Rural' = c_pcp_share_est_sick_visits_rural, 
                 'Full' = c_pcp_share_est_sick_visits)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
get_table(models_a,
          control_means_a = control_means_a, 
          title = "Table : PCP market share of established E/M visits", source_note = "Note: SE clustered at state", 
          file_name = "by-urban-rural/market-share-pcp-est-em-visits")

# Clean up ---------------------------------------------------------------------

rm(list = ls())

df <- read_csv('data/output/final_df_state.csv')
df_c <- read_csv('data/output/final_df_county.csv')
source('analysis/0-helpers.R')
