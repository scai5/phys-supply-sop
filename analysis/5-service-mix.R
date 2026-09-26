# About ------------------------------------------------------------------------

# Physician service mix
# Author:         Shirley Cai 
# Date created:   07/09/2026 
# Last edited:    09/25/2026

# Preparation ------------------------------------------------------------------

df_c <- df_c %>%
  mutate(
    full_fips = paste0(formatC(state_fips, width = 2, flag = "0"), 
                       formatC(county_fips, width = 3, flag = "0")),
    full_fips = as.numeric(full_fips)
  )

df_util <- read_csv('data/output/final_service_mix.csv')
df_util_untrim <- df_util 
df_util <- df_util_untrim %>% filter(wrvu_per_service < 5)

## TESTING ZONE 
summary(df_util$chronic_care)
summary(df_util$adv_care_plan)
summary(df_util$behavioral_screening)
summary(df_util$smoking_cessation)
summary(df_util$inr_monitor)
summary(df_util$home_inr_monitor)

# Callaway and Sant'Anna -------------------------------------------------------

md_df <- df_util %>% filter(D_aprn == 0) %>% mutate(treat_year = ifelse(treat == 0, 0, effective_year))
aprn_df <- df_util %>% filter(D_aprn == 1) %>% mutate(treat_year = ifelse(treat == 0, 0, effective_year))
df_c <- df_c %>% mutate(treat_year = ifelse(treat == 0, 0, effective_year))

pre_periods <- 5
post_periods <- 5
balance_e <- NULL
  
# PCP service mix
pcp_wrvu_service <- get_cs("wrvu_per_service", "wRVUs per service", md_df, pre_periods, post_periods, balance_e, file_pre = "service-mix/", file_ext = "_pcp", idname = "npi")
pcp_wrvu_service_office <- get_cs("wrvu_office_per_service", "wRVUs per service (office only)", md_df, pre_periods, post_periods, balance_e, file_pre = "service-mix/", file_ext = "_pcp", idname = "npi")
pcp_wrvu_service_2013 <- get_cs("wrvu_2013_per_service", "wRVUs (2013) per service", md_df, pre_periods, post_periods, balance_e, file_pre = "service-mix/", file_ext = "_pcp", idname = "npi")
pcp_wrvu_service_office_2013 <- get_cs("wrvu_2013_office_per_service", "wRVUs (2013) per service (office only)", md_df, pre_periods, post_periods, balance_e, file_pre = "service-mix/", file_ext = "_pcp", idname = "npi")

# APRN service mix
aprn_wrvu_service <- get_cs("wrvu_per_service", "wRVUs per service", aprn_df, pre_periods, post_periods, balance_e, file_pre = "service-mix/", file_ext = "_aprn", idname = "npi")
aprn_wrvu_service_office <- get_cs("wrvu_office_per_service", "wRVUs per service (office only)", aprn_df, pre_periods, post_periods, balance_e, file_pre = "service-mix/", file_ext = "_aprn", idname = "npi")
aprn_wrvu_service_2013 <- get_cs("wrvu_2013_per_service", "wRVUs (2013) per service", aprn_df, pre_periods, post_periods, balance_e, file_pre = "service-mix/", file_ext = "_aprn", idname = "npi")
aprn_wrvu_service_office_2013 <- get_cs("wrvu_2013_office_per_service", "wRVUs (2013) per service (office only)", aprn_df, pre_periods, post_periods, balance_e, file_pre = "service-mix/", file_ext = "_aprn", idname = "npi")

# Billing for new patients
pcp_new_sick_visits <- get_cs("new_sick_visits", "New E/M visits", md_df, pre_periods, post_periods, balance_e, file_pre = "service-mix/", file_ext = "_pcp", idname = "npi")
pcp_new_medicare_visits <- get_cs("new_medicare_visits", "New to Medicare visits", md_df, pre_periods, post_periods, balance_e, file_pre = "service-mix/", file_ext = "_pcp", idname = "npi")
aprn_new_sick_visits <- get_cs("new_sick_visits", "New E/M visits", aprn_df, pre_periods, post_periods, balance_e, file_pre = "service-mix/", file_ext = "_aprn", idname = "npi")
aprn_new_medicare_visits <- get_cs("new_medicare_visits", "New to Medicare visits", aprn_df, pre_periods, post_periods, balance_e, file_pre = "service-mix/", file_ext = "_aprn", idname = "npi")

# Billing for new patients (county level)
c_pcp_new_sick_visits <- get_cs("pcp_new_em_visits", "PCP new E/M visits (county)", df_c, pre_periods, post_periods, balance_e, file_pre = "service-volume/", file_ext = "_county", idname = "full_fips")
c_pcp_share_new_sick_visits <- get_cs("pcp_share_new_em_visits", "Share PCP new E/M visits (county)", df_c, pre_periods, post_periods, balance_e, file_pre = "service-volume/", file_ext = "_county", idname = "full_fips")
c_pcp_new_medicare_visits <- get_cs("pcp_new_medicare_visits", "PCP new Medicare visits (county)", df_c, pre_periods, post_periods, balance_e, file_pre = "service-volume/", file_ext = "_county", idname = "full_fips")
c_pcp_share_new_medicare_visits <- get_cs("pcp_share_new_medicare_visits", "Share PCP new Medicare visits (county)", df_c, pre_periods, post_periods, balance_e, file_pre = "service-volume/", file_ext = "_county", idname = "full_fips")

# Billing for established patients
pcp_est_sick_visits <- get_cs("est_sick_visits", "Est. E/M visits", md_df, pre_periods, post_periods, balance_e, file_pre = "service-mix/", file_ext = "_pcp", idname = "npi")
aprn_est_sick_visits <- get_cs("est_sick_visits", "Est. E/M visits", aprn_df, pre_periods, post_periods, balance_e, file_pre = "service-mix/", file_ext = "_aprn", idname = "npi")
c_pcp_est_sick_visits <- get_cs("pcp_est_em_visits", "PCP est. E/M visits (county)", df_c, pre_periods, post_periods, balance_e, file_pre = "service-volume/", file_ext = "_county", idname = "full_fips")
c_pcp_share_est_sick_visits <- get_cs("pcp_share_est_em_visits", "Share PCP est. E/M visits (county)", df_c, pre_periods, post_periods, balance_e, file_pre = "service-volume/", file_ext = "_county", idname = "full_fips")

# Export tables ----------------------------------------------------------------

models_a <- list('pcp_wrvu_service' = pcp_wrvu_service, 
                 'pcp_new_sick_visits' = pcp_new_sick_visits, 
                 'pcp_est_sick_visits' = pcp_est_sick_visits)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
models_b <- list('aprn_wrvu_service' = aprn_wrvu_service, 
                 'aprn_new_sick_visits' = aprn_new_sick_visits, 
                 'aprn_est_sick_visits' = aprn_est_sick_visits)
control_means_b <- sapply(models_b, get_control_mean, control_group = "notyettreated")

get_table(models_a, models_b,
          control_means_a, control_means_b,
          title = "Table : Service mix", source_note = "Note: SE clustered at state", file_name = "service-mix/service-mix", 
          panel_a_label = "Panel A: PCP", panel_b_label = "Panel B: APRN", 
          col_labels = c("(1)" = "wRVU per service", 
                         "(2)" = "New E/M visits", 
                         "(3)" = "Established E/M visits"))

# Clean up ---------------------------------------------------------------------

rm(list = ls())

df <- read_csv('data/output/final_df_state.csv')
df_c <- read_csv('data/output/final_df_county.csv')
source('analysis/0-helpers.R')
