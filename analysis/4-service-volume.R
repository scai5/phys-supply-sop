# About ------------------------------------------------------------------------

# Service volume
# Author:         Shirley Cai 
# Date created:   09/25/2026 
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

# Restrict sample to certain years pre and post policy 
pre_periods <- 5
post_periods <- 5
balance_e <- NULL

# Callaway and Sant'Anna -------------------------------------------------------

md_df <- df_util %>% filter(D_aprn == 0) %>% mutate(treat_year = ifelse(treat == 0, 0, effective_year))
aprn_df <- df_util %>% filter(D_aprn == 1) %>% mutate(treat_year = ifelse(treat == 0, 0, effective_year))
df_c <- df_c %>% mutate(treat_year = ifelse(treat == 0, 0, effective_year))

# PCP volume 
pcp_services <- get_cs("tot_services", "Services per PCP", md_df, pre_periods, post_periods, balance_e, file_pre = "service-volume/", file_ext = "_pcp", idname = "npi")
pcp_services_office <- get_cs("tot_services_office", "Office services per PCP", md_df, pre_periods, post_periods, file_pre = "service-volume/", file_ext = "_pcp", idname = "npi")
pcp_wrvu <- get_cs("tot_wrvu", "wRVUs per PCP", md_df, pre_periods, post_periods, balance_e, file_pre = "service-volume/", file_ext = "_pcp", idname = "npi")
pcp_wrvu_2013 <- get_cs("tot_wrvu_2013", "wRVUs (2013) per PCP", md_df, pre_periods, post_periods, balance_e, file_pre = "service-volume/", file_ext = "_pcp", idname = "npi")
pcp_wrvu_office <- get_cs("tot_wrvu_office", "wRVUs (office only) per PCP", md_df, pre_periods, post_periods, balance_e, file_pre = "service-volume/", file_ext = "_pcp", idname = "npi")
pcp_wrvu_office_2013 <- get_cs("tot_wrvu_2013_office", "wRVUs (2013, office only) per PCP", md_df, pre_periods, post_periods, balance_e, file_pre = "service-volume/", file_ext = "_pcp", idname = "npi")
pcp_bene <- get_cs("tot_benes", "Beneficiaries per PCP", md_df, pre_periods, post_periods, balance_e, file_pre = "service-volume/", file_ext = "_pcp", idname = "npi")

# APRN volume 
aprn_services <- get_cs("tot_services", "Services per APRN", aprn_df, pre_periods, post_periods, balance_e, file_pre = "service-volume/", file_ext = "_aprn", idname = "npi")
aprn_services_office <- get_cs("tot_services_office", "Office services per APRN", aprn_df, pre_periods, post_periods, balance_e, file_pre = "service-volume/", file_ext = "_aprn", idname = "npi")
aprn_wrvu <- get_cs("tot_wrvu", "wRVUs per APRN", aprn_df, pre_periods, post_periods, balance_e, file_pre = "service-volume/", file_ext = "_aprn", idname = "npi")
aprn_wrvu_2013 <- get_cs("tot_wrvu_2013", "wRVUs (2013) per APRN", aprn_df, pre_periods, post_periods, balance_e, file_pre = "service-volume/", file_ext = "_aprn", idname = "npi")
aprn_wrvu_office <- get_cs("tot_wrvu_office", "wRVUs (office only) per APRN", aprn_df, pre_periods, post_periods, balance_e, file_pre = "service-volume/", file_ext = "_aprn", idname = "npi")
aprn_wrvu_office_2013 <- get_cs("tot_wrvu_2013_office", "wRVUs (2013, office only) per APRN", aprn_df, pre_periods, post_periods, balance_e, file_pre = "service-volume/", file_ext = "_aprn", idname = "npi")
aprn_bene <- get_cs("tot_benes", "Beneficiaries per APRN", aprn_df, pre_periods, post_periods, balance_e, file_pre = "service-volume/", file_ext = "_aprn", idname = "npi")

# County level
c_aprn_services <- get_cs("aprn_services_per_bene", "APRN services per Medicare bene (county)", df_c, pre_periods, post_periods, balance_e, file_pre = "service-volume/", file_ext = "_county", idname = "full_fips")
c_aprn_services_office <- get_cs("aprn_services_office_per_bene", "APRN office services per Medicare bene (county)", df_c, pre_periods, post_periods, balance_e, file_pre = "service-volume/", file_ext = "_county", idname = "full_fips")
# TODO: PCP share of benes

# Export tables ----------------------------------------------------------------

models_a <- list('pcp_bene' = pcp_bene, 
                 'pcp_services' = pcp_services, 
                 'pcp_wrvu' = pcp_wrvu)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
models_b <- list('aprn_bene' = aprn_bene, 
                 'aprn_services' = aprn_services, 
                 'aprn_wrvu' = aprn_wrvu)
control_means_b <- sapply(models_b, get_control_mean, control_group = "notyettreated")

get_table(models_a, models_b,
          control_means_a, control_means_b,
          title = "Table : Service volume", source_note = "Note: SE clustered at state", file_name = "service-volume/service-volume", 
          panel_a_label = "Panel A: PCP", panel_b_label = "Panel B: APRN", 
          col_labels = c("(1)" = "Beneficiaries", 
                         "(2)" = "Services", 
                         "(3)" = "Total wRVUs"))

# Clean up ---------------------------------------------------------------------

rm(list = ls())

df <- read_csv('data/output/final_df_state.csv')
df_c <- read_csv('data/output/final_df_county.csv')
source('analysis/0-helpers.R')