# About ------------------------------------------------------------------------

# New PCPs / entrants
# Author:         Shirley Cai 
# Date created:   02/27/2026 
# Last edited:    09/25/2026

# Preparation ------------------------------------------------------------------

df_c <- df_c %>%
  mutate(
    full_fips = paste0(formatC(state_fips, width = 2, flag = "0"), 
                       formatC(county_fips, width = 3, flag = "0")),
    full_fips = as.numeric(full_fips)
  )

# Restrict sample to certain years pre and post policy 
pre_periods <- 5
post_periods <- 5
balance_e <- NULL

# Callaway and Sant'Anna -------------------------------------------------------

df <- df %>% mutate(treat_year = ifelse(treat == 0, 0, effective_year))
df_c <- df_c %>% mutate(treat_year = ifelse(treat == 0, 0, effective_year))

# State counts per capita
pcp_pc <- get_cs("new_pcp_per_10k", "New PCPs per 10k people", df, pre_periods, post_periods, balance_e, file_pre = "new-pcps/", file_ext = "_state")
md_pcp_pc <- get_cs("new_md_pcp_per_10k", "New MD PCPs per 10k people", df, pre_periods, post_periods, balance_e, file_pre = "new-pcps/", file_ext = "_state")
do_pcp_pc <- get_cs("new_do_pcp_per_10k", "New DO PCPs per 10k people", df,pre_periods, post_periods, balance_e, file_pre = "new-pcps/", file_ext = "_state")

# County counts per capita
c_pcp_pc <- get_cs("new_pcp_per_10k", "New PCPs per 10k people", df_c, pre_periods, post_periods, balance_e, file_pre = "new-pcps/", file_ext = "_county", idname = "full_fips")
c_md_pcp_pc <- get_cs("new_md_pcp_per_10k", "New MD PCPs per 10k people", df_c, pre_periods, post_periods, balance_e, file_pre = "new-pcps/", file_ext = "_county", idname = "full_fips")
c_do_pcp_pc <- get_cs("new_do_pcp_per_10k", "New DO PCPs per 10k people", df_c,pre_periods, post_periods, balance_e, file_pre = "new-pcps/", file_ext = "_county", idname = "full_fips")

aprn_pc <- get_cs("aprn_medicare_per_10k", "APRNs billing Medicare", df_c,pre_periods, post_periods, balance_e, file_pre = "new-pcps/", file_ext = "_county", idname = "full_fips")

# Export tables ----------------------------------------------------------------

models_a <- list('pcp_state' = pcp_pc, 
                 'md_pcp_state' = md_pcp_pc, 
                 'do_pcp_state' = do_pcp_pc)
control_means_a <- sapply(models_a, get_control_mean, control_group = "notyettreated")
models_b <- list('pcp_county' = c_pcp_pc, 
                 'md_pcp_county' = c_md_pcp_pc, 
                 'do_pcp_county' = c_do_pcp_pc)
control_means_b <- sapply(models_b, get_control_mean, control_group = "notyettreated")

get_table(models_a, models_b,
          control_means_a, control_means_b,
          title = "Table : New PCPs per 10,000", source_note = "Note: SE clustered at state", file_name = "new-pcps/new-pcps", 
          panel_a_label = "Panel A: State", panel_b_label = "Panel B: County", 
          col_labels = c("(1)" = "PCPs", 
                         "(2)" = "MD PCPs", 
                         "(3)" = "DO PCPs"))

# Clean up ---------------------------------------------------------------------

rm(list = ls())

df <- read_csv('data/output/final_df_state.csv')
df_c <- read_csv('data/output/final_df_county.csv')
source('analysis/0-helpers.R')
