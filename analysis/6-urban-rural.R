# About ------------------------------------------------------------------------

# Stratify by urban / rural
# Author:         Shirley Cai 
# Date created:   06/22/2026 
# Last edited:    06/22/2026

# Data prep --------------------------------------------------------------------

df_urban <- df_ss %>% filter(D_rural == 0)
df_rural <- df_ss %>% filter(D_rural == 1)

# Event studies ----------------------------------------------------------------

df_urban <- df_urban %>% mutate(treat_year = ifelse(treat == 0, 0, effective_year))
df_rural <- df_rural %>% mutate(treat_year = ifelse(treat == 0, 0, effective_year))

pre_periods <- 5
post_periods <- 5
balance_e <- NULL

# Urban
get_cs("pcp_per_10k", "PCPs per 10k people", df_urban, pre_periods, post_periods, balance_e, file_pre = "rural-urban/cs/urban_")
get_cs("new_pcp_per_10k", "New PCPs per 10k people", df_urban, pre_periods, post_periods, balance_e, file_pre = "rural-urban/cs/urban_")

# Rural
get_cs("pcp_per_10k", "PCPs per 10k people", df_rural, pre_periods, post_periods, balance_e, file_pre = "rural-urban/cs/rural_")
get_cs("new_pcp_per_10k", "New PCPs per 10k people", df_rural, pre_periods, post_periods, balance_e, file_pre = "rural-urban/cs/rural_")

# Staggered DiD ----------------------------------------------------------------

pre_periods <- 5
post_periods <- 5
balance_e <- NULL

# Urban 
pcp_urban <- get_cs_did("pcp_per_10k", "PCPs per 10k people", df_urban, pre_periods, post_periods, balance_e)
new_pcp_urban <- get_cs_did("new_pcp_per_10k", "New PCPs per 10k people", df_urban, pre_periods, post_periods, balance_e)

# Rural 
pcp_rural <- get_cs_did("pcp_per_10k", "PCPs per 10k people", df_rural, pre_periods, post_periods, balance_e)
new_pcp_rural <- get_cs_did("new_pcp_per_10k", "New PCPs per 10k people", df_rural, pre_periods, post_periods, balance_e)

## Export as tables ------------------------------------------------------------

# TODO 

# Stacked DiD ------------------------------------------------------------------

# Urban 
 
# Rural 

## Export as tables ------------------------------------------------------------

# TODO
