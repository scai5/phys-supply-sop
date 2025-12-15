# About ------------------------------------------------------------------------

# Event study
# Author:         Shirley Cai 
# Date created:   11/14/2025 
# Last edited:    12/12/2025 

# Event time descriptives ------------------------------------------------------

# Observations per event time 
df %>%
  filter(treat == 1) %>% 
  ggplot(aes(x = time_to_treat)) + 
  geom_histogram(binwidth = 1) + 
  labs(
    x = "Time to treat (treated only)", 
    y = "Count"
  )
ggsave(
  "results/event-study/time_to_treat_hist.png", scale = 2,
  width = 1200, height = 900, units = "px"
)

df_c %>%
  filter(treat == 1) %>% 
  ggplot(aes(x = time_to_treat)) + 
  geom_histogram(binwidth = 1) + 
  labs(
    x = "Time to treat (treated only)", 
    y = "Count"
  )
ggsave(
  "results/event-study/time_to_treat_hist_county.png", scale = 2,
  width = 1200, height = 900, units = "px"
)

# Trim data --------------------------------------------------------------------

df <- df %>% mutate(treat_year = ifelse(treat == 0, Inf, effective_year))
df_c <- df_c %>% mutate(treat_year = ifelse(treat == 0, Inf, effective_year))

# Restrict sample to certain years pre and post policy 

pre_periods <- 5
post_periods <- 5

df_trim <- df %>% filter(time_to_treat >= -pre_periods, time_to_treat <= post_periods)
df_c_trim <- df_c %>% filter(time_to_treat >= -pre_periods, time_to_treat <= post_periods)

# Sun and Abraham --------------------------------------------------------------

# Sun and Abraham specification
get_sunab <- function(outcome, var_desc, df, period = "year", file_pre = "", file_ext = ""){
  fml_string <- as.formula(paste0(outcome, " ~ sunab(treat_year, ", period, ") | state + year"))
  dd_out <- feols(fml_string, data = df, vcov = cluster ~ state)
  
  p <- dd_out %>% 
    ggiplot(
      main = paste0("Outcome: ", var_desc), 
      xlab = "Time to SOP expansion", 
      ref.line = -1
    ) 
  
  ggsave(
    filename = paste0('results/event-study/sunab/', file_pre, outcome, file_ext, '_sunab.png'), 
    plot = p, 
    scale = 2, width = 1200, height = 900, units = 'px'
  )
  return(p)
}

## State level -----------------------------------------------------------------

# Counts
get_sunab("tot_md", "Number of MDs", df_trim, "year")
get_sunab("tot_md_pcp", "Number of MD PCPs", df_trim, "year")
get_sunab("tot_md_spec", "Number of MD specialists", df_trim, "year")

get_sunab("tot_do", "Number of DOs", df_trim, "year")
get_sunab("tot_do_pcp", "Number of DO PCPs", df_trim, "year")
get_sunab("tot_do_spec", "Number of DO specialists", df_trim, "year")

# Counts per capita
get_sunab("md_per_10k", "MDs per 10k people", df_trim, "year")
get_sunab("md_pcp_per_10k", "MD PCPs per 10k people", df_trim, "year")
get_sunab("md_spec_per_10k", "MD specialists per 10k people", df_trim, "year")

get_sunab("do_per_10k", "DOs per 10k people", df_trim, "year")
get_sunab("do_pcp_per_10k", "DO PCPs per 10k people", df_trim, "year")
get_sunab("do_spec_per_10k", "DO specialists per 10k people", df_trim, "year")

# Average FM wage
get_sunab("fm_wage_real", "Family med annual wage", df_trim, "year")
get_sunab("im_wage_real", "Internal med annual wage", df_trim, "year")
get_sunab("peds_wage_real", "Pediatrics annual wage", df_trim, "year")

## County level ----------------------------------------------------------------

# Counts
get_sunab("tot_md", "Number of MDs", df_c_trim, "year", "county/")
get_sunab("tot_md_pcp", "Number of MD PCPs", df_c_trim, "year", "county/")
get_sunab("tot_md_spec", "Number of MD specialists", df_c_trim, "year", "county/")

get_sunab("tot_do", "Number of DOs", df_c_trim, "year", "county/")
get_sunab("tot_do_pcp", "Number of DO PCPs", df_c_trim, "year", "county/")
get_sunab("tot_do_spec", "Number of DO specialists", df_c_trim, "year", "county/")

# Counts per capita
get_sunab("md_per_10k", "MDs per 10k people", df_c_trim, "year", "county/")
get_sunab("md_pcp_per_10k", "MD PCPs per 10k people", df_c_trim, "year", "county/")
get_sunab("md_spec_per_10k", "MD specialists per 10k people", df_c_trim, "year", "county/")

get_sunab("do_per_10k", "DOs per 10k people", df_c_trim, "year", "county/")
get_sunab("do_pcp_per_10k", "DO PCPs per 10k people", df_c_trim, "year", "county/")
get_sunab("do_spec_per_10k", "DO specialists per 10k people", df_c_trim, "year", "county/")

# Callaway and Sant'Anna -------------------------------------------------------

df <- df %>% mutate(treat_year = ifelse(treat == 0, 0, effective_year))
df_c <- df_c %>% mutate(treat_year = ifelse(treat == 0, 0, effective_year))

# Callaway and Sant'anna specification 
get_cs <- function(outcome, var_desc, df, pre_periods, post_periods, balance_e = NULL, file_pre = "", file_ext = ""){
  out <- att_gt(yname = outcome, 
                tname = "year", 
                idname = "state_fips", 
                gname = "treat_year",
                data = df,
                allow_unbalanced_panel = TRUE,
                clustervars = c("state_fips"))
  es <- aggte(out, 
              na.rm = TRUE,
              type = "dynamic", 
              min_e = -pre_periods, 
              max_e = post_periods,
              balance_e = balance_e)
  p <- ggdid(es,
             xlab = "Time to SOP expansion", 
             title = paste0("Outcome: ", var_desc))
  
  ggsave(
    filename = paste0('results/event-study/cs/', file_pre, outcome, file_ext, '.png'), 
    plot = p, 
    scale = 2, width = 1200, height = 900, units = 'px'
  )
  return(p)
}

pre_periods <- 5
post_periods <- 5
balance_e <- NULL

## State level -----------------------------------------------------------------

# Counts
get_cs("tot_md", "Number of MDs", df, pre_periods, post_periods, balance_e)
get_cs("tot_md_pcp", "Number of MD PCPs", df, pre_periods, post_periods, balance_e)
get_cs("tot_md_spec", "Number of MD specialists", df,  pre_periods, post_periods, balance_e)

get_cs("tot_do", "Number of DOs", df, pre_periods, post_periods, balance_e)
get_cs("tot_do_pcp", "Number of DO PCPs", df, pre_periods, post_periods, balance_e)
get_cs("tot_do_spec", "Number of DO specialists", df,  pre_periods, post_periods, balance_e)

# Counts per capita
get_cs("md_per_10k", "MDs per 10k people", df, pre_periods, post_periods, balance_e)
get_cs("md_pcp_per_10k", "MD PCPs per 10k people", df, pre_periods, post_periods, balance_e)
get_cs("md_spec_per_10k", "MD specialists per 10k people", df,pre_periods, post_periods, balance_e)

get_cs("do_per_10k", "DOs per 10k people", df, pre_periods, post_periods, balance_e)
get_cs("do_pcp_per_10k", "DO PCPs per 10k people", df,pre_periods, post_periods, balance_e)
get_cs("do_spec_per_10k", "DO specialists per 10k people", df, pre_periods, post_periods, balance_e)

# Average FM wage
get_cs("fm_wage_real", "Family med annual wage", df, pre_periods, post_periods, balance_e)
get_cs("im_wage_real", "Internal  med annual wage", df, pre_periods, post_periods, balance_e)
get_cs("peds_wage_real", "Pediatrics annual wage", df, pre_periods, post_periods, balance_e)

## County level ----------------------------------------------------------------

# Counts
get_cs("tot_md", "Number of MDs", df_c, pre_periods, post_periods, balance_e, "county/")
get_cs("tot_md_pcp", "Number of MD PCPs", df_c, pre_periods, post_periods, balance_e, "county/")
get_cs("tot_md_spec", "Number of MD specialists", df_c,  pre_periods, post_periods, balance_e, "county/")

get_cs("tot_do", "Number of DOs", df_c, pre_periods, post_periods, balance_e, "county/")
get_cs("tot_do_pcp", "Number of DO PCPs", df_c, pre_periods, post_periods, balance_e, "county/")
get_cs("tot_do_spec", "Number of DO specialists", df_c,  pre_periods, post_periods, balance_e, "county/")

# Counts per capita
get_cs("md_per_10k", "MDs per 10k people", df_c, pre_periods, post_periods, balance_e, "county/")
get_cs("md_pcp_per_10k", "MD PCPs per 10k people", df_c, pre_periods, post_periods, balance_e, "county/")
get_cs("md_spec_per_10k", "MD specialists per 10k people", df_c,pre_periods, post_periods, balance_e, "county/")

get_cs("do_per_10k", "DOs per 10k people", df_c, pre_periods, post_periods, balance_e, "county/")
get_cs("do_pcp_per_10k", "DO PCPs per 10k people", df_c,pre_periods, post_periods, balance_e, "county/")
get_cs("do_spec_per_10k", "DO specialists per 10k people", df_c, pre_periods, post_periods, balance_e, "county/")

# Synthetic DiD ----------------------------------------------------------------

# Need balanced panel for synthetic DiD
# I lose a lot of treated states if I do things this way due to 2009 missing data?
# Only keep 13 treated states using +-3 years time window
# Only keep 9 treated states using +-4 years time window
# Only keep 8 treated states using +-5 years time window

pre_periods <- 3
post_periods <- 3

# Get balanced panel 
balanced_df <- df %>% 
  filter(!is.na(effective_year)) %>% 
  group_by(state) %>% 
  filter(all((-pre_periods:post_periods) %in% time_to_treat)) %>% 
  ungroup()
never_treated <- df %>% 
  filter(is.infinite(treat_year))
balanced_df <- bind_rows(balanced_df, never_treated)

message("Balanced panel ---------")
message(paste0("Number of states: ", length(unique(balanced_df$state))))
message(paste0("Number of treated states: ", length(unique(balanced_df$state[balanced_df$treat == 1]))))
