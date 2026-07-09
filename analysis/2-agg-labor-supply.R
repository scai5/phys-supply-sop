# About ------------------------------------------------------------------------

# Aggregate labor supply
# Author:         Shirley Cai 
# Date created:   11/14/2025 
# Last edited:    06/18/2026

# Trim data --------------------------------------------------------------------

df <- df %>% mutate(treat_year = ifelse(treat == 0, Inf, effective_year))
df_c <- df_c %>% mutate(treat_year = ifelse(treat == 0, Inf, effective_year))

# Restrict sample to certain years pre and post policy 
pre_periods <- 5
post_periods <- 5

df_trim <- df %>% filter(time_to_treat >= -pre_periods, time_to_treat <= post_periods)
df_c_trim <- df_c %>% filter(time_to_treat >= -pre_periods, time_to_treat <= post_periods)

# Event studies ----------------------------------------------------------------

## Basic TWFE ------------------------------------------------------------------

# State counts per 10k
get_eventplot("pcp_per_10k", "PCPs per 10k people", df_trim, "agg-supply/state/twfe/")
get_eventplot("md_pcp_per_10k", "MD PCPs per 10k people", df_trim, "agg-supply/state/twfe/")
get_eventplot("do_pcp_per_10k", "DO PCPs per 10k people", df_trim, "agg-supply/state/twfe/")

# County counts per 10k 
get_eventplot("pcp_per_10k", "PCPs per 10k people", df_c_trim, "agg-supply/county/twfe/")
get_eventplot("md_pcp_per_10k", "MD PCPs per 10k people", df_c_trim, "agg-supply/county/twfe/")
get_eventplot("do_pcp_per_10k", "DO PCPs per 10k people", df_c_trim, "agg-supply/county/twfe/")

## Sun and Abraham -------------------------------------------------------------

# State counts per 10k
get_sunab("pcp_per_10k", "PCPs per 10k people", df_trim, "year", "agg-supply/state/sunab/")
get_sunab("md_pcp_per_10k", "MD PCPs per 10k people", df_trim, "year", "agg-supply/state/sunab/")
get_sunab("do_pcp_per_10k", "DO PCPs per 10k people", df_trim, "year", "agg-supply/state/sunab/")

# County counts per 10k 
get_sunab("pcp_per_10k", "PCPs per 10k people", df_c_trim, "year", "agg-supply/county/sunab/")
get_sunab("md_pcp_per_10k", "MD PCPs per 10k people", df_c_trim, "year", "agg-supply/county/sunab/")
get_sunab("do_pcp_per_10k", "DO PCPs per 10k people", df_c_trim, "year", "agg-supply/county/sunab/")

## Callaway and Sant'Anna ------------------------------------------------------

df <- df %>% mutate(treat_year = ifelse(treat == 0, 0, effective_year))
df_c <- df_c %>% mutate(treat_year = ifelse(treat == 0, 0, effective_year))

pre_periods <- 5
post_periods <- 5
balance_e <- NULL

# State counts per 10k
get_cs("pcp_per_10k", "PCPs per 10k people", df, pre_periods, post_periods, balance_e, file_pre = "agg-supply/state/cs/")
get_cs("md_pcp_per_10k", "MD PCPs per 10k people", df, pre_periods, post_periods, balance_e, file_pre = "agg-supply/state/cs/")
get_cs("do_pcp_per_10k", "DO PCPs per 10k people", df,pre_periods, post_periods, balance_e, file_pre = "agg-supply/state/cs/")

# County counts per 10k
get_cs("pcp_per_10k", "PCPs per 10k people", df_c, pre_periods, post_periods, balance_e, file_pre = "agg-supply/county/cs/")
get_cs("md_pcp_per_10k", "MD PCPs per 10k people", df_c, pre_periods, post_periods, balance_e, file_pre = "agg-supply/county/cs/")
get_cs("do_pcp_per_10k", "DO PCPs per 10k people", df_c,pre_periods, post_periods, balance_e, file_pre = "agg-supply/county/cs/")

## Stacked a la Cengiz et al. (2019) -------------------------------------------

df <- df %>% mutate(treat_year = ifelse(treat == 0, Inf, effective_year))
df_c <- df_c %>% mutate(treat_year = ifelse(treat == 0, Inf, effective_year))

pre_periods <- 5
post_periods <- 5

stacked_state <- get_stacked_df(df, pre_periods, post_periods)
stacked_county <- get_stacked_df(df_c, pre_periods, post_periods)

# State counts per 10k 
get_stacked("pcp_per_10k", "PCPs per 10k people", stacked_state, "agg-supply/state/stacked/")
get_stacked("md_pcp_per_10k", "MD PCPs per 10k people", stacked_state, "agg-supply/state/stacked/")
get_stacked("do_pcp_per_10k", "DO PCPs per 10k people", stacked_state, "agg-supply/state/stacked/")

# County counts per 10k
get_stacked("pcp_per_10k", "PCPs per 10k people", stacked_county, "agg-supply/county/stacked/")
get_stacked("md_pcp_per_10k", "MD PCPs per 10k people", stacked_county, "agg-supply/county/stacked/")
get_stacked("do_pcp_per_10k", "DO PCPs per 10k people", stacked_county, "agg-supply/county/stacked/")

# Staggered DiD ----------------------------------------------------------------

## Using all data --------------------------------------------------------------

pre_periods <- Inf
post_periods <- Inf
balance_e <- NULL

# State counts per 10k
pcp_pc <- get_cs_did("pcp_per_10k", "PCPs per 10k people", df, pre_periods, post_periods, balance_e)
md_pcp_pc <- get_cs_did("md_pcp_per_10k", "MD PCPs per 10k people", df, pre_periods, post_periods, balance_e)
do_pcp_pc <- get_cs_did("do_pcp_per_10k", "DO PCPs per 10k people", df,pre_periods, post_periods, balance_e)

# County counts per 10k
c_pcp_pc <- get_cs_did("pcp_per_10k", "PCPs per 10k people", df_c, pre_periods, post_periods, balance_e)
c_md_pcp_pc <- get_cs_did("md_pcp_per_10k", "MD PCPs per 10k people", df_c, pre_periods, post_periods, balance_e)
c_do_pcp_pc <- get_cs_did("do_pcp_per_10k", "DO PCPs per 10k people", df_c,pre_periods, post_periods, balance_e)

### Export as tables -----------------------------------------------------------

gof_map <- tribble(
  ~raw,          ~clean,          ~fmt,     ~omit,
  "r.squared",   "R2",               3,     FALSE
)

# State counts per 10k 
avg_pcp <- mean(df$pcp_per_10k, na.rm = TRUE)
avg_md <- mean(df$md_pcp_per_10k, na.rm = TRUE)
avg_do <- mean(df$do_pcp_per_10k, na.rm = TRUE)
models <- list("PCPs" = pcp_pc, "MD PCPs" = md_pcp_pc, "DO PCPs" = do_pcp_pc)
rows <- tribble(
  ~term,                 ~All,     ~MD,    ~DO, 
  'Mean PCPs per 10k', avg_pcp, avg_md, avg_do)
get_table(models, gof_map, rows, "PCPs per 10k", "Note: SE clustered at state", "agg-supply/state/all-obs/pcp_per_10k")

# County counts per 10k 
avg_pcp <- mean(df_c$pcp_per_10k, na.rm = TRUE)
avg_md <- mean(df_c$md_pcp_per_10k, na.rm = TRUE)
avg_do <- mean(df_c$do_pcp_per_10k, na.rm = TRUE)
models <- list("PCPs" = c_pcp_pc, "MD PCPs" = c_md_pcp_pc, "DO PCPs" = c_do_pcp_pc)
rows <- tribble(
  ~term,                 ~All,     ~MD,    ~DO, 
  'Mean PCPs per 10k', avg_pcp, avg_md, avg_do)
get_table(models, gof_map, rows, "PCPs per 10k", "Note: SE clustered at state", "agg-supply/county/all-obs/pcp_per_10k")

## 5 years pre and post --------------------------------------------------------

pre_periods <- 5
post_periods <- 5
balance_e <- NULL

# State counts per 10k
pcp_pc <- get_cs_did("pcp_per_10k", "PCPs per 10k people", df, pre_periods, post_periods, balance_e)
md_pcp_pc <- get_cs_did("md_pcp_per_10k", "MD PCPs per 10k people", df, pre_periods, post_periods, balance_e)
do_pcp_pc <- get_cs_did("do_pcp_per_10k", "DO PCPs per 10k people", df,pre_periods, post_periods, balance_e)

# County counts per 10k
c_pcp_pc <- get_cs_did("pcp_per_10k", "PCPs per 10k people", df_c, pre_periods, post_periods, balance_e)
c_md_pcp_pc <- get_cs_did("md_pcp_per_10k", "MD PCPs per 10k people", df_c, pre_periods, post_periods, balance_e)
c_do_pcp_pc <- get_cs_did("do_pcp_per_10k", "DO PCPs per 10k people", df_c,pre_periods, post_periods, balance_e)

### Export as tables -----------------------------------------------------------

gof_map <- tribble(
  ~raw,          ~clean,          ~fmt,     ~omit, 
  "r.squared",   "R2",               3,     FALSE
)

# State counts per 10k 
avg_pcp <- mean(df$pcp_per_10k, na.rm = TRUE)
avg_md <- mean(df$md_pcp_per_10k, na.rm = TRUE)
avg_do <- mean(df$do_pcp_per_10k, na.rm = TRUE)
models <- list("PCPs" = pcp_pc, "MD PCPs" = md_pcp_pc, "DO PCPs" = do_pcp_pc)
rows <- tribble(
  ~term,                 ~All,     ~MD,    ~DO, 
  'Mean PCPs per 10k', avg_pcp, avg_md, avg_do)
get_table(models, gof_map, rows, "PCPs per 10k", "Note: SE clustered at state", "agg-supply/state/5-pre-post/pcp_per_10k")

# County counts per 10k 
avg_pcp <- mean(df_c$pcp_per_10k, na.rm = TRUE)
avg_md <- mean(df_c$md_pcp_per_10k, na.rm = TRUE)
avg_do <- mean(df_c$do_pcp_per_10k, na.rm = TRUE)
models <- list("PCPs" = c_pcp_pc, "MD PCPs" = c_md_pcp_pc, "DO PCPs" = c_do_pcp_pc)
rows <- tribble(
  ~term,                 ~All,     ~MD,    ~DO, 
  'Mean PCPs per 10k', avg_pcp, avg_md, avg_do)
get_table(models, gof_map, rows, "PCPs per 10k", "Note: SE clustered at state", "agg-supply/county/5-pre-post/pcp_per_10k")

# Stacked DiD ------------------------------------------------------------------

pre_periods <- 5
post_periods <- 5

outcomes <- c("pcp_per_10k", "md_pcp_per_10k", "do_pcp_per_10k")

# State counts per 10k 
state_res <- map(outcomes, get_stacked_att, df = stacked_state, 
                 pre_periods = pre_periods, post_periods = post_periods)
names(state_res) <- outcomes

# County counts per 10k
county_res <- map(outcomes, get_stacked_att, df = stacked_county, 
                  pre_periods = pre_periods, post_periods = post_periods)
names(county_res) <- outcomes

### Export as tables -----------------------------------------------------------

stars <- function(p) case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")

avg_pcp <- mean(stacked_state$pcp_per_10k, na.rm = TRUE)
avg_md <- mean(stacked_state$md_pcp_per_10k, na.rm = TRUE)
avg_do <- mean(stacked_state$do_pcp_per_10k, na.rm = TRUE)
mean_row <- c(round(avg_pcp, 3), round(avg_md, 3), round(avg_do, 3))
get_stacked_table(state_res, mean_row, "PCPs per 10k", "Note: SE clustered at state", "agg-supply/state/stacked/pcp_per_10k")

avg_pcp <- mean(stacked_county$pcp_per_10k, na.rm = TRUE)
avg_md <- mean(stacked_county$md_pcp_per_10k, na.rm = TRUE)
avg_do <- mean(stacked_county$do_pcp_per_10k, na.rm = TRUE)
mean_row <- c(round(avg_pcp, 3), round(avg_md, 3), round(avg_do, 3))
get_stacked_table(county_res, mean_row, "PCPs per 10k", "Note: SE clustered at state", "agg-supply/county/stacked/pcp_per_10k")

# Clean up ---------------------------------------------------------------------

rm(list = ls())

df <- read_csv('data/output/final_df_state.csv')
df_c <- read_csv('data/output/final_df_county.csv')
source('analysis/0-helpers.R')
