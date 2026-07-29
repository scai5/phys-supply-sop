# About ------------------------------------------------------------------------

# New PCPs / entrants
# Author:         Shirley Cai 
# Date created:   02/27/2026 
# Last edited:    06/17/2026

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
get_eventplot("new_pcp_per_10k", "New PCPs per 10k people", df_trim, "new-pcps/state/twfe/")
get_eventplot("new_md_pcp_per_10k", "New MD PCPs per 10k people", df_trim, "new-pcps/state/twfe/")
get_eventplot("new_do_pcp_per_10k", "New DO PCPs per 10k people", df_trim, "new-pcps/state/twfe/")

# County counts per 10k
get_eventplot("new_pcp_per_10k", "New PCPs per 10k people", df_c_trim, "new-pcps/county/twfe/")
get_eventplot("new_md_pcp_per_10k", "New MD PCPs per 10k people", df_c_trim, "new-pcps/county/twfe/")
get_eventplot("new_do_pcp_per_10k", "New DO PCPs per 10k people", df_c_trim, "new-pcps/county/twfe/")

## Sun and Abraham -------------------------------------------------------------

# State counts per 10k
get_sunab("new_pcp_per_10k", "New PCPs per 10k people", df_trim, "year", "new-pcps/state/sunab/")
get_sunab("new_md_pcp_per_10k", "New MD PCPs per 10k people", df_trim, "year", "new-pcps/state/sunab/")
get_sunab("new_do_pcp_per_10k", "New DO PCPs per 10k people", df_trim, "year", "new-pcps/state/sunab/")

# County counts per 10k
get_sunab("new_pcp_per_10k", "New PCPs per 10k people", df_c_trim, "year", "new-pcps/county/sunab/")
get_sunab("new_md_pcp_per_10k", "New MD PCPs per 10k people", df_c_trim, "year", "new-pcps/county/sunab/")
get_sunab("new_do_pcp_per_10k", "New DO PCPs per 10k people", df_c_trim, "year", "new-pcps/county/sunab/")

## Callaway and Sant'Anna ------------------------------------------------------

df <- df %>% mutate(treat_year = ifelse(treat == 0, 0, effective_year))
df_c <- df_c %>% mutate(treat_year = ifelse(treat == 0, 0, effective_year))

pre_periods <- 5
post_periods <- 5
balance_e <- NULL

# State counts per capita
get_cs("new_pcp_per_10k", "New PCPs per 10k people", df, pre_periods, post_periods, balance_e, file_pre = "new-pcps/state/cs/")
get_cs("new_md_pcp_per_10k", "New MD PCPs per 10k people", df, pre_periods, post_periods, balance_e, file_pre = "new-pcps/state/cs/")
get_cs("new_do_pcp_per_10k", "New DO PCPs per 10k people", df,pre_periods, post_periods, balance_e, file_pre = "new-pcps/state/cs/")

# County counts per capita
get_cs("new_pcp_per_10k", "New PCPs per 10k people", df_c, pre_periods, post_periods, balance_e, file_pre = "new-pcps/county/cs/")
get_cs("new_md_pcp_per_10k", "New MD PCPs per 10k people", df_c, pre_periods, post_periods, balance_e, file_pre = "new-pcps/county/cs/")
get_cs("new_do_pcp_per_10k", "New DO PCPs per 10k people", df_c,pre_periods, post_periods, balance_e, file_pre = "new-pcps/county/cs/")

get_cs("aprn_medicare_per_10k", "APRNs billing Medicare", df_c,pre_periods, post_periods, balance_e, file_pre = "new-pcps/county/cs/")

## Stacked a la Cengiz et al. (2019) -------------------------------------------

df <- df %>% mutate(treat_year = ifelse(treat == 0, Inf, effective_year))
df_c <- df_c %>% mutate(treat_year = ifelse(treat == 0, Inf, effective_year))

pre_periods <- 5
post_periods <- 5

stacked_state <- get_stacked_df(df, pre_periods, post_periods)
stacked_county <- get_stacked_df(df_c, pre_periods, post_periods)

# State counts per 10k 
get_stacked("new_pcp_per_10k", "New PCPs per 10k people", stacked_state, "new-pcps/state/stacked/")
get_stacked("new_md_pcp_per_10k", "New MD PCPs per 10k people", stacked_state, "new-pcps/state/stacked/")
get_stacked("new_do_pcp_per_10k", "New DO PCPs per 10k people", stacked_state, "new-pcps/state/stacked/")

# County counts per 10k
get_stacked("new_pcp_per_10k", "New PCPs per 10k people", stacked_county, "new-pcps/county/stacked/")
get_stacked("new_md_pcp_per_10k", "New MD PCPs per 10k people", stacked_county, "new-pcps/county/stacked/")
get_stacked("new_do_pcp_per_10k", "New DO PCPs per 10k people", stacked_county, "new-pcps/county/stacked/")

# Staggered DiD ----------------------------------------------------------------

## Using all data --------------------------------------------------------------

pre_periods <- Inf
post_periods <- Inf
balance_e <- NULL

# State counts per 10k
pcp_pc <- get_cs_did("new_pcp_per_10k", "New PCPs per 10k people", df, pre_periods, post_periods, balance_e)
md_pcp_pc <- get_cs_did("new_md_pcp_per_10k", "New MD PCPs per 10k people", df, pre_periods, post_periods, balance_e)
do_pcp_pc <- get_cs_did("new_do_pcp_per_10k", "New DO PCPs per 10k people", df, pre_periods, post_periods, balance_e)

# County counts per 10k
c_pcp_pc <- get_cs_did("new_pcp_per_10k", "New PCPs per 10k people", df_c, pre_periods, post_periods, balance_e)
c_md_pcp_pc <- get_cs_did("new_md_pcp_per_10k", "New MD PCPs per 10k people", df_c, pre_periods, post_periods, balance_e)
c_do_pcp_pc <- get_cs_did("new_do_pcp_per_10k", "New DO PCPs per 10k people", df_c, pre_periods, post_periods, balance_e)

### Export as tables -----------------------------------------------------------

gof_map <- tribble(
  ~raw,          ~clean,          ~fmt,     ~omit,
  "r.squared",   "R2",               3,     FALSE
)

avg_pcp <- mean(df$new_pcp_per_10k, na.rm = TRUE)
avg_md <- mean(df$new_md_pcp_per_10k, na.rm = TRUE)
avg_do <- mean(df$new_do_pcp_per_10k, na.rm = TRUE)
models <- list("New PCPs" = pcp_pc, "New MD PCPs" = md_pcp_pc, "New DO PCPs" = do_pcp_pc)
rows <- tribble(
  ~term,                 ~All,     ~MD,    ~DO, 
  'Mean new PCPs per 10k', avg_pcp, avg_md, avg_do)
get_table(models, gof_map, rows, "New PCPs", "Note: SE clustered at state", "new-pcps/state/all-obs/new_pcp_per_10k")

avg_pcp <- mean(df_c$new_pcp_per_10k, na.rm = TRUE)
avg_md <- mean(df_c$new_md_pcp_per_10k, na.rm = TRUE)
avg_do <- mean(df_c$new_do_pcp_per_10k, na.rm = TRUE)
models <- list("New PCPs" = c_pcp_pc, "New MD PCPs" = c_md_pcp_pc, "New DO PCPs" = c_do_pcp_pc)
rows <- tribble(
  ~term,                 ~All,     ~MD,    ~DO, 
  'Mean new PCPs per 10k', avg_pcp, avg_md, avg_do)
get_table(models, gof_map, rows, "New PCPs", "Note: SE clustered at state", "new-pcps/county/all-obs/new_pcp_per_10k")

## 5 years pre and post --------------------------------------------------------

pre_periods <- 5
post_periods <- 5
balance_e <- NULL

# State counts per 10k 
pcp_pc <- get_cs_did("new_pcp_per_10k", "New PCPs per 10k people", df, pre_periods, post_periods, balance_e)
md_pcp_pc <- get_cs_did("new_md_pcp_per_10k", "New MD PCPs per 10k people", df, pre_periods, post_periods, balance_e)
do_pcp_pc <- get_cs_did("new_do_pcp_per_10k", "New DO PCPs per 10k people", df, pre_periods, post_periods, balance_e)

# County counts per 10k
c_pcp_pc <- get_cs_did("new_pcp_per_10k", "New PCPs per 10k people", df_c, pre_periods, post_periods, balance_e)
c_md_pcp_pc <- get_cs_did("new_md_pcp_per_10k", "New MD PCPs per 10k people", df_c, pre_periods, post_periods, balance_e)
c_do_pcp_pc <- get_cs_did("new_do_pcp_per_10k", "New DO PCPs per 10k people", df_c, pre_periods, post_periods, balance_e)

### Export as tables -----------------------------------------------------------

gof_map <- tribble(
  ~raw,          ~clean,          ~fmt,     ~omit,
  "r.squared",   "R2",               3,     FALSE
)

avg_pcp <- mean(df$new_pcp_per_10k, na.rm = TRUE)
avg_md <- mean(df$new_md_pcp_per_10k, na.rm = TRUE)
avg_do <- mean(df$new_do_pcp_per_10k, na.rm = TRUE)
models <- list("New PCPs" = pcp_pc, "New MD PCPs" = md_pcp_pc, "New DO PCPs" = do_pcp_pc)
rows <- tribble(
  ~term,                 ~All,     ~MD,    ~DO, 
  'Mean new PCPs per 10k', avg_pcp, avg_md, avg_do)
get_table(models, gof_map, rows, "New PCPs", "Note: SE clustered at state", "new-pcps/state/5-pre-post/new_pcp_per_10k")

avg_pcp <- mean(df_c$new_pcp_per_10k, na.rm = TRUE)
avg_md <- mean(df_c$new_md_pcp_per_10k, na.rm = TRUE)
avg_do <- mean(df_c$new_do_pcp_per_10k, na.rm = TRUE)
models <- list("New PCPs" = c_pcp_pc, "New MD PCPs" = c_md_pcp_pc, "New DO PCPs" = c_do_pcp_pc)
rows <- tribble(
  ~term,                 ~All,     ~MD,    ~DO, 
  'Mean new PCPs per 10k', avg_pcp, avg_md, avg_do)
get_table(models, gof_map, rows, "New PCPs", "Note: SE clustered at state", "new-pcps/county/5-pre-post/new_pcp_per_10k")

# Stacked DiD ------------------------------------------------------------------

pre_periods <- 5
post_periods <- 5

outcomes <- c("new_pcp_per_10k", "new_md_pcp_per_10k", "new_do_pcp_per_10k")

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

avg_pcp <- mean(stacked_state$new_pcp_per_10k, na.rm = TRUE)
avg_md <- mean(stacked_state$new_md_pcp_per_10k, na.rm = TRUE)
avg_do <- mean(stacked_state$new_do_pcp_per_10k, na.rm = TRUE)
mean_row <- c(round(avg_pcp, 3), round(avg_md, 3), round(avg_do, 3))
get_stacked_table(state_res, mean_row, "New PCPs", "Note: SE clustered at state", "new-pcps/state/stacked/new_pcp_per_10k")

avg_pcp <- mean(stacked_county$new_pcp_per_10k, na.rm = TRUE)
avg_md <- mean(stacked_county$new_md_pcp_per_10k, na.rm = TRUE)
avg_do <- mean(stacked_county$new_do_pcp_per_10k, na.rm = TRUE)
mean_row <- c(round(avg_pcp, 3), round(avg_md, 3), round(avg_do, 3))
get_stacked_table(county_res, mean_row, "New PCPs", "Note: SE clustered at state", "new-pcps/county/stacked/new_pcp_per_10k")

# Clean up ---------------------------------------------------------------------

rm(list = ls())

df <- read_csv('data/output/final_df_state.csv')
df_c <- read_csv('data/output/final_df_county.csv')
source('analysis/0-helpers.R')
