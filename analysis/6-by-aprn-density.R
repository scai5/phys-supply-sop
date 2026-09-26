# About ------------------------------------------------------------------------

# "High impact" counties DiD
# Author:         Shirley Cai 
# Date created:   02/27/2026 
# Last edited:    07/27/2026 

# Idea: Leverage within state variation using pre-policy APRN presence @ county
# Pre-policy APRN presence = APRNs / (APRN + PCP) in 2010

# Specifications 
#   1. DiD within counties with the highest quartile of APRN presence 
#   2. DiD within counties with the lowest quartile of APRN presence 
#   3. DiD within counties with the highest quartile within state 
#   4. DiD within counties with the lowest quartile within state
#   5. Triple difference

# Remaining endogeneity issue: Counties with high and low APRN presence are going 
# to have differences in productivity, etc., so we cannot compare them directly
#       -> SSIV, instrumenting for # APRNs billing 

# 0. Data preparation ----------------------------------------------------------

df_c <- df_c %>%
  mutate(
    full_fips = paste0(formatC(state_fips, width = 2, flag = "0"), 
                       formatC(county_fips, width = 3, flag = "0")),
    full_fips = as.numeric(full_fips)
  )

df_ss <- df_c %>% filter(year >= 2010, year <= 2022)

## 0.1 Full sample -------------------------------------------------------------

pre_periods <- 5
post_periods <- 5
balance_e <- NULL

get_cs("pcp_per_10k", "PCPs per 10k people", df_ss, pre_periods, post_periods, balance_e, 
       file_root = "results/high-low-did/event-study/", 
       file_ext = "_all", idname = "full_fips")

get_cs("new_pcp_per_10k", "New PCPs per 10k people", df_ss, pre_periods, post_periods, balance_e, 
       file_root = "results/high-low-did/event-study/", 
       file_ext = "_all", idname = "full_fips")

# Staggered DiD
pcp_pc <- get_cs_did("pcp_per_10k", "PCPs per 10k people", df_ss, pre_periods, post_periods, balance_e, idname = "full_fips")
new_pcp_pc <- get_cs_did("new_pcp_per_10k", "New PCPs per 10k people", df_ss, pre_periods, post_periods, balance_e, idname = "full_fips")

# 1. DiD within highest quartile APRN share ------------------------------------

df_q <- df_ss %>% filter(share_quartile == 4)

## Event study -----------------------------------------------------------------

# Staggered
get_cs("pcp_per_10k", "PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, 
       file_root = "results/high-low-did/event-study/", 
       file_ext = "_high", idname = "full_fips")
get_cs("new_pcp_per_10k", "New PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, 
       file_root = "results/high-low-did/event-study/", 
       file_ext = "_high", idname = "full_fips")

# Stacked
# TODO

## DiD -------------------------------------------------------------------------

# Staggered
pcp_pc_high <- get_cs_did("pcp_per_10k", "PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, idname = "full_fips")
new_pcp_pc_high <- get_cs_did("new_pcp_per_10k", "New PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, idname = "full_fips")

# Stacked
# TODO

# 2. DiD within lowest quartile APRN share -------------------------------------

df_q <- df_ss %>% filter(share_quartile == 1)

## Event study -----------------------------------------------------------------

# Staggered
get_cs("pcp_per_10k", "PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, 
       file_root = "results/high-low-did/event-study/", 
       file_ext = "_low", idname = "full_fips")
get_cs("new_pcp_per_10k", "New PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, 
       file_root = "results/high-low-did/event-study/", 
       file_ext = "_low", idname = "full_fips")

# Stacked 
# TODO

## DiD -------------------------------------------------------------------------

# Staggered
pcp_pc_low <- get_cs_did("pcp_per_10k", "PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, idname = "full_fips")
new_pcp_pc_low <- get_cs_did("new_pcp_per_10k", "New PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, idname = "full_fips")

# Stacked 
# TODO

# 3. DiD within highest quartile in same state ---------------------------------

df_q <- df_ss %>% filter(share_quartile_state == 4)

## Event study -----------------------------------------------------------------

# Staggered
get_cs("pcp_per_10k", "PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, 
       file_root = "results/high-low-did/event-study/", 
       file_ext = "_high_state", idname = "full_fips")
get_cs("new_pcp_per_10k", "New PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, 
       file_root = "results/high-low-did/event-study/", 
       file_ext = "_high_state", idname = "full_fips")

# Stacked 
# TODO

## DiD -------------------------------------------------------------------------

# Staggered
pcp_pc_high_state <- get_cs_did("pcp_per_10k", "PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, idname = "full_fips")
new_pcp_pc_high_state <- get_cs_did("new_pcp_per_10k", "New PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, idname = "full_fips")

# Stacked 
# TODO

# 4. DiD within lowest quartile in same state ----------------------------------

df_q <- df_ss %>% filter(share_quartile_state == 1)

## Event study -----------------------------------------------------------------

# Staggered
get_cs("pcp_per_10k", "PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, 
       file_root = "results/high-low-did/event-study/", 
       file_ext = "_low_state", idname = "full_fips")
get_cs("new_pcp_per_10k", "New PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, 
       file_root = "results/high-low-did/event-study/", 
       file_ext = "_low_state", idname = "full_fips")

# Stacked 
# TODO

## DiD -------------------------------------------------------------------------

# Staggered 
pcp_pc_low_state <- get_cs_did("pcp_per_10k", "PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, idname = "full_fips")
new_pcp_pc_low_state <- get_cs_did("new_pcp_per_10k", "New PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, idname = "full_fips")

# Stacked 
# TODO

# 4. Interacted triple diff ----------------------------------------------------

# TODO

# Export tables ----------------------------------------------------------------

gof_map <- tribble(
  ~raw,          ~clean,          ~fmt,     ~omit,
  "r.squared",   "R2",               3,     FALSE
)

## Q1 (lowest competitive pressure) --------------------------------------------

avg_pcp <- mean(df_ss$pcp_per_10k[df_ss$share_quartile == 1], na.rm = TRUE)
avg_new_pcp <- mean(df_ss$new_pcp_per_10k[df_ss$share_quartile == 1], na.rm = TRUE)
avg_pcp_state <- mean(df_ss$pcp_per_10k[df_ss$share_quartile_state == 1], na.rm = TRUE)
avg_new_pcp_state <- mean(df_ss$new_pcp_per_10k[df_ss$share_quartile_state == 1], na.rm = TRUE)
rows <- tribble(
  ~term,               ~pcp,        ~new,    ~pcp_state,        ~new_state, 
  'Mean dep. var.', avg_pcp, avg_new_pcp, avg_pcp_state, avg_new_pcp_state)

models <- list("PCPs" = pcp_pc_low, 
               "New PCPs" = new_pcp_pc_low, 
               "PCPs" = pcp_pc_low_state, 
               "New PCPs" = new_pcp_pc_low_state)
converted_models <- convert_models(models)

res <- modelsummary(converted_models, 
                    stars = c('*' = .1, '**' = .05, '***' = 0.01),
                    gof_map = gof_map,
                    add_rows = rows,
                    output = "gt")
res <- res %>% 
  tab_header(title = md("Counties with low APRN share")) %>% 
  tab_spanner(label = "Q1 nationally", columns = c(2:3)) %>%
  tab_spanner(label = "Q1 in each state", columns = c(4:5)) %>% 
  tab_source_note(source_note = "SE clustered at the state level.") %>% 
  opt_horizontal_padding(scale = 3)

gtsave(res, "results/high-low-did/low-impact-counties.html")
gtsave(res, "results/high-low-did/low-impact-counties.tex")

## Q4 (highest competitive pressure) -------------------------------------------

avg_pcp <- mean(df_ss$pcp_per_10k[df_ss$share_quartile == 4], na.rm = TRUE)
avg_new_pcp <- mean(df_ss$new_pcp_per_10k[df_ss$share_quartile == 4], na.rm = TRUE)
avg_pcp_state <- mean(df_ss$pcp_per_10k[df_ss$share_quartile_state == 4], na.rm = TRUE)
avg_new_pcp_state <- mean(df_ss$new_pcp_per_10k[df_ss$share_quartile_state == 4], na.rm = TRUE)
rows <- tribble(
  ~term,               ~pcp,        ~new,    ~pcp_state,        ~new_state, 
  'Mean dep. var.', avg_pcp, avg_new_pcp, avg_pcp_state, avg_new_pcp_state)

models <- list("PCPs" = pcp_pc_high, 
               "New PCPs" = new_pcp_pc_high, 
               "PCPs" = pcp_pc_high_state, 
               "New PCPs" = new_pcp_pc_high_state)
converted_models <- convert_models(models)

res <- modelsummary(converted_models, 
                    stars = c('*' = .1, '**' = .05, '***' = 0.01),
                    gof_map = gof_map,
                    add_rows = rows,
                    output = "gt")
res <- res %>% 
  tab_header(title = md("Counties with high APRN share")) %>% 
  tab_spanner(label = "Q4 nationally", columns = c(2:3)) %>%
  tab_spanner(label = "Q4 in each state", columns = c(4:5)) %>% 
  tab_source_note(source_note = "SE clustered at the state level.") %>% 
  opt_horizontal_padding(scale = 3)

gtsave(res, "results/high-low-did/high-impact-counties.html")
gtsave(res, "results/high-low-did/high-impact-counties.tex")

## High vs low (national quartiles) --------------------------------------------

avg_pcp <- mean(df_ss$pcp_per_10k, na.rm = TRUE)
avg_new <- mean(df_ss$new_pcp_per_10k, na.rm = TRUE)
avg_pcp_high <- mean(df_ss$pcp_per_10k[df_ss$share_quartile == 4], na.rm = TRUE)
avg_new_high <- mean(df_ss$new_pcp_per_10k[df_ss$share_quartile == 4], na.rm = TRUE)
avg_pcp_low <- mean(df_ss$pcp_per_10k[df_ss$share_quartile == 1], na.rm = TRUE)
avg_new_low <- mean(df_ss$new_pcp_per_10k[df_ss$share_quartile == 1], na.rm = TRUE)
rows <- tribble(
  ~term,            ~full_pcp,     ~q1_pcp,      ~q4_pcp, ~full_new,     ~q1_new,      ~q4_new, 
  'Mean dep. var.',   avg_pcp, avg_pcp_low, avg_pcp_high,   avg_new, avg_new_low, avg_new_high)

models <- list("Full" = pcp_pc, 
               "Q1" = pcp_pc_low, 
               "Q4" = pcp_pc_high, 
               "Full" = new_pcp_pc,
               "Q1" = new_pcp_pc_low,
               "Q4" = new_pcp_pc_high)
converted_models <- convert_models(models)

res <- modelsummary(converted_models, 
                    stars = c('*' = .1, '**' = .05, '***' = 0.01),
                    gof_map = gof_map,
                    add_rows = rows,
                    output = "gt")
res <- res %>% 
  tab_header(title = md("Quartiles defined nationally")) %>% 
  tab_spanner(label = "PCPs per 10k", columns = c(2:4)) %>%
  tab_spanner(label = "New PCPs per 10k", columns = c(5:7)) %>% 
  tab_source_note(source_note = "SE clustered at the state level.") %>% 
  opt_horizontal_padding(scale = 3)

gtsave(res, "results/high-low-did/national-quartiles.html")
gtsave(res, "results/high-low-did/national-quartiles.tex")

# High vs low (quartiles by state) ---------------------------------------------

avg_pcp <- mean(df_ss$pcp_per_10k, na.rm = TRUE)
avg_new <- mean(df_ss$new_pcp_per_10k, na.rm = TRUE)
avg_pcp_high <- mean(df_ss$pcp_per_10k[df_ss$share_quartile_state == 4], na.rm = TRUE)
avg_new_high <- mean(df_ss$new_pcp_per_10k[df_ss$share_quartile_state == 4], na.rm = TRUE)
avg_pcp_low <- mean(df_ss$pcp_per_10k[df_ss$share_quartile_state == 1], na.rm = TRUE)
avg_new_low <- mean(df_ss$new_pcp_per_10k[df_ss$share_quartile_state == 1], na.rm = TRUE)
rows <- tribble(
  ~term,            ~full_pcp,     ~q1_pcp,      ~q4_pcp, ~full_new,     ~q1_new,      ~q4_new, 
  'Mean dep. var.',   avg_pcp, avg_pcp_low, avg_pcp_high,   avg_new, avg_new_low, avg_new_high)

models <- list("Full" = pcp_pc, 
               "Q1" = pcp_pc_low_state, 
               "Q4" = pcp_pc_high_state, 
               "Full" = new_pcp_pc,
               "Q1" = new_pcp_pc_low_state,
               "Q4" = new_pcp_pc_high_state)
converted_models <- convert_models(models)

res <- modelsummary(converted_models, 
                    stars = c('*' = .1, '**' = .05, '***' = 0.01),
                    gof_map = gof_map,
                    add_rows = rows,
                    output = "gt")
res <- res %>% 
  tab_header(title = md("Quartiles defined per state")) %>% 
  tab_spanner(label = "PCPs per 10k", columns = c(2:4)) %>%
  tab_spanner(label = "New PCPs per 10k", columns = c(5:7)) %>% 
  tab_source_note(source_note = "SE clustered at the state level.") %>% 
  opt_horizontal_padding(scale = 3)

gtsave(res, "results/high-low-did/state-quartiles.html")
gtsave(res, "results/high-low-did/state-quartiles.tex")