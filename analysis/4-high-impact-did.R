# About ------------------------------------------------------------------------

# "High impact" counties DiD
# Author:         Shirley Cai 
# Date created:   02/27/2026 
# Last edited:    06/22/2026 

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

aprn_share <- df_c %>% filter(year == 2010) 

# Create APRN share = APRN / (APRN + PCP)
aprn_share <- aprn_share %>% 
  mutate(
    aprn_share = tot_aprn / (tot_aprn + tot_pcp), 
    share_quartile = ntile(aprn_share, 4)
  )
aprn_share <- aprn_share %>% 
  group_by(state_fips) %>% 
  mutate(
    share_quartile_state = ntile(aprn_share, 4)
  ) %>% 
  ungroup()
aprn_share <- aprn_share %>% 
  select(state_fips, county_fips, aprn_share, share_quartile, share_quartile_state)

df_ss <- df_c %>% 
  filter(year >= 2010, year <= 2022) %>% 
  left_join(aprn_share, by = c('state_fips', 'county_fips'))

df_ss %>% 
  mutate(rurality = as.factor(ifelse(D_rural == 1, "nonmetro", "metro"))) %>%
  ggplot(aes(x = aprn_share, fill = rurality)) + 
  geom_density(alpha = 0.6, position = 'identity', bw = 0.03)
ggsave(
  "results/descr/aprn_share_rural.png",  
  scale = 1.5, 
  width = 1200, height = 900, units = "px"
)

## 0.1. Graph location of counties with highest and lowest quantile -----------------
county_sf <- counties(cb = TRUE) %>%
  shift_geometry() %>% 
  clean_names() %>% 
  mutate(
    statefp = as.numeric(statefp), 
    countyfp = as.numeric(countyfp)
  )

spatial_data <- aprn_share %>% 
  left_join(county_sf, by = c('state_fips' = 'statefp', 'county_fips' = 'countyfp'))

map_share <- spatial_data %>% 
  ggplot() + 
  geom_sf(aes(fill = aprn_share, geometry = geometry),
          color = "#ffffff", size = 0.025) +
  labs(fill = "APRN share") + 
  coord_sf(datum = NA)
ggsave(
  "results/descr/maps/aprn_share_2010.png",  
  plot =  map_share, 
  scale = 1.5, 
  width = 1200, height = 900, units = "px"
)

map_quartile <- spatial_data %>%
  ggplot() + 
  geom_sf(aes(fill = as.factor(share_quartile), geometry = geometry),
          color = "#ffffff", size = 0.025) + 
  labs(fill = "APRN share quartile") + 
  coord_sf(datum = NA)
ggsave(
  "results/descr/maps/aprn_quartile_2010.png",  
  plot =  map_quartile, 
  scale = 1.5, 
  width = 1200, height = 900, units = "px"
)

map_state_quartile <- spatial_data %>%
  ggplot() + 
  geom_sf(aes(fill = as.factor(share_quartile_state), geometry = geometry),
          color = "#ffffff", size = 0.025) + 
  labs(fill = "APRN share quartile among state") + 
  coord_sf(datum = NA)
ggsave(
  "results/descr/maps/aprn_quartile_state_2010.png",  
  plot =  map_state_quartile, 
  scale = 1.5, 
  width = 1200, height = 900, units = "px"
)

map_q1 <- spatial_data %>%
  ggplot() + 
  geom_sf(aes(fill = as.factor(share_quartile == 1), geometry = geometry),
          color = "#ffffff", size = 0.025) + 
  labs(fill = "APRN share Q1") + 
  coord_sf(datum = NA)
ggsave(
  "results/descr/maps/aprn_q1_2010.png",  
  plot =  map_q1, 
  scale = 1.5, 
  width = 1200, height = 900, units = "px"
)

map_q4 <- spatial_data %>%
  ggplot() + 
  geom_sf(aes(fill = as.factor(share_quartile == 4), geometry = geometry),
          color = "#ffffff", size = 0.025) + 
  labs(fill = "APRN share Q4") + 
  coord_sf(datum = NA)
ggsave(
  "results/descr/maps/aprn_q4_2010.png",  
  plot =  map_q4, 
  scale = 1.5, 
  width = 1200, height = 900, units = "px"
)

map_state_q1 <- spatial_data %>%
  ggplot() + 
  geom_sf(aes(fill = as.factor(share_quartile_state == 1), geometry = geometry),
          color = "#ffffff", size = 0.025) + 
  labs(fill = "APRN share Q1 within state") + 
  coord_sf(datum = NA)
ggsave(
  "results/descr/maps/aprn_q1_state_2010.png",  
  plot =  map_state_q1, 
  scale = 1.5, 
  width = 1200, height = 900, units = "px"
)

map_state_q4 <- spatial_data %>%
  ggplot() + 
  geom_sf(aes(fill = as.factor(share_quartile_state == 4), geometry = geometry),
          color = "#ffffff", size = 0.025) + 
  labs(fill = "APRN share Q4 within state") + 
  coord_sf(datum = NA)
ggsave(
  "results/descr/maps/aprn_q4_state_2010.png",  
  plot =  map_state_q4, 
  scale = 1.5, 
  width = 1200, height = 900, units = "px"
)

## 0.2 Full sample -------------------------------------------------------------

pre_periods <- 5
post_periods <- 5
balance_e <- NULL

get_cs("pcp_per_10k", "PCPs per 10k people", df_ss, pre_periods, post_periods, balance_e, 
       file_root = "results/high-low-did/event-study/", 
       file_ext = "_all")

get_cs("new_pcp_per_10k", "New PCPs per 10k people", df_ss, pre_periods, post_periods, balance_e, 
       file_root = "results/high-low-did/event-study/", 
       file_ext = "_all")

# Staggered DiD
pcp_pc <- get_cs_did("pcp_per_10k", "PCPs per 10k people", df_ss, pre_periods, post_periods, balance_e)
new_pcp_pc <- get_cs_did("new_pcp_per_10k", "New PCPs per 10k people", df_ss, pre_periods, post_periods, balance_e)

# 1. DiD within highest quartile APRN share ------------------------------------

df_q <- df_ss %>% filter(share_quartile == 4)

## Event study -----------------------------------------------------------------

# Staggered
get_cs("pcp_per_10k", "PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, 
       file_root = "results/high-low-did/event-study/", 
       file_ext = "_high")
get_cs("new_pcp_per_10k", "New PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, 
       file_root = "results/high-low-did/event-study/", 
       file_ext = "_high")

# Stacked
# TODO

## DiD -------------------------------------------------------------------------

# Staggered
pcp_pc_high <- get_cs_did("pcp_per_10k", "PCPs per 10k people", df_q, pre_periods, post_periods, balance_e)
new_pcp_pc_high <- get_cs_did("new_pcp_per_10k", "New PCPs per 10k people", df_q, pre_periods, post_periods, balance_e)

# Stacked
# TODO

# 2. DiD within lowest quartile APRN share -------------------------------------

df_q <- df_ss %>% filter(share_quartile == 1)

## Event study -----------------------------------------------------------------

# Staggered
get_cs("pcp_per_10k", "PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, 
       file_root = "results/high-low-did/event-study/", 
       file_ext = "_low")
get_cs("new_pcp_per_10k", "New PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, 
       file_root = "results/high-low-did/event-study/", 
       file_ext = "_low")

# Stacked 
# TODO

## DiD -------------------------------------------------------------------------

# Staggered
pcp_pc_low <- get_cs_did("pcp_per_10k", "PCPs per 10k people", df_q, pre_periods, post_periods, balance_e)
new_pcp_pc_low <- get_cs_did("new_pcp_per_10k", "New PCPs per 10k people", df_q, pre_periods, post_periods, balance_e)

# Stacked 
# TODO

# 3. DiD within highest quartile in same state ---------------------------------

df_q <- df_ss %>% filter(share_quartile_state == 4)

## Event study -----------------------------------------------------------------

# Staggered
get_cs("pcp_per_10k", "PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, 
       file_root = "results/high-low-did/event-study/", 
       file_ext = "_high_state")
get_cs("new_pcp_per_10k", "New PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, 
       file_root = "results/high-low-did/event-study/", 
       file_ext = "_high_state")

# Stacked 
# TODO

## DiD -------------------------------------------------------------------------

# Staggered
pcp_pc_high_state <- get_cs_did("pcp_per_10k", "PCPs per 10k people", df_q, pre_periods, post_periods, balance_e)
new_pcp_pc_high_state <- get_cs_did("new_pcp_per_10k", "New PCPs per 10k people", df_q, pre_periods, post_periods, balance_e)

# Stacked 
# TODO

# 4. DiD within lowest quartile in same state ----------------------------------

df_q <- df_ss %>% filter(share_quartile_state == 1)

## Event study -----------------------------------------------------------------

# Staggered
get_cs("pcp_per_10k", "PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, 
       file_root = "results/high-low-did/event-study/", 
       file_ext = "_low_state")
get_cs("new_pcp_per_10k", "New PCPs per 10k people", df_q, pre_periods, post_periods, balance_e, 
       file_root = "results/high-low-did/event-study/", 
       file_ext = "_low_state")

# Stacked 
# TODO

## DiD -------------------------------------------------------------------------

# Staggered 
pcp_pc_low_state <- get_cs_did("pcp_per_10k", "PCPs per 10k people", df_q, pre_periods, post_periods, balance_e)
new_pcp_pc_low_state <- get_cs_did("new_pcp_per_10k", "New PCPs per 10k people", df_q, pre_periods, post_periods, balance_e)

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