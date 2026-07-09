# About ------------------------------------------------------------------------

# "High impact" counties SSIV
# Author:         Shirley Cai 
# Date created:   12/18/2025 
# Last edited:    03/01/2026 

# Idea: Leverage within state variation using pre-policy APRN presence @ county

# Recentered shift-share design a la Borusyak and Hull
#   - Shift: Expansion of SOP 
#   - Share: APRNs / (APRNs + PCPs) in pre-period (2010) 
#   - Recentered by subtracting expected instrument

# Test: Billing APRNs ----------------------------------------------------------

pre_periods <- 5
post_periods <- 5
balance_e <- NULL

get_cs("aprn_medicare_per_10k", "Billing APRNs", df_ss, pre_periods, post_periods, balance_e, file_pre = "archive/")

# "Global" variables -----------------------------------------------------------

controls <- c("unemp_rate", "income_pc", 
              "prop_female", "prop_black", 
              "prop_aian", "prop_asian", 
              "prop_hisp", "prop_medicare")

# Construct uncentered shift-share instrument ----------------------------------

# Uses df_ss, which is created in 5-high-impact-did.R

# Create uncentered instrument = SOP * pre-policy share
df_ss <- df_ss %>% mutate(ssiv = FPA * aprn_share)

# Clean up 
df_ss <- df_ss %>% 
  select(
    county, state_fips, county_fips, year, 
    tot_pcp, pcp_per_10k, tot_md_pcp, md_pcp_per_10k, tot_do_pcp, do_pcp_per_10k, 
    new_pcp, new_pcp_per_10k, new_md_pcp, new_md_pcp_per_10k, new_do_pcp, new_do_pcp_per_10k, 
    tot_aprn, aprn_per_10k, aprn_medicare_per_10k, population, 
    aprn_share, ssiv, 
    FPA, treat_year,
    one_of(controls)
  )

# Recentering the shift-share instrument ---------------------------------------

## Randomly generate B permutations --------------------------------------------

# GOAL: Create sop_alloc_1 through sop_alloc_B random permutations

# Do not include always treated states
states <- sort(unique(df_ss$state_fips[df_ss$FPA == 0]))
yr <- sort(unique(df_ss$year))
cohorts <- c(2011, 2012, 2013, 2014, 2015, 2016, 2018, 2019, 2021, 2022, NA)
cohort_sizes <- c(3, 1, 1, 1, 4, 3, 2, 1, 2, 1, 19)
panel <- expand_grid(state = states, year = yr)

# Function to randomly draw allocations
draw_permutation <- function(states, cohorts, cohort_sizes) {
  perm <- sample(states)
  idx <- 1
  assignment <- vector("list", length(cohorts))
  names(assignment) <- ifelse(is.na(cohorts), "never_treated", as.character(cohorts))
  
  for (i in seq_along(cohorts)) {
    assignment[[i]] <- perm[idx:(idx + cohort_sizes[i] - 1)]
    idx <- idx + cohort_sizes[i]
  }
  return(assignment)
}

# For a given draw, return a state-year indicator column
make_alloc_column <- function(assignment, yr) {
  # Build a state -> first treated year lookup, NA if never treated
  cohort_years <- names(assignment)
  state_cohort <- map_dfr(cohort_years, function(c) {
    tibble(
      state  = assignment[[c]],
      cohort = ifelse(c == "never_treated", NA_real_, as.numeric(c))
    )
  })
  
  # For each state-year, indicator == 1 if year >= cohort (staggered adoption)
  expand_grid(state = states, year = yr) %>%
    left_join(state_cohort, by = "state") %>%
    mutate(alloc = as.integer(!is.na(cohort) & yr >= cohort)) %>%
    pull(alloc)
}

set.seed(1234)
B <- 10000

alloc_matrix <- map_dfc(1:B, function(b) {
  assignment <- draw_permutation(states, cohorts, cohort_sizes)
  tibble(!!paste0("sop_alloc_", b) := make_alloc_column(assignment, yr))
})

panel_alloc <- bind_cols(panel, alloc_matrix)

# Add allocation for states who are always treated 
states <- sort(unique(df_ss$state_fips[df_ss$FPA == 1 & df_ss$year == 2010]))
add_alloc <- expand_grid(state = states, year = yr) %>% 
  mutate(!!!setNames(rep(list(1), B), paste0("sop_alloc_", 1:B)))

panel_alloc <- bind_rows(panel_alloc, add_alloc) %>%
  mutate(ev_shift = rowMeans(across(starts_with("sop_alloc_"))))

ev_shift <- panel_alloc %>% select(state, year, ev_shift)

# Clean up 
rm(panel, alloc_matrix, assignment)
gc()

# Save to temp 
write_csv(panel_alloc, "data/temp/ssiv/sop_permutation.csv")
write_csv(ev_shift, "data/temp/ssiv/ev_shift.csv")

## Recentering the SSIV --------------------------------------------------------

# Expected instrument = average of permutation instruments 
df_ss <- df_ss %>% left_join(ev_shift, by = c('state_fips' = 'state', 'year'))
df_ss <- df_ss %>% 
  mutate(
    ev_ssiv = ev_shift * aprn_share, 
    ssiv_recentered = ssiv - ev_ssiv
  )

## Generate distribution of T stat for inference -------------------------------

# For each SOP allocation, compute the instrument and run the IV 
# Generate the distribution of test statistic T 
distr_pcp_recenter <- rep(NA, B)
distr_pcp_recenter_dr <- rep(NA, B)
distr_new_pcp_recenter <- rep(NA, B)
distr_new_pcp_recenter_dr <- rep(NA, B)

# Loop takes about 1 hour to run to completion
for(i in 1:B){
  # Create SSIV for this specific permutation 
  df_i <- df_ss %>% 
    left_join(panel_alloc %>% select(state, year, paste0('sop_alloc_', i)), 
              by = c('state_fips' = 'state', 'year')) %>% 
    rename(permute_sop = paste0('sop_alloc_', i))
  df_i <- df_i %>% 
    mutate(
      permute_ssiv = permute_sop * aprn_share, 
      permute_ssiv_recentered = permute_ssiv - ev_ssiv
    )
  
  # BH 2023: Test statistic T = sample cov(recentered instrument, implied resid)
  # Specifications: PCP recenter, PCP recenter (DR), 
  #                 new PCP recenter, new PCP recenter (DR)
  
  # Outcome: PCPs 
  out <- get_recenter("pcp_per_10k", controls, df_i, iv = "permute_ssiv_recentered")
  df_temp <- df_i %>% filter(if_all(all_of(c(controls, "pcp_per_10k", "aprn_share", "aprn_medicare_per_10k", "permute_ssiv_recentered")), ~!is.na(.)))
  resids <- get_ols("pcp_per_10k", controls, df_temp)$residuals
  distr_pcp_recenter[i] <- cov(df_temp$permute_ssiv_recentered, resids, use = "pairwise.complete.obs")
  
  out <- get_recenter_dr("pcp_per_10k", controls, df_i, iv = "permute_ssiv")
  df_temp <- df_i %>% filter(if_all(all_of(c(controls, "pcp_per_10k", "aprn_share", "aprn_medicare_per_10k", "permute_ssiv_recentered", "ev_ssiv")), ~!is.na(.)))
  resids <- get_ols("pcp_per_10k", c(controls, "ev_ssiv"), df_temp)$residuals
  distr_pcp_recenter_dr[i] <- cov(df_temp$permute_ssiv_recentered, resids, use = "pairwise.complete.obs")
  
  # Outcome: New PCPs
  out <- get_recenter("new_pcp_per_10k", controls, df_i, iv = "permute_ssiv_recentered")
  df_temp <- df_i %>% filter(if_all(all_of(c(controls, "new_pcp_per_10k", "aprn_share", "aprn_medicare_per_10k", "permute_ssiv_recentered")), ~!is.na(.)))
  resids <- get_ols("new_pcp_per_10k", controls, df_temp)$residuals
  distr_new_pcp_recenter[i] <- cov(df_temp$permute_ssiv_recentered, resids, use = "pairwise.complete.obs")
  
  out <- get_recenter_dr("new_pcp_per_10k", controls, df_i, iv = "permute_ssiv")
  df_temp <- df_i %>% filter(if_all(all_of(c(controls, "new_pcp_per_10k", "aprn_share", "aprn_medicare_per_10k", "permute_ssiv", "ev_ssiv")), ~!is.na(.)))
  resids <- get_ols("new_pcp_per_10k", c(controls, "ev_ssiv"), df_temp)$residuals
  distr_new_pcp_recenter_dr[i] <- cov(df_temp$permute_ssiv_recentered, resids, use = "pairwise.complete.obs")
}

# Save to temp
test_stat_distr <- data.frame(
  T_pcp_recenter = distr_pcp_recenter, 
  T_pcp_recenter_dr = distr_pcp_recenter_dr,
  T_new_pcp_recenter = distr_new_pcp_recenter,
  T_new_pcp_recenter_dr = distr_new_pcp_recenter_dr
)
write_csv(test_stat_distr, "data/temp/ssiv/test_stat_distr.csv")

# Running the IV ---------------------------------------------------------------

# For each outcome, run
#   1. OLS 
#   2. Uncentered IV 
#   3. Recentered IV
#   4. Recentered IV (doubly-robust)

# Aggregate PCPs 
pcp_ols <- get_ols("pcp_per_10k", c("aprn_medicare_per_10k", controls), df_ss)
pcp_uncenter <- get_uncenter("pcp_per_10k", controls, df_ss)
pcp_recenter <- get_recenter("pcp_per_10k", controls, df_ss)
pcp_recenter_dr <- get_recenter_dr("pcp_per_10k", controls, df_ss)

# New PCPs
new_pcp_ols <- get_ols("new_pcp_per_10k", c("aprn_medicare_per_10k", controls), df_ss)
new_pcp_uncenter <- get_uncenter("new_pcp_per_10k", controls, df_ss)
new_pcp_recenter <- get_recenter("new_pcp_per_10k", controls, df_ss)
new_pcp_recenter_dr <- get_recenter_dr("new_pcp_per_10k", controls, df_ss)

## Randomization inference for recentered specifications -----------------------

# Idea: Compare realized test statistic to the distribution of test statistics

# Outcome: PCPs
df_temp <- df_ss %>% filter(if_all(all_of(c(controls, "pcp_per_10k", "aprn_share", "aprn_medicare_per_10k", "ssiv_recentered")), ~!is.na(.)))
resids <- get_ols("pcp_per_10k", controls, df_temp)$residuals
t_pcp_recenter <- cov(df_temp$ssiv_recentered, resids)

df_temp <- df_ss %>% filter(if_all(all_of(c(controls, "pcp_per_10k", "aprn_share", "aprn_medicare_per_10k", "ssiv_recentered", "ev_ssiv")), ~!is.na(.)))
resids <- get_ols("pcp_per_10k", c(controls, "ev_ssiv"), df_temp)$residuals
t_pcp_recenter_dr <- cov(df_temp$ssiv_recentered, resids)

# Outcome: New PCPs
df_temp <- df_ss %>% filter(if_all(all_of(c(controls, "new_pcp_per_10k", "aprn_share", "aprn_medicare_per_10k", "ssiv_recentered")), ~!is.na(.)))
resids <- get_ols("new_pcp_per_10k", controls, df_temp)$residuals
t_new_pcp_recenter <- cov(df_temp$ssiv_recentered, resids)

df_temp <- df_ss %>% filter(if_all(all_of(c(controls, "new_pcp_per_10k", "aprn_share", "aprn_per_10k", "ssiv_recentered", "ev_ssiv")), ~!is.na(.)))
resids <- get_ols("new_pcp_per_10k", c(controls, "ev_ssiv"), df_temp)$residuals
t_new_pcp_recenter_dr <- cov(df_temp$ssiv_recentered, resids)

### RI p-values ----------------------------------------------------------------

# Rejection rate = number of simulated test statistics larger than the realized one, includes realized 

p_pcp_recenter <- (1 + sum(abs(distr_pcp_recenter) >= abs(t_pcp_recenter))) / (B + 1)
p_pcp_recenter_dr <- (1 + sum(abs(distr_pcp_recenter_dr) >= abs(t_pcp_recenter_dr))) / (B + 1)

p_new_pcp_recenter <- (1 + sum(abs(distr_new_pcp_recenter) <= abs(t_new_pcp_recenter))) / (B + 1)
p_new_pcp_recenter_dr <- (1 + sum(abs(distr_new_pcp_recenter_dr) <= abs(t_new_pcp_recenter_dr))) / (B + 1)

### RI 95% confidence interval -------------------------------------------------

# Idea: Use grid search to find values for which the prob = 0.05 to get 95% CI

# TODO

# Export as table --------------------------------------------------------------

controls <- c("unemp_rate", "income_pc", 
              "prop_female", "prop_black", 
              "prop_aian", "prop_asian", 
              "prop_hisp", "prop_medicare")

coef_map <- c(ssiv = "Shift-share IV", 
              ssiv_recentered = "Shift-share IV", 
              ev_ssiv = "Expected SSIV", 
              aprn_medicare_per_10k = "Billing APRNs per 10k", 
              fit_aprn_medicare_per_10k = "Billing APRNs per 10k",
              unemp_rate = "Unemployment rate", 
              income_pc = "Income per capita", 
              prop_female = "% female", 
              prop_black = "% Black", 
              prop_aian = "% Native Am.", 
              prop_asian = "% Asian", 
              prop_hisp = "% Hispanic", 
              prop_medicare = "% aged Medicare enrollment")

gof_map <- tribble(
  ~raw,                  ~clean,                ~fmt,
  "nobs",                "Observations",         0,
  "r.squared",           "R2",                   3   
)

## Outcomes: PCPs --------------------------------------------------------------

avg_pcp <- mean(df_ss$pcp_per_10k, na.rm = TRUE)
rows <- tribble(
  ~term,                ~OLS, ~Uncenter, ~Recenter,     ~DR, 
  'Mean physicians', avg_pcp,   avg_pcp,   avg_pcp, avg_pcp)

models <- list("OLS" = pcp_ols, 
               "Uncenter" = pcp_uncenter, 
               "Recenter" = pcp_recenter, 
               "Recenter DR" = pcp_recenter_dr)

out <- modelsummary(models, 
                    coef_map = coef_map,
                    gof_map = gof_map,
                    add_rows = rows, 
                    output = "gt")

out <- out %>% 
  tab_header(
    title = md("Outcome: PCPs per 10k")
  ) %>% 
  tab_spanner(
    label = "Shift-share IV", 
    columns = c("Uncenter", "Recenter", "Recenter DR")
  ) %>% 
  tab_source_note(
    source_note = "SE clustered at the state level are reported in parenthesis. 
                   95% RI confidence intervals based on SOP counterfactuals are reported in brackets."
  ) %>% 
  opt_horizontal_padding(scale = 3)

gtsave(out, paste0("results/ssiv/pcp_per_10k.html"))
gtsave(out, paste0("results/ssiv/pcp_per_10k.tex"))

### First-stage ----------------------------------------------------------------

avg_aprn <- mean(df_ss$aprn_medicare_per_10k, na.rm = TRUE)
rows <- tribble(
  ~term,        ~Uncenter, ~Recenter,     ~DR, 
  'Mean APRNs',  avg_aprn,  avg_aprn, avg_aprn)

models <- list("Uncenter" = pcp_uncenter$iv_first_stage$aprn_medicare_per_10k, 
               "Recenter" = pcp_recenter$iv_first_stage$aprn_medicare_per_10k, 
               "Recenter DR" = pcp_recenter_dr$iv_first_stage$aprn_medicare_per_10k)

out <- modelsummary(models, 
                    stars = c('*' = .1, '**' = .05, '***' = 0.01),
                    coef_map = coef_map, 
                    gof_map = gof_map,
                    add_rows = rows, 
                    output = "gt")

out <- out %>% 
  tab_header(
    title = md("First-stage: Billing APRNs per 10k")
  ) %>% 
  tab_source_note(
    source_note = "SE clustered at the state level."
  ) %>% 
  opt_horizontal_padding(scale = 3)

gtsave(out, paste0("results/ssiv/pcp_per_10k_first_stage.html"))
gtsave(out, paste0("results/ssiv/pcp_per_10k_first_stage.tex"))

## Outcomes: New PCPs ----------------------------------------------------------

avg_pcp <- mean(df_ss$new_pcp_per_10k, na.rm = TRUE)
rows <- tribble(
  ~term,                ~OLS, ~Uncenter, ~Recenter,     ~DR, 
  'Mean physicians', avg_pcp,   avg_pcp,   avg_pcp, avg_pcp)

models <- list("OLS" = new_pcp_ols, 
               "Uncenter" = new_pcp_uncenter, 
               "Recenter" = new_pcp_recenter, 
               "Recenter DR" = new_pcp_recenter_dr)

out <- modelsummary(models, 
                    coef_map = coef_map,
                    gof_map = gof_map,
                    add_rows = rows, 
                    output = "gt")

out <- out %>% 
  tab_header(
    title = md("Outcome: New PCPs per 10k")
  ) %>% 
  tab_spanner(
    label = "Shift-share IV", 
    columns = c("Uncenter", "Recenter", "Recenter DR")
  ) %>% 
  tab_source_note(
    source_note = "SE clustered at the state level are reported in parenthesis. 
                   95% RI confidence intervals based on SOP counterfactuals are reported in brackets."
  ) %>% 
  opt_horizontal_padding(scale = 3)

gtsave(out, paste0("results/ssiv/new_pcp_per_10k.html"))
gtsave(out, paste0("results/ssiv/new_pcp_per_10k.tex"))

### First-stage ----------------------------------------------------------------

avg_aprn <- mean(df_ss$aprn_medicare_per_10k, na.rm = TRUE)
rows <- tribble(
  ~term,        ~Uncenter, ~Recenter,     ~DR, 
  'Mean APRNs',  avg_aprn,  avg_aprn, avg_aprn)

models <- list("Uncenter" = new_pcp_uncenter$iv_first_stage$aprn_medicare_per_10k, 
               "Recenter" = new_pcp_recenter$iv_first_stage$aprn_medicare_per_10k, 
               "Recenter DR" = new_pcp_recenter_dr$iv_first_stage$aprn_medicare_per_10k)

out <- modelsummary(models, 
                    stars = c('*' = .1, '**' = .05, '***' = 0.01),
                    coef_map = coef_map, 
                    gof_map = gof_map,
                    add_rows = rows, 
                    output = "gt")

out <- out %>% 
  tab_header(
    title = md("First-stage: Billing APRNs per 10k")
  ) %>% 
  tab_source_note(
    source_note = "SE clustered at the state level."
  ) %>% 
  opt_horizontal_padding(scale = 3)

gtsave(out, paste0("results/ssiv/new_pcp_per_10k_first_stage.html"))
gtsave(out, paste0("results/ssiv/new_pcp_per_10k_first_stage.tex"))