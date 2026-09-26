# About ------------------------------------------------------------------------

# Helper functions 
# Author:         Shirley Cai 
# Date created:   12/18/2025 
# Last edited:    01/07/2026 

# Descriptives -----------------------------------------------------------------

# Function that returns summary stats
describe <- function(variable){
  summ <- summary(variable)
  summ <- c("Mean" = mean(variable, na.rm = TRUE),
            "SD" = sd(variable, na.rm = TRUE),
            "Min" = min(variable, na.rm = TRUE),
            "Max" = max(variable, na.rm = TRUE),
            "N" = sum(!is.na(variable)))
  return(tibble(summ))
}

# Get treatment / control means and SDs, difference in means, t-stat and p-value
getBalanceRow <- function(covariate, treatment){
  t_group <- covariate[treatment == 1]
  c_group <- covariate[treatment == 0]
  
  res <- t.test(t_group, c_group, conf.level = 0.95, na.action = na.omit, var.equal = TRUE)
  
  balanceRow <- c("control_mean" = mean(c_group, na.rm = TRUE),
                  "control_sd" = sd(c_group, na.rm = TRUE),
                  "treat_mean" = mean(t_group, na.rm = TRUE), 
                  "treat_sd" = sd(t_group, na.rm = TRUE), 
                  "diff_mean" = mean(t_group, na.rm = TRUE) - mean(c_group, na.rm = TRUE),
                  "t-stat" = res$statistic, 
                  "p-value" = res$p.value)
}

# Event study ------------------------------------------------------------------

# Basic TWFE specification 
get_eventplot <- function(outcome, var_desc, df, file_pre = "", file_ext = ""){
  fml_string <- as.formula(paste0(outcome, " ~ i(time_to_treat, treat, ref = -1) | state_fips + year"))
  dd_out <- feols(fml_string, data = df, vcov = cluster ~ state_fips)
  
  p <- dd_out %>% 
    ggiplot(
      main = paste0("Outcome: ", var_desc), 
      xlab = "Time to SOP expansion", 
      ref.line = -1
    ) 
  
  ggsave(
    filename = paste0('results/event-study/', file_pre, outcome, file_ext, '.png'), 
    plot = p, 
    scale = 2, width = 1200, height = 900, units = 'px'
  )
  return(dd_out)
}

# Sun and Abraham specification
get_sunab <- function(outcome, var_desc, df, period = "year", file_pre = "", file_ext = ""){
  fml_string <- as.formula(paste0(outcome, " ~ sunab(treat_year, ", period, ") | state_fips + year"))
  dd_out <- feols(fml_string, data = df, vcov = cluster ~ state_fips)
  
  p <- dd_out %>% 
    ggiplot(
      main = paste0("Outcome: ", var_desc), 
      xlab = "Time to SOP expansion", 
      ref.line = -1
    ) 
  
  ggsave(
    filename = paste0('results/event-study/', file_pre, outcome, file_ext, '.png'), 
    plot = p, 
    scale = 2, width = 1200, height = 900, units = 'px'
  )
  return(dd_out)
}

# Callaway and Sant'Anna event study and ATT estimate 
get_cs <- function(outcome, var_desc, df, pre_periods, post_periods, balance_e = NULL, 
                   file_root = "results/", file_pre = "", file_ext = "", 
                   idname = "state_fips", control_group = "notyettreated", 
                   allow_unbalanced_panel = TRUE){
  out <- att_gt(yname = outcome, 
                tname = "year", 
                idname = idname, 
                gname = "treat_year",
                data = df,
                allow_unbalanced_panel = allow_unbalanced_panel,
                control_group = control_group, 
                clustervars = c("state_fips"))
  
  # Event study
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
    filename = paste0(file_root, file_pre, outcome, file_ext, '.png'), 
    plot = p, 
    scale = 2, width = 1200, height = 900, units = 'px'
  )
  
  # Staggered DiD
  did <- aggte(out, 
               na.rm = TRUE, 
               type = "simple", 
               min_e = -pre_periods, 
               max_e = post_periods, 
               balance_e = balance_e)
  return(did)
}

# Helper to get control mean
get_control_mean <- function(model, control_group = c("notyettreated", "nevertreated")) {
  control_group <- match.arg(control_group)
  
  d <- model$DIDparams$data
  gname <- model$DIDparams$gname
  yname <- model$DIDparams$yname
  tname <- model$DIDparams$tname
  
  in_control <- switch(control_group,
                       nevertreated  = d[[gname]] == 0,
                       notyettreated = d[[gname]] == 0 | d[[tname]] < d[[gname]]
  )
  
  mean(d[[yname]][in_control], na.rm = TRUE)
}

# Stacked ----------------------------------------------------------------------

get_stacked_df <- function(df, pre_periods, post_periods, unit_name = "state_fips"){
  
  df <- df %>% mutate(treat_year = ifelse(is.infinite(treat_year), NA, treat_year))
  
  cohorts <- df %>% 
    filter(!is.na(treat_year)) %>% 
    distinct(treat_year) %>% 
    pull(treat_year)
  
  never_treated <- df %>% filter(is.na(treat_year))
  
  stacked_df <- map_dfr(cohorts, function(g){
    
    # Treated units in this cohort 
    treated_units <- df %>% filter(treat_year == g)
    
    # Bind with never treated controls 
    sub_exp <- bind_rows(treated_units, never_treated) %>% 
      filter(year >= g - pre_periods, year <= g + post_periods) %>% 
      mutate(
        cohort      = g,                          # sub-experiment
        rel_time    = year - g,                   # relative time
        treated     = as.integer(!is.na(treat_year)),
        post        = as.integer(year >= g),
        unit_fe     = paste0(unit_name, "_", g),
        time_fe     = paste0(year,       "_", g)
      )
    
    sub_exp
  })
}

get_stacked <- function(outcome, var_desc, df, file_pre = "", file_ext = ""){
  fml_string <- as.formula(paste0(outcome, " ~ i(rel_time, treat, ref = -1) | unit_fe + time_fe"))
  dd_out <- feols(fml_string, data = df, vcov = cluster ~ unit_fe)
  
  p <- dd_out %>% 
    ggiplot(
      main = paste0("Outcome: ", var_desc), 
      xlab = "Time to SOP expansion", 
      ref.line = -1
    ) 
  
  ggsave(
    filename = paste0('results/event-study/', file_pre, outcome, file_ext, '.png'), 
    plot = p, 
    scale = 2, width = 1200, height = 900, units = 'px'
  )
  return(dd_out)
}


get_stacked_att <- function(outcome, df, pre_periods, post_periods){
  fml_string <- as.formula(paste0(outcome, " ~ i(rel_time, treat, ref = -1) | unit_fe + time_fe"))
  dd_out <- feols(fml_string, data = df, vcov = cluster ~ unit_fe)
  
  coefs <- coef(dd_out)
  vcov <- vcov(dd_out)
  
  post_names <- names(coefs)[grepl("rel_time::[0-9]", names(coefs))]
  post_coefs <- coefs[post_names]
  
  post_weights <- df %>%
    filter(rel_time %in% as.numeric(str_extract(post_names, "(?<=::)-?[0-9]+"))) %>%
    count(rel_time) %>%
    arrange(rel_time) %>%
    mutate(w = n / sum(n)) %>%
    pull(w)
  
  att <- sum(post_coefs * post_weights)
  
  w_vec   <- matrix(post_weights, nrow = 1)
  att_var <- w_vec %*% vcov[post_names, post_names] %*% t(w_vec)
  att_se  <- sqrt(att_var)
  
  att_ci_lower <- att - 1.96 * att_se
  att_ci_upper <- att + 1.96 * att_se
  att_pval     <- 2 * pnorm(-abs(att / att_se))
  
  return(list("att" = att, "se" = att_se, "pval" = att_pval))
}

get_stacked_table <- function(results, mean_row, title, source_note, file_name){
  att_row <- map_chr(results, ~ paste0(sprintf("%.3f", .x$att), stars(.x$pval)))
  se_row  <- map_chr(results, ~ paste0("(", sprintf("%.3f", .x$se), ")"))
  
  tbl_data <- tibble(
    term       = c("ATT", "", "Mean outcome per 10k"),
    outcome1   = c(att_row[1], se_row[1], mean_row[1]),
    outcome2   = c(att_row[2], se_row[2], mean_row[2]),
    outcome3   = c(att_row[3], se_row[3], mean_row[3])
  )
  
  gt_table <- tbl_data %>%
    gt() %>%
    tab_header(title = md(title)) %>% 
    cols_label(term = "", outcome1 = "New PCPs", outcome2 = "New MD PCPs", outcome3 = "New DO PCPs") %>%
    tab_source_note("* p < 0.1, ** p < 0.05, *** p < 0.01") %>%
    tab_source_note(source_note) %>% 
    opt_horizontal_padding(scale = 3)
  
  gtsave(gt_table, paste0("results/did/", file_name, ".html"))
  gtsave(gt_table, paste0("results/did/", file_name, ".tex"))
}

# Tables -----------------------------------------------------------------------

# Helper to convert AGGTEobj to modelsummary-compatible format, including mean, effect size, N
convert_aggte <- function(model, control_mean) {
  # Estimate
  att <- model$overall.att
  se  <- model$overall.se
  z_stat <- att / se
  p_val  <- 2 * pnorm(-abs(z_stat))
  
  # Get sample size N
  n_val <- tryCatch({
    if (!is.null(model$DIDparams$data)) {
      nrow(model$DIDparams$data)
    } else {
      NA_integer_
    }
  }, error = function(e) NA_integer_)
  
  effect_size <- 100 * att / control_mean  # % of control mean
  
  tidy_df <- data.frame(
    term      = c("ATT", "Control group mean", "Implied effect (%)", "N"),
    estimate  = c(sprintf("%.3f", att),
                  sprintf("%.3f", control_mean),
                  sprintf("%.1f", effect_size),
                  formatC(n_val, format = "d", big.mark = ",")),
    std.error = c(sprintf("%.3f", se), NA, NA, NA),
    statistic = c(z_stat, NA, NA, NA),
    p.value   = c(p_val, NA, NA, NA),
    stringsAsFactors = FALSE
  )
  
  glance_df <- data.frame(nobs = NA_integer_, stringsAsFactors = FALSE)
  
  structure(list(tidy = tidy_df, glance = glance_df), class = "modelsummary_list")
}

# Wrapper to convert a list of models using convert_aggte
convert_models <- function(models, control_means){
  if (!setequal(names(models), names(control_means))) {
    stop("names(models) and names(control_means) must match exactly.")
  }
  out <- Map(convert_aggte, models, control_means[names(models)])
  names(out) <- names(models)
  out
}

# Create and save a table
get_table <- function(models_a, models_b = NULL,
                      control_means_a, control_means_b = NULL,
                      title, source_note, file_name, 
                      panel_a_label = "Panel A", panel_b_label = "Panel B", 
                      col_labels = NULL){
  
  # Convert to make readable by modelsummary
  converted_a <- convert_models(models_a, control_means_a)
  
  if (is.null(models_b)) {
    # Single-panel path — no rbind
    res <- modelsummary(
      converted_a,
      stars   = c('*' = .1, '**' = .05, '***' = 0.01),
      gof_map = list(),
      fmt     = NULL,
      output  = "gt"
    )
  } else {
    converted_b <- convert_models(models_b, control_means_b)
    panels <- setNames(list(converted_a, converted_b), c(panel_a_label, panel_b_label))
    
    res <- modelsummary(
      panels,
      shape   = "rbind",
      stars   = c('*' = .1, '**' = .05, '***' = 0.01),
      gof_map = list(),
      fmt     = NULL,
      output  = "gt"
    )
  }
  
  if (!is.null(col_labels)) {
    res <- res %>% cols_label(.list = as.list(col_labels))
  }
  
  res <- res %>%
    tab_header(title = md(title)) %>%
    tab_source_note(source_note = source_note) %>%
    opt_horizontal_padding(scale = 3)
  
  gtsave(res, paste0("results/", file_name, ".html"))
  gtsave(res, paste0("results/", file_name, ".tex"))
}

# Helpers for shift-share IV ---------------------------------------------------

get_ols <- function(outcome, controls, df){
  fml_string <- paste(outcome, 
                      paste(controls, collapse = " + "), 
                      sep = " ~ ")
  out <- feols(as.formula(paste(fml_string, "| state_fips + year")), 
               data = df, 
               vcov = cluster ~ state_fips)
  return(out)
}

get_uncenter <- function(outcome, controls, df){
  fml_string <- paste(outcome, 
                      paste(controls, collapse = " + "), 
                      sep = " ~ ")
  out <- feols(as.formula(paste(fml_string, "| state_fips + year | aprn_medicare_per_10k ~ ssiv")), 
               data = df, 
               vcov = cluster ~ state_fips)
  return(out)
}

get_recenter <- function(outcome, controls, df, iv = "ssiv_recentered"){
  fml_string <- paste(outcome, 
                      paste(controls, collapse = " + "), 
                      sep = " ~ ")
  out <- feols(as.formula(paste(fml_string, "| state_fips + year | aprn_medicare_per_10k ~ ", iv)), 
               data = df, 
               vcov = cluster ~ state_fips)
  return(out)
}

get_recenter_dr <- function(outcome, controls, df, iv = "ssiv"){
  controls <- c(controls, "ev_ssiv")
  fml_string <- paste(outcome, 
                      paste(controls, collapse = " + "), 
                      sep = " ~ ")
  out <- feols(as.formula(paste(fml_string, "| state_fips + year | aprn_medicare_per_10k ~ ", iv)), 
               data = df, 
               vcov = cluster ~ state_fips)
  return(out)
}
