# About ------------------------------------------------------------------------

# Descriptive statistics 
# Author:         Shirley Cai 
# Date created:   11/14/2025 
# Last edited:    12/12/2025 

# Helper functions -------------------------------------------------------------

# Function that returns summary stats
describe <- function(variable){
  summ <- summary(variable)
  summ <- c("Mean" = mean(variable, na.rm=TRUE),
            "SD" = sd(variable, na.rm=TRUE),
            "Min" = min(variable, na.rm=TRUE),
            "Max" = max(variable, na.rm=TRUE),
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

# Summary tables ---------------------------------------------------------------

## Summary stats (state) -------------------------------------------------------

# Summarize for all obs
summ <- df %>% 
  reframe(across(c(tot_md, tot_do, 
                   md_per_10k, md_pcp_per_10k, md_spec_per_10k,
                   fm_wage_a_mean, im_wage_a_mean, peds_wage_a_mean,
                   population,
                   Dmissing_md, Dmissing_do, Dmissing_pop,
                   FPA),
                 describe))
summ <- as.data.frame(t(summ))
colnames(summ) <- c("Mean", "Std. Dev", "Min", "Max", "N")

varnames <- c("tot_md" = "Total MDs", 
              "tot_do" = "Total DOs", 
              "md_per_10k" = "MDs per 10,000", 
              "md_pcp_per_10k" = "MD PCPs per 10,000", 
              "md_spec_per_10k" = "MD specialists per 10,000", 
              "fm_wage_a_mean" = "Family medicine annual wage", 
              "im_wage_a_mean" = "Internal medicine annual wage", 
              "peds_wage_a_mean" = "Pediatrics annual wage", 
              "population" = "Population", 
              "Dmissing_md" = "Missing MD", 
              "Dmissing_do" = "Missing DO", 
              "Dmissing_pop" = "Missing population")
summ <- summ %>% 
  add_column(Variable = rownames(summ), .before="Mean") %>% 
  mutate(
    Variable = dplyr::recode(Variable, !!!varnames)
  )
rownames(summ) <- NULL

# Format table 
summ_tab <- gt(summ, rowname_col = "Variable") %>%
  tab_stubhead(label = "Variable") %>% 
  tab_header(
    title = md("**Summary Statistics**")
  ) %>%
  fmt_number( 
    decimals = 3, 
    drop_trailing_zeros = TRUE
  ) %>% 
  tab_style(
    style = cell_text(align = "left", indent = px(20)),
    locations = cells_stub()
  ) %>% 
  opt_horizontal_padding(scale = 3)

gtsave(summ_tab, "results/descr/summ-stats.html")
gtsave(summ_tab, "results/descr/summ-stats.tex")

# Summary stats (county) -------------------------------------------------------

# TODO
