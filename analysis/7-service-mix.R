# About ------------------------------------------------------------------------

# Physician service mix
# Author:         Shirley Cai 
# Date created:   07/09/2026 
# Last edited:    07/09/2026

# Preparation ------------------------------------------------------------------

df_util <- read_csv('data/output/final_service_mix.csv')

# TODO: Change directory root
dir_root <- 'archive/prelim/service_mix/'

# Summary descriptives ---------------------------------------------------------

varnames <- c('tot_wrvu' = "Total wRVUs", 
              'tot_wrvu_2013' = "Total 2013 wRVUs", 
              'tot_wrvu_office' = "Total office-based wRVUs", 
              'tot_wrvu_2013_office' = "Total office-based 2013 wRVUs", 
              'wrvu_per_service' = "wRVUs per service", 
              'wrvu_2013_per_service' = "2013 wRVUs per service", 
              'wrvu_office_per_service' = "Office-based wRVUs per service", 
              'wrvu_2013_office_per_service' = "Office-based 2013 wRVUs per service", 
              'D_aprn' = "APRN",
              'FPA' = "FPA", 
              'treat' = "Treated")

## All observations ------------------------------------------------------------

summ <- df_util %>% 
  reframe(across(c(tot_wrvu, tot_wrvu_2013, 
                   tot_wrvu_office, tot_wrvu_2013_office, 
                   wrvu_per_service, wrvu_2013_per_service, 
                   wrvu_office_per_service, wrvu_2013_office_per_service, 
                   D_aprn, FPA, treat),
                 describe))
summ <- as.data.frame(t(summ))
colnames(summ) <- c("Mean", "Std. Dev", "Min", "Max", "N")

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
    title = md("**PCPs and APRNs**")
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

gtsave(summ_tab, paste0(dir_root, "summ-stats.html"))
gtsave(summ_tab, paste0(dir_root, "summ-stats.tex"))

## PCPs only -------------------------------------------------------------------

summ <- df_util %>% filter(D_aprn == 0) %>% 
  reframe(across(c(tot_wrvu, tot_wrvu_2013, 
                   tot_wrvu_office, tot_wrvu_2013_office, 
                   wrvu_per_service, wrvu_2013_per_service, 
                   wrvu_office_per_service, wrvu_2013_office_per_service, 
                   D_aprn, FPA, treat),
                 describe))
summ <- as.data.frame(t(summ))
colnames(summ) <- c("Mean", "Std. Dev", "Min", "Max", "N")

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
    title = md("**PCP service mix**")
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

gtsave(summ_tab, paste0(dir_root, "summ-stats-pcp.html"))
gtsave(summ_tab, paste0(dir_root, "summ-stats-pcp.tex"))

## APRNs only ------------------------------------------------------------------

summ <- df_util %>% filter(D_aprn == 1) %>% 
  reframe(across(c(tot_wrvu, tot_wrvu_2013, 
                   tot_wrvu_office, tot_wrvu_2013_office, 
                   wrvu_per_service, wrvu_2013_per_service, 
                   wrvu_office_per_service, wrvu_2013_office_per_service, 
                   D_aprn, FPA, treat),
                 describe))
summ <- as.data.frame(t(summ))
colnames(summ) <- c("Mean", "Std. Dev", "Min", "Max", "N")

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
    title = md("**APRN service mix**")
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

gtsave(summ_tab, paste0(dir_root, "summ-stats-aprn.html"))
gtsave(summ_tab, paste0(dir_root, "summ-stats-aprn.tex"))

# Event time descriptives ------------------------------------------------------

# Observations per event time 
df_util %>%
  filter(treat == 1) %>% 
  ggplot(aes(x = time_to_treat)) + 
  geom_histogram(binwidth = 1) + 
  labs(
    x = "Time to treat (treated only)", 
    y = "Count"
  )
ggsave(
  paste0(dir_root, "time_to_treat_hist.png"), scale = 2,
  width = 1200, height = 900, units = "px"
)

# Trends by cohort -------------------------------------------------------------

df_util_untrim <- df_util 
df_util <- df_util_untrim %>% filter(wrvu_per_service < 5)

years <- sort(unique(df_util$effective_year[!is.na(df_util$effective_year)]))
years <- years[years > 2013]

# Break out cohort into different graphs 
for(yr in years){
  cohort_states <- df_util %>% filter(effective_year == yr) 
  cohort_states <- sort(unique(cohort_states$state))
  
  md_df <- df_util %>% filter(D_aprn == 0) %>% 
    filter(treat == 0 | effective_year == yr) %>% 
    group_by(treat, year) %>% 
    summarize(
      wrvu_per_service = mean(wrvu_2013_per_service)
    ) 
  
  aprn_df <- df_util %>% filter(D_aprn == 1) %>% 
    filter(treat == 0 | effective_year == yr) %>% 
    group_by(treat, year) %>% 
    summarize(
      wrvu_per_service = mean(wrvu_2013_per_service)
    ) 
  
  # PCP trend
  control_ref <- md_df %>%
    filter(treat == 0, year == yr) %>%
    pull(wrvu_per_service)
  treat_ref <- md_df %>%
    filter(treat == 1, year == yr) %>%
    pull(wrvu_per_service)
  md_df %>% 
    mutate(
      wrvu_per_service = ifelse(treat == 1, wrvu_per_service - treat_ref, wrvu_per_service - control_ref),
      treat = ifelse(treat == 1, "Ever-treated", "Never treated"),
    ) %>%
    ggplot(aes(x = year, y = wrvu_per_service, color = as.factor(treat))) + 
    geom_line() + 
    geom_vline(xintercept = yr, linetype = "dashed", color = "gray") +
    labs(
      x = "Year", 
      y = "Centered wRVUs per service for PCPs", 
      color = "", 
      subtitle = paste0("Cohort treated in ", yr),
      caption = paste0("Cohort includes ",  paste(cohort_states, collapse = ", "), ". Control is never treated")
    )
  ggsave(
    paste0(dir_root, "pcp/cohort_", yr, "trend_pcp.png"), scale = 2, 
    width = 1200, height = 900, units = "px"
  )
  
  # APRNs trend
  control_ref <- aprn_df %>%
    filter(treat == 0, year == yr) %>%
    pull(wrvu_per_service)
  treat_ref <- aprn_df %>%
    filter(treat == 1, year == yr) %>%
    pull(wrvu_per_service)
  aprn_df %>% 
    mutate(
      wrvu_per_service = ifelse(treat == 1, wrvu_per_service - treat_ref, wrvu_per_service - control_ref),
      treat = ifelse(treat == 1, "Ever-treated", "Never treated"),
    ) %>%
    ggplot(aes(x = year, y = wrvu_per_service, color = as.factor(treat))) + 
    geom_line() + 
    geom_vline(xintercept = yr, linetype = "dashed", color = "gray") +
    labs(
      x = "Year", 
      y = "Centered wRVUs per service for APRNs", 
      color = "", 
      subtitle = paste0("Cohort treated in ", yr),
      caption = paste0("Cohort includes ",  paste(cohort_states, collapse = ", "), ". Control is never treated")
    )
  ggsave(
    paste0(dir_root, "aprn/cohort_", yr, "trend_aprn.png"), scale = 2, 
    width = 1200, height = 900, units = "px"
  )
}

# Trim data --------------------------------------------------------------------

md_df <- df_util %>% filter(D_aprn == 0) %>% mutate(treat_year = ifelse(treat == 0, Inf, effective_year))
aprn_df <- df_util %>% filter(D_aprn == 1) %>% mutate(treat_year = ifelse(treat == 0, Inf, effective_year))

# Restrict sample to certain years pre and post policy 
pre_periods <- 5
post_periods <- 5

md_df_trim <- md_df %>% filter(time_to_treat >= -pre_periods, time_to_treat <= post_periods)
aprn_df_trim <- aprn_df %>% filter(time_to_treat >= -pre_periods, time_to_treat <= post_periods)

# Service mix ------------------------------------------------------------------

md_df <- md_df %>% mutate(treat_year = ifelse(treat == 0, 0, effective_year))
aprn_df <- aprn_df %>% mutate(treat_year = ifelse(treat == 0, 0, effective_year))

pre_periods <- 5
post_periods <- 5
balance_e <- NULL

## Event study -----------------------------------------------------------------

# PCP service mix
get_cs("wrvu_per_service", "wRVUs per service", md_df, pre_periods, post_periods, balance_e, file_pre = "service-mix/cs/", file_ext = "_pcp")
get_cs("wrvu_office_per_service", "wRVUs per service (office only)", md_df, pre_periods, post_periods, balance_e, file_pre = "service-mix/cs/", file_ext = "_pcp")
get_cs("wrvu_2013_per_service", "wRVUs (2013) per service", md_df, pre_periods, post_periods, balance_e, file_pre = "service-mix/cs/", file_ext = "_pcp")
get_cs("wrvu_2013_office_per_service", "wRVUs (2013) per service (office only)", md_df, pre_periods, post_periods, balance_e, file_pre = "service-mix/cs/", file_ext = "_pcp")

# APRN service mix
get_cs("wrvu_per_service", "wRVUs per service", aprn_df, pre_periods, post_periods, balance_e, file_pre = "service-mix/cs/", file_ext = "_aprn")
get_cs("wrvu_office_per_service", "wRVUs per service (office only)", aprn_df, pre_periods, post_periods, balance_e, file_pre = "service-mix/cs/", file_ext = "_aprn")
get_cs("wrvu_2013_per_service", "wRVUs (2013) per service", aprn_df, pre_periods, post_periods, balance_e, file_pre = "service-mix/cs/", file_ext = "_aprn")
get_cs("wrvu_2013_office_per_service", "wRVUs (2013) per service (office only)", aprn_df, pre_periods, post_periods, balance_e, file_pre = "service-mix/cs/", file_ext = "_aprn")

## Staggered DiD ---------------------------------------------------------------

# PCP service mix
wrvu_per_service <- get_cs_did("wrvu_per_service", "wRVUs per service", md_df, pre_periods, post_periods, balance_e)
wrvu_office_per_service <- get_cs_did("wrvu_office_per_service", "wRVUs per service (office only)", md_df, pre_periods, post_periods, balance_e)
wrvu_2013_per_service <- get_cs_did("wrvu_2013_per_service", "wRVUs (2013) per service", md_df, pre_periods, post_periods, balance_e)
wrvu_2013_office_per_service <- get_cs_did("wrvu_2013_office_per_service", "wRVUs (2013) per service (office only)", md_df, pre_periods, post_periods, balance_e)

# APRN service mix
wrvu_per_service <- get_cs_did("wrvu_per_service", "wRVUs per service", aprn_df, pre_periods, post_periods, balance_e)
wrvu_office_per_service <- get_cs_did("wrvu_office_per_service", "wRVUs per service (office only)", aprn_df, pre_periods, post_periods, balance_e)
wrvu_2013_per_service <- get_cs_did("wrvu_2013_per_service", "wRVUs (2013) per service", aprn_df, pre_periods, post_periods, balance_e)
wrvu_2013_office_per_service <- get_cs_did("wrvu_2013_office_per_service", "wRVUs (2013) per service (office only)", aprn_df, pre_periods, post_periods, balance_e)

### Export ---------------------------------------------------------------------

gof_map <- tribble(
  ~raw,          ~clean,          ~fmt,     ~omit,
  "r.squared",   "R2",               3,     FALSE
)

# PCP service mix
avg_wrvu <- mean(md_df$wrvu_per_service, na.rm = TRUE)
avg_wrvu_office <- mean(md_df$wrvu_office_per_service, na.rm = TRUE)
avg_wrvu_2013 <- mean(md_df$wrvu_2013_per_service, na.rm = TRUE)
avg_wrvu_2013_office <- mean(md_df$wrvu_2013_office_per_service, na.rm = TRUE)
models <- list("WRVUs" = wrvu_per_service, 
               "WRVUs (office)" = wrvu_office_per_service, 
               "WRUs (2013)" = wrvu_2013_per_service, 
               "WRUs (2013, office)" = wrvu_2013_office_per_service)
rows <- tribble(
  ~term,                   ~wrvu,    ~wrvu_office,    ~wrvu_2013,    ~wrvu_2013_office,
  'Mean wRVUs per service', avg_wrvu, avg_wrvu_office, avg_wrvu_2013, avg_wrvu_2013_office)
get_table(models, gof_map, rows, "PCP service mix", "Note: SE clustered at state", "service-mix/5-pre-post/pcp_service_mix")

# APRN service mix
avg_wrvu <- mean(aprn_df$wrvu_per_service, na.rm = TRUE)
avg_wrvu_office <- mean(aprn_df$wrvu_office_per_service, na.rm = TRUE)
avg_wrvu_2013 <- mean(aprn_df$wrvu_2013_per_service, na.rm = TRUE)
avg_wrvu_2013_office <- mean(aprn_df$wrvu_2013_office_per_service, na.rm = TRUE)
models <- list("WRVUs" = wrvu_per_service, 
               "WRVUs (office)" = wrvu_office_per_service, 
               "WRUs (2013)" = wrvu_2013_per_service, 
               "WRUs (2013, office)" = wrvu_2013_office_per_service)
rows <- tribble(
  ~term,                   ~wrvu,    ~wrvu_office,    ~wrvu_2013,    ~wrvu_2013_office,
  'Mean wRVUs per service', avg_wrvu, avg_wrvu_office, avg_wrvu_2013, avg_wrvu_2013_office)
get_table(models, gof_map, rows, "APRN service mix", "Note: SE clustered at state", "service-mix/5-pre-post/aprn_service_mix")

# Service volume ---------------------------------------------------------------

## Event study -----------------------------------------------------------------

# PCP volume 
get_cs("tot_services", "Services per PCP", md_df, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/", file_ext = "_pcp")
get_cs("tot_services_office", "Office services per PCP", md_df, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/", file_ext = "_pcp")
get_cs("tot_wrvu", "wRVUs per PCP", md_df, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/", file_ext = "_pcp")
get_cs("tot_wrvu_2013", "wRVUs (2013) per PCP", md_df, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/", file_ext = "_pcp")
get_cs("tot_wrvu_office", "wRVUs (office only) per PCP", md_df, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/", file_ext = "_pcp")
get_cs("tot_wrvu_2013_office", "wRVUs (2013, office only) per PCP", md_df, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/", file_ext = "_pcp")
get_cs("tot_benes", "Beneficiaries per PCP", md_df, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/", file_ext = "_pcp")

# APRN volume 
get_cs("tot_services", "Services per APRN", aprn_df, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/", file_ext = "_aprn")
get_cs("tot_services_office", "Office services per APRN", aprn_df, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/", file_ext = "_aprn")
get_cs("tot_wrvu", "wRVUs per APRN", aprn_df, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/", file_ext = "_aprn")
get_cs("tot_wrvu_2013", "wRVUs (2013) per APRN", aprn_df, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/", file_ext = "_aprn")
get_cs("tot_wrvu_office", "wRVUs (office only) per APRN", aprn_df, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/", file_ext = "_aprn")
get_cs("tot_wrvu_2013_office", "wRVUs (2013, office only) per APRN", aprn_df, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/", file_ext = "_aprn")
get_cs("tot_benes", "Beneficiaries per APRN", aprn_df, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/", file_ext = "_aprn")

# APRN volume (county level)
df_c <- df_c %>% mutate(treat_year = ifelse(treat == 0, Inf, effective_year))
get_cs("aprn_services_per_bene", "APRN services per Medicare bene (county)", df_c, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/", file_ext = "_county")
get_cs("aprn_services_office_per_bene", "APRN office services per Medicare bene (county)", df_c, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/", file_ext = "_county")

## Staggered DiD ---------------------------------------------------------------

# PCP volume
tot_services <- get_cs_did("tot_services", "Services per PCP", md_df, pre_periods, post_periods, balance_e)
tot_services_office <- get_cs_did("tot_services_office", "Office services per PCP", md_df, pre_periods, post_periods, balance_e)
tot_wrvu_2013 <- get_cs_did("tot_wrvu", "wRVUs (2013) per PCP", md_df, pre_periods, post_periods, balance_e)
tot_wrvu_2013_office <- get_cs_did("tot_wrvu_office", "wRVUs (2013, office only) per PCP", md_df, pre_periods, post_periods, balance_e)
tot_benes <- get_cs_did("tot_benes", "Beneficiaries per PCP", md_df, pre_periods, post_periods, balance_e)

avg_service <- mean(md_df$tot_services, na.rm = TRUE)
avg_wrvu <- mean(md_df$tot_wrvu_2013, na.rm = TRUE)
avg_benes <- mean(md_df$tot_benes, na.rm = TRUE)
models <- list("Services" = tot_services, 
               "wRVUs (2013)" = tot_wrvu_2013, 
               "Beneficiaries" = tot_benes)
rows <- tribble(
  ~term,             ~service,    ~wrvu,    ~benes,
  'Mean outcome', avg_service, avg_wrvu, avg_benes)
get_table(models, gof_map, rows, "PCP service volume", "Note: SE clustered at state", "service-volume/5-pre-post/pcp_service_volume")

# APRN volume 
tot_services <- get_cs_did("tot_services", "Services per APRN", aprn_df, pre_periods, post_periods, balance_e)
tot_services_office <- get_cs_did("tot_services_office", "Office services per APRN", aprn_df, pre_periods, post_periods, balance_e)
tot_wrvu_2013 <- get_cs_did("tot_wrvu", "wRVUs (2013) per APRN", aprn_df, pre_periods, post_periods, balance_e)
tot_wrvu_2013_office <- get_cs_did("tot_wrvu_office", "wRVUs (2013, office only) per APRN", aprn_df, pre_periods, post_periods, balance_e)
tot_benes <- get_cs_did("tot_benes", "Beneficiaries per APRN", aprn_df, pre_periods, post_periods, balance_e)

avg_service <- mean(aprn_df$tot_services, na.rm = TRUE)
avg_wrvu <- mean(aprn_df$tot_wrvu_2013, na.rm = TRUE)
avg_benes <- mean(aprn_df$tot_benes, na.rm = TRUE)
models <- list("Services" = tot_services, 
               "wRVUs (2013)" = tot_wrvu_2013, 
               "Beneficiaries" = tot_benes)
rows <- tribble(
  ~term,             ~service,    ~wrvu,    ~benes,
  'Mean outcome', avg_service, avg_wrvu, avg_benes)
get_table(models, gof_map, rows, "APRN service volume", "Note: SE clustered at state", "service-volume/5-pre-post/aprn_service_volume")

# APRN volume (county level)
aprn_services_per_bene <- get_cs_did("aprn_services_per_bene", "APRN services per Medicare bene (county)", df_c, pre_periods, post_periods, balance_e)
aprn_services_office_per_bene <- get_cs_did("aprn_services_office_per_bene", "APRN office services per Medicare bene (county)", df_c, pre_periods, post_periods, balance_e)

avg_service <- mean(df_c$aprn_services_per_bene, na.rm = TRUE)
avg_office_service <- mean(df_c$aprn_services_office_per_bene, na.rm = TRUE)
models <- list("APRN services per bene" = aprn_services_per_bene, 
               "APRN office services per bene" = aprn_services_office_per_bene)
rows <- tribble(
  ~term,             ~service,    ~office,
  'Mean outcome', avg_service, avg_office_service)
get_table(models, gof_map, rows, "APRN services per bene (county)", "Note: SE clustered at state", "service-volume/5-pre-post/aprn_county_volume")

# High impact split ------------------------------------------------------------

# Quartiles based on 2010 APRN density

## Highest quartile counties ---------------------------------------------------

df_q <- md_df %>% filter(share_quartile == 4)

### Event study ----------------------------------------------------------------

# PCP service mix
get_cs("wrvu_per_service", "wRVUs per service", df_q, pre_periods, post_periods, balance_e, file_pre = "service-mix/cs/high/", file_ext = "_pcp")
get_cs("wrvu_office_per_service", "wRVUs per service (office only)", df_q, pre_periods, post_periods, balance_e, file_pre = "service-mix/cs/high/", file_ext = "_pcp")
get_cs("wrvu_2013_per_service", "wRVUs (2013) per service", df_q, pre_periods, post_periods, balance_e, file_pre = "service-mix/cs/high/", file_ext = "_pcp")
get_cs("wrvu_2013_office_per_service", "wRVUs (2013) per service (office only)", df_q, pre_periods, post_periods, balance_e, file_pre = "service-mix/cs/high/", file_ext = "_pcp")

# PCP volume 
get_cs("tot_services", "Total services", df_q, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/high/", file_ext = "_pcp")
get_cs("tot_services_office", "Total office services", df_q, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/high/", file_ext = "_pcp")
get_cs("tot_wrvu", "Total wRVUs", df_q, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/high/", file_ext = "_pcp")
get_cs("tot_wrvu_2013", "Total wRVUs (2013)", df_q, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/high/", file_ext = "_pcp")
get_cs("tot_wrvu_office", "Total wRVUs (office only)", df_q, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/high/", file_ext = "_pcp")
get_cs("tot_wrvu_2013_office", "Total wRVUs (2013, office only)", df_q, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/high/", file_ext = "_pcp")
get_cs("tot_benes", "Total beneficiaries", df_q, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/high/", file_ext = "_pcp")

### Staggered DiD --------------------------------------------------------------

# PCP service volume
tot_services_q4 <- get_cs_did("tot_services", "Services per PCP", df_q, pre_periods, post_periods, balance_e)
tot_wrvu_2013_q4 <- get_cs_did("tot_wrvu", "wRVUs (2013) per PCP", df_q, pre_periods, post_periods, balance_e)
tot_benes_q4 <- get_cs_did("tot_benes", "Beneficiaries per PCP", df_q, pre_periods, post_periods, balance_e)

## Lowest quartile counties ----------------------------------------------------

df_q <- md_df %>% filter(share_quartile == 1)

### Event study ----------------------------------------------------------------

# PCP service mix
get_cs("wrvu_per_service", "wRVUs per service", df_q, pre_periods, post_periods, balance_e, file_pre = "service-mix/cs/low/", file_ext = "_pcp")
get_cs("wrvu_office_per_service", "wRVUs per service (office only)", df_q, pre_periods, post_periods, balance_e, file_pre = "service-mix/cs/low/", file_ext = "_pcp")
get_cs("wrvu_2013_per_service", "wRVUs (2013) per service", df_q, pre_periods, post_periods, balance_e, file_pre = "service-mix/cs/low/", file_ext = "_pcp")
get_cs("wrvu_2013_office_per_service", "wRVUs (2013) per service (office only)", df_q, pre_periods, post_periods, balance_e, file_pre = "service-mix/cs/low/", file_ext = "_pcp")

# PCP volume 
get_cs("tot_services", "Total services", df_q, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/low/", file_ext = "_pcp")
get_cs("tot_services_office", "Total office services", df_q, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/low/", file_ext = "_pcp")
get_cs("tot_wrvu", "Total wRVUs", df_q, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/low/", file_ext = "_pcp")
get_cs("tot_wrvu_2013", "Total wRVUs (2013)", df_q, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/low/", file_ext = "_pcp")
get_cs("tot_wrvu_office", "Total wRVUs (office only)", df_q, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/low/", file_ext = "_pcp")
get_cs("tot_wrvu_2013_office", "Total wRVUs (2013, office only)", df_q, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/low/", file_ext = "_pcp")
get_cs("tot_benes", "Total beneficiaries", df_q, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/low/", file_ext = "_pcp")

### Staggered DiD --------------------------------------------------------------

# PCP service volume
tot_services_q1 <- get_cs_did("tot_services", "Services per PCP", df_q, pre_periods, post_periods, balance_e)
tot_wrvu_2013_q1 <- get_cs_did("tot_wrvu", "wRVUs (2013) per PCP", df_q, pre_periods, post_periods, balance_e)
tot_benes_q1 <- get_cs_did("tot_benes", "Beneficiaries per PCP", df_q, pre_periods, post_periods, balance_e)

## Highest quartile within same state state counties ---------------------------

df_q <- md_df %>% filter(share_quartile_state == 4)

### Event study ----------------------------------------------------------------

# PCP service mix
get_cs("wrvu_per_service", "wRVUs per service", df_q, pre_periods, post_periods, balance_e, file_pre = "service-mix/cs/high-state/", file_ext = "_pcp")
get_cs("wrvu_office_per_service", "wRVUs per service (office only)", df_q, pre_periods, post_periods, balance_e, file_pre = "service-mix/cs/high-state/", file_ext = "_pcp")
get_cs("wrvu_2013_per_service", "wRVUs (2013) per service", df_q, pre_periods, post_periods, balance_e, file_pre = "service-mix/cs/high-state/", file_ext = "_pcp")
get_cs("wrvu_2013_office_per_service", "wRVUs (2013) per service (office only)", df_q, pre_periods, post_periods, balance_e, file_pre = "service-mix/cs/high-state/", file_ext = "_pcp")

# PCP volume 
get_cs("tot_services", "Total services", df_q, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/high-state/", file_ext = "_pcp")
get_cs("tot_services_office", "Total office services", df_q, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/high-state/", file_ext = "_pcp")
get_cs("tot_wrvu", "Total wRVUs", df_q, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/high-state/", file_ext = "_pcp")
get_cs("tot_wrvu_2013", "Total wRVUs (2013)", df_q, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/high-state/", file_ext = "_pcp")
get_cs("tot_wrvu_office", "Total wRVUs (office only)", df_q, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/high-state/", file_ext = "_pcp")
get_cs("tot_wrvu_2013_office", "Total wRVUs (2013, office only)", df_q, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/high-state/", file_ext = "_pcp")
get_cs("tot_benes", "Total beneficiaries", df_q, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/high-state/", file_ext = "_pcp")

### Staggered DiD --------------------------------------------------------------

# PCP service volume
tot_services_q4_state <- get_cs_did("tot_services", "Services per PCP", df_q, pre_periods, post_periods, balance_e)
tot_wrvu_2013_q4_state <- get_cs_did("tot_wrvu", "wRVUs (2013) per PCP", df_q, pre_periods, post_periods, balance_e)
tot_benes_q4_state <- get_cs_did("tot_benes", "Beneficiaries per PCP", df_q, pre_periods, post_periods, balance_e)

## Lowest quartile within same state counties ----------------------------------

df_q <- md_df %>% filter(share_quartile_state == 1)

### Event study ----------------------------------------------------------------

# PCP service mix
get_cs("wrvu_per_service", "wRVUs per service", df_q, pre_periods, post_periods, balance_e, file_pre = "service-mix/cs/low-state/", file_ext = "_pcp")
get_cs("wrvu_office_per_service", "wRVUs per service (office only)", df_q, pre_periods, post_periods, balance_e, file_pre = "service-mix/cs/low-state/", file_ext = "_pcp")
get_cs("wrvu_2013_per_service", "wRVUs (2013) per service", df_q, pre_periods, post_periods, balance_e, file_pre = "service-mix/cs/low-state/", file_ext = "_pcp")
get_cs("wrvu_2013_office_per_service", "wRVUs (2013) per service (office only)", df_q, pre_periods, post_periods, balance_e, file_pre = "service-mix/cs/low-state/", file_ext = "_pcp")

# PCP volume 
get_cs("tot_services", "Total services", df_q, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/low-state/", file_ext = "_pcp")
get_cs("tot_services_office", "Total office services", df_q, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/low-state/", file_ext = "_pcp")
get_cs("tot_wrvu", "Total wRVUs", df_q, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/low-state/", file_ext = "_pcp")
get_cs("tot_wrvu_2013", "Total wRVUs (2013)", df_q, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/low-state/", file_ext = "_pcp")
get_cs("tot_wrvu_office", "Total wRVUs (office only)", df_q, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/low-state/", file_ext = "_pcp")
get_cs("tot_wrvu_2013_office", "Total wRVUs (2013, office only)", df_q, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/low-state/", file_ext = "_pcp")
get_cs("tot_benes", "Total beneficiaries", df_q, pre_periods, post_periods, balance_e, file_pre = "service-volume/cs/low-state/", file_ext = "_pcp")

### Staggered DiD --------------------------------------------------------------

# PCP service volume
tot_services_q1_state <- get_cs_did("tot_services", "Services per PCP", df_q, pre_periods, post_periods, balance_e)
tot_wrvu_2013_q1_state <- get_cs_did("tot_wrvu", "wRVUs (2013) per PCP", df_q, pre_periods, post_periods, balance_e)
tot_benes_q1_state <- get_cs_did("tot_benes", "Beneficiaries per PCP", df_q, pre_periods, post_periods, balance_e)

## Export staggered DiD --------------------------------------------------------

# Rerun aggregate 
tot_services_full <- get_cs_did("tot_services", "Services per PCP", md_df, pre_periods, post_periods, balance_e)
tot_wrvu_2013_full <- get_cs_did("tot_wrvu", "wRVUs (2013) per PCP", md_df, pre_periods, post_periods, balance_e)
tot_benes_full <- get_cs_did("tot_benes", "Beneficiaries per PCP", md_df, pre_periods, post_periods, balance_e)

# Services 
avg_services <- mean(md_df$tot_services, na.rm = TRUE)
avg_services_high <- mean(md_df$tot_services[md_df$share_quartile == 4], na.rm = TRUE)
avg_services_high_state <- mean(md_df$tot_services[md_df$share_quartile_state == 4], na.rm = TRUE)
avg_services_low <- mean(md_df$tot_services[md_df$share_quartile == 1], na.rm = TRUE)
avg_services_low_state <- mean(md_df$tot_services[md_df$share_quartile_state == 1], na.rm = TRUE)
rows <- tribble(
  ~term,            ~full,     ~q1,      ~q4, ~full_state,     ~q1_state,      ~q4_state, 
  'Mean outcome',   avg_services, avg_services_low, avg_services_high,   avg_services, avg_services_low_state, avg_services_high_state)

models <- list("Full" = tot_services_full, 
               "Q1" = tot_services_q1, 
               "Q4" = tot_services_q4, 
               "Full" = tot_services_full,
               "Q1" = tot_services_q1_state,
               "Q4" = tot_services_q4_state)
converted_models <- convert_models(models)

res <- modelsummary(converted_models, 
                    stars = c('*' = .1, '**' = .05, '***' = 0.01),
                    gof_map = gof_map,
                    add_rows = rows,
                    output = "gt")
res <- res %>% 
  tab_header(title = md("Services per PCP")) %>% 
  tab_spanner(label = "National quartiles", columns = c(2:4)) %>%
  tab_spanner(label = "State quartiles", columns = c(5:7)) %>% 
  tab_source_note(source_note = "SE clustered at the state level.") %>% 
  opt_horizontal_padding(scale = 3)

gtsave(res, "results/did/service-volume/pcp_services.html")
gtsave(res, "results/did/service-volume/pcp_services.tex")

# wRVUs 
avg_services <- mean(md_df$tot_wrvu_2013, na.rm = TRUE)
avg_services_high <- mean(md_df$tot_wrvu_2013[md_df$share_quartile == 4], na.rm = TRUE)
avg_services_high_state <- mean(md_df$tot_wrvu_2013[md_df$share_quartile_state == 4], na.rm = TRUE)
avg_services_low <- mean(md_df$tot_wrvu_2013[md_df$share_quartile == 1], na.rm = TRUE)
avg_services_low_state <- mean(md_df$tot_wrvu_2013[md_df$share_quartile_state == 1], na.rm = TRUE)
rows <- tribble(
  ~term,            ~full,     ~q1,      ~q4, ~full_state,     ~q1_state,      ~q4_state, 
  'Mean outcome',   avg_services, avg_services_low, avg_services_high,   avg_services, avg_services_low_state, avg_services_high_state)

models <- list("Full" = tot_wrvu_2013_full, 
               "Q1" = tot_wrvu_2013_q1, 
               "Q4" = tot_wrvu_2013_q4, 
               "Full" = tot_wrvu_2013_full,
               "Q1" = tot_wrvu_2013_q1_state,
               "Q4" = tot_wrvu_2013_q4_state)
converted_models <- convert_models(models)

res <- modelsummary(converted_models, 
                    stars = c('*' = .1, '**' = .05, '***' = 0.01),
                    gof_map = gof_map,
                    add_rows = rows,
                    output = "gt")
res <- res %>% 
  tab_header(title = md("wRVUs (2013) per PCP")) %>% 
  tab_spanner(label = "National quartiles", columns = c(2:4)) %>%
  tab_spanner(label = "State quartiles", columns = c(5:7)) %>% 
  tab_source_note(source_note = "SE clustered at the state level.") %>% 
  opt_horizontal_padding(scale = 3)

gtsave(res, "results/did/service-volume/pcp_wrvus.html")
gtsave(res, "results/did/service-volume/pcp_wrvus.tex")

# Beneficiaries
avg_services <- mean(md_df$tot_benes, na.rm = TRUE)
avg_services_high <- mean(md_df$tot_benes[md_df$share_quartile == 4], na.rm = TRUE)
avg_services_high_state <- mean(md_df$tot_benes[md_df$share_quartile_state == 4], na.rm = TRUE)
avg_services_low <- mean(md_df$tot_benes[md_df$share_quartile == 1], na.rm = TRUE)
avg_services_low_state <- mean(md_df$tot_benes[md_df$share_quartile_state == 1], na.rm = TRUE)
rows <- tribble(
  ~term,            ~full,     ~q1,      ~q4, ~full_state,     ~q1_state,      ~q4_state, 
  'Mean outcome',   avg_services, avg_services_low, avg_services_high,   avg_services, avg_services_low_state, avg_services_high_state)

models <- list("Full" = tot_benes_full, 
               "Q1" = tot_benes_q1, 
               "Q4" = tot_benes_q4, 
               "Full" = tot_benes_full,
               "Q1" = tot_benes_q1_state,
               "Q4" = tot_benes_q4_state)
converted_models <- convert_models(models)

res <- modelsummary(converted_models, 
                    stars = c('*' = .1, '**' = .05, '***' = 0.01),
                    gof_map = gof_map,
                    add_rows = rows,
                    output = "gt")
res <- res %>% 
  tab_header(title = md("Beneficiaries per PCP")) %>% 
  tab_spanner(label = "National quartiles", columns = c(2:4)) %>%
  tab_spanner(label = "State quartiles", columns = c(5:7)) %>% 
  tab_source_note(source_note = "SE clustered at the state level.") %>% 
  opt_horizontal_padding(scale = 3)

gtsave(res, "results/did/service-volume/pcp_benes.html")
gtsave(res, "results/did/service-volume/pcp_benes.tex")
