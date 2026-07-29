# About ------------------------------------------------------------------------

# Clean NPPES
# Author:         Shirley Cai 
# Date created:   02/03/2026
# Last edited:    07/15/2026

# Goal: Export out MD vs DO credentials and specialty

# Taxonomy codes ---------------------------------------------------------------

years <- formatC(13:22, width = 2, flag = "0")

# Taxonomy codes
physician_codes <- "^20" 
aprn_codes <- "^(363L|364S|3675|367A)"

read_npi <- function(yr){
  if(yr == 13){
    file_str <- paste0('data/input/nppes/npi20', yr, '12.dta')
    raw <- read_dta(file_str, 
                    col_select = c(npi, entity, starts_with('ptaxcode'), starts_with('pprimtax')))
  } else if(yr >= 14 & yr <= 18){
    zip_str <- paste0('data/input/nppes/npi20', yr, '12.csv.zip')
    file_str <- paste0('npi20', yr, '12.csv')
    raw <- read_csv(unz(zip_str, file_str), 
                    col_select = c(npi, entity, starts_with('ptaxcode'), starts_with('pprimtax')))
  } else if(yr == 19){
    zip_str <- paste0('data/input/nppes/npi_20', yr, '12_csv.zip')
    file_str <- paste0('npi_20', yr, '12.csv')
    raw <- read_csv(unz(zip_str, file_str),
                    col_select = c(npi, entity, starts_with('ptaxcode'), starts_with('pprimtax')))
    raw <- raw %>% mutate(entity = case_when(entity == "Individual" ~ 1,
                                             entity == "Organization" ~ 2, 
                                             .default = NA),
                          entity = as.double(entity))
  } else{
    file_str <- paste0('data/input/nppes/npi20', yr, '12.parquet')
    raw <- read_parquet(file_str,
                        col_select = c(npi, entity, starts_with('ptaxcode'), starts_with('pprimtax')))
  }
  
  raw <- raw %>% 
    filter(entity == 1)
  
  # Pivot to get primary taxonomy
  raw <- raw %>% 
    pivot_longer(
      cols = matches("^(ptaxcode|pprimtax)\\d+$"),
      names_to = c(".value", "num"),
      names_pattern = "(ptaxcode|pprimtax)(\\d+)"
    ) %>% 
    filter(pprimtax == "Y") %>% 
    select(npi, ptaxcode)
  
  # Filter out non-phys, non-APRN codes
  raw <- raw %>% 
    filter(str_detect(ptaxcode, physician_codes) | str_detect(ptaxcode, aprn_codes))
  
  # Create year variable
  raw <- raw %>% mutate(year = as.numeric(paste0("20", yr)))
  
  return(raw)
}

raw <- lapply(years, read_npi)
ptax <- do.call(rbind.data.frame, raw)  

# Free up memory
rm(raw)
gc()

## Assign specialty indicators -------------------------------------------------

# Assign PCP or specialist based on NPPES taxonomy
#   PCP:  Internal medicine   207R00000X, 207RA0000X
#         General practice    208D00000X
#         Family medicine     207Q00000X 
#         General pediatrics  208000000X, 2080A0000X
#   Specialist: Everyone else 
ptax <- ptax %>%
  mutate(
    D_missing_spec = is.na(ptaxcode), 
    D_pcp = (ptaxcode == "207R00000X" | ptaxcode == "207RA0000X" | 
               ptaxcode == "208D00000X" | ptaxcode == "207Q00000X" | 
               ptaxcode == "208000000X" | ptaxcode == "2080A0000X"), 
    D_spec = !D_pcp
  )

# Assign APRN indicator based on NPPES taxonomy
ptax <- ptax %>% 
  mutate(
    D_aprn = grepl(aprn_codes, ptaxcode)
  )

## Ensure no duplicates --------------------------------------------------------

ptax <- ptax %>% distinct_at(vars(npi, year, D_pcp, D_aprn), .keep_all = TRUE)
ptax <- ptax %>% 
  arrange(npi, year, D_pcp) %>% 
  distinct(npi, year, .keep_all = TRUE)

ptax <- ptax %>% select(-c(D_missing_spec))

## Export taxonomy -------------------------------------------------------------

write_csv(ptax, 'data/output/nppes_taxonomy.csv')

# Credentials ------------------------------------------------------------------

core <- read_csv('data/input/nppes/core_202509.csv') %>% 
  clean_names() %>% 
  filter(entity == 'Individual') 
core <- core %>% select(npi, pcredential, pcredentialoth)

message(paste0('Total obs: ', nrow(core)))
message(paste0('Missing credentials: ', sum(is.na(core$pcredential))))

# Assign MD or DO using NPPES credentials 
core <- core %>% 
  mutate(pcredential = gsub("\\.| ", "", pcredential)) %>% 
  filter(!((grepl("DMD|DDS", pcredential) %>% replace_na(FALSE))))

core <- core %>%
  mutate(
    D_missing_cred = is.na(pcredential), 
    D_md = (grepl("MD", pcredential) | grepl("MEDICALDOCTOR", pcredential)),
    D_md = ifelse(D_missing_cred, NA, D_md), 
    D_do = grepl("DO", pcredential), 
    D_do = ifelse(D_missing_cred, NA, D_do)
  )

# Drop if not MD or DO or not missing
core <- core %>% filter(D_md | D_do | D_missing_cred)
core <- core %>% select(-c(D_missing_cred))

## Export credentials ----------------------------------------------------------

write_csv(core, 'data/output/nppes_credentials.csv')

# Export -----------------------------------------------------------------------

# Merge core and taxonomy
ptax <- ptax %>% mutate(npi = as.numeric(npi))
df <- ptax %>% left_join(core, by = 'npi')

write_csv(df, 'data/output/nppes.csv')
rm(list = ls())
