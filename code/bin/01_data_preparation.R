#### 01_data_preparation.R --------------------------------------------------------
#### Clean-up -----------------------------------------------------------------
rm(list = ls())

#### Load packages and helper functions ------------------------------------
source(here::here("code", "utils", "packages.R"))
load_packages()
source(here::here("code", "utils", "helper_functions.R"))
?voteswitchR

#### Load data ---------------------------------------------------------------
## Raw 
raw <- voteswitchR::switches

## Raw imputed (list of 5)
raw_imp <- voteswitchR::switches_imp

## Raked
raked <- voteswitchR::raked_switches

## Raked imputed (list of 5)
raked_imp <- voteswitchR::raked_switches_imp



#### Quality check ---------------------------------------------------------------
## Raw
me_raw <- voteswitchR::calculate_meas_error(raw, type = "mae")
err_raw <- mean(me_raw$elec_errors$mean_error_t, na.rm = TRUE)

## Raked
me_raked <- voteswitchR::calculate_meas_error(raked, type = "mae")
err_raked <- mean(me_raked$elec_errors$mean_error_t, na.rm = TRUE)

## Raw imputed
me_raw_imp <- lapply(raw_imp, function(x)
  voteswitchR::calculate_meas_error(x, type = "mae")
)

err_raw_imp <- mean(
  sapply(me_raw_imp, function(x)
    mean(x$elec_errors$mean_error_t, na.rm = TRUE))
)

## Raked imputed
me_raked_imp <- lapply(raked_imp, function(x)
  voteswitchR::calculate_meas_error(x, type = "mae")
)

err_raked_imp <- mean(
  sapply(me_raked_imp, function(x)
    mean(x$elec_errors$mean_error_t, na.rm = TRUE))
)

## Compare data quality
tibble::tibble(
  dataset = c("raw", "raw_imp", "raked", "raked_imp"),
  mean_mae = c(err_raw, err_raw_imp, err_raked, err_raked_imp)
)

## Select best raked imputation
mae_imp <- sapply(me_raked_imp, function(x)
  mean(x$elec_errors$mean_error_t, na.rm = TRUE)
)

best_imp <- which.min(mae_imp)
best_raked_imp <- raked_imp[[best_imp]]

## Drop unused objects
rm(
  raw, raw_imp, raked, raked_imp,
  me_raw, me_raked, me_raw_imp, me_raked_imp,
  mae_imp, best_imp
)


## MARPOR data
writeLines("ce92e7c5324c33fb32bb5860cff2eb82", "~/manifesto_api.key")
manifestoR::mp_setapikey("~/manifesto_api.key")
mp_main <- manifestoR::mp_maindataset()

#### Inspect raw data ---------------------------------------------------
names(voteswitchR::raked_switches)
names(voteswitchR::mappings)
head(voteswitchR::raked_switches$switch_from)
head(voteswitchR::mappings$peid)

voteswitchR::mappings %>%
  select(peid) %>%
  mutate(index = row_number()) %>%
  head()

#### Generate party-election-level raked transition matrices ---------------------
mappings_ext <- voteswitchR::mappings %>%
  mutate(
    parfam_use  = coalesce(parfam_harmonized, parfam),
    country_id  = as.integer(factor(iso2c)),
    election_id = as.integer(factor(elec_id)),
    party_group = case_when(
      parfam_use %in% c("mrp", "lib") ~ "mrp",
      parfam_use %in% c("nat") ~ "nat",
      TRUE ~ "Other"
    )
  )

model_data <- voteswitchR::raked_switches %>%
  recode_switches(
    mappings      = mappings_ext,
    switch_factor = "party_group",
    type          = "party-elections",
    assign_others = "Other"
  )

# Summarize country and year
unique_combinations <- unique(model_data[["data"]][, c("countryname", "year")])
unique_combinations <- unique_combinations[order(unique_combinations$countryname, unique_combinations$year), ]
unique_combinations

#### Add immigration position and shift -----------------------------------------
mp_main <- mp_main %>%
  mutate(year = year(edate))

mp_main_collapse <- mp_main %>%
  group_by(party, year, countryname) %>%
  summarise(
    per601_2 = mean(per601_2, na.rm = TRUE),
    per602_2 = mean(per602_2, na.rm = TRUE),
    per607   = mean(per607,   na.rm = TRUE),
    per608   = mean(per608,   na.rm = TRUE),
    rile     = mean(rile,     na.rm = TRUE),
    partyname = first(partyname),
    partyabbrev = first(partyabbrev),
    .groups = "drop"
  )

# === Full merged dataset before filtering ===
df_switching_full <- model_data$data %>%
  left_join(
    mp_main_collapse,
    by = c("party_harmonized" = "party", "year", "countryname")
  ) %>%
  group_by(countryname, party_harmonized) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    immigration_pos = per602_2 + per607,
    immigration_neg = per601_2 + per608,
    immigration_logit = log((immigration_pos + 0.5) / (immigration_neg + 0.5)),
    immigration_logit_lag = lag(immigration_logit),
    immigration_shift = immigration_logit - immigration_logit_lag
  ) %>%
  ungroup() %>%
  filter(!is.na(immigration_logit), !is.na(immigration_shift))

# === Subset for analysis (six-country sample) ===
df_switching <- df_switching_full %>%
  filter(countryname %in% c("Germany", "Austria", "Belgium", "Sweden", "Norway", "Netherlands"))

#### Add cordon sanitaire variable ----------------------------------------------
cordon_data <- tibble::tribble(
  ~countryname,   ~year, ~cordon,
  "Austria",      2013,  1,
  "Austria",      2017,  0,
  "Austria",      2019,  1,
  "Belgium",      2014,  1,
  "Belgium",      2019,  1,
  "Germany",      2017,  1,
  "Germany",      2021,  1,
  "Netherlands",  2010,  0,
  "Netherlands",  2012,  1,
  "Netherlands",  2017,  1,
  "Netherlands",  2021,  1,
  "Norway",       2013,  0,
  "Norway",       2017,  0,
  "Sweden",       2010,  1,
  "Sweden",       2014,  1,
  "Sweden",       2018,  1,
  "Sweden",       2022,  1
)

df_switching <- df_switching %>%
  left_join(cordon_data, by = c("countryname", "year"))

#### Create six-country (Europe) subset ----------------------------------------

six_countries <- c("Germany", "Austria", "Belgium", "Sweden", "Norway", "Netherlands")

df_switching_europe <- df_switching_full %>%
  dplyr::filter(countryname %in% six_countries)

model_data_europe <- list(
  data        = model_data$data %>%
    dplyr::filter(countryname %in% six_countries),
  y_names     = model_data$y_names,
  y_structure = model_data$y_structure)
  
  
#### Save processed data ------------------------------------------------------

fs::dir_create(here::here("data", "processed"))

saveRDS(model_data,          here::here("data", "processed", "model_data_full.rds"))   # volledige versie
saveRDS(df_switching_full,   here::here("data", "processed", "df_switching_full.rds"))
saveRDS(model_data,          here::here("data", "processed", "model_data.rds"))        # subset (6 landen)
saveRDS(df_switching,        here::here("data", "processed", "df_switching.rds"))
saveRDS(model_data_europe,   here::here("data", "processed", "model_data_europe.rds")) # 6 landen als 'Europe'
saveRDS(df_switching_europe, here::here("data", "processed", "df_switching_europe.rds"))