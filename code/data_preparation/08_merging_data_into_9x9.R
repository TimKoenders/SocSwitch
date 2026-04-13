#### 08_merging_data_into_9x9.R -----------------------------------------------

#### Clean-up -----------------------------------------------------------------
rm(list = ls())

#### Load packages and helper functions ---------------------------------------
source(here::here("code", "utils", "packages.R"))
load_packages()

source(here::here("code", "utils", "helper_functions.R"))

#### Load dependent-variable data ---------------------------------------------
y_names_9x9 <- readRDS(
  here::here("data", "processed", "generalized_9x9_y_names.rds")
)

df_9x9_wide <- readRDS(
  here::here("data", "processed", "generalized_9x9_wide.rds")
) %>%
  dplyr::mutate(
    country_raw = stringr::str_extract(elec_id, "^[^-]+"),
    country = dplyr::case_when(
      country_raw == "DNK"   ~ "DK",
      country_raw == "BE-VL" ~ "BE",
      country_raw == "BE-WA" ~ "BE",
      TRUE ~ country_raw
    ),
    year = as.integer(stringr::str_extract(elec_id, "(?<=-)(\\d{4})(?=-\\d{2}$)")),
    month = as.integer(stringr::str_extract(elec_id, "(?<=\\d{4}-)\\d{2}$")),
    election_date = as.Date(sprintf("%04d-%02d-15", year, month))
  ) %>%
  dplyr::select(-country_raw)

#### Load independent-variable data -------------------------------------------
ess_polarization_election <- readRDS(
  here::here("data", "processed", "ess_polarization_election.rds")
)

positions_sd_party_election <- readRDS(
  here::here("data", "processed", "positions_sd_party_election.rds")
) %>%
  dplyr::mutate(
    country_code = dplyr::case_when(
      countryname == "Austria" ~ "AT",
      countryname == "Australia" ~ "AU",
      countryname == "Belgium" ~ "BE",
      countryname == "Canada" ~ "CA",
      countryname == "Switzerland" ~ "CH",
      countryname == "Germany" ~ "DE",
      countryname == "Denmark" ~ "DK",
      countryname == "Spain" ~ "ES",
      countryname == "Finland" ~ "FI",
      countryname == "France" ~ "FR",
      countryname == "United Kingdom" ~ "GB",
      countryname == "Greece" ~ "GR",
      countryname == "Ireland" ~ "IE",
      countryname == "Iceland" ~ "IS",
      countryname == "Italy" ~ "IT",
      countryname == "Japan" ~ "JP",
      countryname == "Luxembourg" ~ "LU",
      countryname == "Netherlands" ~ "NL",
      countryname == "New Zealand" ~ "NZ",
      countryname == "Norway" ~ "NO",
      countryname == "Portugal" ~ "PT",
      countryname == "Sweden" ~ "SE",
      countryname == "United States" ~ "US",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(country_code)) %>%
  dplyr::mutate(
    month = lubridate::month(edate),
    elec_id = sprintf("%s-%04d-%02d", country_code, election_year, month)
  )

controls_election <- readRDS(
  here::here("data", "processed", "controls_election.rds")
)

eb_salience_country <- readRDS(
  here::here("data", "processed", "eb_salience_country.rds")
) %>%
  dplyr::mutate(
    country = dplyr::case_when(
      country == "DNK"   ~ "DK",
      country == "BE-VL" ~ "BE",
      country == "BE-WA" ~ "BE",
      TRUE ~ country
    )
  )

#### Construct election-level salience matches --------------------------------
eb_salience_election <- df_9x9_wide %>%
  dplyr::select(elec_id, country, election_date) %>%
  dplyr::distinct() %>%
  dplyr::left_join(
    eb_salience_country,
    by = "country",
    relationship = "many-to-many"
  ) %>%
  dplyr::filter(wave_date <= election_date) %>%
  dplyr::group_by(elec_id) %>%
  dplyr::slice_max(order_by = wave_date, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    salience_match_id = sprintf("%s_%s", country, format(wave_date, "%Y%m%d"))
  ) %>%
  dplyr::select(
    elec_id,
    za,
    eb_wave,
    wave_date,
    immigration,
    unemployment,
    environment_climate,
    salience_match_id
  )

#### Check uniqueness of merge keys -------------------------------------------
stopifnot(
  nrow(df_9x9_wide) == dplyr::n_distinct(df_9x9_wide$elec_id),
  nrow(ess_polarization_election) == dplyr::n_distinct(ess_polarization_election$elec_id),
  nrow(controls_election) == dplyr::n_distinct(controls_election$elec_id),
  nrow(eb_salience_election) == dplyr::n_distinct(eb_salience_election$elec_id)
)

#### Inspect match coverage: polarization -------------------------------------
matched_polarization <- dplyr::semi_join(
  df_9x9_wide,
  ess_polarization_election,
  by = "elec_id"
)

matched_polarization_summary <- matched_polarization %>%
  dplyr::summarise(
    n_elections = dplyr::n_distinct(elec_id),
    n_countries = dplyr::n_distinct(country),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE)
  )

cat(
  "Polarization matched",
  matched_polarization_summary$n_elections,
  "elections in",
  matched_polarization_summary$n_countries,
  "countries, covering",
  matched_polarization_summary$min_year,
  "to",
  matched_polarization_summary$max_year,
  ".\n"
)

#### Inspect match coverage: SD party positioning -----------------------------
matched_positions <- dplyr::semi_join(
  df_9x9_wide,
  positions_sd_party_election %>% dplyr::distinct(elec_id),
  by = "elec_id"
)

matched_positions_summary <- matched_positions %>%
  dplyr::summarise(
    n_elections = dplyr::n_distinct(elec_id),
    n_countries = dplyr::n_distinct(country),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE)
  )

cat(
  "Social democratic party positioning matched",
  matched_positions_summary$n_elections,
  "elections in",
  matched_positions_summary$n_countries,
  "countries, covering",
  matched_positions_summary$min_year,
  "to",
  matched_positions_summary$max_year,
  ".\n"
)

#### Inspect match coverage: issue salience -----------------------------------
matched_salience <- dplyr::semi_join(
  df_9x9_wide,
  eb_salience_election,
  by = "elec_id"
)

matched_salience_summary <- matched_salience %>%
  dplyr::summarise(
    n_elections = dplyr::n_distinct(elec_id),
    n_countries = dplyr::n_distinct(country),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE)
  )

cat(
  "Issue salience matched",
  matched_salience_summary$n_elections,
  "elections in",
  matched_salience_summary$n_countries,
  "countries, covering",
  matched_salience_summary$min_year,
  "to",
  matched_salience_summary$max_year,
  ".\n"
)

#### Inspect match coverage: controls -----------------------------------------
matched_controls <- dplyr::semi_join(
  df_9x9_wide,
  controls_election,
  by = "elec_id"
)

matched_controls_summary <- matched_controls %>%
  dplyr::summarise(
    n_elections = dplyr::n_distinct(elec_id),
    n_countries = dplyr::n_distinct(country),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE)
  )

cat(
  "Controls matched",
  matched_controls_summary$n_elections,
  "elections in",
  matched_controls_summary$n_countries,
  "countries, covering",
  matched_controls_summary$min_year,
  "to",
  matched_controls_summary$max_year,
  ".\n"
)

#### Merge independent variables into 9x9 data --------------------------------
df_9x9_model <- df_9x9_wide %>%
  dplyr::left_join(
    ess_polarization_election %>%
      dplyr::select(
        elec_id,
        essround,
        fieldwork_start,
        fieldwork_end,
        sd,
        sd_z,
        bc,
        bc_z,
        ess_match_id
      ),
    by = "elec_id"
  ) %>%
  dplyr::left_join(
    positions_sd_party_election %>%
      dplyr::select(
        elec_id,
        party,
        partyname,
        parfam,
        edate,
        stateconomy,
        movement_current,
        movement_lagged,
        moderation_current,
        moderation_lagged,
        moderation_current_std,
        moderation_lagged_std,
        pervote
      ),
    by = "elec_id"
  ) %>%
  dplyr::left_join(
    eb_salience_election %>%
      dplyr::select(
        elec_id,
        za,
        eb_wave,
        wave_date,
        immigration,
        unemployment,
        environment_climate,
        salience_match_id
      ),
    by = "elec_id"
  ) %>%
  dplyr::left_join(
    controls_election %>%
      dplyr::select(-country, -year, -month, -election_date),
    by = "elec_id"
  ) %>%
  dplyr::mutate(
    has_polarization = !is.na(sd_z),
    has_positions = !is.na(stateconomy),
    has_salience = !is.na(immigration),
    has_controls = !is.na(enp)
  )

#### Check merged data ---------------------------------------------------------
df_9x9_model %>%
  dplyr::count(has_polarization)

df_9x9_model %>%
  dplyr::count(has_positions)

df_9x9_model %>%
  dplyr::count(has_salience)

df_9x9_model %>%
  dplyr::count(has_controls)

df_9x9_model %>%
  dplyr::count(has_polarization, has_positions, has_salience, has_controls)

merged_summary <- df_9x9_model %>%
  dplyr::summarise(
    n_elections_total = dplyr::n_distinct(elec_id),
    n_countries_total = dplyr::n_distinct(country),
    min_year_total = min(year, na.rm = TRUE),
    max_year_total = max(year, na.rm = TRUE)
  )

polarization_summary <- df_9x9_model %>%
  dplyr::filter(has_polarization) %>%
  dplyr::summarise(
    n_elections = dplyr::n_distinct(elec_id),
    n_countries = dplyr::n_distinct(country),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE)
  )

positions_summary <- df_9x9_model %>%
  dplyr::filter(has_positions) %>%
  dplyr::summarise(
    n_elections = dplyr::n_distinct(elec_id),
    n_countries = dplyr::n_distinct(country),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE)
  )

salience_summary <- df_9x9_model %>%
  dplyr::filter(has_salience) %>%
  dplyr::summarise(
    n_elections = dplyr::n_distinct(elec_id),
    n_countries = dplyr::n_distinct(country),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE)
  )

controls_summary <- df_9x9_model %>%
  dplyr::filter(has_controls) %>%
  dplyr::summarise(
    n_elections = dplyr::n_distinct(elec_id),
    n_countries = dplyr::n_distinct(country),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE)
  )

both_summary <- df_9x9_model %>%
  dplyr::filter(has_polarization, has_positions) %>%
  dplyr::summarise(
    n_elections = dplyr::n_distinct(elec_id),
    n_countries = dplyr::n_distinct(country),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE)
  )

all_three_summary <- df_9x9_model %>%
  dplyr::filter(has_polarization, has_positions, has_salience) %>%
  dplyr::summarise(
    n_elections = dplyr::n_distinct(elec_id),
    n_countries = dplyr::n_distinct(country),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE)
  )

full_model_summary <- df_9x9_model %>%
  dplyr::filter(has_polarization, has_positions, has_salience, has_controls) %>%
  dplyr::summarise(
    n_elections = dplyr::n_distinct(elec_id),
    n_countries = dplyr::n_distinct(country),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE)
  )

cat(
  "Final 9x9 file contains",
  merged_summary$n_elections_total,
  "elections in",
  merged_summary$n_countries_total,
  "countries, covering",
  merged_summary$min_year_total,
  "to",
  merged_summary$max_year_total,
  ".\n"
)

cat(
  "Of these,",
  polarization_summary$n_elections,
  "elections in",
  polarization_summary$n_countries,
  "countries have matched polarization measures, covering",
  polarization_summary$min_year,
  "to",
  polarization_summary$max_year,
  ".\n"
)

cat(
  "Of these,",
  positions_summary$n_elections,
  "elections in",
  positions_summary$n_countries,
  "countries have matched social democratic party-positioning measures, covering",
  positions_summary$min_year,
  "to",
  positions_summary$max_year,
  ".\n"
)

cat(
  "Of these,",
  salience_summary$n_elections,
  "elections in",
  salience_summary$n_countries,
  "countries have matched issue-salience measures, covering",
  salience_summary$min_year,
  "to",
  salience_summary$max_year,
  ".\n"
)

cat(
  "Of these,",
  controls_summary$n_elections,
  "elections in",
  controls_summary$n_countries,
  "countries have matched control variables, covering",
  controls_summary$min_year,
  "to",
  controls_summary$max_year,
  ".\n"
)

cat(
  "Of these,",
  both_summary$n_elections,
  "elections in",
  both_summary$n_countries,
  "countries have both matched polarization and party-positioning measures, covering",
  both_summary$min_year,
  "to",
  both_summary$max_year,
  ".\n"
)

cat(
  "Of these,",
  all_three_summary$n_elections,
  "elections in",
  all_three_summary$n_countries,
  "countries have matched polarization, party-positioning, and issue-salience measures, covering",
  all_three_summary$min_year,
  "to",
  all_three_summary$max_year,
  ".\n"
)

cat(
  "Of these,",
  full_model_summary$n_elections,
  "elections in",
  full_model_summary$n_countries,
  "countries have matched polarization, party-positioning, issue-salience, and control variables, covering",
  full_model_summary$min_year,
  "to",
  full_model_summary$max_year,
  ".\n"
)

#### Keep useful column order -------------------------------------------------
df_9x9_model <- df_9x9_model %>%
  dplyr::select(
    elec_id,
    country,
    year,
    dplyr::all_of(y_names_9x9),
    has_polarization,
    has_positions,
    has_salience,
    has_controls,
    dplyr::everything()
  )

#### Save ---------------------------------------------------------------------
saveRDS(
  df_9x9_model,
  here::here("data", "processed", "generalized_9x9_model.rds")
)