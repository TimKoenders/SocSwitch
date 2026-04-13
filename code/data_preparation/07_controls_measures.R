#### 07_controls_measures.R ---------------------------------------------------
#### Clean-up -----------------------------------------------------------------
rm(list = ls())

#### Load packages and helper functions ---------------------------------------
source(here::here("code", "utils", "packages.R"))
load_packages()

source(here::here("code", "utils", "helper_functions.R"))

#### Load data ----------------------------------------------------------------
positions_party_election <- readRDS(
  here::here("data", "processed", "positions_party_election.rds")
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
  dplyr::select(elec_id, country, year, month, election_date) %>%
  dplyr::distinct()

#### Harmonize manifesto data to election IDs ---------------------------------
positions_party_election <- positions_party_election %>%
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
    ),
    month = lubridate::month(edate),
    elec_id = sprintf("%s-%04d-%02d", country_code, election_year, month)
  ) %>%
  dplyr::filter(!is.na(country_code))

#### Keep only elections in final analysis universe ---------------------------
positions_party_election <- positions_party_election %>%
  dplyr::semi_join(df_9x9_wide, by = "elec_id")

#### Family-election control file ---------------------------------------------
# Aggregate manifesto information to the election-by-party-family level.
# For each family in each election:
# 1. family_vote_share = total vote share of all parties in that family
# 2. family_stateconomy = vote-share-weighted mean state economy position

family_election_controls_long <- positions_party_election %>%
  dplyr::group_by(country_code, elec_id, edate, election_year, parfam) %>%
  dplyr::summarise(
    family_vote_share = sum(pervote, na.rm = TRUE),
    family_stateconomy = stats::weighted.mean(
      stateconomy,
      w = pervote,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  dplyr::arrange(country_code, parfam, edate)

#### Lagged family vote shares ------------------------------------------------
family_election_controls_long <- family_election_controls_long %>%
  dplyr::group_by(country_code, parfam) %>%
  dplyr::arrange(edate, .by_group = TRUE) %>%
  dplyr::mutate(
    family_vote_share_lag = dplyr::lag(family_vote_share, 1)
  ) %>%
  dplyr::ungroup()

#### Effective number of parties ----------------------------------------------
enp_election <- positions_party_election %>%
  dplyr::group_by(elec_id) %>%
  dplyr::summarise(
    enp = 1 / sum((pervote / 100)^2, na.rm = TRUE),
    .groups = "drop"
  )

#### Wide family-level controls -----------------------------------------------
# Keep party-family controls in wide form using MPDS parfam codes.
# This avoids imposing a second family mapping at this stage.

controls_vote_share_wide <- family_election_controls_long %>%
  dplyr::select(elec_id, parfam, family_vote_share_lag) %>%
  dplyr::mutate(
    control = paste0("lag_vote_share_parfam_", parfam)
  ) %>%
  dplyr::select(-parfam) %>%
  tidyr::pivot_wider(
    names_from = control,
    values_from = family_vote_share_lag
  )

controls_position_wide <- family_election_controls_long %>%
  dplyr::select(elec_id, parfam, family_stateconomy) %>%
  dplyr::mutate(
    control = paste0("stateconomy_parfam_", parfam)
  ) %>%
  dplyr::select(-parfam) %>%
  tidyr::pivot_wider(
    names_from = control,
    values_from = family_stateconomy
  )

#### Merge all controls to election level -------------------------------------
controls_election <- df_9x9_wide %>%
  dplyr::select(elec_id, country, year, month, election_date) %>%
  dplyr::distinct() %>%
  dplyr::left_join(enp_election, by = "elec_id") %>%
  dplyr::left_join(controls_vote_share_wide, by = "elec_id") %>%
  dplyr::left_join(controls_position_wide, by = "elec_id")

#### Check uniqueness ---------------------------------------------------------
stopifnot(
  nrow(df_9x9_wide) == dplyr::n_distinct(df_9x9_wide$elec_id),
  nrow(enp_election) == dplyr::n_distinct(enp_election$elec_id),
  nrow(controls_election) == dplyr::n_distinct(controls_election$elec_id)
)

#### Summary statistics -------------------------------------------------------
controls_summary <- controls_election %>%
  dplyr::summarise(
    n_elections = dplyr::n_distinct(elec_id),
    n_countries = dplyr::n_distinct(country),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    n_with_enp = sum(!is.na(enp)),
    mean_enp = mean(enp, na.rm = TRUE),
    min_enp = min(enp, na.rm = TRUE),
    max_enp = max(enp, na.rm = TRUE)
  )

controls_summary

cat(
  "Control file contains",
  controls_summary$n_elections,
  "elections in",
  controls_summary$n_countries,
  "countries, covering",
  controls_summary$min_year,
  "to",
  controls_summary$max_year,
  ".\n"
)

cat(
  "Effective number of parties is observed for",
  controls_summary$n_with_enp,
  "elections, with values ranging from",
  round(controls_summary$min_enp, 2),
  "to",
  round(controls_summary$max_enp, 2),
  ".\n"
)

#### Save ---------------------------------------------------------------------
saveRDS(
  family_election_controls_long,
  here::here("data", "processed", "family_election_controls_long.rds")
)

saveRDS(
  controls_election,
  here::here("data", "processed", "controls_election.rds")
)