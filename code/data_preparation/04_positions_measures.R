#### 04_positions_measures.R --------------------------------------------------
#### Clean-up -----------------------------------------------------------------
rm(list = ls())

#### Load packages and helper functions ---------------------------------------
source(here::here("code", "utils", "packages.R"))
load_packages()

source(here::here("code", "utils", "helper_functions.R"))

#### Paths --------------------------------------------------------------------
proc_file <- here::here("data", "manifesto", "MPDataset_MPDS2025a_stata14.dta")

#### Load data ----------------------------------------------------------------
manifesto <- haven::read_dta(proc_file)

#### Construct Lowe-style state economy position ------------------------------
# Interventionist side:
# per403 + per404 + per406 + per412 + per413 + per504 + per506 + per701
# Market-oriented side:
# per401 + per402 + per407 + per414 + per505

manifesto <- manifesto %>%
  dplyr::mutate(
    state_left_share  = per403 + per404 + per406 + per412 + per413 + per504 + per506 + per701,
    state_right_share = per401 + per402 + per407 + per414 + per505,
    state_left_n      = total * state_left_share / 100,
    state_right_n     = total * state_right_share / 100,
    stateconomy       = log((state_right_n + 0.5) / (state_left_n + 0.5))
  )

#### Compute party movement ---------------------------------------------------
# For election t:
# movement_current = change between t-1 and t
# movement_lagged  = change between t-2 and t-1

manifesto <- manifesto %>%
  dplyr::arrange(party, edate) %>%
  dplyr::group_by(party) %>%
  dplyr::mutate(
    movement_current = stateconomy - dplyr::lag(stateconomy, 1),
    movement_lagged  = dplyr::lag(stateconomy, 1) - dplyr::lag(stateconomy, 2)
  ) %>%
  dplyr::ungroup()

#### Construct party-election file for all parties -----------------------------
# Keep all parties because the analysis is conducted at the party-election
# level and because the positions of competing party families are needed
# for downstream controls.

positions_party_election <- manifesto %>%
  dplyr::transmute(
    country,
    countryname,
    party,
    partyname,
    parfam,
    edate,
    election_year = lubridate::year(edate),
    stateconomy,
    movement_current,
    movement_lagged,
    moderation_current = movement_current,
    moderation_lagged  = movement_lagged,
    pervote
  ) %>%
  dplyr::mutate(
    moderation_current_std = as.numeric(scale(moderation_current)),
    moderation_lagged_std  = as.numeric(scale(moderation_lagged))
  ) %>%
  dplyr::arrange(countryname, edate, partyname)

#### Convenience subset: social democratic parties -----------------------------
# In MPDS, parfam == 30 denotes social democratic parties.

positions_sd_party_election <- positions_party_election %>%
  dplyr::filter(parfam == 30) %>%
  dplyr::arrange(countryname, edate, partyname)

#### Summary statistics -------------------------------------------------------
positions_summary <- positions_party_election %>%
  dplyr::summarise(
    n_party_elections = dplyr::n(),
    n_parties = dplyr::n_distinct(party),
    n_countries = dplyr::n_distinct(country),
    min_year = min(election_year, na.rm = TRUE),
    max_year = max(election_year, na.rm = TRUE)
  )

cat(
  "Party-positioning file contains",
  positions_summary$n_party_elections,
  "party-elections from",
  positions_summary$n_parties,
  "parties in",
  positions_summary$n_countries,
  "countries, covering",
  positions_summary$min_year,
  "to",
  positions_summary$max_year,
  ".\n"
)

sd_summary <- positions_sd_party_election %>%
  dplyr::summarise(
    n_party_elections = dplyr::n(),
    n_parties = dplyr::n_distinct(party),
    n_countries = dplyr::n_distinct(country),
    min_year = min(election_year, na.rm = TRUE),
    max_year = max(election_year, na.rm = TRUE)
  )

cat(
  "Social democratic subset contains",
  sd_summary$n_party_elections,
  "party-elections from",
  sd_summary$n_parties,
  "parties in",
  sd_summary$n_countries,
  "countries, covering",
  sd_summary$min_year,
  "to",
  sd_summary$max_year,
  ".\n"
)

#### Save ---------------------------------------------------------------------
saveRDS(
  positions_party_election,
  here::here("data", "processed", "positions_party_election.rds")
)

saveRDS(
  positions_sd_party_election,
  here::here("data", "processed", "positions_sd_party_election.rds")
)