#### 06_generalized_9x9_data.R ------------------------------------------------

#### Clean-up -----------------------------------------------------------------
rm(list = ls())

source(here::here("code", "utils", "packages.R"))
load_packages()

source(here::here("code", "utils", "helper_functions.R"))

#### Load prepared data -------------------------------------------------------
best_raked_imp <- readRDS(
  here::here("data", "processed", "best_raked_imp_fam.rds")
)

#### Prepare election identifiers ---------------------------------------------
df_switches <- best_raked_imp %>%
  dplyr::mutate(
    switch_from = as.numeric(switch_from),
    switch_to   = as.numeric(switch_to),
    country_raw = stringr::str_extract(elec_id, "^[^-]+(?:-[^-]+)?"),
    country = dplyr::case_when(
      country_raw == "DNK"   ~ "DK",
      country_raw == "BE-VL" ~ "BE",
      country_raw == "BE-WA" ~ "BE",
      TRUE ~ country_raw),
    year = as.numeric(stringr::str_extract(elec_id, "\\d{4}"))
  )

#### Recode into generalized 9-category family scheme -------------------------
# Uses the harmonized family variables already present in best_raked_imp
df_switches <- df_switches %>%
  dplyr::mutate(
    fam_from = dplyr::case_when(
      switch_from == 98 ~ "other",
      switch_from == 99 ~ "non_voting",
      parfam_harmonized_from == "soc" ~ "social_democratic",
      parfam_harmonized_from == "lef" ~ "far_left",
      parfam_harmonized_from == "nat" ~ "far_right",
      parfam_harmonized_from == "eco" ~ "green",
      parfam_harmonized_from == "lib" ~ "liberal",
      parfam_harmonized_from == "chr" ~ "christian_democratic",
      parfam_harmonized_from == "con" ~ "conservative",
      TRUE ~ "other"
    ),
    fam_to = dplyr::case_when(
      switch_to == 98 ~ "other",
      switch_to == 99 ~ "non_voting",
      parfam_harmonized_to == "soc" ~ "social_democratic",
      parfam_harmonized_to == "lef" ~ "far_left",
      parfam_harmonized_to == "nat" ~ "far_right",
      parfam_harmonized_to == "eco" ~ "green",
      parfam_harmonized_to == "lib" ~ "liberal",
      parfam_harmonized_to == "chr" ~ "christian_democratic",
      parfam_harmonized_to == "con" ~ "conservative",
      TRUE ~ "other"
    )
  )

#### Define family order ------------------------------------------------------
fam_levels <- c(
  "social_democratic",
  "far_left",
  "green",
  "liberal",
  "christian_democratic",
  "conservative",
  "far_right",
  "other",
  "non_voting"
)

#### Quick diagnostics --------------------------------------------------------
message("Coverage of generalized family coding:")
df_switches %>%
  dplyr::count(fam_from, fam_to, wt = weights, sort = TRUE) %>%
  print(n = 30)

message("Number of elections:")
print(dplyr::n_distinct(df_switches$elec_id))

#### Aggregate to election-specific 9x9 matrices ------------------------------
df_9x9_long <- df_switches %>%
  dplyr::mutate(
    fam_from = factor(fam_from, levels = fam_levels),
    fam_to   = factor(fam_to, levels = fam_levels)
  ) %>%
  dplyr::group_by(elec_id, country, year, fam_from, fam_to) %>%
  dplyr::summarise(
    weights = sum(weights, na.rm = TRUE),
    .groups = "drop"
  )

#### Ensure all 81 cells exist for every election -----------------------------
all_elections <- df_switches %>%
  dplyr::distinct(elec_id, country, year)

all_cells <- tidyr::expand_grid(
  fam_from = factor(fam_levels, levels = fam_levels),
  fam_to   = factor(fam_levels, levels = fam_levels)
)

df_9x9_long_complete <- all_elections %>%
  tidyr::crossing(all_cells) %>%
  dplyr::left_join(
    df_9x9_long,
    by = c("elec_id", "country", "year", "fam_from", "fam_to")
  ) %>%
  dplyr::mutate(
    weights = dplyr::coalesce(weights, 0)
  ) %>%
  dplyr::arrange(country, year, elec_id, fam_from, fam_to)

#### Construct outcome names --------------------------------------------------
df_9x9_long_complete <- df_9x9_long_complete %>%
  dplyr::mutate(
    y_name = paste0(as.character(fam_from), "_to_", as.character(fam_to))
  )

y_names_9x9 <- tidyr::expand_grid(
  fam_from = fam_levels,
  fam_to   = fam_levels
) %>%
  dplyr::mutate(
    y_name = paste0(fam_from, "_to_", fam_to)
  ) %>%
  dplyr::pull(y_name)

#### Wide election-level dependent-variable data ------------------------------
df_9x9_wide <- df_9x9_long_complete %>%
  dplyr::select(elec_id, country, year, y_name, weights) %>%
  tidyr::pivot_wider(
    names_from = y_name,
    values_from = weights,
    values_fill = 0
  ) %>%
  dplyr::arrange(country, year, elec_id)

#### Checks -------------------------------------------------------------------
message("Checking that all 81 outcome columns exist...")
stopifnot(all(y_names_9x9 %in% names(df_9x9_wide)))

message("Checking number of rows equals number of elections...")
stopifnot(nrow(df_9x9_wide) == dplyr::n_distinct(df_switches$elec_id))

check_totals <- df_9x9_wide %>%
  dplyr::mutate(
    total_weight = rowSums(dplyr::across(dplyr::all_of(y_names_9x9)))
  ) %>%
  dplyr::select(elec_id, country, year, total_weight)

message("Summary of row totals across 81 cells:")
print(summary(check_totals$total_weight))

message("First few row totals:")
print(check_totals, n = 10)

message("Example of first rows of wide data:")
print(
  df_9x9_wide %>%
    dplyr::select(elec_id, country, year, dplyr::all_of(y_names_9x9[1:12])),
  n = 5,
  width = Inf
)

#### Save ---------------------------------------------------------------------
saveRDS(
  df_9x9_long_complete,
  here::here("data", "processed", "generalized_9x9_long.rds")
)

saveRDS(
  df_9x9_wide,
  here::here("data", "processed", "generalized_9x9_wide.rds")
)

saveRDS(
  y_names_9x9,
  here::here("data", "processed", "generalized_9x9_y_names.rds")
)

message("Saved:")
message("- generalized_9x9_long.rds")
message("- generalized_9x9_wide.rds")
message("- generalized_9x9_y_names.rds")