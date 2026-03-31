#### 02_mapping_to_families.R --------------------------------------------------------

#### Clean-up -----------------------------------------------------------------
rm(list = ls())
source(here::here("code", "utils", "packages.R"))
load_packages()
source(here::here("code", "utils", "helper_functions.R"))
?voteswitchR

#### Load prepared data -------------------------------------------------------
best_raked_imp <- readRDS(
  here::here("data", "processed", "best_raked_imp_fam.rds")
)
mappings <- readRDS(
  here::here("data", "processed", "mappings.rds")
)

#### Country / Year / Election Coverage --------------------------------------
# List of contexts
best_raked_imp %>%
  dplyr::mutate(
    country = substr(elec_id, 1, 2),
    year = substr(elec_id, 4, 7)
  ) %>%
  dplyr::distinct(country, year) %>%
  dplyr::arrange(country, year) %>%
  print(n = Inf)

# Summary for coverage
best_raked_imp %>%
  dplyr::mutate(
    country = stringr::str_extract(elec_id, "^[A-Z]{2}"),
    year = as.numeric(stringr::str_extract(elec_id, "\\d{4}"))
  ) %>%
  dplyr::summarise(
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    n_elections = dplyr::n_distinct(elec_id),
    n_countries = dplyr::n_distinct(country)
  )


#### Aggregate into 6 party family scheme -------------------------------------

best_raked_imp_fam6 <- best_raked_imp %>%
  dplyr::left_join(
    mappings %>%
      dplyr::select(elec_id, stack, parfam_from = parfam),
    by = c("elec_id", "switch_from" = "stack")
  ) %>%
  dplyr::left_join(
    mappings %>%
      dplyr::select(elec_id, stack, parfam_to = parfam),
    by = c("elec_id", "switch_to" = "stack")
  ) %>%
  dplyr::mutate(
    fam6_from = dplyr::case_when(
      parfam_harmonized_from == "soc" ~ "social_democratic",
      parfam_harmonized_from == "lef" ~ "far_left",
      parfam_harmonized_from == "nat" ~ "far_right",
      parfam_harmonized_from == "eco" ~ "green",
      !is.na(parfam_from) & stringr::str_detect(tolower(parfam_from), "lib|chr|christ") ~ "mainstream_right",
      parfam_harmonized_from %in% c("oth", "non") ~ "other",
      TRUE ~ "other"
    ),
    fam6_to = dplyr::case_when(
      parfam_harmonized_to == "soc" ~ "social_democratic",
      parfam_harmonized_to == "lef" ~ "far_left",
      parfam_harmonized_to == "nat" ~ "far_right",
      parfam_harmonized_to == "eco" ~ "green",
      !is.na(parfam_to) & stringr::str_detect(tolower(parfam_to), "lib|chr|christ") ~ "mainstream_right",
      parfam_harmonized_to %in% c("oth", "non") ~ "other",
      TRUE ~ "other"
    )
  )


