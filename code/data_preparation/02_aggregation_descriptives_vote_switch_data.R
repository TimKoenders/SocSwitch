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

#### Country / Year / Election Coverage --------------------------------------
# Unique list of election contexts
sort(unique(best_raked_imp$elec_id))

# Generate country and year indicator
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


#### Aggregate into party family scheme -------------------------------------
# Total sample
df_switches <- best_raked_imp %>%
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
    fam_from = dplyr::case_when(
      switch_from == 98 ~ "residual_parties",
      switch_from == 99 ~ "non_voting",
      parfam_harmonized_from == "soc" ~ "social_democratic",
      parfam_harmonized_from == "lef" ~ "far_left",
      parfam_harmonized_from == "nat" ~ "far_right",
      parfam_harmonized_from == "eco" ~ "green",
      parfam_harmonized_from == "lib" ~ "liberal",
      parfam_harmonized_from == "chr" ~ "christian_democratic",
      parfam_harmonized_from == "con" ~ "conservative",
      parfam_harmonized_from == "mrp" ~ "mrp",
      parfam_harmonized_from == "agr" ~ "agrarian",
      parfam_harmonized_from == "eth" ~ "ethnic_regional",
      parfam_harmonized_from == "sip" ~ "special_interest",
      parfam_harmonized_from == "oth" ~ "other",
      TRUE ~ "other"
    ),
    fam_to = dplyr::case_when(
      switch_to == 98 ~ "residual_parties",
      switch_to == 99 ~ "non_voting",
      parfam_harmonized_to == "soc" ~ "social_democratic",
      parfam_harmonized_to == "lef" ~ "far_left",
      parfam_harmonized_to == "nat" ~ "far_right",
      parfam_harmonized_to == "eco" ~ "green",
      parfam_harmonized_to == "lib" ~ "liberal",
      parfam_harmonized_to == "chr" ~ "christian_democratic",
      parfam_harmonized_to == "con" ~ "conservative",
      parfam_harmonized_to == "mrp" ~ "mrp",
      parfam_harmonized_to == "agr" ~ "agrarian",
      parfam_harmonized_to == "eth" ~ "ethnic_regional",
      parfam_harmonized_to == "sip" ~ "special_interest",
      parfam_harmonized_to == "oth" ~ "other",
      TRUE ~ "other"
    )
  )

#### Voter transition matrix table: Austria 2024 -------------------------------

party_order <- c("P3", "P2", "P1", "P5", "P4", "P6", "OTH", "NON")

df_raw <- best_raked_imp %>%
  dplyr::filter(elec_id == "AT-2024-09") %>%
  dplyr::mutate(
    from = dplyr::case_when(
      switch_from == 99 ~ "NON",
      switch_from %in% c(1, 2, 3, 4, 5, 6) ~ paste0("P", switch_from),
      TRUE ~ "OTH"
    ),
    to = dplyr::case_when(
      switch_to == 99 ~ "NON",
      switch_to %in% c(1, 2, 3, 4, 5, 6) ~ paste0("P", switch_to),
      TRUE ~ "OTH"
    ),
    from = factor(from, levels = party_order),
    to   = factor(to, levels = party_order)
  ) %>%
  dplyr::group_by(from, to) %>%
  dplyr::summarise(weight = sum(weights, na.rm = TRUE), .groups = "drop")

total_weight <- sum(df_raw$weight, na.rm = TRUE)

row_totals <- df_raw %>%
  dplyr::group_by(from) %>%
  dplyr::summarise(row_total = sum(weight), .groups = "drop") %>%
  dplyr::mutate(`2019` = 100 * row_total / total_weight)

col_totals <- df_raw %>%
  dplyr::group_by(to) %>%
  dplyr::summarise(col_total = sum(weight), .groups = "drop") %>%
  dplyr::mutate(value = 100 * col_total / total_weight)

df_cells <- df_raw %>%
  dplyr::mutate(pct = 100 * weight / total_weight)

mat <- df_cells %>%
  dplyr::select(from, to, pct) %>%
  tidyr::pivot_wider(
    names_from = to,
    values_from = pct,
    values_fill = 0
  ) %>%
  dplyr::left_join(
    row_totals %>%
      dplyr::select(from, `2019`),
    by = "from"
  ) %>%
  dplyr::arrange(from)

bottom_row <- col_totals %>%
  dplyr::select(to, value) %>%
  tidyr::pivot_wider(
    names_from = to,
    values_from = value
  ) %>%
  dplyr::mutate(from = "2024", `2019` = 100) %>%
  dplyr::select(from, dplyr::all_of(party_order), `2019`)

final_table <- dplyr::bind_rows(
  mat %>% dplyr::mutate(from = as.character(from)),
  bottom_row
) %>%
  dplyr::mutate(dplyr::across(-from, ~ round(.x, 1)))

label_map <- c(
  "P3" = "SPÖ",
  "P2" = "ÖVP",
  "P1" = "FPÖ",
  "P5" = "GRÜNE",
  "P4" = "NEOS",
  "P6" = "KPÖ",
  "OTH" = "OTH",
  "NON" = "NON"
)

final_table_named <- final_table %>%
  dplyr::mutate(from = dplyr::recode(from, !!!label_map)) %>%
  dplyr::rename_with(
    ~ dplyr::recode(.x, !!!label_map),
    .cols = -from
  )

final_table_named

#### Conceptual quantities ---------------------------------------------------

# Keep focal categories separate and collapse the rest into "other"
df_fam <- df_switches %>%
  dplyr::mutate(
    from = dplyr::case_when(
      fam_from %in% c(
        "social_democratic",
        "non_voting",
        "liberal",
        "christian_democratic",
        "conservative",
        "green",
        "far_left",
        "far_right"
      ) ~ fam_from,
      TRUE ~ "other"
    ),
    to = dplyr::case_when(
      fam_to %in% c(
        "social_democratic",
        "non_voting",
        "liberal",
        "christian_democratic",
        "conservative",
        "green",
        "far_left",
        "far_right"
      ) ~ fam_to,
      TRUE ~ "other"
    )
  )

# Total electorate
total_w <- sum(df_fam$weights)

# p_ij
df_p <- df_fam %>%
  dplyr::group_by(from, to) %>%
  dplyr::summarise(weights = sum(weights), .groups = "drop") %>%
  dplyr::mutate(p_ij = weights / total_w)

# Social democratic focal party
focal <- "social_democratic"

# Retention
R_sd <- df_p %>%
  dplyr::filter(from == focal, to == focal) %>%
  dplyr::pull(p_ij)

# Gross losses
L_sd <- df_p %>%
  dplyr::filter(from == focal, to != focal) %>%
  dplyr::transmute(
    j = to,
    L_ij = p_ij
  )

# Gross gains
G_sd <- df_p %>%
  dplyr::filter(to == focal, from != focal) %>%
  dplyr::transmute(
    j = from,
    G_ij = p_ij
  )

# Dyadic quantities
df_sd_dyad <- dplyr::full_join(L_sd, G_sd, by = "j") %>%
  dplyr::mutate(
    L_ij = dplyr::coalesce(L_ij, 0),
    G_ij = dplyr::coalesce(G_ij, 0),
    V_ij = L_ij + G_ij,
    B_ij = G_ij - L_ij
  ) %>%
  dplyr::arrange(dplyr::desc(V_ij))

R_sd
df_sd_dyad

#### Visualization ------------------------------------------------------------

# Add country and year identifiers
df_switches <- df_switches %>%
  dplyr::mutate(
    country = stringr::str_extract(elec_id, "^[A-Z]{2}"),
    year = as.numeric(stringr::str_extract(elec_id, "\\d{4}"))
  )

# Define regions
continental_europe <- c("AT", "BE", "CH", "DE", "FR", "NL")
nordic_europe      <- c("DK", "SE", "FI", "NO")
anglophone_europe  <- c("GB", "IE")
southern_europe    <- c("IT", "ES", "PT", "GR")

df_switches <- df_switches %>%
  dplyr::mutate(
    space = dplyr::case_when(
      country %in% continental_europe ~ "Continental Europe",
      country %in% nordic_europe ~ "Nordic Europe",
      country %in% anglophone_europe ~ "Anglophone Europe",
      country %in% southern_europe ~ "Southern Europe",
      TRUE ~ "Other"
    ),
    period = dplyr::case_when(
      !is.na(year) & year < 2000 ~ "Before 2000",
      !is.na(year) & year >= 2000 ~ "2000 and after",
      TRUE ~ NA_character_
    )
  )

# Helper
compute_sd_dyad <- function(data) {
  focal <- "social_democratic"
  
  df_fam_sub <- data %>%
    dplyr::mutate(
      from = dplyr::case_when(
        fam_from %in% c(
          "social_democratic",
          "non_voting",
          "liberal",
          "christian_democratic",
          "conservative",
          "green",
          "far_left",
          "far_right"
        ) ~ fam_from,
        TRUE ~ "other"
      ),
      to = dplyr::case_when(
        fam_to %in% c(
          "social_democratic",
          "non_voting",
          "liberal",
          "christian_democratic",
          "conservative",
          "green",
          "far_left",
          "far_right"
        ) ~ fam_to,
        TRUE ~ "other"
      )
    )
  
  total_w_sub <- sum(df_fam_sub$weights, na.rm = TRUE)
  
  df_p_sub <- df_fam_sub %>%
    dplyr::group_by(from, to) %>%
    dplyr::summarise(weights = sum(weights, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(p_ij = weights / total_w_sub)
  
  L_sd_sub <- df_p_sub %>%
    dplyr::filter(from == focal, to != focal) %>%
    dplyr::transmute(j = to, L_ij = p_ij)
  
  G_sd_sub <- df_p_sub %>%
    dplyr::filter(to == focal, from != focal) %>%
    dplyr::transmute(j = from, G_ij = p_ij)
  
  dplyr::full_join(L_sd_sub, G_sd_sub, by = "j") %>%
    dplyr::mutate(
      L_ij = dplyr::coalesce(L_ij, 0),
      G_ij = dplyr::coalesce(G_ij, 0),
      V_ij = L_ij + G_ij,
      B_ij = G_ij - L_ij
    ) %>%
    dplyr::filter(!(j %in% c("residual_parties", "other", "mrp")))
}

#### Pooled plot --------------------------------------------------------------

df_pooled_plot <- compute_sd_dyad(df_switches) %>%
  dplyr::arrange(dplyr::desc(abs(B_ij))) %>%
  dplyr::mutate(
    j = factor(j, levels = rev(j))
  )

ggplot(df_pooled_plot, aes(x = j, y = 100 * B_ij)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "",
    y = "Net balance (share of electorate)"
  )

#### Region plot --------------------------------------------------------------

df_space_plot <- df_switches %>%
  dplyr::filter(space != "Other") %>%
  dplyr::group_split(space) %>%
  purrr::map_dfr(function(x) {
    compute_sd_dyad(x) %>%
      dplyr::mutate(space = dplyr::first(x$space))
  }) %>%
  dplyr::mutate(
    j = tidytext::reorder_within(j, abs(B_ij), space),
    space = factor(
      space,
      levels = c(
        "Continental Europe",
        "Nordic Europe",
        "Anglophone Europe",
        "Southern Europe"
      )
    )
  )

ggplot(df_space_plot, aes(x = j, y = B_ij)) +
  geom_col() +
  coord_flip() +
  tidytext::scale_x_reordered() +
  facet_wrap(~ space, scales = "free_y", ncol = 2) +
  labs(
    x = "",
    y = "Net balance (share of electorate)",
    title = "",
    subtitle = ""
  )

#### Selected country plot ----------------------------------------------------

selected_countries <- c("AT", "DE", "NL", "DK")

country_labels <- c(
  "AT" = "Austria",
  "DE" = "Germany",
  "NL" = "Netherlands",
  "DK" = "Denmark"
)

# Prefer party abbreviations if available
if ("partyabbrev" %in% names(mappings)) {
  label_col <- "partyabbrev"
} else if ("party_abbrev" %in% names(mappings)) {
  label_col <- "party_abbrev"
} else if ("abbr" %in% names(mappings)) {
  label_col <- "abbr"
} else {
  stop("No party abbreviation column found in `mappings`.")
}

mappings_labels <- mappings %>%
  dplyr::transmute(
    elec_id,
    stack,
    party_label = .data[[label_col]]
  )

# Add party labels to switch data
df_switches_party <- df_switches %>%
  dplyr::filter(country %in% selected_countries) %>%
  dplyr::left_join(
    mappings_labels %>%
      dplyr::rename(party_from = party_label),
    by = c("elec_id", "switch_from" = "stack")
  ) %>%
  dplyr::left_join(
    mappings_labels %>%
      dplyr::rename(party_to = party_label),
    by = c("elec_id", "switch_to" = "stack")
  )

# Compute net exchanges between social democrats and all other categories
compute_sd_party_dyad <- function(data) {
  df_sub <- data %>%
    dplyr::mutate(
      from = dplyr::case_when(
        switch_from == 99 ~ "Non-voting",
        switch_from == 98 ~ "Other parties",
        fam_from == "social_democratic" ~ "Social democrats",
        TRUE ~ party_from
      ),
      to = dplyr::case_when(
        switch_to == 99 ~ "Non-voting",
        switch_to == 98 ~ "Other parties",
        fam_to == "social_democratic" ~ "Social democrats",
        TRUE ~ party_to
      )
    )
  
  total_w <- sum(df_sub$weights, na.rm = TRUE)
  
  if (is.na(total_w) || total_w == 0) {
    return(tibble::tibble())
  }
  
  df_p <- df_sub %>%
    dplyr::filter(!is.na(from), !is.na(to)) %>%
    dplyr::group_by(from, to) %>%
    dplyr::summarise(weights = sum(weights, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(p_ij = weights / total_w)
  
  L_sd <- df_p %>%
    dplyr::filter(from == "Social democrats", to != "Social democrats") %>%
    dplyr::transmute(j = to, L_ij = p_ij)
  
  G_sd <- df_p %>%
    dplyr::filter(to == "Social democrats", from != "Social democrats") %>%
    dplyr::transmute(j = from, G_ij = p_ij)
  
  dplyr::full_join(L_sd, G_sd, by = "j") %>%
    dplyr::mutate(
      L_ij = dplyr::coalesce(L_ij, 0),
      G_ij = dplyr::coalesce(G_ij, 0),
      B_ij = G_ij - L_ij
    ) %>%
    dplyr::filter(!is.na(j), j != "Other parties")
}

# Determine year coverage shown in facet labels
country_years <- df_switches %>%
  dplyr::filter(country %in% selected_countries) %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    country_label = paste0(
      dplyr::recode(country, !!!country_labels),
      " (", min_year, "–", max_year, ")"
    )
  ) %>%
  dplyr::select(country, country_label)

# Compute pooled country-level exchanges
df_country_plot <- df_switches_party %>%
  dplyr::group_by(country) %>%
  dplyr::group_modify(~ compute_sd_party_dyad(.x)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    j = dplyr::case_when(
      country == "DE" & j %in% c(
        "GRUENE", "GRÜNE", "GREENS", "Greens/Alliance'90",
        "Alliance 90/The Greens", "90/Greens", "Greens/90"
      ) ~ "GRÜNE",
      country == "DE" & j %in% c(
        "PDS", "L-PDS", "LINKE", "The Left",
        "The Left. Party of Democratic Socialism"
      ) ~ "LINKE",
      country == "DK" & j == "SF" ~ "Socialists",
      country == "DK" & j == "DF" ~ "People's Party",
      country == "DK" & j == "KF" ~ "Conservatives",
      country == "DK" & j %in% c("FP", "FrP") ~ "Progress",
      country == "DK" & j == "KrF" ~ "Christian Democrats",
      country == "DK" & j %in% c("NY", "Nye B.") ~ "Nye Borgerlige",
      country == "DK" & j %in% c("D", "Dem.") ~ "Denmark Democrats",
      country == "DK" & j %in% c("RF", "Rad. V.") ~ "Radikale Venstre",
      country == "NL" & j == "D'66" ~ "D66",
      TRUE ~ j
    )
  ) %>%
  dplyr::group_by(country, j) %>%
  dplyr::summarise(
    L_ij = sum(L_ij, na.rm = TRUE),
    G_ij = sum(G_ij, na.rm = TRUE),
    B_ij = sum(B_ij, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::filter(
    !(country == "AT" & j %in% c("PILZ", "BZÖ", "TS", "TEAM STRONACH")),
    !(country == "DE" & j %in% c("PDS", "L-PDS", "NPD", "REP")),
    !(country == "DK" & j %in% c(
      "ALT", "ALTERNATIV", "ALTERNATIVE",
      "NB", "NYE BORGERLIGE", "Nye Borgerlige",
      "RETSFORBUNDET", "RETFORBUNDET",
      "K", "KD", "KRISTDEMOKRATERNE",
      "DKP", "RV", "Radikale Venstre", "V", "LA",
      "KRF", "NA", "CD", "EL", "D", "Denmark Democrats",
      "FK", "VS"
    )),
    !(country == "NL" & j %in% c(
      "50PLUS", "CD", "U55+", "Unie 55+", "RPF", "LN", "GPV", "SGP",
      "PvdD", "LPF", "CU", "FvD"
    ))
  ) %>%
  dplyr::left_join(country_years, by = "country") %>%
  dplyr::mutate(
    country = factor(country, levels = selected_countries),
    country_label = factor(
      country_label,
      levels = country_years$country_label[
        match(selected_countries, country_years$country)
      ]
    )
  ) %>%
  dplyr::group_by(country_label) %>%
  dplyr::mutate(
    j = tidytext::reorder_within(j, abs(B_ij), country_label)
  ) %>%
  dplyr::ungroup()

# Plot
ggplot(df_country_plot, aes(x = j, y = 100 * B_ij)) +
  geom_col() +
  coord_flip() +
  tidytext::scale_x_reordered() +
  facet_wrap(~ country_label, scales = "free_y", ncol = 2) +
  labs(
    x = "",
    y = "Net balance (percentage points of electorate)",
    title = "",
    subtitle = ""
  )