#### 05_salience_measures.R ---------------------------------------------------
#### Clean-up -----------------------------------------------------------------
rm(list = ls())

#### Load packages ------------------------------------------------------------
source(here::here("code", "utils", "packages.R"))
load_packages()

#### Null-coalescing helper ---------------------------------------------------
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

#### Paths --------------------------------------------------------------------
eurobaro_path <- here::here("data", "eurobarometer")
output_path    <- here::here("data", "processed")

dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

#### List Eurobarometer files -------------------------------------------------
eb_files <- list.files(
  path = eurobaro_path,
  pattern = "\\.(dta|sav)$",
  recursive = TRUE,
  full.names = TRUE
)

#### File index ---------------------------------------------------------------
eb_index <- tibble::tibble(
  file = eb_files,
  folder = basename(dirname(eb_files)),
  filename = basename(eb_files),
  extension = stringr::str_to_lower(tools::file_ext(eb_files))
) %>%
  dplyr::mutate(
    za = stringr::str_extract(filename, "\\d{4}")
  ) %>%
  dplyr::filter(!is.na(za)) %>%
  dplyr::arrange(za, folder, filename)
#### Wave lookup ---------------------------------------------------------------
# The lookup stores the base question numbers for the three MIP blocks:
# country, personal, and EU. Extraction is done by matching all variables that
# start with that base question number, which makes the script robust to split
# ballots and TCC variants such as QA6A1, QA6A2, QA6B1, QA6B2, QA3C, QA3D, etc.

eb_lookup <- tibble::tribble(
  ~za,    ~eb_wave, ~year, ~month_mid, ~country_q, ~personal_q, ~eu_q,
  "3640", "57.2",   2002,  5.0,        "Q2",       NA,          NA,
  "3904", "59.1",   2003,  3.5,        "Q5",       NA,          NA,
  "3938", "60.1",   2003, 10.5,        "Q26",      NA,          NA,
  "4056", "61.0",   2004,  2.5,        "Q27",      NA,          NA,
  "4229", "62.0",   2004, 10.5,        "Q33",      NA,          NA,
  "4411", "63.4",   2005,  5.5,        "QA26",     NA,          NA,
  "4414", "64.2",   2005, 10.5,        "QA30",     NA,          NA,
  "4506", "65.2",   2006,  4.0,        "QA28",     NA,          NA,
  "4507", "65.3",   2006,  5.5,        "QD1",      NA,          NA,
  "4526", "66.1",   2006,  9.5,        "QA23",     NA,          NA,
  "4528", "66.3",   2006, 11.5,        "QA26",     NA,          NA,
  "4530", "67.2",   2007,  4.5,        "QA18",     NA,          NA,
  "4565", "68.1",   2007, 10.0,        "QA6",      NA,          NA,
  "4744", "69.2",   2008,  4.0,        "QA6",      NA,          NA,
  "4819", "70.1",   2008, 10.5,        "QA8",      "QA9",       NA,
  "4971", "71.1",   2009,  1.5,        "QA5",      "QA6",       NA,
  "4973", "71.3",   2009,  6.5,        "QA4",      "QA5",       NA,
  "4994", "72.4",   2009, 10.5,        "QA5",      "QA6",       NA,
  "5234", "73.4",   2010,  5.0,        "QA7",      "QA8",       NA,
  "5449", "74.2",   2010, 11.5,        "QA6",      "QA7",       "QA8",
  "5481", "75.3",   2011,  5.0,        "QA7",      "QA8",       "QA9",
  "5567", "76.3",   2011, 11.0,        "QA6",      "QA7",       "QA8",
  "5612", "77.3",   2012,  5.0,        "QA7",      "QA8",       "QA9",
  "5685", "78.1",   2012, 11.0,        "QA5",      "QA6",       "QA7",
  "5689", "79.3",   2013,  5.0,        "QA6",      "QA7",       "QA8",
  "5876", "80.1",   2013, 11.0,        "QA4",      "QA5",       "QA6",
  "5913", "81.2",   2014,  3.0,        "QA4",      "QA5",       "QA6",
  "5928", "81.4",   2014,  5.5,        "QA4",      "QA5",       "QA6",
  "5932", "82.3",   2014, 11.0,        "QA3",      "QA4",       "QA5",
  "5964", "83.1",   2015,  2.5,        "QA3",      "QA4",       "QA5",
  "5998", "83.3",   2015,  5.0,        "QA3",      "QA4",       "QA5",
  "6643", "84.3",   2015, 11.0,        "QA3",      "QA4",       "QA5",
  "6694", "85.2",   2016,  5.0,        "QA3",      "QA4",       "QA5",
  "6788", "86.2",   2016, 11.0,        "QA3",      "QA4",       "QA5",
  "6863", "87.3",   2017,  5.0,        "QA3",      "QA4",       "QA5",
  "6928", "88.3",   2017, 11.0,        "QA3",      "QA4",       "QA5",
  "6963", "89.1",   2018,  3.0,        "QA3",      "QA4",       "QA5",
  "7489", "90.3",   2018, 11.0,        "QA3",      "QA4",       "QA5",
  "7562", "91.2",   2019,  3.0,        "QA1",      NA,          "QA2",
  "7576", "91.5",   2019,  6.5,        "QA3",      "QA4",       "QA5",
  "7601", "92.3",   2019, 11.5,        "QA3",      "QA4",       "QA5",
  "7649", "93.1",   2020,  7.5,        "QA3",      "QA4",       "QA5",
  "7780", "94.3",   2020, 10.5,        "QA3",      "QA4",       "QA5",
  "7783", "95.3",   2021,  6.5,        "QA3",      "QA4",       "QA5",
  "7848", "96.3",   2021, 10.5,        "QA3",      "QA4",       "QA5",
  "7902", "97.5",   2022,  6.5,        "QA3",      "QA4",       "QA5",
  "7952", "98.1",   2022, 10.5,        NA,         NA,          "QC1",
  "7953", "98.2",   2023,  1.5,        "QA3",      "QA4",       "QA5",
  "7997", "99.4",   2023,  5.5,        "QA3",      "QA4",       "QA5",
  "8779", "100.2",  2023, 10.5,        "QA3",      "QA4",       "QA5",
  "8841", "101.1",  2024,  4.5,        "QA3",      "QA4",       "QA5",
  "8904", "102.1",  2024, 10.5,        "QA3",      "QA4",       "QA5"
)

setdiff(eb_lookup$za, unique(eb_index$za))


#### Match lookup -------------------------------------------------------------
eb_index <- eb_index %>%
  dplyr::left_join(eb_lookup, by = "za") %>%
  dplyr::filter(!is.na(eb_wave)) %>%
  dplyr::mutate(
    wave_date = as.Date(sprintf(
      "%d-%02d-15",
      year,
      pmin(pmax(round(month_mid), 1), 12)
    )),
    wave_time = year + (month_mid - 1) / 12
  )

#### Helpers ------------------------------------------------------------------

read_eb_file <- function(file, extension) {
  if (extension == "dta") {
    return(haven::read_dta(file))
  }
  if (extension == "sav") {
    return(haven::read_sav(file))
  }
  stop("Unsupported file extension: ", extension)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

find_country_var <- function(df) {
  candidates <- c("isocntry", "country", "tnscntry", "nation", "v7", "v6")
  hits <- candidates[candidates %in% names(df)]
  if (length(hits) == 0) {
    return(NA_character_)
  }
  hits[1]
}

find_id_var <- function(df) {
  candidates <- c("v5", "caseid", "id", "serial", "uniqid", "unique_caseid")
  hits <- candidates[candidates %in% names(df)]
  if (length(hits) == 0) {
    return(NA_character_)
  }
  hits[1]
}

find_block_vars <- function(df, q_base) {
  if (is.na(q_base) || is.null(q_base) || q_base == "") {
    return(character(0))
  }
  
  nm <- names(df)
  nm_lc <- stringr::str_to_lower(nm)
  
  lab <- unname(labelled::var_label(df))
  lab <- purrr::map_chr(lab, ~ .x %||% NA_character_)
  lab_lc <- stringr::str_to_lower(lab)
  
  q_base_lc <- stringr::str_to_lower(q_base)
  q_prefix <- stringr::str_extract(q_base_lc, "^[a-z]+")
  q_number <- stringr::str_extract(q_base_lc, "\\d+")
  
  name_pattern <- paste0("^", q_prefix, q_number, "([a-z]\\d*)?([_.](\\d+|t))?$")
  name_hits <- nm[stringr::str_detect(nm_lc, name_pattern)]
  
  if (length(name_hits) > 0) {
    return(name_hits)
  }
  
  label_pattern <- paste0("^", q_base_lc, "([a-z]\\d*)?\\b")
  label_hits <- nm[!is.na(lab_lc) & stringr::str_detect(lab_lc, label_pattern)]
  
  label_hits
}

extract_item_number <- function(var_name, issue_label_raw = NA_character_) {
  out <- stringr::str_extract(
    stringr::str_to_lower(var_name),
    "(?<=_|\\.)\\d+$"
  )
  
  out_num <- suppressWarnings(as.numeric(out))
  
  if (!is.na(out_num)) {
    return(out_num)
  }
  
  label_lc <- stringr::str_to_lower(issue_label_raw %||% "")
  
  if (stringr::str_detect(label_lc, "immigration")) {
    return(1)
  }
  if (stringr::str_detect(label_lc, "unemployment")) {
    return(2)
  }
  if (stringr::str_detect(label_lc, "environment|climate")) {
    return(3)
  }
  
  NA_real_
}

harmonize_issue <- function(issue_label_raw) {
  x <- stringr::str_to_lower(issue_label_raw %||% "")
  
  dplyr::case_when(
    x == "" ~ NA_character_,
    stringr::str_detect(x, "immigration|integration of foreigners") ~ "immigration",
    stringr::str_detect(x, "unemployment") ~ "unemployment",
    stringr::str_detect(x, "environment|climate change|protecting the environment|the environment") ~ "environment_climate",
    TRUE ~ NA_character_
  )
}

recode_selected <- function(x) {
  x_num <- suppressWarnings(as.numeric(haven::zap_labels(x)))
  
  dplyr::case_when(
    x_num == 1 ~ 1,
    x_num == 0 ~ 0,
    TRUE ~ NA_real_
  )
}

collapse_selected <- function(x) {
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(NA_real_)
  }
  if (any(x == 1)) {
    return(1)
  }
  if (all(x == 0)) {
    return(0)
  }
  
  NA_real_
}

extract_mip_block <- function(df, q_base) {
  vars <- find_block_vars(df, q_base)
  
  if (length(vars) == 0) {
    return(NULL)
  }
  
  country_var <- find_country_var(df)
  id_var <- find_id_var(df)
  
  if (is.na(country_var) || is.na(id_var)) {
    return(NULL)
  }
  
  var_labels <- labelled::var_label(df[vars])
  var_labels <- unname(var_labels)
  names(var_labels) <- vars
  
  df %>%
    dplyr::select(
      dplyr::all_of(country_var),
      dplyr::all_of(id_var),
      dplyr::all_of(vars)
    ) %>%
    dplyr::rename(
      isocntry_raw = dplyr::all_of(country_var),
      resp_id_raw  = dplyr::all_of(id_var)
    ) %>%
    dplyr::mutate(
      isocntry = as.character(isocntry_raw),
      resp_id  = as.character(resp_id_raw)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(vars),
      names_to = "issue_var",
      values_to = "selected_raw"
    ) %>%
    dplyr::mutate(
      issue_label_raw = purrr::map_chr(unname(var_labels[issue_var]), ~ .x %||% NA_character_),
      item_num = purrr::map2_dbl(issue_var, issue_label_raw, extract_item_number),
      selected_num = suppressWarnings(as.numeric(haven::zap_labels(selected_raw))),
      selected_bin = recode_selected(selected_raw),
      issue_harmonized = harmonize_issue(issue_label_raw)
    ) %>%
    dplyr::select(
      resp_id,
      isocntry,
      issue_var,
      item_num,
      issue_label_raw,
      issue_harmonized,
      selected_raw,
      selected_num,
      selected_bin
    )
}

extract_mip_wave <- function(file, extension, za, eb_wave, year, month_mid, wave_date, wave_time, country_q) {
  if (is.na(country_q) || is.null(country_q) || country_q == "") {
    return(NULL)
  }
  
  df <- read_eb_file(file, extension)
  
  out <- extract_mip_block(df, country_q)
  
  if (is.null(out) || nrow(out) == 0) {
    return(NULL)
  }
  
  out %>%
    dplyr::mutate(
      file = file,
      filename = basename(file),
      za = za,
      eb_wave = eb_wave,
      year = year,
      month_mid = month_mid,
      wave_date = wave_date,
      wave_time = wave_time
    ) %>%
    dplyr::select(
      file,
      filename,
      za,
      eb_wave,
      year,
      month_mid,
      wave_date,
      wave_time,
      resp_id,
      isocntry,
      issue_var,
      item_num,
      issue_label_raw,
      issue_harmonized,
      selected_raw,
      selected_num,
      selected_bin
    )
}
  
#### Match files to lookup ----------------------------------------------------
eb_index <- eb_index %>%
  dplyr::left_join(eb_lookup, by = "za")

#### Helper to recover country_q directly from file ----------------------------
guess_country_q <- function(file, extension) {
  df <- read_eb_file(file, extension)
  
  nm <- names(df)
  nm_lc <- stringr::str_to_lower(nm)
  
  lab <- unname(labelled::var_label(df))
  lab <- purrr::map_chr(lab, ~ .x %||% NA_character_)
  lab_lc <- stringr::str_to_lower(lab)
  
  candidates <- tibble::tibble(
    var = nm,
    var_lc = nm_lc,
    lab = lab,
    lab_lc = lab_lc
  ) %>%
    dplyr::filter(
      !is.na(lab_lc),
      stringr::str_detect(lab_lc, "important issues"),
      !stringr::str_detect(lab_lc, "pers|personal|eu|european union|tcc"),
      (
        stringr::str_detect(lab_lc, "immigration") |
          stringr::str_detect(lab_lc, "unemployment") |
          stringr::str_detect(lab_lc, "environment|climate")
      )
    ) %>%
    dplyr::mutate(
      q_base = stringr::str_extract(stringr::str_to_upper(lab), "Q[A-Z]*\\d+")
    ) %>%
    dplyr::filter(!is.na(q_base))
  
  if (nrow(candidates) > 0) {
    return(candidates$q_base[1])
  }
  
  candidates_name <- tibble::tibble(var = nm, var_lc = nm_lc) %>%
    dplyr::mutate(
      q_base = stringr::str_extract(stringr::str_to_upper(var), "^Q[A-Z]*\\d+")
    ) %>%
    dplyr::filter(!is.na(q_base))
  
  if (nrow(candidates_name) > 0) {
    return(candidates_name$q_base[1])
  }
  
  NA_character_
}

#### Fill missing or incorrect country_q --------------------------------------
eb_index <- eb_index %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    country_q = country_q %||% guess_country_q(file, extension)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    wave_date = as.Date(sprintf(
      "%d-%02d-15",
      year,
      pmin(pmax(round(month_mid), 1), 12)
    )),
    wave_time = year + (month_mid - 1) / 12
  )

#### Inspect recovered country_q ----------------------------------------------
print(
  eb_index %>%
    dplyr::select(
      za, folder, filename, eb_wave, year, month_mid,
      country_q, personal_q, eu_q
    ) %>%
    dplyr::arrange(year, za),
  n = nrow(eb_index)
)

#### Check which files still have no country_q --------------------------------
eb_index %>%
  dplyr::filter(is.na(country_q)) %>%
  dplyr::select(za, folder, filename, extension) %>%
  print(n = Inf)


#### Extract ------------------------------------------------------------------
handlers(global = TRUE)
handlers("txtprogressbar")

with_progress({
  p <- progressor(steps = nrow(eb_index))
  
  eb_mip_raw <- purrr::pmap_dfr(
    list(
      file       = eb_index$file,
      extension  = eb_index$extension,
      za         = eb_index$za,
      eb_wave    = eb_index$eb_wave,
      year       = eb_index$year,
      month_mid  = eb_index$month_mid,
      wave_date  = eb_index$wave_date,
      wave_time  = eb_index$wave_time,
      country_q  = eb_index$country_q
    ),
    function(...) {
      p()
      extract_mip_wave(...)
    }
  )
})

eb_mip_raw %>%
  dplyr::summarise(
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE)
  )

eb_mip_raw %>%
  dplyr::count(year) %>%
  print(n = Inf)

#### Collapse to respondent level ---------------------------------------------
eb_mip_resp <- eb_mip_raw %>%
  dplyr::filter(!is.na(issue_harmonized)) %>%
  dplyr::group_by(za, eb_wave, year, month_mid, wave_date,
                  isocntry, resp_id, issue_harmonized) %>%
  dplyr::summarise(
    selected_bin = collapse_selected(selected_bin),
    .groups = "drop"
  )

#### Aggregate to country-wave ------------------------------------------------
eb_salience <- eb_mip_resp %>%
  dplyr::group_by(isocntry, za, eb_wave, year, month_mid, wave_date, issue_harmonized) %>%
  dplyr::summarise(
    salience = mean(selected_bin, na.rm = TRUE),
    n = sum(!is.na(selected_bin)),
    .groups = "drop"
  ) %>%
  dplyr::filter(n >= 500)

#### Keep only target issues --------------------------------------------------
eb_salience <- eb_salience %>%
  dplyr::filter(issue_harmonized %in% c(
    "immigration", "unemployment", "environment_climate"
  ))

#### Wide format (ready for merge) --------------------------------------------
eb_salience_wide <- eb_salience %>%
  tidyr::pivot_wider(
    names_from = issue_harmonized,
    values_from = salience
  ) %>%
  dplyr::rename(
    country = isocntry
  ) %>%
  dplyr::arrange(country, wave_date)




#### Save ---------------------------------------------------------------------
saveRDS(
  eb_salience_wide,
  here::here("data", "processed", "eb_salience_country.rds")
)

print(eb_salience_wide, n = 200)


