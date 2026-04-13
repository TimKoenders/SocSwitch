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
  switch(
    extension,
    dta = haven::read_dta(file),
    sav = haven::read_sav(file),
    stop("Unsupported file extension: ", extension)
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

find_country_var <- function(df) {
  candidates <- c("isocntry", "country", "tnscntry", "nation", "v7", "v6")
  hits <- intersect(candidates, names(df))
  if (length(hits) == 0) NA_character_ else hits[1]
}

find_id_var <- function(df) {
  candidates <- c("v5", "caseid", "id", "serial", "uniqid", "unique_caseid")
  hits <- intersect(candidates, names(df))
  if (length(hits) == 0) NA_character_ else hits[1]
}

find_block_vars <- function(df, q_base, labels = NULL) {
  if (is.na(q_base) || is.null(q_base) || q_base == "") {
    return(character(0))
  }
  
  nm <- names(df)
  nm_lc <- tolower(nm)
  
  q_base_lc <- tolower(q_base)
  q_prefix <- sub("^([a-z]+).*", "\\1", q_base_lc)
  q_number <- sub(".*?(\\d+).*", "\\1", q_base_lc)
  
  name_pattern <- paste0("^", q_prefix, q_number, "([a-z]\\d*)?([_.](\\d+|t))?$")
  name_hits <- nm[grepl(name_pattern, nm_lc, perl = TRUE)]
  
  if (length(name_hits) > 0) {
    return(name_hits)
  }
  
  if (is.null(labels)) {
    labels <- unname(labelled::var_label(df))
  }
  
  labels_chr <- vapply(
    labels,
    function(x) if (is.null(x) || length(x) == 0 || all(is.na(x))) NA_character_ else as.character(x)[1],
    character(1)
  )
  labels_lc <- tolower(labels_chr)
  
  label_pattern <- paste0("^", q_base_lc, "([a-z]\\d*)?\\b")
  nm[!is.na(labels_lc) & grepl(label_pattern, labels_lc, perl = TRUE)]
}

extract_item_number_vec <- function(var_name, issue_label_raw) {
  out <- stringr::str_extract(tolower(var_name), "(?<=_|\\.)\\d+$")
  out_num <- suppressWarnings(as.numeric(out))
  
  label_lc <- tolower(ifelse(is.na(issue_label_raw), "", issue_label_raw))
  
  out_num[is.na(out_num) & grepl("immigration", label_lc)] <- 1
  out_num[is.na(out_num) & grepl("unemployment", label_lc)] <- 2
  out_num[is.na(out_num) & grepl("environment|climate", label_lc)] <- 3
  
  out_num
}

harmonize_issue_vec <- function(issue_label_raw) {
  x <- tolower(ifelse(is.na(issue_label_raw), "", issue_label_raw))
  
  out <- rep(NA_character_, length(x))
  out[x == ""] <- NA_character_
  out[grepl("immigration|integration of foreigners", x)] <- "immigration"
  out[grepl("unemployment", x)] <- "unemployment"
  out[grepl("environment|climate change|protecting the environment|the environment", x)] <- "environment_climate"
  
  out
}

recode_selected_vec <- function(x) {
  x_num <- suppressWarnings(as.numeric(haven::zap_labels(x)))
  out <- rep(NA_real_, length(x_num))
  out[x_num == 1] <- 1
  out[x_num == 0] <- 0
  out
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
  country_var <- find_country_var(df)
  id_var <- find_id_var(df)
  
  if (is.na(country_var) || is.na(id_var)) {
    return(NULL)
  }
  
  var_labels <- unname(labelled::var_label(df))
  names(var_labels) <- names(df)
  
  vars <- find_block_vars(df, q_base, labels = var_labels)
  
  if (length(vars) == 0) {
    return(NULL)
  }
  
  issue_label_raw_lookup <- var_labels[vars]
  issue_label_raw_lookup <- vapply(
    issue_label_raw_lookup,
    function(x) if (is.null(x) || length(x) == 0 || all(is.na(x))) NA_character_ else as.character(x)[1],
    character(1)
  )
  
  out <- df %>%
    dplyr::select(
      dplyr::all_of(country_var),
      dplyr::all_of(id_var),
      dplyr::all_of(vars)
    ) %>%
    dplyr::rename(
      isocntry_raw = dplyr::all_of(country_var),
      resp_id_raw = dplyr::all_of(id_var)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(vars),
      names_to = "issue_var",
      values_to = "selected_raw"
    )
  
  out$issue_label_raw <- unname(issue_label_raw_lookup[out$issue_var])
  out$item_num <- extract_item_number_vec(out$issue_var, out$issue_label_raw)
  out$selected_num <- suppressWarnings(as.numeric(haven::zap_labels(out$selected_raw)))
  out$selected_bin <- recode_selected_vec(out$selected_raw)
  out$issue_harmonized <- harmonize_issue_vec(out$issue_label_raw)
  out$isocntry <- as.character(out$isocntry_raw)
  out$resp_id <- as.character(out$resp_id_raw)
  
  out %>%
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
  
  out$file <- file
  out$filename <- basename(file)
  out$za <- za
  out$eb_wave <- eb_wave
  out$year <- year
  out$month_mid <- month_mid
  out$wave_date <- wave_date
  out$wave_time <- wave_time
  
  out %>%
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

#### Helper to recover country_q directly from file ---------------------------
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

#### Fill missing or incorrect country_q -------------------------------------
if (!"country_q" %in% names(eb_index)) {
  eb_index <- eb_index %>%
    dplyr::mutate(country_q = NA_character_)
}

eb_index <- eb_index %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    country_q = dplyr::coalesce(country_q, guess_country_q(file, extension))
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
future::plan(future::multisession, workers = max(1, parallelly::availableCores() - 1))

handlers(global = TRUE)
handlers("txtprogressbar")

with_progress({
  p <- progressor(steps = nrow(eb_index))
  
  eb_mip_raw <- furrr::future_pmap_dfr(
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
    function(file, extension, za, eb_wave, year, month_mid, wave_date, wave_time, country_q) {
      p()
      extract_mip_wave(
        file = file,
        extension = extension,
        za = za,
        eb_wave = eb_wave,
        year = year,
        month_mid = month_mid,
        wave_date = wave_date,
        wave_time = wave_time,
        country_q = country_q
      )
    },
    .options = furrr::furrr_options(seed = TRUE)
  )
})

future::plan(future::sequential)


#### Quality checks ----------------------------------------------------------

eb_mip_raw %>%
  dplyr::summarise(
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE)
  )

eb_mip_raw %>%
  dplyr::count(year) %>%
  print(n = Inf)

eb_mip_raw %>%
  dplyr::count(issue_harmonized) %>%
  print(n = Inf)

eb_mip_raw %>%
  dplyr::distinct(isocntry) %>%
  print(n = Inf)


#### Collapse to respondent level ---------------------------------------------
eb_mip_resp <- eb_mip_raw %>%
  dplyr::filter(!is.na(issue_harmonized)) %>%
  dplyr::group_by(
    za, eb_wave, year, month_mid, wave_date,
    isocntry, resp_id, issue_harmonized
  ) %>%
  dplyr::summarise(
    selected_bin = collapse_selected(selected_bin),
    .groups = "drop"
  )

#### Aggregate to country-wave -------------------------------------------------
eb_salience <- eb_mip_resp %>%
  dplyr::group_by(
    isocntry, za, eb_wave, year, month_mid, wave_date, issue_harmonized
  ) %>%
  dplyr::summarise(
    salience = mean(selected_bin, na.rm = TRUE),
    n = sum(!is.na(selected_bin)),
    .groups = "drop"
  ) %>%
  dplyr::filter(
    n >= 500,
    issue_harmonized %in% c("immigration", "unemployment", "environment_climate")
  )

#### Harmonize country codes ---------------------------------------------------
eb_salience <- eb_salience %>%
  dplyr::mutate(
    country = dplyr::case_when(
      isocntry %in% c("GB", "GB-GBN", "GB-NIR") ~ "GB",
      isocntry %in% c("DE-W", "DE-E") ~ "DE",
      isocntry == "CY-TCC" ~ "CY",
      isocntry == "RS-KM" ~ "RS",
      TRUE ~ isocntry
    )
  )

#### Wide format (ready for merge) --------------------------------------------
eb_salience_wide <- eb_salience %>%
  dplyr::group_by(
    country, za, eb_wave, year, month_mid, wave_date, issue_harmonized
  ) %>%
  dplyr::summarise(
    salience = mean(salience, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = issue_harmonized,
    values_from = salience,
    values_fill = NA_real_
  ) %>%
  dplyr::arrange(country, wave_date)
print(eb_salience_wide)

#### Save ---------------------------------------------------------------------
saveRDS(
  eb_salience_wide,
  here::here("data", "processed", "eb_salience_country.rds")
)

print(eb_salience_wide, n = 200)