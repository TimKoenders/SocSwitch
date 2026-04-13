#### 03_polarization_meaures.R --------------------------------------------------------
#### Clean-up -----------------------------------------------------------------
rm(list = ls())

#### Load packages and helper functions --------------------------------------
source(here::here("code", "utils", "packages.R"))

#### Paths -------------------------------------------------------------------
proc_file     <- here::here("data", "ess", "ess.dta")
#### Load data ----------------------------------------------------------------
ess         <- haven::read_dta(proc_file)


#### Check structure ----------------------------------------------------
# Structure of ess data
str(ess)

# Time scope
sort(unique(ess$essround))

# Geographical scope
sort(unique(ess$cntry))

# Cross-tab of countries by ESS round
table(ess$essround, ess$cntry)

# More readable summary
scope_summary <- ess %>%
  dplyr::distinct(essround, cntry) %>%
  dplyr::arrange(essround, cntry)

scope_summary
#### Define economically left-leaning subset ---------------------------------------------------------------------
# Use: gincdif (Government should reduce differences in income levels)
ess <- ess %>%
  dplyr::mutate(
    gincdif = haven::zap_missing(gincdif)
  )
ess_left <- ess %>%
  dplyr::filter(gincdif %in% c(1, 2))

# Check frequency in this category
ess %>%
  dplyr::mutate(gincdif = haven::zap_missing(gincdif)) %>%
  dplyr::summarise(
    n_left = sum(gincdif %in% c(1, 2), na.rm = TRUE),
    n_total = sum(!is.na(gincdif)),
    share_left = n_left / n_total
  )

# Check if only used the strongly agree group
ess_left_strict <- ess %>%
  dplyr::filter(gincdif == 1)
ess %>%
  dplyr::summarise(
    share_left_strict = mean(gincdif == 1, na.rm = TRUE)
  )

# Generate both options
ess_left_broad  <- ess %>% dplyr::filter(gincdif %in% c(1, 2))
ess_left_strict <- ess %>% dplyr::filter(gincdif == 1)

# Year variable
ess <- ess %>%
  dplyr::mutate(
    year = as.numeric(essround) * 2 + 2000
  )

# Summary statistics: economically left-leaning subset
ess_left_broad %>%
  dplyr::group_by(cntry, essround) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::summarise(
    avg_n_per_country_wave = mean(n),
    min_n = min(n),
    max_n = max(n)
  )


#### Create cultural scale ---------------------------------------------------
# All items are recoded so that higher values indicate more culturally
# conservative or TAN positions. ESS missing categories such as refusal,
# don't know, and no answer are explicitly set to NA before any reversals.

ess_left <- ess_left %>%
  dplyr::mutate(
    
    # "Gay men and lesbians should be free to live their own life as they wish"
    # Original coding: 1 = Agree strongly, 5 = Disagree strongly
    # Higher = more conservative / less supportive
    freehms = dplyr::if_else(
      freehms %in% c(7, 8, 9),
      NA_real_,
      as.numeric(freehms)
    ),
    freehms_r = freehms,
    
    # "[Country] should allow people of the same race or ethnic group as most
    # [country] people to come and live here"
    # Original coding: 1 = Allow many, 4 = Allow none
    # Higher = more conservative / more restrictive
    imsmetn = dplyr::if_else(
      imsmetn %in% c(7, 8, 9),
      NA_real_,
      as.numeric(imsmetn)
    ),
    imsmetn_r = imsmetn,
    
    # "[Country] should allow people of a different race or ethnic group from most
    # [country] people to come and live here"
    # Original coding: 1 = Allow many, 4 = Allow none
    # Higher = more conservative / more restrictive
    imdfetn = dplyr::if_else(
      imdfetn %in% c(7, 8, 9),
      NA_real_,
      as.numeric(imdfetn)
    ),
    imdfetn_r = imdfetn,
    
    # "[Country] should allow people from the poorer countries outside Europe
    # to come and live here"
    # Original coding: 1 = Allow many, 4 = Allow none
    # Higher = more conservative / more restrictive
    impcntr = dplyr::if_else(
      impcntr %in% c(7, 8, 9),
      NA_real_,
      as.numeric(impcntr)
    ),
    impcntr_r = impcntr,
    
    # "Would you say it is generally bad or good for [country]'s economy that
    # people come to live here from other countries?"
    # Original coding: 0 = Bad for the economy, 10 = Good for the economy
    # Higher = more conservative / more negative about immigration
    imbgeco = dplyr::if_else(
      imbgeco %in% c(77, 88, 99),
      NA_real_,
      as.numeric(imbgeco)
    ),
    imbgeco_r = 10 - imbgeco,
    
    # "Would you say that [country]'s cultural life is generally undermined or
    # enriched by people coming to live here from other countries?"
    # Original coding: 0 = Cultural life undermined, 10 = Cultural life enriched
    # Higher = more conservative / more negative about immigration
    imueclt = dplyr::if_else(
      imueclt %in% c(77, 88, 99),
      NA_real_,
      as.numeric(imueclt)
    ),
    imueclt_r = 10 - imueclt,
    
    # "European unification should go further" versus
    # "European unification has already gone too far"
    # Original coding: 0 = Gone too far, 10 = Go further
    # Higher = more conservative / more anti-EU
    euftf = dplyr::if_else(
      euftf %in% c(77, 88, 99),
      NA_real_,
      as.numeric(euftf)
    ),
    euftf_r = 10 - euftf
  )

# Set items
tan_vars <- c(
  "freehms_r",
  "imsmetn_r",
  "imdfetn_r",
  "impcntr_r",
  "imbgeco_r",
  "imueclt_r",
  "euftf_r"
)

items_tan <- ess_left %>%
  dplyr::select(dplyr::all_of(tan_vars))

# Cronbach's alpha for TAN scale
alpha_tan <- psych::alpha(
  items_tan %>%
    tidyr::drop_na()
)

alpha_tan$total$raw_alpha
alpha_tan

# Create cultural scale: unweighted average of standardized items
ess_left <- ess_left %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(tan_vars),
      ~ (. - mean(., na.rm = TRUE)) / stats::sd(., na.rm = TRUE),
      .names = "{.col}_z"
    )
  )

tan_z_vars <- paste0(tan_vars, "_z")

ess_left <- ess_left %>%
  dplyr::mutate(
    gal_tan = rowMeans(
      dplyr::select(., dplyr::all_of(tan_z_vars)),
      na.rm = TRUE
    ),
    gal_tan = dplyr::if_else(is.nan(gal_tan), NA_real_, gal_tan)
  )

# Check scale
ess_left %>%
  dplyr::summarise(
    n_nonmissing = sum(!is.na(gal_tan)),
    share_nonmissing = mean(!is.na(gal_tan))
  )

#### Measure 0: Positional shift (mean and median) ----------------------------
positional_shift <- ess_left %>%
  dplyr::group_by(essround, cntry) %>%
  dplyr::summarise(
    gal_tan_mean   = mean(gal_tan, na.rm = TRUE),
    gal_tan_median = stats::median(gal_tan, na.rm = TRUE),
    .groups = "drop"
  )

# Inspect
summary(positional_shift$gal_tan_mean)
summary(positional_shift$gal_tan_median)
positional_shift %>% dplyr::arrange(dplyr::desc(gal_tan_mean))

# Standardize measure
positional_shift <- positional_shift %>%
  dplyr::mutate(
    gal_tan_mean_z   = as.numeric(scale(gal_tan_mean)),
    gal_tan_median_z = as.numeric(scale(gal_tan_median))
  )

#### Measure 1: Polarization (standard deviation) -----------------------------
pol_dispersion <- ess_left %>%
  dplyr::group_by(essround, cntry) %>%
  dplyr::summarise(
    sd = stats::sd(gal_tan, na.rm = TRUE),
    .groups = "drop"
  )

# Inspect
pol_dispersion %>% dplyr::arrange(dplyr::desc(sd))
summary(pol_dispersion$sd)


#### Measure 2: Polarization (bimodality coefficient) -------------------------
bimodality_coefficient <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  
  if (n < 4) return(NA_real_)
  
  m3 <- moments::skewness(x)
  m4 <- moments::kurtosis(x) - 3
  
  (m3^2 + 1) / (m4 + 3 * ((n - 1)^2 / ((n - 2) * (n - 3))))
}

pol_bimodality <- ess_left %>%
  dplyr::group_by(essround, cntry) %>%
  dplyr::summarise(
    bc = bimodality_coefficient(gal_tan),
    .groups = "drop"
  )

# Inspect
summary(pol_bimodality$bc)
pol_bimodality %>% dplyr::arrange(dplyr::desc(bc))


#### Standardize and compare measures ----------------------------------------
pol_dispersion <- pol_dispersion %>%
  dplyr::mutate(
    sd_z = as.numeric(scale(sd))
  )

pol_bimodality <- pol_bimodality %>%
  dplyr::mutate(
    bc_z = as.numeric(scale(bc))
  )

pol_measures <- pol_dispersion %>%
  dplyr::select(essround, cntry, sd, sd_z) %>%
  dplyr::left_join(
    pol_bimodality %>%
      dplyr::select(essround, cntry, bc, bc_z),
    by = c("essround", "cntry")
  )

pol_measures <- positional_shift %>%
  dplyr::select(
    essround, cntry,
    gal_tan_mean, gal_tan_mean_z,
    gal_tan_median, gal_tan_median_z
  ) %>%
  dplyr::left_join(
    pol_dispersion %>%
      dplyr::select(essround, cntry, sd, sd_z),
    by = c("essround", "cntry")
  ) %>%
  dplyr::left_join(
    pol_bimodality %>%
      dplyr::select(essround, cntry, bc, bc_z),
    by = c("essround", "cntry")
  )

# Inspect summaries
summary(pol_measures$sd_z)
summary(pol_measures$bc_z)

# Inspect highest and lowest country-rounds
pol_measures %>% dplyr::arrange(dplyr::desc(sd_z))
pol_measures %>% dplyr::arrange(dplyr::desc(bc_z))

# Optional: scatterplot
plot(pol_measures$sd_z, pol_measures$bc_z)
abline(stats::lm(bc_z ~ sd_z, data = pol_measures), lty = 2)


#### Create ESS fieldwork table -----------------------------------------------
ess_fieldwork <- tibble::tribble(
  ~cntry, ~essround, ~fieldwork_start, ~fieldwork_end,
  
  # ESS1
  "AT", 1, "02.02.2003", "30.09.2003",
  "BE", 1, "01.10.2002", "30.04.2003",
  "CZ", 1, "24.11.2002", "09.03.2003",
  "DK", 1, "28.10.2002", "19.06.2003",
  "FI", 1, "09.09.2002", "10.12.2002",
  "FR", 1, "15.09.2003", "15.12.2003",
  "DE", 1, "20.11.2002", "16.05.2003",
  "GR", 1, "29.01.2003", "15.03.2003",
  "HU", 1, "29.10.2002", "26.11.2002",
  "IE", 1, "11.12.2002", "12.04.2003",
  "IL", 1, "15.10.2002", "15.01.2003",
  "IT", 1, "13.01.2003", "30.06.2003",
  "LU", 1, "14.04.2003", "14.08.2003",
  "NL", 1, "01.09.2002", "24.02.2003",
  "NO", 1, "16.09.2002", "17.01.2003",
  "PL", 1, "30.09.2002", "19.12.2002",
  "PT", 1, "26.09.2002", "20.01.2003",
  "SI", 1, "17.10.2002", "30.11.2002",
  "ES", 1, "19.11.2002", "20.02.2003",
  "SE", 1, "23.09.2002", "20.12.2002",
  "CH", 1, "09.09.2002", "08.02.2003",
  "GB", 1, "24.09.2002", "04.02.2003",
  
  # ESS2
  "AT", 2, "06.01.2005", "25.04.2005",
  "BE", 2, "04.10.2004", "31.01.2005",
  "CZ", 2, "01.10.2004", "13.12.2004",
  "DK", 2, "09.10.2004", "31.01.2005",
  "EE", 2, "30.09.2004", "19.01.2005",
  "FI", 2, "20.09.2004", "17.12.2004",
  "FR", 2, "27.11.2004", "04.03.2005",
  "DE", 2, "26.08.2004", "16.01.2005",
  "GR", 2, "10.01.2005", "20.03.2005",
  "HU", 2, "02.04.2005", "31.05.2005",
  "IS", 2, "24.04.2005", "04.12.2005",
  "IE", 2, "18.01.2005", "20.06.2005",
  "IT", 2, "02.02.2006", "29.05.2006",
  "LU", 2, "13.09.2004", "26.01.2005",
  "NL", 2, "11.09.2004", "19.02.2005",
  "NO", 2, "15.09.2004", "15.01.2005",
  "PL", 2, "10.10.2004", "22.12.2004",
  "PT", 2, "15.10.2004", "17.03.2005",
  "SK", 2, "04.10.2004", "12.12.2004",
  "SI", 2, "18.10.2004", "30.11.2004",
  "ES", 2, "27.09.2004", "31.01.2005",
  "SE", 2, "29.09.2004", "19.01.2005",
  "CH", 2, "15.09.2004", "28.02.2005",
  "TR", 2, "17.12.2005", "01.07.2006",
  "GB", 2, "27.09.2004", "16.03.2005",
  "UA", 2, "28.01.2005", "10.03.2005",
  
  # ESS3
  "AT", 3, "18.07.2007", "05.11.2007",
  "BE", 3, "23.10.2006", "19.02.2007",
  "BG", 3, "20.11.2006", "01.01.2007",
  "CY", 3, "02.10.2006", "10.12.2006",
  "DK", 3, "19.09.2006", "02.05.2007",
  "EE", 3, "25.10.2006", "21.05.2007",
  "FI", 3, "18.09.2006", "20.12.2006",
  "FR", 3, "19.09.2006", "07.04.2007",
  "DE", 3, "01.09.2006", "15.01.2007",
  "HU", 3, "21.11.2006", "28.01.2007",
  "IE", 3, "14.09.2006", "31.08.2007",
  "LV", 3, "28.06.2007", "02.09.2007",
  "NL", 3, "16.09.2006", "18.03.2007",
  "NO", 3, "21.08.2006", "19.12.2006",
  "PL", 3, "02.10.2006", "13.12.2006",
  "PT", 3, "12.10.2006", "28.02.2007",
  "RO", 3, "01.12.2006", "31.01.2007",
  "RU", 3, "18.09.2006", "09.01.2007",
  "SK", 3, "01.12.2006", "28.02.2007",
  "SI", 3, "18.10.2006", "04.12.2006",
  "ES", 3, "25.10.2006", "04.03.2007",
  "SE", 3, "21.09.2006", "03.02.2007",
  "CH", 3, "24.08.2006", "02.04.2007",
  "UA", 3, "06.12.2006", "12.01.2007",
  "GB", 3, "05.09.2006", "14.01.2007",
  
  # ESS4
  "AT", 4, "01.11.2010", "28.02.2011",
  "BE", 4, "13.11.2008", "20.03.2009",
  "BG", 4, "06.03.2009", "31.05.2009",
  "HR", 4, "22.12.2008", "31.03.2009",
  "CY", 4, "29.09.2008", "21.12.2008",
  "CZ", 4, "08.06.2009", "08.07.2009",
  "DK", 4, "01.09.2008", "11.01.2009",
  "EE", 4, "05.11.2008", "11.03.2009",
  "FI", 4, "19.09.2008", "05.02.2009",
  "FR", 4, "28.09.2008", "31.01.2009",
  "DE", 4, "27.08.2008", "31.01.2009",
  "GR", 4, "15.07.2009", "20.11.2009",
  "HU", 4, "20.02.2009", "20.04.2009",
  "IE", 4, "11.09.2009", "12.03.2010",
  "IL", 4, "31.08.2008", "13.03.2009",
  "LV", 4, "02.04.2009", "08.09.2009",
  "LT", 4, "16.10.2009", "12.01.2010",
  "NL", 4, "08.09.2008", "28.06.2009",
  "NO", 4, "25.08.2008", "20.01.2009",
  "PL", 4, "03.11.2008", "15.02.2009",
  "PT", 4, "09.10.2008", "08.03.2009",
  "RO", 4, "02.12.2008", "19.01.2009",
  "RU", 4, "08.11.2008", "09.04.2009",
  "SK", 4, "17.11.2008", "09.02.2009",
  "SI", 4, "20.10.2008", "20.01.2009",
  "ES", 4, "05.09.2008", "31.01.2009",
  "SE", 4, "15.09.2008", "03.02.2009",
  "CH", 4, "30.08.2008", "17.04.2009",
  "TR", 4, "02.11.2008", "17.05.2009",
  "UA", 4, "01.03.2009", "02.04.2009",
  "GB", 4, "01.09.2008", "19.01.2009",
  
  # ESS5
  "AT", 5, "24.05.2013", "10.10.2013",
  "BE", 5, "11.10.2010", "06.05.2011",
  "BG", 5, "17.12.2010", "28.03.2011",
  "HR", 5, "16.09.2011", "14.12.2011",
  "CY", 5, "01.01.2011", "21.06.2011",
  "CZ", 5, "20.01.2011", "08.03.2011",
  "DK", 5, "20.09.2010", "08.03.2011",
  "EE", 5, "10.10.2010", "28.05.2011",
  "FI", 5, "13.09.2010", "30.12.2010",
  "FR", 5, "15.10.2010", "06.04.2011",
  "DE", 5, "15.09.2010", "03.02.2011",
  "GR", 5, "06.05.2011", "05.07.2011",
  "HU", 5, "19.10.2010", "10.12.2010",
  "IE", 5, "20.09.2011", "31.01.2012",
  "IL", 5, "09.01.2011", "13.06.2011",
  "LT", 5, "21.04.2011", "20.08.2011",
  "NL", 5, "27.09.2010", "02.04.2011",
  "NO", 5, "09.09.2010", "15.02.2011",
  "PL", 5, "01.10.2010", "06.02.2011",
  "PT", 5, "11.10.2010", "23.03.2011",
  "RU", 5, "24.12.2010", "14.05.2011",
  "SK", 5, "29.10.2010", "28.02.2011",
  "SI", 5, "20.10.2010", "31.01.2011",
  "ES", 5, "11.04.2011", "24.07.2011",
  "SE", 5, "27.09.2010", "01.03.2011",
  "CH", 5, "02.10.2010", "23.03.2011",
  "UA", 5, "13.05.2011", "30.07.2011",
  "GB", 5, "31.08.2010", "28.02.2011",
  
  # ESS6
  "AL", 6, "01.12.2012", "12.02.2013",
  "BE", 6, "10.09.2012", "24.12.2012",
  "BG", 6, "09.02.2013", "30.04.2013",
  "CY", 6, "01.10.2012", "31.12.2012",
  "CZ", 6, "09.01.2013", "11.03.2013",
  "DK", 6, "10.01.2013", "24.04.2013",
  "EE", 6, "01.09.2012", "28.01.2013",
  "FI", 6, "03.09.2012", "02.02.2013",
  "FR", 6, "08.02.2013", "30.06.2013",
  "DE", 6, "06.09.2012", "22.01.2013",
  "HU", 6, "10.11.2012", "17.02.2013",
  "IS", 6, "03.10.2012", "23.03.2013",
  "IE", 6, "15.10.2012", "09.02.2013",
  "IL", 6, "03.09.2012", "05.03.2013",
  "IT", 6, "01.06.2013", "20.12.2013",
  "XK", 6, "14.02.2013", "15.03.2013",
  "LT", 6, "21.05.2013", "25.08.2013",
  "NL", 6, "28.08.2012", "30.03.2013",
  "NO", 6, "14.08.2012", "08.02.2013",
  "PL", 6, "19.09.2012", "08.01.2013",
  "PT", 6, "24.10.2012", "20.03.2013",
  "RU", 6, "10.10.2012", "27.12.2012",
  "SK", 6, "24.10.2012", "06.03.2013",
  "SI", 6, "01.10.2012", "31.12.2012",
  "ES", 6, "23.01.2013", "14.05.2013",
  "SE", 6, "01.10.2012", "05.05.2013",
  "CH", 6, "01.09.2012", "22.04.2013",
  "UA", 6, "11.07.2013", "09.08.2013",
  "GB", 6, "01.09.2012", "07.02.2013",
  
  # ESS7
  "AT", 7, "14.10.2014", "05.05.2015",
  "BE", 7, "10.09.2014", "01.02.2015",
  "CZ", 7, "24.11.2014", "09.02.2015",
  "DK", 7, "12.09.2014", "17.02.2015",
  "EE", 7, "07.09.2014", "29.12.2014",
  "FI", 7, "03.09.2014", "09.02.2015",
  "FR", 7, "31.10.2014", "03.03.2015",
  "DE", 7, "18.08.2014", "05.02.2015",
  "HU", 7, "24.04.2015", "26.06.2015",
  "IE", 7, "04.09.2014", "31.01.2015",
  "IL", 7, "12.05.2015", "13.12.2015",
  "LT", 7, "11.04.2015", "14.06.2015",
  "NL", 7, "08.09.2014", "15.01.2015",
  "NO", 7, "20.08.2014", "08.01.2015",
  "PL", 7, "17.04.2015", "14.09.2015",
  "PT", 7, "02.02.2015", "30.11.2015",
  "SI", 7, "09.10.2014", "01.02.2015",
  "ES", 7, "22.01.2015", "25.06.2015",
  "SE", 7, "01.08.2014", "30.01.2015",
  "CH", 7, "29.08.2014", "20.02.2015",
  "GB", 7, "01.09.2014", "25.02.2015",
  
  # ESS8
  "AT", 8, "19.09.2016", "28.12.2016",
  "BE", 8, "14.09.2016", "31.01.2017",
  "CZ", 8, "24.10.2016", "19.12.2016",
  "EE", 8, "01.10.2016", "31.01.2017",
  "FI", 8, "15.09.2016", "08.03.2017",
  "FR", 8, "10.11.2016", "11.03.2017",
  "DE", 8, "23.08.2016", "26.03.2017",
  "HU", 8, "14.05.2017", "16.09.2017",
  "IS", 8, "02.11.2016", "08.06.2017",
  "IE", 8, "25.11.2016", "08.05.2017",
  "IL", 8, "10.09.2016", "08.02.2017",
  "IT", 8, "11.09.2017", "19.11.2017",
  "LT", 8, "04.10.2017", "28.12.2017",
  "NL", 8, "01.09.2016", "31.01.2017",
  "NO", 8, "22.08.2016", "17.01.2017",
  "PL", 8, "07.11.2016", "22.02.2017",
  "PT", 8, "20.10.2016", "15.06.2017",
  "RU", 8, "03.01.2017", "19.03.2017",
  "SI", 8, "21.09.2016", "11.01.2017",
  "ES", 8, "16.02.2017", "23.06.2017",
  "SE", 8, "26.08.2016", "10.02.2017",
  "CH", 8, "01.09.2016", "02.03.2017",
  "GB", 8, "01.09.2016", "20.03.2017",
  
  # ESS9
  "AL", 9, "19.12.2018", "15.03.2019",
  "AT", 9, "18.09.2018", "12.01.2019",
  "BE", 9, "20.09.2018", "28.01.2019",
  "BG", 9, "16.11.2018", "15.12.2018",
  "HR", 9, "20.09.2019", "27.01.2020",
  "CY", 9, "17.09.2018", "26.05.2019",
  "CZ", 9, "17.11.2018", "06.02.2019",
  "DK", 9, "13.09.2018", "08.01.2019",
  "EE", 9, "01.10.2018", "02.03.2019",
  "FI", 9, "03.09.2018", "18.02.2019",
  "FR", 9, "19.10.2018", "01.04.2019",
  "DE", 9, "29.08.2018", "04.03.2019",
  "HU", 9, "31.01.2019", "22.05.2019",
  "IS", 9, "05.10.2019", "31.01.2020",
  "IE", 9, "05.11.2018", "05.04.2019",
  "IT", 9, "17.12.2018", "10.03.2019",
  "LV", 9, "10.10.2019", "21.01.2020",
  "LT", 9, "21.09.2019", "15.12.2019",
  "ME", 9, "22.05.2019", "30.10.2019",
  "NL", 9, "28.08.2018", "22.01.2019",
  "NO", 9, "04.10.2018", "16.05.2019",
  "PL", 9, "26.10.2018", "20.03.2019",
  "PT", 9, "27.10.2019", "23.12.2019",
  "RS", 9, "01.10.2018", "01.03.2019",
  "SK", 9, "14.06.2019", "07.12.2019",
  "SI", 9, "24.09.2018", "01.02.2019",
  "ES", 9, "08.11.2019", "27.01.2020",
  "SE", 9, "30.08.2018", "23.05.2019",
  "CH", 9, "01.09.2018", "11.02.2019",
  "GB", 9, "31.08.2018", "22.02.2019",
  
  # ESS10
  "AT", 10, "30.08.2021", "06.12.2021",
  "BE", 10, "27.10.2021", "03.09.2022",
  "BG", 10, "28.06.2021", "30.09.2021",
  "HR", 10, "05.05.2021", "26.11.2021",
  "CY", 10, "09.03.2022", "19.08.2022",
  "CZ", 10, "07.07.2021", "29.09.2021",
  "EE", 10, "07.06.2021", "31.12.2021",
  "FI", 10, "31.08.2021", "31.01.2022",
  "FR", 10, "23.08.2021", "31.12.2021",
  "DE", 10, "05.10.2021", "04.01.2022",
  "GR", 10, "09.11.2021", "23.05.2022",
  "HU", 10, "10.06.2021", "16.10.2021",
  "IS", 10, "28.07.2021", "11.02.2022",
  "IE", 10, "23.11.2021", "16.12.2022",
  "IL", 10, "01.02.2022", "17.07.2022",
  "IT", 10, "25.10.2021", "26.04.2022",
  "LV", 10, "01.11.2021", "31.01.2022",
  "LT", 10, "01.07.2021", "15.12.2021",
  "ME", 10, "03.11.2021", "30.03.2022",
  "NL", 10, "01.10.2021", "03.04.2022",
  "MK", 10, "23.10.2021", "07.03.2022",
  "NO", 10, "10.06.2021", "04.05.2022",
  "PL", 10, "25.01.2022", "25.05.2022",
  "PT", 10, "16.08.2021", "06.03.2022",
  "RS", 10, "11.01.2022", "25.05.2022",
  "SK", 10, "25.05.2021", "21.10.2021",
  "SI", 10, "18.09.2020", "26.08.2021",
  "ES", 10, "21.01.2022", "31.05.2022",
  "SE", 10, "10.12.2021", "17.01.2022",
  "CH", 10, "04.05.2021", "02.05.2022",
  "GB", 10, "15.08.2021", "02.09.2022",
  
  # ESS11
  "AT", 11, "13.06.2023", "03.12.2023",
  "BE", 11, "28.06.2023", "13.02.2024",
  "BG", 11, "08.12.2023", "26.04.2024",
  "CY", 11, "15.05.2023", "29.06.2024",
  "CZ", 11, "01.02.2024", "04.08.2024",
  "HR", 11, "24.06.2023", "19.01.2024",
  "EE", 11, "16.09.2024", "01.12.2024",
  "FI", 11, "01.08.2023", "30.01.2024",
  "FR", 11, "23.08.2023", "04.02.2024",
  "DE", 11, "09.05.2023", "21.12.2023",
  "GR", 11, "05.02.2024", "09.05.2024",
  "HU", 11, "05.05.2023", "10.11.2023",
  "IS", 11, "20.02.2024", "09.06.2024",
  "IE", 11, "27.06.2023", "03.01.2024",
  "IL", 11, "05.12.2023", "03.11.2024",
  "IT", 11, "09.10.2023", "21.04.2024",
  "LV", 11, "05.10.2023", "28.04.2024",
  "LT", 11, "04.09.2023", "31.12.2023",
  "ME", 11, "27.02.2024", "03.05.2024",
  "NL", 11, "31.03.2023", "07.11.2023",
  "NO", 11, "17.04.2023", "30.11.2023",
  "PL", 11, "21.10.2023", "26.03.2024",
  "PT", 11, "25.09.2023", "29.02.2024",
  "RS", 11, "11.12.2023", "02.05.2024",
  "SK", 11, "08.09.2023", "12.12.2023",
  "SI", 11, "21.03.2023", "14.08.2023",
  "ES", 11, "08.02.2024", "01.06.2024",
  "SE", 11, "08.03.2023", "08.12.2023",
  "CH", 11, "09.03.2023", "31.01.2024",
  "UA", 11, "30.04.2024", "03.07.2024",
  "GB", 11, "03.07.2023", "09.12.2023"
) %>%
  dplyr::mutate(
    fieldwork_start = lubridate::dmy(fieldwork_start),
    fieldwork_end   = lubridate::dmy(fieldwork_end)
  ) %>%
  dplyr::arrange(cntry, essround)

#### Merge polarization measures with ESS fieldwork ---------------------------
pol_measures_fw <- pol_measures %>%
  dplyr::left_join(
    ess_fieldwork,
    by = c("cntry", "essround")
  ) %>%
  dplyr::arrange(cntry, essround)


#### Create matching ID -------------------------------------------------------
best_raked_imp <- readRDS(
  here::here("data", "processed", "best_raked_imp_fam.rds")
)

election_contexts <- tibble::tibble(
  elec_id = sort(unique(best_raked_imp$elec_id))
) %>%
  dplyr::mutate(
    country_raw = stringr::str_extract(elec_id, "^[^-]+"),
    year        = as.integer(stringr::str_extract(elec_id, "(?<=-)(\\d{4})(?=-\\d{2}$)")),
    month       = as.integer(stringr::str_extract(elec_id, "(?<=-)(\\d{2})$"))
  ) %>%
  dplyr::mutate(
    country = dplyr::case_when(
      country_raw == "DNK"   ~ "DK",
      country_raw == "BE-VL" ~ "BE",
      country_raw == "BE-WA" ~ "BE",
      TRUE ~ country_raw
    ),
    election_date = lubridate::ymd(sprintf("%04d-%02d-01", year, month))
  )

#### Match each election context to the latest ESS fieldwork end date ---------
ess_to_election_link <- election_contexts %>%
  dplyr::left_join(
    pol_measures_fw,
    by = c("country" = "cntry"),
    relationship = "many-to-many"
  ) %>%
  dplyr::filter(!is.na(fieldwork_end), fieldwork_end <= election_date) %>%
  dplyr::group_by(elec_id) %>%
  dplyr::slice_max(order_by = fieldwork_end, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    elec_id,
    country,
    year,
    month,
    election_date,
    essround,
    fieldwork_start,
    fieldwork_end,
    sd,
    sd_z,
    bc,
    bc_z
  ) %>%
  dplyr::arrange(country, year, month, elec_id) %>%
  dplyr::mutate(
    ess_match_id = sprintf("%s-R%d", country, essround)
  )

#### Final election-level polarization file -----------------------------------
ess_polarization_election <- ess_to_election_link %>%
  dplyr::select(
    elec_id,
    country,
    year,
    month,
    election_date,
    essround,
    fieldwork_start,
    fieldwork_end,
    sd,
    sd_z,
    bc,
    bc_z,
    ess_match_id
  ) %>%
  dplyr::arrange(country, year, month, elec_id)

#### Inspect ESS-election matches ---------------------------------------------
matched_elections <- ess_polarization_election

unmatched_elections <- election_contexts %>%
  dplyr::anti_join(matched_elections, by = "elec_id") %>%
  dplyr::arrange(country, year, month, elec_id)

# Full matched and unmatched lists
matched_elections %>% print(n = Inf)
unmatched_elections %>% print(n = Inf)

# Summary of matched coverage
matched_summary <- matched_elections %>%
  dplyr::summarise(
    n_elections = dplyr::n_distinct(elec_id),
    n_countries = dplyr::n_distinct(country),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE)
  )

matched_summary

# First matched election per country
matched_elections %>%
  dplyr::group_by(country) %>%
  dplyr::slice_min(order_by = election_date, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(country) %>%
  print(n = Inf)

#### Save ---------------------------------------------------------------------
saveRDS(
  election_contexts,
  here::here("data", "processed", "election_contexts.rds")
)

saveRDS(
  ess_polarization_election,
  here::here("data", "processed", "ess_polarization_election.rds")
)