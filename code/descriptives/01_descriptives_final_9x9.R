#### 01_descriptives_final_9x9.R ----------------------------------------------
#### Clean-up -----------------------------------------------------------------
rm(list = ls())

#### Load packages and helper functions ---------------------------------------
source(here::here("code", "utils", "packages.R"))
load_packages()

#### Load final data ----------------------------------------------------------
df_9x9_model <- readRDS(
  here::here("data", "processed", "generalized_9x9_model.rds")
)
 