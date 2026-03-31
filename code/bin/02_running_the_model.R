#### 02_running_the_model.R --------------------------------------------------------
#### Clean-up -----------------------------------------------------------------
rm(list = ls())

#### Load packages and helper functions ------------------------------------
source(here::here("code", "utils", "packages.R"))
load_packages()
source(here::here("code", "utils", "helper_functions.R"))

#### Load processed data -----------------------------------------------------
model_data_full      <- readRDS(here::here("data", "processed", "model_data_full.rds"))
df_switching_full    <- readRDS(here::here("data", "processed", "df_switching_full.rds"))
model_data           <- readRDS(here::here("data", "processed", "model_data.rds"))
df_switching         <- readRDS(here::here("data", "processed", "df_switching.rds"))
model_data_europe    <- readRDS(here::here("data", "processed", "model_data_europe.rds"))
df_switching_europe  <- readRDS(here::here("data", "processed", "df_switching_europe.rds"))


#### Define helper function --------------------------------------------------
run_and_summarize <- function(df_switching_obj, model_data_obj, label, party_group = NULL) {
  
  message("\n=== Running MAVCL model for: ", label, " ===")
  
  if (!is.null(party_group)) {
    message("Filtering data for party group: ", party_group)
    df_switching_obj <- df_switching_obj %>%
      dplyr::filter(party_group == !!party_group)
  }
  
  tic(paste("MAVCL", label))
  fit <- run_mavcl(
    data = df_switching_obj,
    y_names = model_data_obj$y_names,
    main_predictor = "immigration_shift",
    baseline = "residual",
    predictor_continuous = TRUE,
    re_parties = FALSE,
    re_elections = FALSE,
    re_countries = FALSE,
    iter = 2000,
    warmup = 1000,
    chains = 4,
    seed = 123,
    adapt_delta = 0.95,
    max_treedepth = 12
  )
  toc()
  
  qoi_out <- compute_qoi(
    mavcl_object            = fit,
    y_structure             = model_data_obj$y_structure,
    conditional_expectation = TRUE,
    average_marginal_effect = TRUE,
    posterior_quantiles     = c(0.5, 0.025, 0.975),
    base                    = "tm1",
    relative                = TRUE
  )
  
  qoi_in <- compute_qoi(
    mavcl_object            = fit,
    y_structure             = model_data_obj$y_structure,
    conditional_expectation = TRUE,
    average_marginal_effect = TRUE,
    posterior_quantiles     = c(0.5, 0.025, 0.975),
    base                    = "t",
    relative                = TRUE
  )
  
  ame_out <- qoi_out$average_marginal_effect$dyadic_losses
  effects_out <- tibble::tibble(
    destination = names(ame_out),
    median_AME  = sapply(ame_out, function(x) x["50%"]),
    lower_AME   = sapply(ame_out, function(x) x["2.5%"]),
    upper_AME   = sapply(ame_out, function(x) x["97.5%"]),
    dataset     = label,
    direction   = "outflow",
    party_group = party_group
  )
  
  ame_in <- qoi_in$average_marginal_effect$dyadic_gains
  effects_in <- tibble::tibble(
    source      = names(ame_in),
    median_AME  = sapply(ame_in, function(x) x["50%"]),
    lower_AME   = sapply(ame_in, function(x) x["2.5%"]),
    upper_AME   = sapply(ame_in, function(x) x["97.5%"]),
    dataset     = label,
    direction   = "inflow",
    party_group = party_group
  )
  
  list(
    fit = fit,
    qoi_outflows = qoi_out,
    qoi_inflows = qoi_in,
    effects_outflows = effects_out,
    effects_inflows = effects_in
  )
}



#### Run models for all party groups across datasets ----------------------------

# MRP (mainstream right)
results_full_mrp <- run_and_summarize(df_switching_full, model_data_full, "Full dataset", party_group = "mrp")
results_subset_mrp <- run_and_summarize(df_switching, model_data, "Subset (six countries)", party_group = "mrp")
results_europe_mrp <- run_and_summarize(df_switching_europe, model_data_europe, "Europe subset", party_group = "mrp")

# NAT (national-conservative)
results_full_nat <- run_and_summarize(df_switching_full, model_data_full, "Full dataset", party_group = "nat")
results_subset_nat <- run_and_summarize(df_switching, model_data, "Subset (six countries)", party_group = "nat")
results_europe_nat <- run_and_summarize(df_switching_europe, model_data_europe, "Europe subset", party_group = "nat")


#### Combine all results --------------------------------------------------------

effects_combined <- dplyr::bind_rows(
  results_full_mrp$effects_outflows,
  results_full_mrp$effects_inflows,
  results_subset_mrp$effects_outflows,
  results_subset_mrp$effects_inflows,
  results_europe_mrp$effects_outflows,
  results_europe_mrp$effects_inflows,
  
  results_full_nat$effects_outflows,
  results_full_nat$effects_inflows,
  results_subset_nat$effects_outflows,
  results_subset_nat$effects_inflows,
  results_europe_nat$effects_outflows,
  results_europe_nat$effects_inflows,
)

print(effects_combined, n = Inf)

#### Run MAVCL model with cordon sanitaire as moderator -----------------------

tic("MAVCL cordon")
fit_mrp_cordon <- run_mavcl(
  data = df_switching %>% filter(party_group == "mrp"),
  y_names = model_data$y_names,
  main_predictor = "immigration_shift",
  moderator = "cordon",          
  predictor_continuous = TRUE,
  re_parties = FALSE,
  re_elections = FALSE,
  re_countries = FALSE,
  iter = 2000,
  warmup = 1000,
  chains = 4,
  seed = 123,
  adapt_delta = 0.95,
  max_treedepth = 12
)
toc()

#### Compute quantities of interest (cordon model) ----------------------------
qoi_outflows_cordon <- compute_qoi(
  mavcl_object           = fit_mrp_cordon,
  y_structure            = model_data$y_structure,
  conditional_expectation = TRUE,
  average_marginal_effect = TRUE,
  posterior_quantiles    = c(0.5, 0.025, 0.975),
  base                   = "tm1",
  relative               = TRUE,
  atmeans                = FALSE
)




