results_path <- "07_results/01_saved_results/"
results_filename <- "simulation_results.rds"

countries <- unique(dhs$country)
names(countries) <- countries

if (RERUN_SIMS) {
  plan(multisession, workers = 12)

  designs <-
    expand_design(
      ipv_design,
      N = N,
      empirical = map(
        .x = countries, 
        .f = ~select(filter(dhs, country == .x), -country)
      ),
      tau = tau_models
    )
    
  results <- diagnose_designs(designs, sims = N_SIMS, bootstrap_sims = 100)
  
  plan(sequential)
  
  write_rds(results, paste0(results_path, results_filename))
} else {
  results <- read_rds(paste0(results_path, results_filename))
}

