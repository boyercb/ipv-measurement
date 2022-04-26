results_path <- "07_results/01_saved_results/"
results_filename <- "simulation_results.rds"
b1_results_filename <- "b1_simulation_results.rds"

countries <- unique(dhs$country)
names(countries) <- countries

b1_design <-
  function(probs, tau) {
    ipv_design(
      N = 2000,
      empirical = simdata,
      probs = probs,
      tau = tau
    )
  }

b1_designs <- 
  expand_design(
    b1_design,
    probs = list(
      c(0.7,  0.3,    0,    0),  # 70% unaffected, 30% violence stops
      c(0.7, 0.10, 0.20,    0),  # 70% unaffected, 15% violence stops, 15% violence reduces
      c(0.7,    0,  0.3,    0),  # 70% unaffected, 30% violence reduces
      c(0.7, 0.10, 0.15, 0.05)   # 70% unaffected, 10% violence stops, 10% violence reduces, 10% violence increases
    ),
    tau = tau_models
  )

designs <-
  expand_design(
    ipv_design,
    N = N,
    empirical = countries,
    probs = list(
      c(0.7,  0.3,    0,    0),  # 70% unaffected, 30% violence stops
      c(0.7, 0.10, 0.20,    0),  # 70% unaffected, 15% violence stops, 15% violence reduces
      c(0.7,    0,  0.3,    0),  # 70% unaffected, 30% violence reduces
      c(0.7, 0.10, 0.15, 0.05)   # 70% unaffected, 10% violence stops, 10% violence reduces, 10% violence increases
    ),
    tau = tau_models
  )

if (RERUN_SIMS) {
  plan(multisession, workers = 3)

  b1_results <- 
    diagnose_designs(b1_designs,
                     sims = N_SIMS,
                     bootstrap_sims = 100)
    
  results <-
    diagnose_designs(designs, sims = N_SIMS, bootstrap_sims = 100)
  
  plan(sequential)
  
  write_rds(b1_results, paste0(results_path, b1_results_filename))
  write_rds(results, paste0(results_path, results_filename))
} else {
  b1_results <- read_rds(paste0(results_path, b1_results_filename))
  results <- read_rds(paste0(results_path, results_filename))
}