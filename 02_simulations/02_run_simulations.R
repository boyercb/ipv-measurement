results_path <- "07_results/01_saved_results/"
results_filename <- "simulation_results.rds"

if (RERUN_SIMS) {
  results <- diagnose_design(design, sims = N_SIMS)
  write_rds(results, paste0(results_path, results_filename))
} else {
  results <- read_rds(paste0(results_path, results_filename))
}

