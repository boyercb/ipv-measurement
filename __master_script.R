
# define script globals ---------------------------------------------------

RERUN_SIMS <- TRUE
N_SIMS <- 1000


# load packages and helper functions --------------------------------------

source("00_packages_and_helpers/01_load_packages.R")

source("00_packages_and_helpers/02_simulation_helpers.R")


# clean empirical datasets and calculate distribution parameters ----------

source("01_clean_data/01_define_variable_lists.R")

source("01_clean_data/02_load_empirical_data.R")

source("01_clean_data/03_empirical_distributions.R")


# define and run simulations ----------------------------------------------

source("02_simulations/01_define_simulation_parameters.R")

source("02_simulations/02_run_simulations.R")

source("02_simulations/03_plot_simulation_results.R")


# TODO:
# 1. decide whether to use blw instead of elw or just control group for elw
# 2. check whether inverse transform needs cov or cor matrix
# 3. verify whether you need to apply correction for inv-normal approx.
# 4. implement subset option for specify PO function
# 5. compare fit of poisson versus negbinom or zip to empirical data
# 6. enumerate causal models of treatment dynamics
# 7. add baseline violence as optional adjustment variable and het fx source
