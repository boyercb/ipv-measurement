# Global parameters -------------------------------------------------------

# sample size
N <- 500

outcomes <- c("slaps", "pushes", "punches", "twists", "kicks",
              "chokes", "weapon", "forcesex", "pressuresex", "degrade")

# this correlation matrix matches the observed structure of the
# 5 month violence data in the control at endline
Sigma <- matrix(
  c(1.00, 0.55, 0.61, 0.58, 0.63, 0.41, 0.36, 0.28, 0.27, 0.21, 
    0.55, 1.00, 0.59, 0.63, 0.53, 0.33, 0.47, 0.29, 0.27, 0.27,
    0.61, 0.59, 1.00, 0.66, 0.60, 0.40, 0.46, 0.24, 0.28, 0.37,
    0.58, 0.63, 0.66, 1.00, 0.56, 0.37, 0.56, 0.29, 0.26, 0.25,
    0.63, 0.53, 0.60, 0.56, 1.00, 0.43, 0.36, 0.19, 0.32, 0.24,
    0.41, 0.33, 0.40, 0.37, 0.43, 1.00, 0.31, 0.13, 0.10, 0.11,
    0.36, 0.47, 0.46, 0.56, 0.36, 0.31, 1.00, 0.22, 0.27, 0.20,
    0.28, 0.29, 0.24, 0.29, 0.19, 0.13, 0.22, 1.00, 0.35, 0.45,
    0.27, 0.27, 0.28, 0.26, 0.32, 0.10, 0.27, 0.35, 1.00, 0.56,
    0.21, 0.27, 0.37, 0.25, 0.24, 0.11, 0.20, 0.45, 0.56, 1.00
  ),
  byrow = TRUE,
  nrow = 10,
  ncol = 10,
  dimnames = list(outcomes, outcomes)
)

# these are the means of the frequency vars in control
lambda <- c(
  "slaps" = 0.18,
  "pushes" = 0.18,
  "punches" = 0.09,
  "twists" = 0.10,
  "kicks" = 0.11,
  "chokes" = 0.05,
  "weapon" = 0.08,
  "forcesex" = 0.53,
  "pressuresex" = 0.09,
  "degrade" = 0.19
)

# example treatment regime
treat <- c(
  "slaps" = 1.10,      # "moderate" violence increases
  "pushes" = 1.10,     # "moderate" violence increases
  "punches" = 0.50,    # more "severe" violence decreases
  "twists" = 0.50,     # more "severe" violence decreases
  "kicks" = 0.50,      # more "severe" violence decreases
  "chokes" = 0.50,     # more "severe" violence decreases
  "weapon" = 0.50,     # more "severe" violence decreases
  "forcesex" = 1.0,    # no effect on sexual violence
  "pressuresex" = 1.0, # no effect on sexual violence
  "degrade" = 1.0      # no effect on sexual violence
)


# Define data generation process ------------------------------------------

# function for creating correlated poisson random variables
correlated_poisson <- function(N, lambda, Sigma, treat = treat) {
  rand_norm <- mvrnorm(n = N, mu = rep(0, nrow(Sigma)), Sigma = Sigma)
  cdf_norm <- pnorm(rand_norm)
  rand_pois_0 <- qpois(cdf_norm, lambda = lambda)
  rand_pois_1 <- rand_pois_0 %*% diag(treat)
  
  stubs <- colnames(rand_pois_0)
  
  colnames(rand_pois_0) <- paste0(stubs, "_Z_0")
  colnames(rand_pois_1) <- paste0(stubs, "_Z_1")
  
  as.data.frame(cbind(rand_pois_0, rand_pois_1)) 
  
}

population <- 
  declare_population(
    N = N,
    lambda = lambda,
    Sigma = Sigma,
    treat = treat,
    handler = correlated_poisson
  )

assignment <- declare_assignment(prob = 0.5)

code_variables <- declare_step(
  freq_violence_Z_1 = slaps_Z_1 + pushes_Z_1 + punches_Z_1 + twists_Z_1 + 
    kicks_Z_1 + chokes_Z_1 + weapon_Z_1 + forcesex_Z_1 + 
    pressuresex_Z_1 + degrade_Z_1,
  freq_violence_Z_0 = slaps_Z_0 + pushes_Z_0 + punches_Z_0 + twists_Z_0 + 
    kicks_Z_0 + chokes_Z_0 + weapon_Z_0 + forcesex_Z_0 + 
    pressuresex_Z_0 + degrade_Z_0,
  any_violence_Z_1 = as.numeric(freq_violence_Z_1 > 0),
  any_violence_Z_0 = as.numeric(freq_violence_Z_0 > 0),
  handler = fabricate
)

estimand <- declare_estimand(
  ATE_cont = mean(freq_violence_Z_1 - freq_violence_Z_0),
  ATE_bin = mean(any_violence_Z_1 - any_violence_Z_0)
)

reveals <- declare_reveal(
  assignment_variables = "Z",
  outcome_variables = c("freq_violence","any_violence",
                        !!outcomes)
)

binary_estimator <- declare_estimator(
  any_violence ~ Z,
  estimand = "ATE_bin",
  model = lm_robust,
  label = "binary outcome"
)

cont_estimator <- declare_estimator(
  freq_violence ~ Z,
  estimand = "ATE_cont",
  model = lm_robust,
  label = "continuous outcome"
)

# Define design -----------------------------------------------------------

design <- 
  population + assignment + code_variables + reveals + 
  estimand + create_outcomes +
  binary_estimator + cont_estimator

diagnose_design(design)
