# Global parameters -------------------------------------------------------

# sample size
N <- 500

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
  dimnames = list(
    c("slaps", "pushes", "punches", "twists", "kicks",
      "chokes", "weapon", "forcesex", "pressuresex", "degrade"
    ),
    c("slaps", "pushes", "punches", "twists", "kicks",
      "chokes", "weapon", "forcesex","pressuresex", "degrade"
    )
  )
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
  
  colnames(rand_pois_0) <- paste0(stubs, "_0")
  colnames(rand_pois_1) <- paste0(stubs, "_1")
  
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

po_slaps <- 
  declare_potential_outcomes(
    slaps ~ (1 - Z) * slaps_0 + Z * slaps_1
  )

po_pushes <- 
  declare_potential_outcomes(
    pushes ~ (1 - Z) * pushes_0 + Z * pushes_1
  )

po_punches <- 
  declare_potential_outcomes(
    punches ~ (1 - Z) * punches_0 + Z * punches_1
  )

po_twists <- 
  declare_potential_outcomes(
    twists ~ (1 - Z) * twists_0 + Z * twists_1
  )

po_kicks <- 
  declare_potential_outcomes(
    kicks ~ (1 - Z) * kicks_0 + Z * kicks_1
  )

po_chokes <- 
  declare_potential_outcomes(
    chokes ~ (1 - Z) * chokes_0 + Z * chokes_1
  )

po_weapon <- 
  declare_potential_outcomes(
    weapon ~ (1 - Z) * weapon_0 + Z * weapon_1
  )

po_forcesex <- 
  declare_potential_outcomes(
    forcesex ~ (1 - Z) * forcesex_0 + Z * forcesex_1
  )

po_pressuresex <- 
  declare_potential_outcomes(
    pressuresex ~ (1 - Z) * pressuresex_0 + Z * pressuresex_1
  )

po_degrade <- 
  declare_potential_outcomes(
    degrade ~ (1 - Z) * degrade_0 + Z * degrade_1
  )

assignment <- declare_assignment(prob = 0.5)

estimand <- declare_estimand(
  ATE_cont = mean(
    (slaps_Z_1 + pushes_Z_1 + punches_Z_1 + twists_Z_1 + 
       kicks_Z_1 + chokes_Z_1 + weapon_Z_1 + forcesex_Z_1 + 
       pressuresex_Z_1 + degrade_Z_1) - 
      (slaps_Z_0 + pushes_Z_0 + punches_Z_0 + twists_Z_0 + 
         kicks_Z_0 + chokes_Z_0 + weapon_Z_0 + forcesex_Z_0 + 
         pressuresex_Z_0 + degrade_Z_0)),
  ATE_bin = mean(
    as.numeric(
      (slaps_Z_1 + pushes_Z_1 + punches_Z_1 + twists_Z_1 + 
       kicks_Z_1 + chokes_Z_1 + weapon_Z_1 + forcesex_Z_1 + 
       pressuresex_Z_1 + degrade_Z_1) > 0) - 
      as.numeric(
       (slaps_Z_0 + pushes_Z_0 + punches_Z_0 + twists_Z_0 + 
         kicks_Z_0 + chokes_Z_0 + weapon_Z_0 + forcesex_Z_0 + 
         pressuresex_Z_0 + degrade_Z_0) > 0))
)


# Define design -----------------------------------------------------------

create_outcomes <- declare_step(
  fabricate,
  any_violence = as.numeric((slaps + pushes + punches + twists + 
                        kicks + chokes + weapon + forcesex + 
                        pressuresex + degrade) > 0),
  freq_violence = (slaps + pushes + punches + twists + 
                        kicks + chokes + weapon + forcesex + 
                        pressuresex + degrade)
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


design <- 
  population + po_slaps + po_pushes + po_punches + po_twists + 
  po_kicks + po_chokes + po_weapon + po_forcesex + 
  po_pressuresex + po_degrade + assignment + estimand + create_outcomes +
  binary_estimator + cont_estimator

diagnose_design(design)
