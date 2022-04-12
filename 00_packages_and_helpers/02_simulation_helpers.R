# define designer function ------------------------------------------------

ipv_design <- function(N, lambda, theta, Rho, tau = -1) {
  # define latent baseline outcomes as a series of correlated poisson random
  # variables representing the number of instances of act Y_i over the study
  # period
  design <-
    declare_population(
      N = N,
      lambda = lambda,
      theta = theta,
      Rho = Rho,
      handler = draw_correlated_counts
    ) + 
    # define how treatment affects outcomes at endline
    declare_potential_outcomes(
      .cols = everything(),
      tau = tau,
      handler = potential_outcomes_function
    ) +
    # define estimands 
    declare_estimands(
      ATE_frequency = mean(Y_Z_1 - Y_Z_0),
      ATE_binary = mean((Y_Z_1 > 0) - (Y_Z_0 > 0))
    ) +
    # just a simple random assignment for now
    declare_assignment(
      prob = 0.5
    ) +
    # reveal potential 
    reveal_outcomes(
      outcome_variables = c("Y1", "Y2", "Y3", "Y4", "Y5", "Y6", "Y7", "Y8", "Y9", "Y10"),
      assignment_variable = Z
    ) +
    # categorize latent violence according to WHO questionnaire categories
    declare_measurement(
      .cols = paste0("Y", 1:length(lambda)),
      handler = categorize_counts
    ) +
    # construct final outcomes
    declare_step(
      .cols = paste0("Y", 1:length(lambda), "_star"),
      handler = construct_outcomes
    ) + 
    declare_estimator(
      Y_star_binary ~ Z,
      estimand = "ATE_binary",
      model = lm_robust,
      label = "binary outcome"
    ) + 
    declare_estimator(
      Y_star ~ Z,
      estimand = "ATE_frequency",
      model = lm_robust,
      label = "continuous outcome"
    )
  
  return(design)

}


# helper functions --------------------------------------------------------

# draw correlated zero-inflated poisson random variables using the inverse normal
# transformation. lambda is vector of marginal means of poisson variables, theta
# is vector of marginal means of bernoulli variables Rho1 and Rho2 are 
# correlation matrices
draw_correlated_counts <- function(N, lambda, theta, Rho) {
  U1 <- mvrnorm(N, rep(0, length(lambda)), Rho)
  U1_inv <- pnorm(U1)
  
  U2 <- mvrnorm(N, rep(0, length(theta)), Rho)
  U2_inv <- pnorm(U2)
  
  Y <- sapply(
    X = seq_along(lambda),
    FUN = function(x) qpois(U1_inv[, x], lambda[x]) * qbinom(U2_inv[, x], 1, 1 - theta[x])
  )
  
  colnames(Y) <- paste0("u", 1:length(lambda))
  
  return(as_tibble(Y))
}

# transform latent violent act counts into WHO categories, i.e.
# 0 - Never
# 1 - Once
# 2 - A few times [2 to 4]
# 3 - Many times [5+]
categorize_counts <- function(data, .cols) {
  mutate(
    data, 
    across(
      .cols = all_of(.cols), 
      .fns = ~ as.numeric(cut(.x, breaks = c(-Inf, 0, 1, 4, Inf))) - 1,
      .names = "{.col}_star"
    )
  )
}

# construct final outcomes from constituent items
construct_outcomes <- function(data, .cols) {
  data %>%
    rowwise() %>%
    mutate(
      Y_star = sum(c_across(.cols)),
      Y_star_binary = as.numeric(Y_star > 0)
    ) %>%
    ungroup() 
}

# define potential outcomes function in which treatment 
potential_outcomes_function <- function(data, .cols, tau, subset = TRUE) {
  mutate(
    data,
    across(
      .cols = .cols, 
      .fns = list(
        Z_1 = ~ tau(.x),
        Z_0 = ~ .x
      ),
      .names = "{str_replace(.col, 'u', 'Y')}_{.fn}"
    )
  ) %>%
    rowwise() %>%
    mutate(
      Y_Z_1 = sum(c_across(ends_with("Z_1"))),
      Y_Z_0 = sum(c_across(ends_with("Z_0")))
    ) %>%
    ungroup() 
}


