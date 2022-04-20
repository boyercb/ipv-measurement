# define designer function ------------------------------------------------

ipv_design <-
  function(N,
           lambda = NULL,
           theta = NULL,
           phi = NULL,
           Rho = diag(rep(1, 10)),
           empirical = NULL,
           probs = c(0.3, 0.7, 0, 0),
           tau = function(x, col, type) { x }) {
  # define latent baseline outcomes as a series of correlated poisson random
  # variables representing the number of instances of act Y_i over the study
  # period
    
  if (!is.null(empirical)) {
    # re-sampling approach from empirical distribution
    design <-
      declare_model(
        handler = fabricatr::resample_data,
        N = N,
        data = empirical
      ) + 
      # define how treatment affects outcomes at endline
      declare_potential_outcomes(
        handler = potential_outcomes_function,
        .cols = paste0("u", 1:ncol(empirical)),
        tau = tau,
        probs = probs,
        categorize = FALSE
      ) +
      # define estimands
      declare_inquiries(
        ATE_frequency = mean(Y_Z_1 - Y_Z_0),
        ATE_frequency_star = ATE_frequency,
        ATE_binary = mean((Y_Z_1 > 0) - (Y_Z_0 > 0))
      ) +
      # just a simple random assignment for now
      declare_assignment(
        Z = randomizr::conduct_ra(N = N, prob = 0.5)
      ) +
      # reveal potential
      declare_measurement(
        Y1 = fabricatr::reveal_outcomes(Y1 ~ Z),
        Y2 = fabricatr::reveal_outcomes(Y2 ~ Z),
        Y3 = fabricatr::reveal_outcomes(Y3 ~ Z),
        Y4 = fabricatr::reveal_outcomes(Y4 ~ Z),
        Y5 = fabricatr::reveal_outcomes(Y5 ~ Z),
        Y6 = fabricatr::reveal_outcomes(Y6 ~ Z),
        Y7 = fabricatr::reveal_outcomes(Y7 ~ Z),
        Y8 = fabricatr::reveal_outcomes(Y8 ~ Z),
        Y9 = fabricatr::reveal_outcomes(Y9 ~ Z),
        Y10 = fabricatr::reveal_outcomes(Y10 ~ Z)
      ) +
      # construct final outcomes
      declare_step(
        handler = construct_outcomes,
        .cols = paste0("Y", 1:ncol(empirical))
      )
    
  } else {
    # simulate from ZIP or ZINB
    design <-
      declare_model(
        handler = draw_correlated_counts,
        N = N,
        lambda = lambda,
        theta = theta,
        phi = phi,
        Rho = Rho
      ) +
      # define how treatment affects outcomes at endline
      declare_potential_outcomes(
        handler = potential_outcomes_function,
        .cols = paste0("u", 1:length(lambda)),
        probs = probs,
        tau = tau
      ) +
      # define estimands 
      declare_inquiries(
        ATE_frequency = mean(Y_Z_1 - Y_Z_0),
        ATE_frequency_star = mean(Y_star_Z_1 - Y_star_Z_0),
        ATE_binary = mean((Y_Z_1 > 0) - (Y_Z_0 > 0))
      ) +
      # just a simple random assignment for now
      declare_assignment(
        Z = randomizr::conduct_ra(N = N, prob = 0.5)
      ) +
      # reveal potential 
      declare_measurement(
        Y1 = fabricatr::reveal_outcomes(Y1 ~ Z),
        Y2 = fabricatr::reveal_outcomes(Y2 ~ Z),
        Y3 = fabricatr::reveal_outcomes(Y3 ~ Z),
        Y4 = fabricatr::reveal_outcomes(Y4 ~ Z),
        Y5 = fabricatr::reveal_outcomes(Y5 ~ Z),
        Y6 = fabricatr::reveal_outcomes(Y6 ~ Z),
        Y7 = fabricatr::reveal_outcomes(Y7 ~ Z),
        Y8 = fabricatr::reveal_outcomes(Y8 ~ Z),
        Y9 = fabricatr::reveal_outcomes(Y9 ~ Z),
        Y10 = fabricatr::reveal_outcomes(Y10 ~ Z) 
      ) +
      # categorize latent violence according to WHO questionnaire categories
      declare_measurement(
        handler = categorize_counts,
        .cols = paste0("Y", 1:length(lambda))
      ) +
      # construct final outcomes
      declare_step(
        handler = construct_outcomes,
        .cols = paste0("Y", 1:length(lambda), "_star")
      ) 
  }
  
  design <-
    design +
    declare_estimator(
      Y_star_binary ~ Z,
      inquiry = "ATE_binary",
      model = estimatr::lm_robust,
      label = "binary outcome"
    ) +
    # declare_estimator(
    #   Y ~ Z + X,
    #   model = glm,
    #   inquiry = "ATE_binary",
    #   family = binomial("logit"),
    #   model_summary = tidy_margins,
    #   term = "Z"
    # ) +
    declare_estimator(
      Y_star ~ Z,
      inquiry = "ATE_frequency_star",
      model = estimatr::lm_robust,
      label = "continuous outcome"
    ) 
  
  return(design)

}


# helper functions --------------------------------------------------------

tidy_margins <- function(x) {
  tidy(margins(x, data = x$data), conf.int = TRUE)
}

# draw correlated zero-inflated poisson random variables using the inverse normal
# transformation. lambda is vector of marginal means of poisson variables, theta
# is vector of marginal means of bernoulli variables Rho1 and Rho2 are 
# correlation matrices
draw_correlated_counts <- function(N, lambda, theta, phi, Rho) {
  U1 <- MASS::mvrnorm(N, rep(0, length(lambda)), Rho)
  U1_inv <- pnorm(U1)
  
  U2 <- MASS::mvrnorm(N, rep(0, length(theta)), Rho)
  U2_inv <- pnorm(U2)
  
  if (is.null(phi)) {
    Y <- qpois(U1_inv, lambda) * qbinom(U2_inv, 1, 1 - theta)
  } else {
    Y <- qnbinom(U1_inv, mu = lambda, size = phi) * qbinom(U2_inv, 1, 1 - theta)
  }
  
  colnames(Y) <- paste0("u", 1:length(lambda))
  
  return(tibble::as_tibble(Y))
}

# transform latent violent act counts into WHO categories, i.e.
# 0 - Never
# 1 - Once
# 2 - A few times [2 to 4]
# 3 - Many times [5+]
categorize_counts <- function(data, .cols) {

  data[, paste0(.cols, "_star")] <-
    sapply(.cols, function (x) {
      as.numeric(cut(data[[x]], breaks = c(-Inf, 0, 1, 4, Inf))) - 1
    })
  
  data
}

# construct final outcomes from constituent items
construct_outcomes <- function(data, .cols) {
  
  data$Y_star <- rowSums(data[, .cols], na.rm = TRUE)
  data$Y_star_binary <- as.numeric(data$Y_star > 0)
  
  data
}

# define potential outcomes function in which treatment 
potential_outcomes_function <- function(data, .cols, probs, categorize = TRUE, tau) {
  type <- fabricatr::draw_categorical(probs, N = nrow(data), category_labels = c("1", "2", "3", "4"))
    
  data[, paste0(gsub('u', 'Y', .cols), "_Z_1")] <-
    sapply(.cols, function(x) tau(data[[x]], .cols, type))
  
  data[, paste0(gsub('u', 'Y', .cols), "_Z_0")] <-
    data[, .cols]
  
  data$Y_Z_1 <- rowSums(data[, paste0(gsub('u', 'Y', .cols), "_Z_1")], na.rm = TRUE)
  data$Y_Z_0 <- rowSums(data[, paste0(gsub('u', 'Y', .cols), "_Z_0")], na.rm = TRUE)
  
  if (categorize) {
    data[, paste0(gsub('u', 'Y', .cols), "_star_Z_1")] <-
      sapply(paste0(gsub('u', 'Y', .cols), "_Z_1"), function (x) {
        as.numeric(cut(data[[x]], breaks = c(-Inf, 0, 1, 4, Inf))) - 1
      })
    
    data[, paste0(gsub('u', 'Y', .cols), "_star_Z_0")] <-
      sapply(paste0(gsub('u', 'Y', .cols), "_Z_0"), function (x) {
        as.numeric(cut(data[[x]], breaks = c(-Inf, 0, 1, 4, Inf))) - 1
      })
    
    data$Y_star_Z_1 <- rowSums(data[, paste0(gsub('u', 'Y', .cols), "_star_Z_1")], na.rm = TRUE)
    data$Y_star_Z_0 <- rowSums(data[, paste0(gsub('u', 'Y', .cols), "_star_Z_0")], na.rm = TRUE)
  }
  
  data
}

