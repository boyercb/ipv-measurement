
# names correspond to the data block in the Stan program
data_list <-
  lapply(
    act_variables, 
    function(x, df) list(N = nrow(df), y = df[[x]]), 
    df = subset(elw, Z == 0)
  )


# fit zero-inflated poisson -----------------------------------------------

mod <- cmdstan_model("01_clean_data/fit_zip_to_data.stan")

params <- sapply(
  data_list, 
  function(x) {
    fit_mle <- mod$optimize(
      data = x,
      seed = 123
    )
    
    fit_mle$mle(c("lambda", "theta"))
  }
)

lambda <- params[1, ]
theta <- params[2, ]


# fit negative binomial ---------------------------------------------------

nb_mod <- cmdstan_model("01_clean_data/fit_nb_zip_to_data.stan")

nb_params <- sapply(
  data_list, 
  function(x) {
    fit_mle <- nb_mod$optimize(
      data = x,
      seed = 123,
      algorithm = 'bfgs'
    )
    
    fit_mle$mle(c("lambda", "theta", "phi"))
  }
)

nb_lambda <- nb_params[1, ]
nb_theta <- nb_params[2, ]
nb_phi <- nb_params[3, ]


Rho <- cor(elw[elw$Z == 0, act_variables]) + 0.3
colnames(Rho) <- NULL
row.names(Rho) <- NULL