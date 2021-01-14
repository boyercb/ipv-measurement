
# names correspond to the data block in the Stan program
data_list <-
  lapply(
    act_variables, 
    function(x, df) list(N = nrow(df), y = df[[x]]), 
    df = elw
  )

mod <- cmdstan_model("01_clean_data/fit_poisson_to_data.stan")

lambda <- sapply(
  data_list, 
  function(x) {
    fit_mle <- mod$optimize(
      data = x,
      seed = 123
    )
    
    fit_mle$mle("lambda")
  }
)
names(lambda) <- NULL

Sigma <- cov(elw[, act_variables])
colnames(Sigma) <- NULL
row.names(Sigma) <- NULL