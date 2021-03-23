// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] y;
}

// The parameters accepted by the model. Our model
// accepts one parameter 'lambda'.
parameters {
  real<lower=0> lambda;
  real<lower=0, upper=1> theta;
}

// The model to be estimated. We model the output
// 'y' to be related to a latent poisson variable 'u' 
// where:
// y = 0 if u = 0
// y = 1 if u = 1
// y = 2 if 2 < u <= 4
// y = 3 if u >= 5

model {
  for (n in 1:N) {
    if (y[n] == 0) {
      target += log_sum_exp(
        bernoulli_lpmf(1 | theta), 
        bernoulli_lpmf(0 | theta) + poisson_lpmf(0 | lambda)
      );
    } else if (y[n] == 1) {
      target += bernoulli_lpmf(0 | theta) + poisson_lpmf(1 | lambda);
    } else if (y[n] == 2) {
      target += bernoulli_lpmf(0 | theta) +
        log(exp(poisson_lcdf(4 | lambda)) - exp(poisson_lcdf(1 | lambda)));
    } else if (y[n] == 3) {
      target += bernoulli_lpmf(0 | theta) + poisson_lccdf(4 | lambda);
    }
  }
}

