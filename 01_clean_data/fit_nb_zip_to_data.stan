// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  array[N] int<lower=0> y;
}

// The parameters accepted by the model. Our model
// accepts three parameters: 'lambda', 'theta' and 'phi'.
parameters {
  real<lower=0> lambda;
  real<lower=0, upper=1> theta;
  real<lower=0> phi;
}

// The model to be estimated. We model the output
// 'y' to be related to a latent negative binomial variable 'u' 
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
        bernoulli_lpmf(0 | theta) + neg_binomial_2_lpmf(0 | lambda, phi)
      );
    } else if (y[n] == 1) {
      target += bernoulli_lpmf(0 | theta) + neg_binomial_2_lpmf(1 | lambda, phi);
    } else if (y[n] == 2) {
      target += bernoulli_lpmf(0 | theta) +
        log(exp(neg_binomial_2_lcdf(4 | lambda, phi)) - exp(neg_binomial_2_lcdf(1 | lambda, phi)));
    } else if (y[n] == 3) {
      target += bernoulli_lpmf(0 | theta) + neg_binomial_2_lccdf(4 | lambda, phi);
    }
  }
}

