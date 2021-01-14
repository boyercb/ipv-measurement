// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] y;
}

// The parameters accepted by the model. Our model
// accepts one parameter 'lambda'.
parameters {
  real<lower=0> lambda;
}

// The model to be estimated. We model the output
// 'y' to be related to a latent poisson variable 'u' 
// where:
// y = 0 if u = 0
// y = 1 if u = 1
// y = 2 if 1 < u <= 4
// y = 3 if u >= 5

model {
  for (n in 1:N) {
    if (y[n] == 0) {
      target += poisson_lpmf(0 | lambda);
    } else if (y[n] == 1) {
      target += poisson_lpmf(1 | lambda);
    } else if (y[n] == 2) {
      target += log(exp(poisson_lcdf(5 | lambda)) - exp(poisson_lcdf(1 | lambda)));
    } else if (y[n] == 3) {
      target += poisson_lccdf(5 | lambda);
    }
  }
}

