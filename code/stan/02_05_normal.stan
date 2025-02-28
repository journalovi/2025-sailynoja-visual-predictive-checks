data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
}
parameters {
  real Intercept;  // temporary intercept for centered predictors
  real<lower=0> sigma;  // dispersion parameter
}
transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior
  lprior += student_t_lpdf(Intercept | 3, 0, 2.5);
  lprior += student_t_lpdf(sigma | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
}
model {
  // likelihood including constants
  // initialize linear predictor term
  vector[N] mu = rep_vector(0.0, N);
  mu += Intercept;
  target += normal_lpdf(Y | mu, sigma);
  // priors including constants
  target += lprior;
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept;
  vector[N] yrep;
  yrep = to_vector(normal_rng(rep_vector(Intercept, N), sigma));
}
