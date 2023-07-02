data {
  int<lower=0> N;
  vector[N] MEDV;
  vector[N] CRIM;
  vector[N] RM;
  vector[N] LSTAT;
  vector[N] PTRATIO;
}


parameters {
  real intercept;
  real beta_CRIM;
  real beta_RM;
  real beta_LSTAT;
  real beta_PTRATIO;
  real sigma;
}


model {
  intercept ~ normal(0, 10);
  beta_CRIM ~ cauchy(-0.9, 1);
  beta_RM ~ normal(10, 2);
  beta_LSTAT ~ cauchy(-1.7, 1);
  beta_PTRATIO ~ normal(-0.3, 0.3);
  sigma ~ normal(0, 10);
  MEDV ~ normal(intercept + beta_CRIM*CRIM + beta_RM*RM + beta_LSTAT*LSTAT + beta_PTRATIO*PTRATIO, sigma);
}

