data {
  int<lower = 0> N;
  int<lower = 0> max_U_plus_one;
  int<lower = 0> t;
  int<lower = 0> U[N];
  real<lower = 0> delta[N, max_U_plus_one];
  matrix[1, 2] f;
  matrix[2, 1] ones;
}

parameters {
  real<lower = 0> h;
  real<lower = 0> mu;
}

transformed parameters {
  matrix[2, 2] Q = rep_matrix(0, 2, 2);
  vector[2] lambda = rep_vector(0, 2);
  
  // Detection rate (live state)
  lambda[1] = mu;
  
  // Transition rate matrix
  Q[1, 1] = -h;
  Q[1, 2] = h;
}

model {
  matrix[1, 2] acc; // Accumulation matrix
  matrix[2, 2] Gamma;
  
  // Priors
  h ~ gamma(1, 4);
  mu ~ gamma(1, 4);
  
  // Likelihood
  for(i in 1:N) {
    matrix[2, 2] Omega[U[i] + 1];
    if(U[i] > 0) {
      for(j in 1:U[i]) {
        Gamma = diag_post_multiply(matrix_exp((Q - diag_matrix(lambda)) * delta[i, j]), lambda);
        Omega[j] = rep_matrix(0, 2, 2);
        Omega[j, 1, 1] = Gamma[1,1 ];
      } // j
      // last det to T
      Gamma = matrix_exp((Q- diag_matrix(lambda)) * delta[i, U[i] + 1]);
      
      Omega[U[i] + 1] = rep_matrix(0, 2, 2);
      Omega[U[i] + 1, 1, 1:2] = Gamma[1, 1:2];
      
      // Calculate likelihood of individual i
      acc = f * Omega[1];
      for(j in 2:(U[i] + 1)) {
        acc *= Omega[j];
      }
      target += log(acc * ones);
      
    }else{
      // Never detected
      Gamma = matrix_exp((Q - diag_matrix(lambda)) * t);
      
      Omega[1] = rep_matrix(0, 2, 2);
      Omega[1, 1, 1:2] = Gamma[1, 1:2];
      
      target += log(f * Omega[1] * ones);
    }
  } //i
}
