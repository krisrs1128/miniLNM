
void matrix_normal_lp(matrix x, int N, int M, real mu, real sigma2) {
  for (n in 1:N) {
    for (m in 1:M) {
      x[n, m] ~ normal(mu, sigma2);
    }
  }
}


void vector_inv_gamma_lp(vector x, int D, real l1, real l2) {
  for (d in 1:D) {
    x[d] ~ inv_gamma(l1, l2);
  }
}

void outcome_mu_lp(matrix mu, matrix x, matrix B, int N, int K, vector sigmas_mu) {
  for (i in 1:N) {
    for (k in 1:K) {
      mu[i, k] ~ normal(x[i] * col(B, k), sigmas_mu[k]);
    }
  }
}

void outcome_y_lp(array[,] int y, int N, matrix mu, int K) {
  vector[K + 1] p;
  for (i in 1:N) {
    p = phi_inv(mu[i]);
    y[i] ~ multinomial(to_vector(p));
  }
}

vector phi_inv(row_vector mu) {
  vector[1 + num_elements(mu)] mu_ = append_row(0.0, mu');
  return softmax(mu_);
}
