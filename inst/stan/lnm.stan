functions {
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
}

data {
    int<lower=1> N;// number of samples
    int<lower=1> K;// number of taxa - 1
    int<lower=1> D;// number of covariates
    array[N, K + 1] int<lower=0> y;
    matrix[N, D] x;
    real sigma_b; // prior SD for B coefficients
    real l1; // inverse gamma hyperparameter for sigmas_mu
    real l2; // inverse gamma hyperparameter for sigmas_mu
}

parameters {
    matrix[D, K] beta;
    matrix[N, K] mu;
    vector<lower=0>[K] sigmas_mu;// diagonal covariance for mu
}

model {
    // these calls define the prior
    matrix_normal_lp(beta, D, K, 0, sigma_b);
    vector_inv_gamma_lp(sigmas_mu, K, l1, l2);

    // this defines the likelihood
    outcome_mu_lp(mu, x, beta, N, K, sigmas_mu);
    outcome_y_lp(y, N, mu, K);
}
