#' @importFrom tibble as_tibble
#' @export
lnm_data <- function(N = 100, D = 5, K = 20) {
  # sample covariates and predictors
  X <- runif(N * D) |>
    matrix(N, D)
  B <- matrix(runif(D * (K - 1)), D, K - 1)

  # sample observed counts
  p <- phi_inverse(X %*% B)
  y <- t(apply(p, 1, \(pi) rmultinom(1, 1e4, pi))) |>
    as.data.frame() |>
    as_tibble()

  # return data
  colnames(X) <- glue("x{seq_len(ncol(X))}")
  colnames(y) <- glue("y{seq_len(ncol(y))}")
  list(X = as_tibble(X), y = y, B = B)
}