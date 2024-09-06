#' Simulates data from a Logistic Normal Multinomial Model.
#'
#' @param N The number of samples in the output data.
#' @param D The number of covariates, each of which can influence the response
#'   composition vector (e.g., the timepoint or disease status).
#' @param K The number of output dimensions (e.g., number of taxa).
#'
#' @return A list with the following components:
#' \describe{
#'   \item{X}{An N x D matrix of covariates.}
#'   \item{y}{The N x K simulated samples.}
#'   \item{B}{The D x K relationship between covariates and outputs.}
#' }
#' @importFrom dplyr as_tibble
#' @examples
#' lnm_data(5, 3, 3)
#' @export
lnm_data <- function(N = 100, D = 5, K = 10) {
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
