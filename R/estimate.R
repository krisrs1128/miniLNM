#' @importFrom formula.tools lhs.vars
#' @importFrom cmdstanr cmdstan_model
#' @export
lnm <- function(formula, data, sigma_b = 2, l1 = 10, l2 = 10, ...) {
  # prepare input data
  formula <- prepare_formula(formula, data)
  ys <- lhs.vars(formula)
  x_data <- model_matrix_df(formula, data)

  # fit model using stan
  data_list <- list(
    y = as.matrix(select(data, ys)),
    x = x_data,
    N = nrow(x_data),
    D = ncol(x_data),
    K = length(ys) - 1,
    sigma_b = sigma_b,
    l1 = l1,
    l2 = l2
  )

  model <- file.path(system.file(package = "miniLNM"), "lnm.stan") |>
    cmdstan_model()

  # return as an lnm class
  new(
    "lnm",
    estimate = model$variational(data_list, ...),
    formula = formula,
    template = data
  )
}

#' @export
phi_inverse <- function(mu) {
  if (is.matrix(mu)) {
    return(t(apply(mu, 1, phi_inverse_)))
  }
  phi_inverse_(mu)
}

phi_inverse_ <- function(mu) {
  sum_exp <- sum(exp(mu))
  c(1 / (1 + sum_exp), exp(mu) / (1 + sum_exp))
}

#' @importFrom dplyr select matches
#' @importFrom formula.tools rhs.vars
model_matrix_df <- function(formula, data) {
  model.matrix(formula, data) |>
    as_tibble() |>
    select(matches(rhs.vars(formula)))
}

#' @export
prepare_newdata <- function(fit, newdata = NULL) {
  if (is.null(newdata)) {
    newdata <- fit@template
  }
  update(fit@formula, NULL ~ .) |>
    model_matrix_df(newdata) |>
    as.matrix()
}

#' LNM Posterior Mean
#' @importFrom cmdstanr as_draws
#' @export
beta_mean <- function(fit) {
  beta_draws <- as_draws(fit@estimate, "beta")
  K <- length(lhs.vars(fit@formula))
  matrix(colMeans(beta_draws), ncol = K - 1)
}

#' LNM Posterior Samples
#' @export
beta_samples <- function(fit, size = 1) {
  beta_draws <- as_draws(fit@estimate, "beta")
  K <- length(lhs.vars(fit@formula))
  ix <- sample(nrow(beta_draws), size, replace = TRUE)

  b_star <- list()
  for (i in seq_along(ix)) {
    b_star[[i]] <- matrix(beta_draws[ix[i], ], ncol = K - 1)
  }

  b_star
}

lnm_predict <- function(object, newdata = NULL, ...) {
  newdata <- prepare_newdata(object, newdata)
  p_hat <- phi_inverse(newdata %*% beta_mean(object))
  colnames(p_hat) <- lhs.vars(object@formula)
  p_hat
}

#' @importFrom formula.tools lhs
lnm_sample <- function(x, size = 1, depth = 5e4, newdata = NULL, ...) {
  newdata <- prepare_newdata(x, newdata)
  b_star <- beta_samples(x, nrow(newdata))

  probs <- matrix(nrow = nrow(newdata), ncol = ncol(b_star[[1]]) + 1)
  for (i in seq_along(b_star)) {
    probs[i, ] <- phi_inverse(newdata[i, ] %*% b_star[[i]])
  }

  # sample and return
  y_star <- t(apply(probs, 1, \(p) rmultinom(1, depth, p)))
  colnames(y_star) <- terms(lhs(x@formula))
  y_star
}

#' @export
setMethod("predict", "lnm", lnm_predict)

#' @export
setMethod("sample", "lnm", lnm_sample)