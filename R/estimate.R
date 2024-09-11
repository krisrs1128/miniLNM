#' Fit a logistic normal multinomial model using R's formula interface.
#'
#' This function fits a logistic normal multinomial (LNM) model to the data
#' using R's formula interface. The LNM model is a generalization of the
#' multinomial logistic regression model, allowing for correlated responses
#' within each category of the response variable. It can be used to learn the
#' relationship between experimental/environmental factors and community
#' composition. It is a statistical model that estimates the probabilities of
#' different outcomes in a multinomial distribution, given a set of covariates.
#' The LNM model assumes that a log-ratio of the outcome probabilities follow a
#' multivariate normal distribution. By fitting the LNM model to observed data,
#' we can infer the effects of the covariates on the outcome compositions.
#'
#' @param formula A formula specifying the model structure.
#' @param data A data frame containing the variables specified in the formula.
#' @param sigma_b The prior standard deviation of the beta coefficients in the
#'   LNM model. See the 'Stan' code definition in inst/stan/lnm.stan for the
#'   full model specification.
#' @param l1 The first inverse gamma hyperprior parameter for sigmas_mu.
#' @param l2 The first inverse gamma hyperprior parameter for sigmas_mu.
#' @param ... Additional arguments to be passed to the underlying vb() call from
#'     'rstan'.
#' @return An object of class "lnm" representing the fitted LNM model.
#' @importFrom formula.tools lhs.vars
#' @importFrom rstan vb
#' @importFrom methods new
#' @importFrom tidyselect any_of
#' @examples
#' example_data <- lnm_data(N = 50, K = 10)
#' xy <- dplyr::bind_cols(example_data[c("X", "y")])
#' fit <- lnm(
#'     starts_with("y") ~ starts_with("x"), xy, 
#'     iter = 25, output_samples = 25
#' )
#' @export
lnm <- function(formula, data, sigma_b = 2, l1 = 10, l2 = 10, ...) {
    # prepare input data
    formula <- prepare_formula(formula, data)
    ys <- lhs.vars(formula)
    x_data <- model_matrix_df(formula, data)

    # fit model using 'Stan'
    data_list <- list(
        y = as.matrix(select(data, any_of(ys))),
        x = x_data,
        N = nrow(x_data),
        D = ncol(x_data),
        K = length(ys) - 1,
        sigma_b = sigma_b,
        l1 = l1,
        l2 = l2
    )

    # return as an lnm class
    new(
        "lnm",
        estimate = vb(stanmodels$lnm, data_list, ...),
        formula = formula,
        template = data
    )
}

#' Inverse log ratio transformation
#'
#' This function applies the inverse logistic function to a vector, which maps
#' the values of the vector to the range (0, 1).
#'
#' @param mu A numeric vector to transform using an inverse log ratio
#'   transformation.
#' @return A numeric vector with values mapped to the range (0, 1) and a
#'   reference coordinate added.
#' @examples
#' phi_inverse(c(-5, 0, 5))
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
#' @importFrom stats as.formula model.matrix rmultinom runif terms update
#' @noRd
model_matrix_df <- function(formula, data) {
    model.matrix(formula, data) |>
        as_tibble() |>
        select(matches(rhs.vars(formula)))
}

#' Design Matrix for a Model
#'
#' This is a helper function to form the design matrix for an LNM regression
#' starting from a fitted model's formula object. It is an analog of
#' model.matrix for the multiresponse setting.
#' @param fit An object of class `lnm` whose estimate slot contains the 'rstan'
#'   fitted logistic normal multinomial model.
#' @param newdata A data.frame containing variables in the formula definition of
#'   the fit, but which hasn't been converted into the matrix format needed for
#'   internal prediction.
#' @return A matrix containing the design matrix that can be multiplied with the
#'   fitted Beta parameter to get fitted compositions.
#' @examples
#' example_data <- lnm_data(N = 10, K = 5)
#' xy <- dplyr::bind_cols(example_data[c("X", "y")])
#' fit <- lnm(
#'     starts_with("y") ~ starts_with("x"), xy,
#'     iter = 5, output_samples = 5
#' )
#' prepare_newdata(fit, example_data[["X"]])
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
#'
#' Average the samples for the beta parameter from the VB posterior mean. This
#' is used to get predicted compositions when using `predict` on an lnm model.
#'
#' @param fit An object of class `lnm` whose estimate slot contains the 'rstan'
#'   fitted logistic normal multinomial model.
#' @return A matrix whose rows are predictors and columns are outcomes in the
#'   beta parameter for the LNM model.
#' @importFrom posterior as_draws_matrix subset_draws
#' @importFrom formula.tools lhs.vars rhs.vars
#' @importFrom utils head
#' @examples
#' example_data <- lnm_data(N = 50, K = 10)
#' xy <- dplyr::bind_cols(example_data[c("X", "y")])
#' fit <- lnm(
#'     starts_with("y") ~ starts_with("x"), xy, 
#'     iter = 25, output_samples = 25
#' )
#' beta_mean(fit)
#' @export
beta_mean <- function(fit) {
    beta_draws <- as_draws_matrix(fit@estimate) |>
        subset_draws(variable = "beta")

    K <- length(lhs.vars(fit@formula))
    beta <- matrix(colMeans(beta_draws), ncol = K - 1)
    rownames(beta) <- colnames(model_matrix_df(fit@formula, fit@template))
    colnames(beta) <- head(lhs.vars(fit@formula), -1)
    beta
}

#' LNM Posterior Samples
#'
#' Return multiple samples for the beta parameter from the VB posterior mean.
#' This is used to simulate new compositions when using `sample` on an lnm
#' model.
#'
#' @param fit An object of class `lnm` whose estimate slot contains the 'rstan'
#'   fitted logistic normal multinomial model.
#' @param size The number of draws from the posterior to return.
#' @return A matrix whose rows are predictors and columns are outcomes in the
#'   beta parameter for the LNM model.
#' @examples
#' example_data <- lnm_data(N = 50, K = 10)
#' xy <- dplyr::bind_cols(example_data[c("X", "y")])
#' fit <- lnm(
#'     starts_with("y") ~ starts_with("x"), xy, 
#'     iter = 25, output_samples = 25
#' )
#' beta_samples(fit, size = 2)
#' @export
beta_samples <- function(fit, size = 1) {
    beta_draws <- as_draws_matrix(fit@estimate) |>
        subset_draws(variable = "beta")
    K <- length(lhs.vars(fit@formula))
    ix <- sample(nrow(beta_draws), size, replace = TRUE)

    b_star <- list()
    for (i in seq_along(ix)) {
        b_star[[i]] <- matrix(beta_draws[ix[i], ], ncol = K - 1)
        rownames(b_star[[i]]) <- colnames(
            model_matrix_df(fit@formula, fit@template)
        )
        colnames(b_star[[i]]) <- head(lhs.vars(fit@formula), -1)
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

#' LNM Fitted Probabilities
#'
#' Given an input dataset, predict the output composition. Specifically, this
#' outputs \eqn{\phi^{-1}(Bx)}, for the inverse log ratio transformation
#' \eqn{\phi^{-1}} and fitted covariate matrix \eqn{B}.
#' @param object An object of class lnm with fitted parameters \eqn{\hat{B}} and
#'   which we want to use to form predictions on new samples.
#' @param newdata New samples on which to form predictions. Defaults to NULL, in
#'   which case predictions are made at the same design points as those used
#'   during the original training.
#' @param ... Additional keyword arguments, for consistency with R's predict
#'   generic (never used).
#' @return A matrix with predictions along rows and outcomes along columns. Rows
#'   sum up to one.
#' @export
#' @examples
#' example_data <- lnm_data(N = 50, K = 10)
#' xy <- dplyr::bind_cols(example_data[c("X", "y")])
#' fit <- lnm(
#'     starts_with("y") ~ starts_with("x"), xy, 
#'     iter = 25, output_samples = 25
#' )
#' head(predict(fit))
setMethod("predict", "lnm", lnm_predict)

#' LNM Fitted Probabilities
#'
#' Given an input dataset, sample compositions that are consistent with the
#' input. Specifically, this samples from a multinomial with mean
#' \eqn{\phi^{-1}(Bx)}. The default depth is 5e4. Modify the "depth" parameter
#' to change this.
#' 
#' @param x An object of class lnm with fitted parameters \eqn{\hat{B}} and
#'   which we want to use to form predictions on new samples.
#' @param newdata New samples on which to form predictions. Defaults to NULL, in
#'   which case predictions are made at the same design points as those used
#'   during the original training.
#' @param size The number of samples to generate.
#' @param depth The depth to use when sampling the multinomial for each
#'   simulated element.
#' @param ... Additional keyword arguments, for consistency with R's predict
#'   generic (never used).
#' @return A matrix of dimension `size` x `n_outcomes`, where each row
#'   represents one sample from the posterior predictive of the fitted
#'   logistic-normal multinomial model. Each row sums up to the depth argument,
#'   which defaults to 5e4.
#' @examples
#' example_data <- lnm_data(N = 50, K = 10)
#' xy <- dplyr::bind_cols(example_data[c("X", "y")])
#' fit <- lnm(
#'     starts_with("y") ~ starts_with("x"), xy, 
#'     iter = 25, output_samples = 25
#' )
#' head(sample(fit))
#' @export
setMethod("sample", "lnm", lnm_sample)
