#' S4 Class for a Logistic Normal Multinomial Model
#'
#' The Logistic Normal Multinomial (LNM) model is used to learn the relationship
#' between experimental/environmental factors and community composition. It is a
#' statistical model that estimates the probabilities of different outcomes in a
#' multinomial distribution, given a set of covariates. The LNM model assumes
#' that a log-ratio of the outcome probabilities follow a multivariate normal
#' distribution. By fitting the LNM model to observed data, we can infer the
#' effects of the covariates on the outcome compositions.
#'
#' This class combines all information into fourthree slots:
#'
#' @slot estimate The fitted logistic normal multinomial model, with parameter B
#'   relating covariates to outcome compositions.
#' @slot template The data used to estimate the parameters in the estimate slot.
#' @slot formula The R formula representation of the relationship between output
#'   compositions and input variables.
#' @export
#'
#' @export
setClass(
    "lnm",
    representation(
        estimate = "ANY",
        template = "ANY",
        formula = "ANY"
    )
)
