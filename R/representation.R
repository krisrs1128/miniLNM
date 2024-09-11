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
#' This class combines all information into three slots:
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

#' Pretty Printing
#'
#' Helper function for printing ANSI in Rmarkdown output. Use this at the start
#' of your Rmarkdown files to include colors in the printed object names in the
#' final compiled output.
#'
#' Taken from the post at
#'
#' https://blog.djnavarro.net/posts/2021-04-18_pretty-little-clis/
#'
#' @param x A character vector potentially including ANSI.
#' @param options Unused placeholder argument.
#' @return A string with HTML reformatted to ensure colors appear in printed
#'   code blocks in rmarkdown output.
#' @examples
#' knitr::knit_hooks$set(output = ansi_aware_handler)
#' options(crayon.enabled = TRUE)
#' @importFrom fansi sgr_to_html
#' @export
ansi_aware_handler <- function(x, options) {
    paste0(
        "<pre class=\"r-output\"><code>",
        sgr_to_html(x = x, warn = FALSE, term.cap = "256"),
        "</code></pre>"
    )
}

#' Concise Formula Representation
#' @examples
#' shorten_formula(as.formula("x1 ~ y2"))
#' @importFrom cli col_cyan col_magenta
#' @noRd
shorten_formula <- function(fmla, n_show = 4) {
    lhs_str <- paste0(head(lhs.vars(fmla), n_show), collapse = " + ")
    if (length(lhs.vars(fmla)) > n_show) {
        lhs_str <- paste(lhs_str, "...")
    }

    rhs_str <- paste0(head(rhs.vars(fmla), n_show), collapse = " + ")
    if (length(rhs.vars(fmla)) > n_show) {
        rhs_str <- paste(rhs_str, "...")
    }
    glue("{col_cyan(lhs_str)} ~ {col_magenta(rhs_str)}")
}

#' @importFrom dplyr as_tibble
#' @importFrom cli col_black
setMethod("show", "lnm", function(object) {
    B <- beta_mean(object)
    cat(col_black("[LNM Model]", "\n"))
    cat(glue("Regression formula: {shorten_formula(object@formula)}"), "\n")
    cat(glue("{nrow(B)}-dimensional input and {ncol(B) + 1}-dimensional output"), "\n")
    cat("First few entries of estimated regression coefficients:\n")
    print(as_tibble(round(B, 2)))
})