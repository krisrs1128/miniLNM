#' Bypass psis check
#'
#' In our tests, we work with small sample sizes where sometimes psis returns
#' NA. This causes downstream failures in rstan's vb(), and hence failures on
#' CRAN. To bypass this failure case, we modify `loo:psis()` so that it never
#' returns NA, and we define a "safe" version of vb (in R/estimate.R) that uses
#' this alternative psis.
#' @importFrom loo psis
#' @noRd
safe_vb_ <- function(object, ...) {
    ns <- asNamespace("loo")
    original_psis <- ns$psis
    on.exit({
        unlockBinding("psis", ns)
        assign("psis", original_psis, envir = ns)
        lockBinding("psis", ns)
    })

    # calls original_psis directly (not loo::psis) so it isn't caught by its
    # own patched binding once installed below
    unlockBinding("psis", ns)
    assign("psis", function(...) {
        p <- original_psis(...)
        if (is.na(p$diagnostics$pareto_k)) {
            p$diagnostics$pareto_k <- 0
        }
        p
    }, envir = ns)
    lockBinding("psis", ns)

    rstan::vb(object, ...)
}
