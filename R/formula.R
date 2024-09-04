tidy_strings <- function() {
    "starts_with|ends_with|matches|any_of|all_of"
}

formula_string <- function(f, xy) {
    xy |>
        select(eval(f)) |>
        colnames()
}

#' @importFrom formula.tools lhs rhs
#' @importFrom glue glue
prepare_formula <- function(f, xy) {
    result <- f
    if (grepl(tidy_strings(), as.character(f))) {
        y_vars <- formula_string(lhs(f), xy)
        x_vars <- formula_string(rhs(f), xy)
        result <- glue("{paste0(y_vars, collapse = '+')} ~ {paste0(x_vars, collapse = '+')}") |>
            as.formula()
    }

    result
}
