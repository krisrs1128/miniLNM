
example_data <- lnm_data(N = 50, K = 10)
xy <- dplyr::bind_cols(example_data[c("X", "y")])
fit <- lnm(
    starts_with("y") ~ starts_with("x"), xy, 
    iter = 25, output_samples = 25
)

test_that("Prediction gives correct output with no newdata.", {
    N <- nrow(example_data[["X"]])
    K <- ncol(example_data[["y"]])

    new_y <- predict(fit)
    expect_true(is.matrix(new_y))
    expect_equal(dim(new_y), c(N, K))
})

test_that("Prediction gives correct output with newdata available.", {
    D <- ncol(example_data[["X"]])
    K <- ncol(example_data[["y"]])

    new_x <- example_data[["X"]][1:20, ]
    new_y <- predict(fit, new_x)
    expect_true(is.matrix(new_y))
    expect_equal(dim(new_y), c(20, K))
})