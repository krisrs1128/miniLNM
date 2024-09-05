
example_data <- lnm_data(N = 50, K = 10)
xy <- dplyr::bind_cols(example_data[c("X", "y")])
fit <- lnm(
    starts_with("y") ~ starts_with("x"), xy, 
    iter = 25, output_samples = 25
)

test_that("sample() gives correct output with no newdata.", {
    N <- nrow(example_data[["X"]])
    K <- ncol(example_data[["y"]])

    y_star <- sample(fit)
    expect_true(is.matrix(y_star))
    expect_equal(dim(y_star), c(N, K))
})

test_that("Sample gives correct output with newdata available.", {
    D <- ncol(example_data[["X"]])
    K <- ncol(example_data[["y"]])

    new_x <- example_data[["X"]][1:20, ]
    y_star <- sample(fit, newdata = new_x)
    expect_true(is.matrix(y_star))
    expect_equal(dim(y_star), c(20, K))
})

test_that("Sampling supports control over sequencing depth.", {
    N <- nrow(example_data[["X"]])
    y_star <- sample(fit, depth = 20)
    expect_equal(rowSums(y_star), rep(20, N))

    y_star <- sample(fit, depth = 5000)
    expect_equal(rowSums(y_star), rep(5000, N))
})