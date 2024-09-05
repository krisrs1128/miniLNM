
example_data <- lnm_data(N = 50, K = 10)
xy <- dplyr::bind_cols(example_data[c("X", "y")])
fit <- lnm(
    starts_with("y") ~ starts_with("x"), xy, 
    iter = 25, output_samples = 25
)

test_that("lnm() provides the right classes on simulated data.", {
    expect_s4_class(fit, "lnm")
    expect_s4_class(fit@estimate, "stanfit")
    expect_s3_class(fit@formula, "formula")
})

test_that("lnm() estimates are the correct dimension.", {
    D <- ncol(example_data[["X"]])
    K <- ncol(example_data[["y"]])
    expect_equal(dim(miniLNM:::beta_mean(fit)), c(D, K - 1))
})