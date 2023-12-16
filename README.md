## miniLNM

A small wrapper of `cmdstanr` for estimating logistic-normal multinomial models.
Includes helpers for prediction and sampling.

```
library(lnm)
example_data <- lnm_data()
xy <- bind_cols(example_data[c("X", "y")])
fit <- lnm(starts_with("y") ~ starts_with("x"), xy)
```

```
p_hat <- predict(fit)
y_star <- sample(fit, depth = 200)
```