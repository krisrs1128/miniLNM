## miniLNM

A small stan script for fitting logistic-normal multinomial models (see `inst/stan/` above). Includes helpers for prediction and sampling.

```
library(miniLNM)
library(tidyverse)

example_data <- lnm_data()
xy <- bind_cols(example_data[c("X", "y")])
fit <- lnm(starts_with("y") ~ starts_with("x"), xy)
```

```
p_hat <- predict(fit)
y_star <- sample(fit, depth = 200)
```
