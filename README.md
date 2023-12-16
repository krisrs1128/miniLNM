## miniLNM

```
example_data <- lnm_data()
xy <- bind_cols(example_data[c("X", "y")])
fit <- lnm(starts_with("y") ~ starts_with("x"), xy)
```
