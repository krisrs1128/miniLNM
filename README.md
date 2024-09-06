# miniLNM

    library(miniLNM)
    library(dplyr)
    knitr::knit_hooks$set(output = miniLNM::ansi_aware_handler)
    options(crayon.enabled = TRUE)

miniLNM is a lightweight package for fitting and using logistic-normal
multinomial models. It wraps a simple Stan script (see `inst/stan/`
folder in the [source code](https://github.com/krisrs1128/miniLNM)) and
defines an S4 class that makes it easy to specify, estimate, and draw
samples from the fit. For example, you can use tidyselect syntax to
relate multiple compositional outputs to a set of influential biological
factors.

    example_data <- lnm_data()
    xy <- bind_cols(example_data[c("X", "y")])
    fit <- lnm(starts_with("y") ~ starts_with("x"), xy)

<pre class="r-output"><code>## Chain 1: ------------------------------------------------------------
## Chain 1: EXPERIMENTAL ALGORITHM:
## Chain 1:   This procedure has not been thoroughly tested and may be unstable
## Chain 1:   or buggy. The interface is subject to change.
## Chain 1: ------------------------------------------------------------
## Chain 1: 
## Chain 1: 
## Chain 1: 
## Chain 1: Gradient evaluation took 0.000222 seconds
## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 2.22 seconds.
## Chain 1: Adjust your expectations accordingly!
## Chain 1: 
## Chain 1: 
## Chain 1: Begin eta adaptation.
## Chain 1: Iteration:   1 / 250 [  0%]  (Adaptation)
## Chain 1: Iteration:  50 / 250 [ 20%]  (Adaptation)
## Chain 1: Iteration: 100 / 250 [ 40%]  (Adaptation)
## Chain 1: Iteration: 150 / 250 [ 60%]  (Adaptation)
## Chain 1: Iteration: 200 / 250 [ 80%]  (Adaptation)
## Chain 1: Success! Found best value [eta = 1] earlier than expected.
## Chain 1: 
## Chain 1: Begin stochastic gradient ascent.
## Chain 1:   iter             ELBO   delta_ELBO_mean   delta_ELBO_med   notes 
## Chain 1:    100       -10458.619             1.000            1.000
## Chain 1:    200        -6963.594             0.751            1.000
## Chain 1:    300        -6514.755             0.524            0.502
## Chain 1:    400        -6421.186             0.396            0.502
## Chain 1:    500        -6289.753             0.321            0.069
## Chain 1:    600        -6191.713             0.270            0.069
## Chain 1:    700        -6267.128             0.233            0.021
## Chain 1:    800        -6103.830             0.208            0.027
## Chain 1:    900        -6109.486             0.185            0.021
## Chain 1:   1000        -6060.980             0.167            0.021
## Chain 1:   1100        -6076.699             0.067            0.016
## Chain 1:   1200        -5989.375             0.019            0.015
## Chain 1:   1300        -6058.614             0.013            0.015
## Chain 1:   1400        -6007.228             0.012            0.012
## Chain 1:   1500        -5989.017             0.010            0.011
## Chain 1:   1600        -5935.948             0.010            0.009   MEAN ELBO CONVERGED   MEDIAN ELBO CONVERGED
## Chain 1: 
## Chain 1: Drawing a sample of size 1000 from the approximate posterior... 
## Chain 1: COMPLETED.
</code></pre>

    ## Warning: Pareto k diagnostic value is 10.21. Resampling is disabled. Decreasing tol_rel_obj may help if variational algorithm has terminated prematurely. Otherwise consider using sampling instead.

The print method gives a concise summary of the fitted model, which is
easier to read than the full Stan output.

    fit

<pre class="r-output"><code>## <span style='color: #000000;'>[LNM Model]
## </span>Regression formula: <span style='color: #00BBBB;'>y1 + y2 + y3 + y4 ...</span> ~ <span style='color: #BB00BB;'>x1 + x2 + x3 + x4 ...</span> 
## 5-dimensional input and 10-dimensional output 
## First few entries of estimated regression coefficients:
## <span style='color: #555555;'># A tibble: 5 × 9</span>
##      y1    y2    y3    y4    y5    y6    y7    y8    y9
##   <span style='color: #555555; font-style: italic;'><dbl></span> <span style='color: #555555; font-style: italic;'><dbl></span> <span style='color: #555555; font-style: italic;'><dbl></span> <span style='color: #555555; font-style: italic;'><dbl></span> <span style='color: #555555; font-style: italic;'><dbl></span> <span style='color: #555555; font-style: italic;'><dbl></span> <span style='color: #555555; font-style: italic;'><dbl></span> <span style='color: #555555; font-style: italic;'><dbl></span> <span style='color: #555555; font-style: italic;'><dbl></span>
## <span style='color: #555555;'>1</span>  0.85  0.3   0.08  0.07  0.97  0.31  0.14  0.19  0.72
## <span style='color: #555555;'>2</span>  0.91  0.74  0.94  0.32  0.36  0.21  0.66  0.67  0.69
## <span style='color: #555555;'>3</span>  0.48  0.95  0.28  0.47  0.22  0.16  0.36  0.77  0.41
## <span style='color: #555555;'>4</span>  0.79  0.98  0     0.44  0.48  0.83  0.08  0.3   0.88
## <span style='color: #555555;'>5</span>  0.63  0.26  0.09  0.88  0.25  0.84  0.69  0.92  0.75
</code></pre>

You can also use `predict`, like in ordinary linear models, and can draw
posterior predictive samples using `sample`.

    p_hat <- predict(fit)
    y_star <- sample(fit, depth = 200)

### Help

We welcome questions and comments about the package either through
[github](https://github.com/krisrs1128/miniLNM/issues) or
[email](mailto:ksankaran@wisc.edu).
