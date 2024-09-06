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
## Chain 1: Iteration: 250 / 250 [100%]  (Adaptation)
## Chain 1: Success! Found best value [eta = 0.1].
## Chain 1: 
## Chain 1: Begin stochastic gradient ascent.
## Chain 1:   iter             ELBO   delta_ELBO_mean   delta_ELBO_med   notes 
## Chain 1:    100      -146954.937             1.000            1.000
## Chain 1:    200       -67374.378             1.091            1.181
## Chain 1:    300       -41366.455             0.937            1.000
## Chain 1:    400       -28653.853             0.813            1.000
## Chain 1:    500       -21171.513             0.721            0.629
## Chain 1:    600       -16583.529             0.647            0.629
## Chain 1:    700       -13444.159             0.588            0.444
## Chain 1:    800       -11305.372             0.538            0.444
## Chain 1:    900        -9819.242             0.495            0.353
## Chain 1:   1000        -8785.811             0.458            0.353
## Chain 1:   1100        -8026.216             0.367            0.277
## Chain 1:   1200        -7487.075             0.256            0.234
## Chain 1:   1300        -7069.103             0.199            0.189
## Chain 1:   1400        -6779.131             0.159            0.151
## Chain 1:   1500        -6576.993             0.127            0.118
## Chain 1:   1600        -6405.850             0.102            0.095
## Chain 1:   1700        -6285.715             0.080            0.072
## Chain 1:   1800        -6173.771             0.063            0.059
## Chain 1:   1900        -6101.136             0.049            0.043
## Chain 1:   2000        -6045.528             0.038            0.031
## Chain 1:   2100        -5980.596             0.030            0.027
## Chain 1:   2200        -5960.839             0.023            0.019
## Chain 1:   2300        -5912.414             0.018            0.018
## Chain 1:   2400        -5887.036             0.014            0.012
## Chain 1:   2500        -5857.171             0.012            0.011
## Chain 1:   2600        -5840.860             0.009            0.009   MEAN ELBO CONVERGED   MEDIAN ELBO CONVERGED
## Chain 1: 
## Chain 1: Drawing a sample of size 1000 from the approximate posterior... 
## Chain 1: COMPLETED.
</code></pre>

    ## Warning: Pareto k diagnostic value is 8.91. Resampling is disabled. Decreasing tol_rel_obj may help if variational algorithm has terminated prematurely. Otherwise consider using sampling instead.

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
## <span style='color: #555555;'>1</span>  0.74  0.64  0.17  0.76  0.68  0.03  0.32  0.84  0.59
## <span style='color: #555555;'>2</span>  0.87  0.81  0.35  0.38  0.91  0.04  0.24  0.41  0.41
## <span style='color: #555555;'>3</span>  0.58  0.84  0.31  0.37  0.52  0.16  0.67  0.58  0.64
## <span style='color: #555555;'>4</span>  0.32  0.63  0.84  0.52  0.72  0.23  0.06  0.16  0.12
## <span style='color: #555555;'>5</span>  0.55  0.37  0.18  0.27 -<span style='color: #BB0000;'>0.03</span>  0.52  0.88  0.9   0.83
</code></pre>

You can also use `predict`, like in ordinary linear models, and can draw
posterior predictive samples using `sample`.

    p_hat <- predict(fit)
    y_star <- sample(fit, depth = 200)

### Help

We welcome questions and comments about the package either through
[github](https://github.com/krisrs1128/miniLNM/issues) or
[email](mailto:ksankaran@wisc.edu).
