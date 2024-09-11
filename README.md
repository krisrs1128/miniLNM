# miniLNM

    library(miniLNM)
    library(dplyr)
    knitr::knit_hooks$set(output = miniLNM::ansi_aware_handler)
    options(crayon.enabled = TRUE)

miniLNM is a lightweight package for fitting and using logistic-normal
multinomial models. It wraps a simple ‘Stan’ script (see `inst/stan/`
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
## Chain 1: Gradient evaluation took 0.003206 seconds
## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 32.06 seconds.
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
## Chain 1:    100       -11758.981             1.000            1.000
## Chain 1:    200        -7210.902             0.815            1.000
## Chain 1:    300        -6537.465             0.578            0.631
## Chain 1:    400        -6378.312             0.440            0.631
## Chain 1:    500        -6340.350             0.353            0.103
## Chain 1:    600        -6251.900             0.296            0.103
## Chain 1:    700        -6199.291             0.255            0.025
## Chain 1:    800        -6177.029             0.224            0.025
## Chain 1:    900        -6143.302             0.200            0.014
## Chain 1:   1000        -6117.542             0.180            0.014
## Chain 1:   1100        -6157.149             0.081            0.008   MEDIAN ELBO CONVERGED
## Chain 1: 
## Chain 1: Drawing a sample of size 1000 from the approximate posterior... 
## Chain 1: COMPLETED.
</code></pre>

    ## Warning: Pareto k diagnostic value is 12.45. Resampling is disabled. Decreasing tol_rel_obj may help if variational algorithm has terminated prematurely. Otherwise consider using sampling instead.

The print method gives a concise summary of the fitted model, which is
easier to read than the full ‘Stan’ output.

    fit

<pre class="r-output"><code>## <span style='color: #000000;'>[LNM Model]
## </span>Regression formula: <span style='color: #00BBBB;'>y1 + y2 + y3 + y4 ...</span> ~ <span style='color: #BB00BB;'>x1 + x2 + x3 + x4 ...</span> 
## 5-dimensional input and 10-dimensional output 
## First few entries of estimated regression coefficients:
## <span style='color: #555555;'># A tibble: 5 × 9</span>
##      y1    y2    y3    y4    y5    y6    y7    y8    y9
##   <span style='color: #555555; font-style: italic;'><dbl></span> <span style='color: #555555; font-style: italic;'><dbl></span> <span style='color: #555555; font-style: italic;'><dbl></span> <span style='color: #555555; font-style: italic;'><dbl></span> <span style='color: #555555; font-style: italic;'><dbl></span> <span style='color: #555555; font-style: italic;'><dbl></span> <span style='color: #555555; font-style: italic;'><dbl></span> <span style='color: #555555; font-style: italic;'><dbl></span> <span style='color: #555555; font-style: italic;'><dbl></span>
## <span style='color: #555555;'>1</span>  0.24  0.23  0.21  0.12  0.46  0.56  0.81  0.86  0.32
## <span style='color: #555555;'>2</span>  0.89  0.6   0.66  0.87  0.01  0.9   0.14  0.74  0.06
## <span style='color: #555555;'>3</span>  0.82  0.89  0.89  0.87 -<span style='color: #BB0000;'>0.02</span>  0.34  0.61  0.59  0.63
## <span style='color: #555555;'>4</span>  0.31  0.95  0.53  0.39  0.12  0.27  0.8   0     0.11
## <span style='color: #555555;'>5</span>  0.23  0.14  0.11  0.2   0.95  0.73  0.33  0.45  0.63
</code></pre>

You can also use `predict`, like in ordinary linear models, and can draw
posterior predictive samples using `sample`.

    p_hat <- predict(fit)
    y_star <- sample(fit, depth = 200)

### Help

We welcome questions and comments about the package either through
[github](https://github.com/krisrs1128/miniLNM/issues) or
[email](mailto:ksankaran@wisc.edu).
