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
## Chain 1: Gradient evaluation took 0.000256 seconds
## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 2.56 seconds.
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
## Chain 1:    100       -11184.466             1.000            1.000
## Chain 1:    200        -6856.152             0.816            1.000
## Chain 1:    300        -6552.809             0.559            0.631
## Chain 1:    400        -6504.088             0.421            0.631
## Chain 1:    500        -6260.436             0.345            0.046
## Chain 1:    600        -6232.001             0.288            0.046
## Chain 1:    700        -6154.505             0.249            0.039
## Chain 1:    800        -6109.442             0.219            0.039
## Chain 1:    900        -6190.336             0.196            0.013
## Chain 1:   1000        -6038.314             0.179            0.025
## Chain 1:   1100        -5991.059             0.079            0.013
## Chain 1:   1200        -6007.286             0.017            0.013
## Chain 1:   1300        -5990.459             0.012            0.008   MEDIAN ELBO CONVERGED
## Chain 1: 
## Chain 1: Drawing a sample of size 1000 from the approximate posterior... 
## Chain 1: COMPLETED.
</code></pre>

    ## Warning: Pareto k diagnostic value is 11.45. Resampling is disabled. Decreasing tol_rel_obj may help if variational algorithm has terminated prematurely. Otherwise consider using sampling instead.

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
## <span style='color: #555555;'>1</span>  0.57  0.41  0.22  0.51  0.05  0.92  0.88  0.86  0.16
## <span style='color: #555555;'>2</span>  0.2   0.12  0.21  0.45  0.95  0.24  0.05  0.6   0.15
## <span style='color: #555555;'>3</span>  0.41  0.33  0.49  0.37  0.9   0.09  0.74  0.59  0.64
## <span style='color: #555555;'>4</span>  0.43  0.03  0.56  0.32  0.97  0.23  0.63  0.59  0.03
## <span style='color: #555555;'>5</span>  0.62  0.33  0.72  0.73  0.61  0.42  0.26  0.86  0.13
</code></pre>

You can also use `predict`, like in ordinary linear models, and can draw
posterior predictive samples using `sample`.

    p_hat <- predict(fit)
    y_star <- sample(fit, depth = 200)

### Help

We welcome questions and comments about the package either through
[github](https://github.com/krisrs1128/miniLNM/issues) or
[email](mailto:ksankaran@wisc.edu).
