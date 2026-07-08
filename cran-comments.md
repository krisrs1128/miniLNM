## Update 0.1.1

This version fixes a check failure that appeared in recent CRAN builds:

```
Error in `if (p$diagnostics$pareto_k > 1) { ... }`: missing value where TRUE/FALSE needed
```

The crash happens when calling the `loo:psis()` function in `rstan::vb()` within
our package tests.

The issue is that `loo::psis()` can return `NaN` Pareto-k estimates in the small
sample sizes considered in our tests.  When `rstan` encounters the `NaN`, it
throws an error for an otherwise successful fit (we only need the point
estimates, underestimating the variance is a known issue in vb fits).  There is
no `vb()` argument that disables this check.

Our approach adds `safe_vb_()` to `R/utils.R`, which temporarily rebinds
`loo::psis` for the duration of a `vb()`. `lnm()` calls this wrapper instead of
`rstan::vb()`. This allows us to return the fit without crashing, but requires
us to use `unlockBinding`, leading to the note below.

## R CMD check results

0 errors | 0 warnings | 1 note

```
* checking R code for possible problems ... NOTE
Found the following possibly unsafe calls:
File 'utils.R':
  unlockBinding("psis", ns)
  unlockBinding("psis", ns)
```

This NOTE is necessary considering the that `loo::psis` is defined elsewhere,
and we don't want to copy the entire `vb()` function into our code because then
we would miss out on any future updates to `rstan`. The modification is
temporary and is returned to the original state through an `on.exit()` call.

## Test environments

* local macOS (aarch64-apple-darwin23), R 4.6.0
* devtools::check_win_release()