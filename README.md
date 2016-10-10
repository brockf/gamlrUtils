# gamlrUtils

Some quality-of-life functions to streamline the process of fitting penalized Gamma models with `gamlr`, specifically:

* `gamlr.formula` provides a formula/data frame interface to `gamlr`.
* `cv.gamlr.formula` does a similar thing for `cv.gamlr`.
* Methods for `predict` and `coef` for both the above.
* A function `cvAlpha.gamlr` to choose both the alpha and lambda parameters via cross-validation, following the approach described in the help page for `cv.gamlr`. Optionally does the cross-validation in parallel.
* Methods for `plot`, `predict` and `coef` for the above.

You can install the development version from Github using `devtools::install_github`.

    install.packages("devtools")
    library(devtools)
    install_github("brockf/gamlrUtils")
    library(gamlrUtils)
    
Note: This package is basically a find-and-replace port of @Hong-Revo's glmnetUtils package.
