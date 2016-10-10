#' @title Utilities for gamlr
#' @description
#' Some quality-of-life functions to streamline the process of fitting elastic net models with the `gamlr` package, specifically:
#'
#' \itemize{
#'   \item `gamlr.formula` provides a formula/data frame interface to `gamlr`.
#'   \item `cv.gamlr.formula` does a similar thing for `cv.gamlr`.
#'   \item Methods for `predict` and `coef` for both the above.
#'   \item A function `cvAlpha.gamlr` to choose both the alpha and lambda parameters via cross-validation, following the approach described in the help page for `cv.gamlr`. Optionally does the cross-validation in parallel.
#'   \item Methods for `plot`, `predict` and `coef` for the above.
#' }
#'
#' @docType package
#' @name gamlrUtils
#' @aliases gamlrUtils-package
NULL


dropIntercept <- function(matr)
{
    if(!is.matrix(matr))
        matr <- as.matrix(matr)
    matr[, -1, drop=FALSE]
}
