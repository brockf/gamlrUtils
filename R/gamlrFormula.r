#' @include gamlrUtils.r
NULL

#' @name gamlr
#' @export
gamlr <- function(x, ...)
UseMethod("gamlr")

#' @rdname gamlr
#' @method gamlr default
#' @export
gamlr.default <- function(x, ...)
gamlr::gamlr(x, ...)


#' Formula interface for elastic net modelling with gamlr
#'
#' @param x For the default method, a matrix of predictor variables.
#' @param formula A model formula; interaction terms are allowed and will be expanded per the usual rules for linear models.
#' @param data A data frame or matrix containing the variables in the formula.
#' @param weights An optional vector of case weights to be used in the fitting process. If missing, defaults to an unweighted fit.
#' @param offset An optional vector of offsets, an \emph{a priori} known component to be included in the linear predictor.
#' @param subset An optional vector specifying the subset of observations to be used to fit the model.
#' @param na.action A function which indicates what should happen when the data contains missing values. For the \code{predict} method, \code{na.action = na.pass} will predict missing values with \code{NA}; \code{na.omit} or \code{na.exclude} will drop them.
#' @param drop.unused.levels Should factors have unused levels dropped? Defaults to \code{FALSE}.
#' @param xlev A named list of character vectors giving the full set of levels to be assumed for each factor.
#' @param sparse Should the model matrix be in sparse format? This can save memory when dealing with many factor variables, each with many levels (but see the warning below).
#' @param ... For \code{gamlr.formula} and \code{gamlr.default}, other arguments to be passed to \code{\link[gamlr:gamlr]{gamlr::gamlr}}; for the \code{predict} and \code{coef} methods, arguments to be passed to their counterparts in package \code{gamlr}.
#'
#' @details
#' The \code{gamlr} function in this package is an S3 generic with a formula and a default method. The former calls the latter, and the latter is simply a direct call to the \code{gamlr} function in package \code{gamlr}. All the arguments to \code{gamlr::gamlr} are (or should be) supported.
#'
#' The code works in a similar manner to \code{lm}, \code{glm} and other modelling functions. The arguments are used to generate a \emph{model frame}, which is a data frame augmented with information about the roles the columns play in fitting the model. This is then turned into a \emph{model matrix} and a response vector, which are passed to \code{gamlr::gamlr} along with any arguments in \code{...}. If \code{sparse} is TRUE, then \code{Matrix::sparse.model.matrix} is used instead of \code{stats::model.matrix} to create the model matrix.
#'
#' The \code{predict} and \code{coef} methods are wrappers for the corresponding methods in the \code{gamlr} package. The former constructs a predictor model matrix from its \code{newdata} argument and passes that as the \code{newx} argument to \code{gamlr:::predict.gamlr}.
#'
#' @section Value:
#' For \code{gamlr.formula}, an object of class \code{gamlr.formula}. This is basically the same object created by \code{gamlr::gamlr}, but with extra components to allow formula usage.
#'
#' @section Warning:
#' Fundamental to R's handling of formulas, model frames and model matrices is a \code{\link{terms}} object, which encodes how variables and their interactions (if any) are organised. One of the attributes of this object is a matrix with one row per variable, and one column per main effect and interaction. Thus, at minimum, this is (approximately) a \eqn{p \times p}{p x p} square matrix where \eqn{p} is the number of main effects in the model. When \eqn{p ~ 16000}, this matrix will be about a gigabyte in size. Because of this, you should use the formula interface with caution when working with wide datasets and limited memory.
#'
#' @seealso
#' \code{\link[gamlr:gamlr]{gamlr::gamlr}}, \code{\link[gamlr:predict.gamlr]{gamlr:::predict.gamlr}}, \code{\link[gamlr:coef.gamlr]{gamlr:::coef.gamlr}}, \code{\link{model.frame}}, \code{\link{model.matrix}}
#'
#' @examples
#' gamlr(mpg ~ ., data=mtcars)
#'
#' \dontrun{
#'
#' # Leukemia example dataset from Trevor Hastie's website
#' download.file("http://web.stanford.edu/~hastie/gamlr/gamlrData/Leukemia.RData",
#'               "Leukemia.RData")
#' load("Leukemia.Rdata")
#' leuk <- do.call(data.frame, Leukemia)
#' gamlr(y ~ ., leuk, family="binomial")
#' }
#' @rdname gamlr
#' @method gamlr formula
#' @export
gamlr.formula <- function(formula, data, ..., weights, offset=NULL, subset=NULL, na.action=getOption("na.action"),
                           drop.unused.levels=FALSE, xlev=NULL, sparse=FALSE)
{
    cl <- match.call(expand.dots=FALSE)
    cl$`...` <- cl$sparse <- NULL
    cl[[1]] <- quote(stats::model.frame)
    mf <- eval.parent(cl)

    x <- if(sparse)
        dropIntercept(Matrix::sparse.model.matrix(attr(mf, "terms"), mf))
    else dropIntercept(model.matrix(attr(mf, "terms"), mf))
    y <- model.response(mf)
    weights <- model.extract(mf, "weights")
    offset <- model.extract(mf, "offset")
    if(is.null(weights))
        weights <- rep(1, length(y))

    model <- gamlr::gamlr(x, y, weights=weights, offset=offset, ...)
    model$call <- match.call()
    model$terms <- terms(mf)
    model$sparse <- sparse
    model$na.action <- attr(mf, "na.action")
    class(model) <- c("gamlr.formula", class(model))
    model
}


#' @param object For the \code{predict} and \code{coef} methods, an object of class \code{gamlr.formula}.
#' @param newdata For the \code{predict} method, a data frame containing the observations for which to calculate predictions.
#' @rdname gamlr
#' @export
#' @method predict gamlr.formula
predict.gamlr.formula <- function(object, newdata, na.action=na.pass, ...)
{
    if(!inherits(object, "gamlr.formula"))
        stop("invalid gamlr.formula object")
    tt <- delete.response(object$terms)
    newdata <- model.frame(tt, newdata, na.action=na.action)
    x <- if(object$sparse)
        dropIntercept(Matrix::sparse.model.matrix(tt, newdata))
    else dropIntercept(model.matrix(tt, newdata))
    class(object) <- class(object)[-1]
    predict(object, x, ...)
}

#' @rdname gamlr
#' @export
#' @method coef gamlr.formula
coef.gamlr.formula <- function(object, ...)
{
    if(!inherits(object, "gamlr.formula"))
        stop("invalid gamlr.formula object")
    class(object) <- class(object)[-1]
    coef(object, ...)
}

