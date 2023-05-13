\name{methods for fair.model objects}
\alias{methods for fair.model objects}
\alias{coef.fair.model}
\alias{residuals.fair.model}
\alias{fitted.fair.model}
\alias{sigma.fair.model}
\alias{deviance.fair.model}
\alias{logLik.fair.model}
\alias{nobs.fair.model}
\alias{print.fair.model}
\alias{summary.fair.model}
\alias{all.equal.fair.model}
\alias{plot.fair.model}
\alias{predict.nclm}
\alias{predict.zlm}
\alias{predict.zlrm}
\alias{predict.frrm}
\alias{predict.fgrrm}
\title{Extract information from fair.model objects}
\description{

  Extract various quantities of interest from an object of class
  \code{fair.model}.

}
\usage{
# methods for all fair.model objects.
\method{coef}{fair.model}(object, ...)
\method{residuals}{fair.model}(object, ...)
\method{fitted}{fair.model}(object, type = "response", ...)
\method{sigma}{fair.model}(object, ...)
\method{deviance}{fair.model}(object, ...)
\method{logLik}{fair.model}(object, ...)
\method{nobs}{fair.model}(object, ...)
\method{print}{fair.model}(x, digits, ...)
\method{summary}{fair.model}(object, ...)
\method{all.equal}{fair.model}(target, current, ...)
\method{plot}{fair.model}(x, support = FALSE, regression = FALSE, ncol = 2, ...)

# predict() methods.
\method{predict}{nclm}(object, new.predictors, new.sensitive, type = "response", ...)
\method{predict}{zlm}(object, new.predictors, type = "response", ...)
\method{predict}{zlrm}(object, new.predictors, type = "response", ...)
\method{predict}{frrm}(object, new.predictors, new.sensitive, type = "response", ...)
\method{predict}{fgrrm}(object, new.predictors, new.sensitive, type = "response", ...)
}
\arguments{
  \item{object,x,target,current}{an object of class \code{fair.model} or
    \code{nclm}.}
  \item{type}{a character string, the type of fitted value. If
    \code{"response"}, \code{fitted()} and \code{predict()} will return the
    fitted values (if the response in the model is continuous) or the
    classification probabilities (if it was discrete). If \code{"class"} and
    \code{object} is a classifier, \code{fitted()} and \code{predict()} will
    return the class labels as a factor. If \code{"link"} and \code{object} is a
    classifier, \code{fitted()} and \code{predict()} will return the linear
    component of the fitted or predicted value, on the scale of the link
    function.}
  \item{digits}{a non-negative integer, the number of significant digits.}
  \item{new.predictors}{a numeric matrix or a data frame containing numeric and
    factor columns; the predictors for the new observations.}
  \item{new.sensitive}{a numeric matrix or a data frame containing numeric and
    factor columns; the sensitive attributes for the new observations.}
  \item{support}{a logical value, whether to draw support lines (diagonal of the
    first quadrant, horizontal line at zero, etc.) in \code{plot()}.}
  \item{regression}{a logical value, whether to draw the regression line of the
    observed values on the fitted values from the model in \code{plot()}.}
  \item{ncol}{a positive integer, the number of columns the plots will be
    arranged into.}
  \item{\dots}{additional arguments, currently ignored.}
}
\author{Marco Scutari}
\keyword{methods}
