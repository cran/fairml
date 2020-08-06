\name{methods for fair.model objects}
\alias{methods for fair.model objects}
\alias{coef.fair.model}
\alias{residuals.fair.model}
\alias{fitted.fair.model}
\alias{sigma.fair.model}
\alias{nobs.fair.model}
\alias{print.fair.model}
\alias{summary.fair.model}
\alias{all.equal.fair.model}
\alias{predict.nclm}
\alias{summary.nclm}
\alias{deviance.nclm}
\title{Extract information from fair.model objects}
\description{

  Extract various quantities of interest from an object of class
  \code{fair.model}.

}
\usage{
# methods for all fair.model objects.
\method{coef}{fair.model}(object, ...)
\method{residuals}{fair.model}(object, ...)
\method{fitted}{fair.model}(object, ...)
\method{sigma}{fair.model}(object, ...)
\method{nobs}{fair.model}(object, ...)
\method{print}{fair.model}(x, digits, ...)
\method{summary}{fair.model}(object, ...)
\method{all.equal}{fair.model}(target, current, ...)

# methods for nclm objects.
\method{predict}{nclm}(object, new.predictors, new.sensitive, ...)
\method{summary}{nclm}(object, ...)
\method{deviance}{nclm}(object, ...)
}
\arguments{
  \item{object,x,target,current}{an object of class \code{fair.model} or
    \code{nclm}.}
  \item{digits}{a non-negative integer, the number of significant digits.}
  \item{new.predictors}{a numeric matrix or a data frame containing numeric and
    factor columns; the predictors for the new observations.}
  \item{new.sensitive}{a numeric matrix or a data frame containing numeric and
    factor columns; the sensitive attributes for the new observations.}
  \item{\dots}{additional arguments, currently ignored.}
}
\keyword{methods}
