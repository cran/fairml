\name{zlrm}
\alias{zlrm}
\title{Zafar's Logistic Regression}
\description{

  A logistic regression model enforcing fairness by bounding the covariance
  between sensitive attributes and predictors.

}
\usage{
zlrm(response, predictors, sensitive, unfairness)
}
\arguments{
  \item{response}{a numeric vector, the response variable.}
  \item{predictors}{a numeric matrix or a data frame containing numeric and
    factor columns; the predictors.}
  \item{sensitive}{a numeric matrix or a data frame containing numeric and
    factor columns; the sensitive attributes.}
  \item{unfairness}{a positive number in [0, 1], how unfair is the model allowed
    to be. A value of \code{0} means the model is completely fair, while a value
    of \code{1} means the model is not constrained to be fair at all.}
}
\details{

  \code{zlrm()} defines fairness as statistical parity.

  The algorithm minimises the log-likelihood of the logistic regression model
  under the constraint that the covariance between each sensitive attribute and
  the fitted values is smaller than \code{unfairness} in absolute value. The
  logistic regression model includes \code{predictors} as explanatory variables;
  the variables \code{sensitive} only appear in the constraints.

  Both \code{sensitive} and \code{predictors} are standardized internally before
  estimating the regression coefficients, which are then rescaled back to match
  the original scales of the variables.

}
\value{

  \code{zlrm()} returns an object of class \code{c("zlrm", "fair.model")}.

}
\references{

  Zafar BJ, Valera I, Gomez-Rodriguez M, Gummadi KP (2019). "Fairness
    Constraints: a Flexible Approach for Fair Classification". Journal of
    Machine Learning Research, 30:1--42. \cr
    \code{https://www.jmlr.org/papers/volume20/18-262/18-262.pdf}

}
\author{Marco Scutari}
\keyword{classification}
