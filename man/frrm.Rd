\name{frrm}
\alias{frrm}
\title{Fair Ridge Regression Model}
\description{

  A regression model enforcing fairness with a ridge penalty.

}
\usage{
frrm(response, predictors, sensitive, unfairness)
}
\arguments{
  \item{response}{a numeric vector, the response variable.}
  \item{predictors}{a numeric matrix or a data frame containing numeric and
    factor columns; the predictors.}
  \item{sensitive}{a numeric matrix or a data frame containing numeric and
    factor columns; the sensitive attributes.}
  \item{unfairness}{a positive number in [0, 1], the proportion of the explained
    variance that can be attributed to the sensitive attributes.}
}
\details{

  \code{frrm()} defines fairness as statistical parity.

  The algorithm works like this:

  \enumerate{

    \item regresses the predictors against the sensitive attributes;
    \item constructs a new set of predictors that are decorrelated from the
      sensitive attributes using the residuals of this regression;
    \item regresses the response against the decorrelated predictors and the
      sensitive attributes, while
    \item using a ridge penalty to control the proportion of variance the
      sensitive attributes can explain with respect to the overall explained
      variance of the model.

  }

  Both \code{sensitive} and \code{predictors} are standardized internally before
  estimating the regression coefficients, which are then rescaled back to match
  the original scales of the variables.

}
\value{

  \code{frrm()} returns an object of class \code{c("frrm", "fair.model")}.

}
\seealso{\link{nclm}}
\author{Marco Scutari}
\keyword{regression}
