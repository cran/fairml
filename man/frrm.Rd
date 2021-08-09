\name{frrm}
\alias{frrm}
\alias{fgrrm}
\title{Fair Ridge Regression Model}
\description{

  A regression model enforcing fairness with a ridge penalty.

}
\usage{
# a fair ridge regression model.
frrm(response, predictors, sensitive, unfairness,
  definition = "sp-komiyama", lambda = 0, save.auxiliary = FALSE)
# a fair generalized ridge regression model.
fgrrm(response, predictors, sensitive, unfairness,
  definition = "sp-komiyama", family = "binomial", lambda = 0,
  save.auxiliary = FALSE)
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
  \item{definition}{a character string, the label of the definition of fairness
    used in fitting the model. Currently either \code{"sp-komiyama"} or
    \code{"eo-komiyama"}. See below for details.}
  \item{family}{a character string, either \code{"binomial"} to fit a logistic
    regression or \code{"gaussian"} to fit a linear regression.}
  \item{lambda}{a non-negative number, a ridge-regression penalty coefficient.
    It defaults to zero.}
  \item{save.auxiliary}{a logical value, whether to save the fitted values and
    the residuals of the auxiliary model that constructs the decorrelated
    predictors. The default value is \code{FALSE}.}
}
\details{

  \code{frrm()} can accommodate different definitions of fairness, which can
  be selected via the \code{definition} argument.
  \itemize{

    \item \code{"sp-komiyama"} uses the same definition of fairness as
      \code{nclm()}: the model bounds the proportion of the variance that is
      explained by the sensitive attributes over the total explained variance.
      This falls within the definition of statistical parity.
    \item \code{"eo-komiyama"} enforces equality of opportunity in a similar
      way: it regresses the fitted values against the sensitive attributes and
      the response, and it bounds the proportion of the variance explained by
      the sensitive attributes over the total explained variance in that model.

  }

  The algorithm works like this:

  \enumerate{

    \item regresses the predictors against the sensitive attributes;
    \item constructs a new set of predictors that are decorrelated from the
      sensitive attributes using the residuals of this regression;
    \item regresses the response against the decorrelated predictors and the
      sensitive attributes; while
    \item using a ridge penalty to control the proportion of variance the
      sensitive attributes can explain with respect to the overall explained
      variance of the model.

  }

  Both \code{sensitive} and \code{predictors} are standardized internally before
  estimating the regression coefficients, which are then rescaled back to match
  the original scales of the variables.

  \code{fgrrm()} is the extension of \code{frrm()} to generalized linear models,
  currently implementing linear (\code{family = "gaussian"}) and logistic
  (\code{family = "binomial"}) regressions. \code{fgrrm()} is equivalent to
  \code{frrm()} with \code{family = "gaussian"}. The definition of fairness are
  identical between \code{frrm()} and \code{fgrrm()}.

}
\value{

  \code{frrm()} returns an object of class \code{c("frrm", "fair.model")}.
  \code{fgrrm()} returns an object of class \code{c("fgrrm", "fair.model")}.

}
\seealso{\link{nclm}, \link{zlm}, \link{zlrm}}
\author{Marco Scutari}
\keyword{regression}
\keyword{classification}
