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
    used in fitting the model. Currently either \code{"sp-komiyama"},
    \code{"eo-komiyama"} or \code{"if-berk"}. It may also be a function: see
    below for details.}
  \item{family}{a character string, either \code{"gaussian"} to fit a linear
    regression, \code{"binomial"} to fit a logistic regression, \code{"poisson"}
    to fit a log-linear regression, \code{"cox"} to fit a Cox proportional
    hazards regression of \code{"multinomial"} to fit a multinomial logistic
    regression.}
  \item{lambda}{a non-negative number, a ridge-regression penalty coefficient.
    It defaults to zero.}
  \item{save.auxiliary}{a logical value, whether to save the fitted values and
    the residuals of the auxiliary model that constructs the decorrelated
    predictors. The default value is \code{FALSE}.}
}
\details{

  \code{frrm()} and \code{fgrrm()} can accommodate different definitions of
  fairness, which can be selected via the \code{definition} argument. The labels
  for the built-in definitions are:
  \itemize{

    \item \code{"sp-komiyama"} for the same definition of fairness as
      \code{nclm()}: the model bounds the proportion of the variance that is
      explained by the sensitive attributes over the total explained variance.
      This falls within the definition of statistical parity.
    \item \code{"eo-komiyama"} enforces equality of opportunity in a similar
      way: it regresses the fitted values against the sensitive attributes and
      the response, and it bounds the proportion of the variance explained by
      the sensitive attributes over the total explained variance in that model.
    \item \code{"if-berk"} enforces individual fairness by penalizing the model
      for each pair of observations with different values of the sensitive
      attributes and different responses.

  }

  Users may also pass a function via the \code{definition} argument to plug
  custom fairness definitions. This function should have signature
  \code{function(model, y, S, U, family)} and return an array with an element
  called \code{"value"} (optionally along with others). The arguments will
  contain the model fitted for the current level of fairness (\code{model}),
  the sanitized response variable (\code{y}), the design matrix for the
  sanitized sensitive attributes (\code{S}), the design matrix for the
  sanitized decorrelated predictors (\code{U}) and the character string
  identifying the family the model belongs to (\code{family}).

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
\references{

  Scutari M, Panero F, Proissl M (2022). "Achieving Fairness with a Simple Ridge
    Penalty". Statistics and Computing, \strong{32}, 77. \cr
    \code{https://link.springer.com/content/pdf/10.1007/s11222-022-10143-w.pdf}

}
\keyword{regression}
\keyword{classification}
