\name{zlm}
\alias{zlm}
\alias{zlm.orig}
\alias{zlrm}
\alias{zlrm.orig}
\title{Zafar's Linear and Logistic Regressions}
\description{

  Linear and logistic regression models enforcing fairness by bounding the
  covariance between sensitive attributes and predictors.

}
\usage{
# a fair linear regression model.
zlm(response, predictors, sensitive, unfairness)
zlm.orig(response, predictors, sensitive, max.abs.cov)
# a fair logistic regression model.
zlrm(response, predictors, sensitive, unfairness)
zlrm.orig(response, predictors, sensitive, max.abs.cov)
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
  \item{max.abs.cov}{a non-negative number, the original bound on the maximum
    absolute covariance from Zafar et al. (2019).}
}
\details{

  \code{zlm()} and \code{zlrm()} define fairness as statistical parity.

  Estimation minimizes the log-likelihood of the regression models under the
  constraint that the correlation between each sensitive attribute and the
  fitted values (on the linear predictor scale, in the case of logistic
  regression) is smaller than \code{unfairness} in absolute value. Both models
  include \code{predictors} as explanatory variables; the variables
  \code{sensitive} only appear in the constraints.

  The only difference between \code{zlm()} and \code{zlm.orig()}, and between
  \code{zlrm()} and \code{zlrm.orig()}, is that the latter uses the original
  constraint on the covariances of the individual sensitive attributes from
  Zafar et al. (2019).

}
\value{

  \code{zlm()} and \code{zlm.orig()} return an object of class
    \code{c("zlm", "fair.model")}.
  \code{zlrm()} and \code{zlrm.orig()} return an object of class
    \code{c("zlrm", "fair.model")}.

}
\references{

  Zafar BJ, Valera I, Gomez-Rodriguez M, Gummadi KP (2019). "Fairness
    Constraints: a Flexible Approach for Fair Classification". Journal of
    Machine Learning Research, 30:1--42. \cr
    \code{https://www.jmlr.org/papers/volume20/18-262/18-262.pdf}

}
\author{Marco Scutari}
\seealso{\link{nclm}, \link{frrm}, \link{fgrrm}}
\keyword{regression}
\keyword{classification}
