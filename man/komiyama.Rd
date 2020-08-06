\name{nclm}
\alias{nclm}
\title{Nonconvex Optimization for Regression with Fairness Constraints}
\description{

  Fair regression model based on nonconvex optimization from Komiyama
  et al. (2018).

}
\usage{
nclm(response, predictors, sensitive, epsilon)
}
\arguments{
  \item{response}{a numeric vector, the response variable.}
  \item{predictors}{a numeric matrix or a data frame containing numeric and
    factor columns; the predictors.}
  \item{sensitive}{a numeric matrix or a data frame containing numeric and
    factor columns; the sensitive attributes.}
  \item{epsilon}{a positive number in [0, 1], the proportion of the explained
    variance that can be attributed to \code{S}.}
}
\details{

  The algorithm proposed by Komiyama et al. (2018) works like this:

  \enumerate{

    \item regresses the predictors against the sensitive attributes;
    \item constructs a new set of predictors that are decorrelated from the
      sensitive attributes using the residuals of this regression;
    \item regresses the response against the decorrelated predictors and the
      sensitive attributes, while
    \item bounding the proportion of variance the sensitive attributes can
      explain with respect to the overall explained variance of the model.

  }

  Both \code{sensitive} and \code{predictors} are standardized internally before
  estimating the regression coefficients, which are then rescaled back to match
  the original scales of the variables. \code{response} is only standardized if
  it has a variance smaller than \code{1}, as that seems to improve the stability
  of the solutions provided by the optimizer (as far as the data included in
  \pkg{fairml} are concerned).

}
\value{

  \code{nclm()} returns an object of class \code{c("nclm", "fair.model")}.

}
\references{

  Komiyama J, Takeda A, Honda J, Shimao H (2018). "Nonconvex Optimization for
    Regression with Fairness Constraints". Proceedints of the 35th International
    Conference on Machine Learning (ICML), PMLR \strong{80}:2737--2746.

}
\author{Marco Scutari}
\keyword{regression}
