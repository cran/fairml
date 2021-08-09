\name{bank}
\docType{data}
\alias{bank}
\title{Bank Marketing}
\description{

  Direct marketing campaigns (phone calls) of a Portuguese banking institution
  to make clients subscribe a term deposit.

}
\usage{
data(bank)
}
\format{

  The data contains 41188 observations and 19 variables. See the UCI Machine
  Learning Repository for details.

}
\note{

  The data set has been pre-processed as in Zafar et al. (2019), with the
  following exceptions:

  \itemize{

    \item the variable \code{duration} has been dropped in order to learn as
      realistic predictive model;
    \item the variable \code{pdays} has been dropped because it is not defined
      for the vast majority of samples;
    \item observations where \code{loan} is \code{"unknown"} have been dropped
      because the corresponding regression coefficient estimated by \code{glm()}
      is \code{NA};
    \item the three observations where \code{default} is \code{"yes"} have been
      dropped to avoid errors in cross-validation (if all those three
      observations are in the test fold it is impossible to compute predictions
      from them).

  }

  In that paper, \code{subscribed} is the response variable, \code{age} is the
  sensitive attribute and the remaining variables are used as predictors.

}
\references{

  UCI Machine Learning Repository. \cr
  \code{https://archive.ics.uci.edu/ml/datasets/bank+marketing}

}
\examples{
data(bank)

# remove loans with unknown status, the corresponding coefficient is NA in glm().
bank = bank[bank$loan != "unknown", ]

# short-hand variable names.
r = bank[, "subscribed"]
s = bank[, c("age")]
p = bank[, setdiff(names(bank), c("subscribed", "age"))]

m = zlrm(response = r, sensitive = s, predictors = p, unfairness = 0.05)
summary(m)
}
\keyword{datasets}

