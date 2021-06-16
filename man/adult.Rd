\name{adult}
\docType{data}
\alias{adult}
\title{Census Income}
\description{

  Predict whether income exceeds $50K per year using the U.S. 1994 Census data.

}
\usage{
data(adult)
}
\format{

  The data contains 30162 observations and 14 variables. See the UCI Machine
  Learning Repository for details.

}
\note{

  The data set has been pre-processed as in Zafar et al. (2019), with the
  following exceptions:

  \itemize{

    \item the data do not include the test sample from the UCI repository;
    \item the variables \code{"capital_gain"} and \code{"capital_loss"} have
      been scaled by \code{1/1000}.

  }

  In that paper, \code{income} is the response variable, \code{sex} and
  \code{race} are the sensitive attributes and the remaining variables are
  used as predictors.

}
\references{

  UCI Machine Learning Repository. \cr
  \code{https://archive.ics.uci.edu/ml/datasets/adult}

}
\examples{
data(adult)

# short-hand variable names.
r = adult[, "income"]
s = adult[, c("sex", "race")]
p = adult[, setdiff(names(adult), c("income", "sex", "race"))]

\dontrun{
m = zlrm(response = r, sensitive = s, predictors = p, unfairness = 0.05)
summary(m)
}}
\keyword{datasets}

