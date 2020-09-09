\name{fairness.profile.plot}
\alias{fairness.profile.plot}
\title{Profile Fair Models with Respect to Tuning Parameters}
\description{

  Visually explore various aspect of a model over the range of possible values
  of the tuning parameters that control its fairness.

}
\usage{
fairness.profile.plot(response, predictors, sensitive, epsilon,
  legend = FALSE, type = "coefficients", model = "nclm", model.args = list())
}
\arguments{
  \item{response}{a numeric vector, the response variable.}
  \item{predictors}{a numeric matrix or a data frame containing numeric and
    factor columns; the predictors.}
  \item{sensitive}{a numeric matrix or a data frame containing numeric and
    factor columns; the sensitive attributes.}
  \item{epsilon}{a vector of positive numbers in [0, 1], the proportion of the
    explained variance that can be attributed to \code{S}. The default value is
    \code{seq(from = 0.00, to = 1, by = 0.02)}.}
  \item{legend}{a logical value, whether to add a legend to the plot.}
  \item{type}{a character string, either \code{"coefficients"} (the default) or
    \code{"variance"}.}
  \item{model}{a character string, the label of the model. Currently only
    \code{"nclm"} is available.}
  \item{model.args}{additional arguments passed to the model.}
}
\details{

  \code{fairness.profile.plot()} fits the model for all the values of the
  argument \code{epsilon}, and produces a profile plot of the regression
  coefficients or the proportion of explained variance.

  If \code{type = "coefficients"}, the coefficients of the model are plotted
  against the values of \code{epsilon}.

  If \code{type = "variance"}, the following quantities are plotted against the
  values of epsilon:

  \enumerate{

    \item the proportion of variance explained by the sensitive attributes (with
      respect to the response;
    \item the proportion of variance explained by the predictors (with respect
      to the response;
    \item the proportion of variance explained by the sensitive attributes (with
      respect to the combined sensitive attributes and predictors).

  }

}
\value{

  A \code{trellis} object containing a \pkg{lattice} plot.

}
\examples{
data(vur.test)
fairness.profile.plot(response = vur.test$y, predictors = vur.test$X,
  sensitive = vur.test$S, type = "coefficients", legend = TRUE)
fairness.profile.plot(response = vur.test$y, predictors = vur.test$X,
  sensitive = vur.test$S, type = "variance", legend = TRUE)
}
\author{Marco Scutari}
\keyword{plots}
\keyword{model selection}
