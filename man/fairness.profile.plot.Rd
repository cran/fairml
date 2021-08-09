\name{fairness.profile.plot}
\alias{fairness.profile.plot}
\title{Profile Fair Models with Respect to Tuning Parameters}
\description{

  Visually explore various aspect of a model over the range of possible values
  of the tuning parameters that control its fairness.

}
\usage{
fairness.profile.plot(response, predictors, sensitive, unfairness,
  legend = FALSE, type = "coefficients", model, model.args = list(), cluster)
}
\arguments{
  \item{response}{a numeric vector, the response variable.}
  \item{predictors}{a numeric matrix or a data frame containing numeric and
    factor columns; the predictors.}
  \item{sensitive}{a numeric matrix or a data frame containing numeric and
    factor columns; the sensitive attributes.}
  \item{unfairness}{a vector of positive numbers in [0, 1], how unfair is the
    model allowed to be. The default value is \code{seq(from = 0.00, to = 1,
    by = 0.02)}.}
  \item{legend}{a logical value, whether to add a legend to the plot.}
  \item{type}{a character string, either \code{"coefficients"} (the default) or
    \code{"constraints"}.}
  \item{model}{a character string, the label of the model. Currently
    \code{"nclm"}, \code{"frrm"}, \code{"fgrrm"}, \code{"zlm"} and \code{"zlrm"}
    are available.}
  \item{model.args}{additional arguments passed to the model.}
  \item{cluster}{an optional cluster object from package \pkg{parallel}, to
    fit models in parallel.}
}
\details{

  \code{fairness.profile.plot()} fits the model for all the values of the
  argument \code{unfairness}, and produces a profile plot of the regression
  coefficients or the proportion of explained variance.

  If \code{type = "coefficients"}, the coefficients of the model are plotted
  against the values of \code{unfairness}.

  If \code{type = "constraints"}, the following quantities are plotted against
  the values of \code{unfairness}:

  \enumerate{

    \item For model \code{"nclm"}, and model \code{"frrm"} with
      \code{definition = "sp-komiyama"}:
    \enumerate{

      \item the proportion of variance explained by the sensitive attributes
        (with respect to the response);
      \item the proportion of variance explained by the predictors (with respect
        to the response);
      \item the proportion of variance explained by the sensitive attributes
        (with respect to the combined sensitive attributes and predictors).

    }

    \item For model \code{"frrm"} with \code{definition = "eo-komiyama"}:
    \enumerate{

      \item the proportion of variance explained by the sensitive attributes
        (with respect to the fitted values);
      \item the proportion of variance explained by the response (with respect
        to the fitted values);
      \item the proportion of variance explained by the sensitive attributes
        (with respect to the combined sensitive attributes and response).

    }

    \item For model \code{"fgrrm"}: same as for \code{"frrm"} with the same
      \code{definition}.

    \item For models \code{"zlm"} and \code{"zlrm"}: the correlations between
      the fitted values (from \code{fitted()} with \code{type = "link"}) and the
      sensitive attributes.

  }

  If \code{type = "precision-recall"} and the \code{model} is a classifier, the
  precision, recall and F1 measures are plotted against the values of
  \code{unfairness}.

  If \code{type = "rmse"} and the \code{model} is a linear regression, the
  residuals mean square error are plotted against the values of
  \code{unfairness}.

}
\value{

  A \code{trellis} object containing a \pkg{lattice} plot.

}
\examples{
data(vur.test)
fairness.profile.plot(response = vur.test$y, predictors = vur.test$X,
  sensitive = vur.test$S, type = "coefficients", model = "nclm", legend = TRUE)
fairness.profile.plot(response = vur.test$y, predictors = vur.test$X,
  sensitive = vur.test$S, type = "constraints", model = "nclm", legend = TRUE)
fairness.profile.plot(response = vur.test$y, predictors = vur.test$X,
  sensitive = vur.test$S, type = "rmse", model = "nclm", legend = TRUE)

# profile plots fitting models in parallel.
\dontrun{
library(parallel)
cl = makeCluster(2)
fairness.profile.plot(response = vur.test$y, predictors = vur.test$X,
  sensitive = vur.test$S, model = "nclm", cluster = cl)
stopCluster(cl)
}}
\author{Marco Scutari}
\keyword{plots}
\keyword{model selection}
