\name{confint.fair.model}
\alias{confint.fair.model}
\alias{plot.fair.confint}
\title{Confidence Intervals for Fair Models}
\description{

  Confidence intervals for the parameters of the models in the \pkg{fairml}
  package.

}
\usage{
\method{confint}{fair.model}(object, parm, level = 0.95, method = "boot",
  method.args = list(), ...)

\method{plot}{fair.confint}(x, support = FALSE, ...)
}
\arguments{
  \item{object}{an object of class \code{fair.model}.}
  \item{parm}{a character vector, the names of the parameters to compute the
    confidence intervals for. The default is to do that for all parameters.}
  \item{level}{a number between 0 and 1, the coverage of the confidence
    intervals.}
  \item{method}{a character string, the method used to compute the confidence
    intervals. See below for details.}
  \item{method.args}{optional arguments passed to the \code{method}.}
  \item{\dots}{additional arguments (unused).}
  \item{x}{an object of class \code{fair.confint}.}
  \item{support}{a logical value, whether to draw a vertical line at zero.}
}
\details{

  The only available \code{method} is \code{"boot"}, which implements
  nonparametric bootstrap with observation resampling. It has the following
  optional arguments:
  \itemize{

    \item \code{response}, \code{predictors}, \code{sensitive}: the data the
      model was fitted from.

    \item \code{R}: a positive integer number, the number of bootstrap samples.
      The default value is \code{200}.

    \item \code{m}: a positive integer number, the size of bootstrap samples.
      The default value is the size of the original sample.

  }

}
\value{

  \code{confint()} returns an object of class \code{fair.confint} which wraps
  a two- or three-dimensinal matrix. The upper and lower bounds of the
  confidence intervals in the columns, the variables are in the rows.

}
\examples{
mgaus = fgrrm(response = vu.test$gaussian, predictors = vu.test$X,
          sensitive = vu.test$S, unfairness = 0.05, family = "gaussian")
ci = confint(mgaus, method = "boot",
       method.args = list(response = vu.test$gaussian, predictors = vu.test$X,
                          sensitive = vu.test$S, R = 20))
ci
plot(ci)
}
\author{Marco Scutari}
\keyword{model selection}
