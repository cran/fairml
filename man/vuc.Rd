\name{vuc.test}
\docType{data}
\alias{vuc.test}
\title{Synthetic data set to test fair classification models}
\description{

  This a synthetic data set used as a test case in the \pkg{fairml} package.

}
\usage{
data(vuc.test)
}
\format{

  The \code{vuc.test} data set is a list with following three elements:
  \itemize{

    \item \code{y}, the response variable;
    \item \code{X}, a numeric matrix containing 3 predictors called \code{X1},
      \code{X2} and \code{X3};
    \item \code{S}, a numeric matrix containing 3 sensitive attributes called
      \code{S1}, \code{S2} and \code{S3}.

  }

}
\note{

  This data set is called \code{vuc.test} because it is generated from a
  \emph{v}ery \emph{u}nfair \emph{c}lassification model in which sensitive
  attributes explain a significant proportion of the deviance.
  The code used to generate the data is as follows.

\preformatted{
library(mvtnorm)
sigma = matrix(0.3, nrow = 6, ncol = 6)
diag(sigma) = 1
n = 1000
X = rmvnorm(n, mean = rep(0, 6), sigma = sigma)
nu = 1 + 0.5 * X[, 1] + 0.6 * X[, 2] + 0.7 * X[, 3] + 0.8 * X[, 4] +
         0.9 * X[, 5] + 1.0 * X[, 6]
y = rbinom(n = nrow(X), size = 1, prob = exp(nu) / (1 + exp(nu)))
S = X[, 4:6]
X = X[, 1:3]
colnames(X) = c("X1", "X2", "X3")
colnames(S) = c("S1", "S2", "S3")
vuc.test = list(y = y, X = X, S = S)
}
}
\examples{
data(vuc.test)
sensitive.attributes.model = glm(y ~ S, data = vuc.test, family = "binomial")
deviance(sensitive.attributes.model)
overall.model = glm(y ~ X + S, data = vuc.test, family = "binomial")
deviance(overall.model)
}
\keyword{datasets}

