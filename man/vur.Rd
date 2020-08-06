\name{vur.test}
\docType{data}
\alias{vur.test}
\title{Synthetic data set to test fair regression models}
\description{

  This a synthetic data set used as a test case in the \pkg{fairml} package.

}
\usage{
data(vur.test)
}
\format{

  The \code{vur.test} data set is a list with following three elements:
  \itemize{

    \item \code{y}, the response variable;
    \item \code{X}, a numeric matrix containing 3 predictors called \code{X1},
      \code{X2} and \code{X3};
    \item \code{S}, a numeric matrix containing 3 sensitive attributes called
      \code{S1}, \code{S2} and \code{S3}.

  }

}
\note{

  This data set is called \code{vur.test} because it is generated from a
  \emph{v}ery \emph{u}nfair \emph{r}egression model in which sensitive
  attributes explain the lion's share of the overall explained variance.
  The code used to generate the data is as follows.

\preformatted{
library(mvtnorm)
sigma = matrix(0.3, nrow = 6, ncol = 6)
diag(sigma) = 1
n = 1000
X = rmvnorm(n, mean = rep(0, 6), sigma = sigma)
y = 2 + 2 * X[, 1] + 3 * X[, 2] + 4 * X[, 3] + 5 * X[, 4] +
        6 * X[, 5] + 7 * X[, 6] + rnorm(n, sd = 10)
S = X[, 4:6]
X = X[, 1:3]
colnames(X) = c("X1", "X2", "X3")
colnames(S) = c("S1", "S2", "S3")
vur.test = list(y = y, X = X, S = S)
}
}
\examples{
data(vur.test)
sensitive.attributes.model = lm(y ~ S, data = vur.test)
summary(sensitive.attributes.model)$r.squared
overall.model = lm(y ~ X + S, data = vur.test)
summary(overall.model)$r.squared
}
\keyword{datasets}

