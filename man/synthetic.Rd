\name{synthetic data sets}
\docType{data}
\alias{vu.test}
\title{Synthetic data set to test fair models}
\description{

  Synthetic data set used as test cases in the \pkg{fairml} package.

}
\usage{
data(vu.test)
}
\format{

  The data are stored a list with following three elements:
  \itemize{

    \item \code{gaussian}, \code{binomial}, \code{poisson}, \code{coxph} and
      \code{multinomial} are response variables for the different families;
    \item \code{X}, a numeric matrix containing 3 predictors called \code{X1},
      \code{X2} and \code{X3};
    \item \code{S}, a numeric matrix containing 3 sensitive attributes called
      \code{S1}, \code{S2} and \code{S3}.

  }

}
\note{

  This data set is called \code{vu.test} because it is generated from
  \emph{v}ery \emph{u}nfair models in which sensitive attributes explain the
  lion's share of the overall explained variance or deviance.

  The code used to generate the predictors and the sensitive attributes is as
  follows.

\preformatted{
library(mvtnorm)
sigma = matrix(0.3, nrow = 6, ncol = 6)
diag(sigma) = 1
n = 1000
X = rmvnorm(n, mean = rep(0, 6), sigma = sigma)
S = X[, 4:6]
X = X[, 1:3]
colnames(X) = c("X1", "X2", "X3")
colnames(S) = c("S1", "S2", "S3")
}

  The continuous response in \code{gaussian} is produced as follows.

\preformatted{
gaussian = 2 + 2 * X[, 1] + 3 * X[, 2] + 4 * X[, 3] + 5 * S[, 1] +
               6 * S[, 2] + 7 * S[, 3] + rnorm(n, sd = 10)
}

  The discrete response in \code{binomial} is produced as follows.

\preformatted{
nu = 1 + 0.5 * X[, 1] + 0.6 * X[, 2] + 0.7 * X[, 3] + 0.8 * S[, 1] +
         0.9 * S[, 2] + 1.0 * S[, 3]
binomial = rbinom(n = nrow(X), size = 1, prob = exp(nu) / (1 + exp(nu)))
binomial = as.factor(binomial)
}

  The log-linear response in \code{poisson} is produced as follows.

\preformatted{
nu = 1 + 0.5 * X[, 1] + 0.6 * X[, 2] + 0.7 * X[, 3] + 0.8 * S[, 1] +
         0.9 * S[, 2] + 1.0 * S[, 3]
poisson = rpois(n = nrow(X), lambda = exp(nu))
}

  The response for the Cox proportional hazards \code{coxph} is
  produced as follows.

\preformatted{
fx = 1 + 0.5 * X[, 1] + 0.6 * X[, 2] + 0.7 * X[, 3] + 0.8 * S[, 1] +
         0.9 * S[, 2] + 1.0 * S[, 3]
hx = exp(fx)
ty = rexp(length(fx), hx)
tcens = rbinom(n = length(fx), prob = 0.3, size = 1)
coxph = cbind(time = ty, status = 1 - tcens)
}

  The discrete response in \code{multinomial} is produced as follows.

\preformatted{
nu1 = 1 + 0.5 * X[, 1] + 0.6 * X[, 2] + 0.7 * X[, 3] + 0.8 * S[, 1] +
          0.9 * S[, 2] + 1.0 * S[, 3]
nu2 = 1 + 0.2 * X[, 1] + 0.2 * X[, 2] + 0.2 * X[, 3] + 0.6 * S[, 1] +
          0.6 * S[, 2] + 0.6 * S[, 3]
nu3 = 1 + 0.7 * X[, 1] + 0.6 * X[, 2] + 0.5 * X[, 3] + 0.1 * S[, 1] +
          0.1 * S[, 2] + 0.1 * S[, 3]
nu4 = 1 + 0.4 * X[, 1] + 0.4 * X[, 2] + 0.4 * X[, 3] + 0.4 * S[, 1] +
          0.4 * S[, 2] + 0.4 * S[, 3]
norm = exp(nu1) + exp(nu2) + exp(nu3) + exp(nu4)
probs = matrix(c(exp(nu1) / norm, exp(nu2) / norm,
                 exp(nu3) / norm, exp(nu4) / norm),
        ncol = 4, byrow = FALSE)
multinomial = apply(probs, MARGIN = 1,
                function(x) sample(letters[1:4], size = 1, prob = x))
multinomial = factor(multinomial, labels = letters[1:4])
}

}
\examples{
summary(fgrrm(response = vu.test$gaussian, predictors = vu.test$X,
  sensitive = vu.test$S, unfairness = 1, family = "gaussian"))
summary(fgrrm(response = vu.test$binomial, predictors = vu.test$X,
  sensitive = vu.test$S, unfairness = 1, family = "binomial"))
summary(fgrrm(response = vu.test$poisson, predictors = vu.test$X,
  sensitive = vu.test$S, unfairness = 1, family = "poisson"))
summary(fgrrm(response = vu.test$coxph, predictors = vu.test$X,
  sensitive = vu.test$S, unfairness = 1, family = "cox"))
summary(fgrrm(response = vu.test$multinomial, predictors = vu.test$X,
  sensitive = vu.test$S, unfairness = 1, family = "multinomial"))
}
\author{Marco Scutari}
\keyword{datasets}

