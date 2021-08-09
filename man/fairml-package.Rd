\name{fairml-package}
\alias{fairml-package}
\alias{fairml}
\docType{package}
\title{Fair models in machine learning}
\description{

  Fair machine learning models: estimation, tuning and prediction.

}
\details{

  \pkg{fairml} implements key algorithms for learning machine learning models
  while enforcing fairness with respect to a set of observed sensitive (or
  protected) attributes.

  Currently \pkg{fairml} implements the following algorithms (references below):
  \itemize{

    \item \code{nclm()}: the non-convex formulation of fair linear regression
      model from Komiyama et al. (2018).

    \item \code{frrm()}: my fair (linear) ridge regression model.

    \item \code{fgrrm()}: my fair generalized (linear) ridge regression model.

    \item \code{zlrm()}: the fair logistic regression with covariance
      constraints from Zafar et al. (2019).

    \item \code{zlrm()}: a fair linear regression with covariance
      constraints following Zafar et al. (2019).

  }

}
\author{

  Marco Scutari\cr
  Istituto Dalle Molle di Studi sull'Intelligenza Artificiale (IDSIA)\cr

  Maintainer: Marco Scutari \email{marco.scutari@gmail.com}

}
\references{

  Komiyama J, Takeda A, Honda J, Shimao H (2018). "Nonconvex Optimization for
    Regression with Fairness Constraints". Proceedints of the 35th International
    Conference on Machine Learning (ICML), PMLR \strong{80}:2737--2746. \cr
    \code{http://proceedings.mlr.press/v80/komiyama18a/komiyama18a.pdf}

  Zafar BJ, Valera I, Gomez-Rodriguez M, Gummadi KP (2019). "Fairness
    Constraints: a Flexible Approach for Fair Classification". Journal of
    Machine Learning Research, 30:1--42. \cr
    \code{https://www.jmlr.org/papers/volume20/18-262/18-262.pdf}

}
\keyword{package}
