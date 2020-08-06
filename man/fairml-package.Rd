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
      from Komiyama et al. (2018).

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
    Conference on Machine Learning (ICML), PMLR \strong{80}:2737--2746.

}
\keyword{package}
