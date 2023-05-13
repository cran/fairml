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

    \item \code{frrm()}: the fair (linear) ridge regression model from Scutari,
      Panero and Proissl (2022).

    \item \code{fgrrm()}: thefair generalized (linear) ridge regression model
      from Scutari, Panero and Proissl (2022), supporting the Gaussian,
      binomial, Poisson, multinomial and Cox (proportional hazards) families.

    \item \code{zlrm()}: the fair logistic regression with covariance
      constraints from Zafar et al. (2019).

    \item \code{zlrm()}: a fair linear regression with covariance
      constraints following Zafar et al. (2019).

  }

  Furthermore, different fairness definitions can be used in \code{frrm()}
  and \code{fgrrm()}:
  \itemize{

    \item \code{"sp-komiyama"}: the statistical parity fairness constraint from
      Komiyama et al. (2018);

    \item \code{"eo-komiyama"}: the analogous equality of opportunity constraint
      built on the proportion of variance (or deviance) explained by sensitive
      attributes;

    \item \code{"if-berk"}: the individual fairness constraint from Berk et al.
      (2017) adapted in Scutari, Panero and Proissl (2022);

    \item user-provided functions for custom definitions.

  }

  In addition, \pkg{fairml} implements diagnostic plots, cross-validation,
  prediction and methods for most of the generics made available for linear
  models from \code{lm()} and \code{glm()}. Profile plots to trace key model
  and goodness-of-fit indicators at varying levels of fairness are available
  from \cr \code{fairness.profile.plot()}.

}
\author{

  Marco Scutari\cr
  Istituto Dalle Molle di Studi sull'Intelligenza Artificiale (IDSIA)\cr

  Maintainer: Marco Scutari \email{scutari@bnlearn.com}

}
\references{

  Berk R, Heidari H, Jabbari S, Joseph M, Kearns M, Morgenstern J, Neel S,
    Roth A (2017). "A Convex Framework for Fair Regression". FATML. \cr
    \code{https://www.fatml.org/media/documents/convex_framework_for_fair_regression.pdf}

  Komiyama J, Takeda A, Honda J, Shimao H (2018). "Nonconvex Optimization for
    Regression with Fairness Constraints". Proceedings of the 35th International
    Conference on Machine Learning (ICML), PMLR \strong{80}:2737--2746. \cr
    \code{http://proceedings.mlr.press/v80/komiyama18a/komiyama18a.pdf}

  Scutari M, Panero F, Proissl M (2022). "Achieving Fairness with a Simple Ridge
    Penalty". Statistics and Computing, \strong{32}, 77. \cr
    \code{https://link.springer.com/content/pdf/10.1007/s11222-022-10143-w.pdf}

  Zafar BJ, Valera I, Gomez-Rodriguez M, Gummadi KP (2019). "Fairness
    Constraints: a Flexible Approach for Fair Classification". Journal of
    Machine Learning Research, 30:1--42. \cr
    \code{https://www.jmlr.org/papers/volume20/18-262/18-262.pdf}

}
\keyword{package}
