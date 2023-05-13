\name{flchain}
\alias{flchain}
\title{Obesity Levels}
\description{

  A re-analysis of the \code{flchain} data set in the \pkg{survival} package.

}
\references{

  Keya KN, Islam R, Pan S, Stockwell I, Foulds J (2020). Equitable Allocation of
    Healthcare Resources with Fair Cox Models. Proceedings of the 2021 SIAM
    International Conference on Data Mining (SDM), 190--198. \cr
    \code{https://epubs.siam.org/doi/pdf/10.1137/1.9781611976700.22}

}
\examples{
library(survival)
data(flchain)

# complete data analysis.
flchain = flchain[complete.cases(flchain), ]
# short-hand variable names.
r = cbind(time = flchain$futime + 1, status = flchain$death)
s = flchain[, c("age", "sex")]
p = flchain[, c("sample.yr", "kappa", "lambda", "flc.grp", "creatinine", "mgus",
                "chapter")]

\dontrun{
m = fgrrm(response = r, sensitive = s, predictors = p, family = "cox",
          unfairness = 0.05)
summary(m)
}}
\keyword{datasets}

