\name{communities.and.crime}
\docType{data}
\alias{communities.and.crime}
\title{iCommunities and Crime Data Set}
\description{

  Combined socio-economic data from the 1990 Census, law enforcement data
  from the 1990 LEMAS survey, and crime data from the 1995 FBI UCR for various
  communities in the United States.

}
\usage{
data(communities.and.crime)
}
\format{

  The data contains 1969 observations and 104 variables. See the UCI Machine
  Learning Repository for details.

}
\note{

  The data set has been pre-processed as in Komiyama et al. (2018), with the
  following exceptions:

  \itemize{

    \item the variable \code{community} has been dropped, as it is
      non-predictive and contains a sizeable number of missing values;
    \item the variables \code{LemasSwornFT}, \code{LemasSwFTPerPop},
      \code{LemasSwFTFieldOps}, \code{LemasSwFTFieldPerPop},
      \code{LemasTotalReq}, \code{LemasTotReqPerPop}, \code{PolicReqPerOffic},
      \code{PolicPerPop}, \code{RacialMatchCommPol}, \code{PctPolicWhite},
      \code{PctPolicBlack}, \code{PctPolicHisp}, \code{PctPolicAsian},
      \code{PctPolicMinor}, \code{OfficAssgnDrugUnits},
      \code{NumKindsDrugsSeiz}, \code{PolicAveOTWorked}, \code{PolicCars},
      \code{PolicOperBudg}, \code{LemasPctPolicOnPatr},
      \code{LemasGangUnitDeploy} and \code{PolicBudgPerPop} have been dropped
      because they have more than 80\% missing values.

  }

  In that paper, \code{ViolentCrimesPerPop} is the response variable,
  \code{racepctblack} and \code{PctForeignBorn} are the sensitive attributes and
  the remaining variables are used as predictors.

}
\references{

  UCI Machine Learning Repository: \cr
  \code{http://archive.ics.uci.edu/ml/datasets/communities+and+crime}

}
\examples{
data(communities.and.crime)

# short-hand variable names.
cc = communities.and.crime[complete.cases(communities.and.crime), ]
r = cc[, "ViolentCrimesPerPop"]
s = cc[, c("racepctblack", "PctForeignBorn")]
p = cc[, setdiff(names(cc), c("ViolentCrimesPerPop", names(s)))]

m = nclm(response = r, sensitive = s, predictors = p, unfairness = 0.05)
summary(m)

m = frrm(response = r, sensitive = s, predictors = p, unfairness = 0.05)
summary(m)
}
\keyword{datasets}

