\name{national.longitudinal.survey}
\docType{data}
\alias{national.longitudinal.survey}
\title{Income and Labour Market Activities}
\description{

  Survey results from the U.S. Bureau of Labor Statistics to gather information
  on the labour market activities and other life events of several groups.

}
\usage{
data(national.longitudinal.survey)
}
\format{

  The data contains 4908 observations and the following variables:

  \itemize{

    \item \code{age}, a numeric variable containing the interviewee's age in
      years;
    \item \code{race}, a factor with 20 levels denoting various racial/ethnic
      origins;
    \item \code{gender}, a factor with levels \code{"Male"} and \code{"Female"}.
    \item \code{grade90}, a factor containing the highest completed school
      grade from "3RD GRADE" to "8TH YR COL OR MORE", with 18 levels;
    \item \code{income06}, a numeric variable, income in 2006 in 10000-USD
      units;
    \item \code{income96}, a numeric variable, income in 1996 in 10000-USD
      units;
    \item \code{income90}, a numeric variable, income in 1990 in 10000-USD
      units;
    \item \code{partner}, a factor encoding whether the interviewee has a
      partner, with levels \code{"No"} and \code{"Yes"};
    \item \code{height}, a numeric variable, the height of the interviewee;
    \item \code{weight}, a numeric variable, the weight of the interviewee;
    \item \code{famsize}, a numeric variable, the number of family members;
    \item \code{genhealth}, a factor with levels \code{"Excellent"},
      \code{"Very Good"}, \code{"Good"}, \code{"Fair"}, \code{"Poor"} encoding
      the general health status of the interviewee;
    \item \code{illegalact}, a numeric variable containing the number of illegal
      acts committed by the interviewee;
    \item \code{charged}, a numeric variable containing the number of illegal
      acts for which the interviewee has been charged;
    \item \code{jobsnum90}, a numeric value, the number of different jobs ever
      reported;
    \item \code{afqt89}, a numeric value, the percentile score of the "Profiles,
      Armed Forces Qualification Test" (AFQT);
    \item \code{typejob90}, a factor with 13 levels encoding different job
      types;
    \item \code{jobtrain90}, a factor with levels \code{"No"} and \code{"Yes"}
      encoding whether the job was classified as training.

  }

}
\note{

  The data set has been pre-processed differently from Komiyama et al. (2018).
  In particular:

  \itemize{

    \item the variables \code{income96} and \code{income06} have been retained
      as alternative responses;
    \item the variables \code{height}, \code{weight}, \code{race},
      \code{partner} and \code{famsize} have been retained;
    \item the variables \code{grade90} and \code{genhealth} are coded as ordered
      factors because they do not make sense on a numeric scale.

  }

  In that paper, \code{income90} is the response variable, \code{gender} and
  \code{age} are the sensitive attributes.

}
\references{

  U.S. Bureau of Labor Statistics:
  https://www.bls.gov/nls/

}
\examples{
data(national.longitudinal.survey)

# short-hand variable names.
nn = national.longitudinal.survey
# remove alternative response variables.
nn = nn[, setdiff(names(nn), c("income96", "income06"))]

m = nclm(response = nn[, "income90"],
      sensitive = nn[, c("gender", "age")],
      predictors = nn[, setdiff(names(nn), c("income90", "gender", "age"))],
      epsilon = 0.05)

summary(m)
}
\keyword{datasets}

