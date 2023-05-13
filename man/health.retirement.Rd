\name{health.retirement}
\docType{data}
\alias{health.retirement}
\title{Health and Retirement Survey}
\description{

  The University of Michigan Health and Retirement Study (HRS) longitudinal
  dataset.

}
\usage{
data(health.retirement)
}
\format{

  The data contains 38653 observations and 27 variables.

}
\note{

  The data set has been minimally pre-processed: the redundant variables
  \code{HISPANIC} and \code{BITHYR} were removed, along with the patient ID
  \code{PID}. A single patient was recorded twice: the duplicate has been
  removed. However, incomplete observations have been left in the data set.

  The number of dependencies in daily activities \code{score} is the response
  (count) variable and \code{marriage}, \code{gender}, \code{race},
  \code{race.ethnicity} and \code{age} are the sensitive attributes. The
  remaining variables are used as predictors.

  The data contain the following variables:

  \itemize{

    \item \code{year}, the year of retirement as a numeric variable;
    \item \code{age}, the age as a numeric variable;
    \item \code{educa}, the number of years in education as a numeric variable;
    \item \code{networth}, household net worth as a numeric variable;
    \item \code{cognition_catnew} cognistion assessment as a numeric variable;
    \item \code{bmi} as a numeric variable;
    \item \code{hlthrte}, a numeric health rating;
    \item \code{bloodp}, blood pressure diagnosis as a numeric variable;
    \item \code{diabetes}, diabetes diagnosis as a numeric variable;
    \item \code{cancer}, cancer diagnosis as a numeric variable;
    \item \code{lung}, lung disease diagnosis as a numeric variable;
    \item \code{heart}, heart condition diagnosis as a numeric variable;
    \item \code{stroke}, stroke diagnosis as a numeric variable;
    \item \code{pchiat}, psychiatric condition diagnosis as a numeric variable;
    \item \code{arthrit}, arthritis diagnosis as a numeric variable;
    \item \code{fall}, recently falling as a numeric variable;
    \item \code{pain}, pain conditions as a numeric variable;
    \item \code{A1c_adj}, biomarker for hemoglobin A1C;
    \item \code{CRP_adj}, biomarker for C-reactive protein;
    \item \code{CYSC_adj}, biomarker for Cystatin C;
    \item \code{HDL_adj}, biomarker for HDL cholesterol;
    \item \code{TC_adj}, biomarker for total cholesterol;
    \item \code{score}, another numeric health rating;
    \item \code{gender}, a factor with levels \code{"Female"} and \code{"Male"};
    \item \code{marriage}, a factor with levels \code{"Married/Partner"} and
      \code{"Not Married"};
    \item \code{race}, a factor withe levels \code{"Black"}, \code{"Other"}
      and \code{"White"};
    \item \code{race.ethnicity}, a factor withe levels \code{"Hispanic"},
      \code{"NHB"}, \code{"NHW"} and \code{"Other"}.

  }

}
\references{

  \code{https://hrs.isr.umich.edu/about}

}
\examples{
data(health.retirement)

# complete data analysis.
health.retirement = health.retirement[complete.cases(health.retirement), ]
# short-hand variable names.
r = health.retirement[, "score"]
s = health.retirement[, c("marriage", "gender", "race", "age")]
p = health.retirement[, setdiff(names(health.retirement), c(names(r), names(s)))]
# drop the second race variable.
p = p[, colnames(p) != "race.ethnicity"]

\dontrun{
# the lambda = 0.1 is very helpful in making model estimation succeed.
m = fgrrm(response = r, sensitive = s, predictors = p, ,
      family = "poisson", unfairness = 0.05, lambda = 0.1)
summary(m)
}}
\keyword{datasets}

