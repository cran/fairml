\name{obesity.levels}
\docType{data}
\alias{obesity.levels}
\title{Obesity Levels}
\description{

  Predict obesity levels based on eating habits and physical condition.

}
\usage{
data(obesity.levels)
}
\format{

  The data contains 2111 observations and 17 variables. See the UCI Machine
  Learning Repository for details.

}
\note{

  The data set has been minimally pre-processed: the only change is that the
  only observation for which the \code{CALC} variable was equal to
  \code{"Always"} has been changed to \code{"Frequently"} to merge the two
  levels.

  The obesity level \code{NObeyesdad} is the response variable (with 7 different
  levels) and \code{Age} and \code{Gender} are the sensitive attributes. The
  remaining variables are used as predictors.

  The data contain the following variables:

  \itemize{

    \item \code{Gender};
    \item \code{Age};
    \item \code{Height};
    \item \code{Weight};
    \item \code{family_history_with_overweight};
    \item \code{FAVC}, frequent consumption of high caloric food as a factor
      with levels \code{"no"} and \code{"yes"};
    \item \code{FCVC}, frequency of consumption of vegetables as a numeric
      variable;
    \item \code{NCP}, number of main meals;
    \item \code{CAEC}, consumption of food between meals as a factor with levels
      \code{"no"}, \code{"Sometimes"}, \code{"Frequently"} and \code{"Always"};
    \item \code{SMOKE}, smoking status as a factor with levels \code{"no"} and
      \code{"yes"};
    \item \code{CH2O}, consumption of water daily as a numeric variable;
    \item \code{SCC}, calories consumption monitoring as a factor with level
      \code{"no"} and \code{"yes"};
    \item \code{FAF}, physical activity frequency as a numeric variable;
    \item \code{TUE}, time using technology devices as a numeric variable;
    \item \code{CALC}, consumption of alcohol as a dfactor with levels
      \code{"no"}, \code{"Sometimes"}, \code{"Frequently"} and \code{"Always"};
    \item \code{MTRANS}, transportation used as a factor with levels
      \code{"Automobile"}, \code{"Bike"}, \code{"Motorbike"},
      \code{"Public_Transportation"} and \code{"Walking"};
    \item \code{NObeyesdad}, the obesity level as a factor with levels
      \code{"Insufficient_Weight"}, \code{"Normal_Weight"},
      \code{"Overweight_Level_I"}, \code{"Overweight_Level_II"},
      \code{"Obesity_Type_I"}, \code{"Obesity_Type_II"},
      \code{"Obesity_Type_III"}.

  }

}
\references{

  UCI Machine Learning Repository. \cr
  \code{https://archive-beta.ics.uci.edu/dataset/544}

}
\examples{
data(obesity.levels)

# short-hand variable names.
r = obesity.levels[, "NObeyesdad"]
s = obesity.levels[, c("Gender", "Age")]
p = obesity.levels[, setdiff(names(obesity.levels), c("NObeyesdad", "Gender", "Age"))]

\dontrun{
# the lambda = 0.1 is very helpful in making model estimation succeed.
m = fgrrm(response = r, sensitive = s, predictors = p, ,
      family = "multinomial", unfairness = 0.05, lambda = 0.1)
summary(m)
}}
\keyword{datasets}

