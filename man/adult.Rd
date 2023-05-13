\name{adult}
\docType{data}
\alias{adult}
\title{Census Income}
\description{

  Predict whether income exceeds $50K per year using the U.S. 1994 Census data.

}
\usage{
data(adult)
}
\format{

  The data contains 30162 observations and 14 variables. See the UCI Machine
  Learning Repository for details.

}
\note{

  The data set has been pre-processed as in Zafar et al. (2019), with the
  following exceptions:

  \itemize{

    \item the data do not include the test sample from the UCI repository;
    \item the variables \code{"capital_gain"} and \code{"capital_loss"} have
      been scaled by \code{1/1000}.

  }

  In that paper, \code{income} is the response variable, \code{sex} and
  \code{race} are the sensitive attributes and the remaining variables are
  used as predictors.

  The data contain the following variables:

  \itemize{

    \item \code{age} as a numeric variable;
    \item \code{workclass}, a factor with 8 levels encoding the type of
      employment (\code{"Private"}, \code{"Self-emp-not-inc"},
      \code{"Federal-gov"}, etc.);
    \item \code{education}, a factor with 10 levels from \code{"Preschool"} to
      \code{"Doctorate"};
    \item \code{education-num}, the number of years in education;
    \item \code{marital-status}, a factor with 7 levels from
      \code{"Married-civ-spouse"} to \code{"Divorced"} and
      \code{"Never-married"};
    \item \code{occupation}, a factor with 14 levels encoding the field of
      employment (\code{"Tech-support"}, \code{"Craft-repair"}, etc.);
    \item \code{relationship} a factor with 6 levels (\code{"Wife"},
      \code{"Own-child"}, etc.);
    \item \code{race}, a factor with levels \code{"White"},
      \code{"Asian-Pac-Islander"}, \code{"Amer-Indian-Eskimo"}, \code{"Other"}
      and \code{"Black"};
    \item \code{sex}, a factor with levels \code{"Female"} and \code{"Male"};
    \item \code{capital-gain} as a numeric variable;
    \item \code{capital-loss} as a numeric variable;
    \item \code{native-country} as a factor with two levels
      \code{"United-States"} and \code{"Non-United-States"};
    \item \code{hours-per-week} as a numeric variable.

  }

}
\references{

  UCI Machine Learning Repository. \cr
  \code{https://archive.ics.uci.edu/ml/datasets/adult}

}
\examples{
data(adult)

# short-hand variable names.
r = adult[, "income"]
s = adult[, c("sex", "race")]
p = adult[, setdiff(names(adult), c("income", "sex", "race"))]

\dontrun{
m = zlrm(response = r, sensitive = s, predictors = p, unfairness = 0.05)
summary(m)
}}
\keyword{datasets}

