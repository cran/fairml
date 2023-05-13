\name{bank}
\docType{data}
\alias{bank}
\title{Bank Marketing}
\description{

  Direct marketing campaigns (phone calls) of a Portuguese banking institution
  to make clients subscribe a term deposit.

}
\usage{
data(bank)
}
\format{

  The data contains 41188 observations and 19 variables. See the UCI Machine
  Learning Repository for details.

}
\note{

  The data set has been pre-processed as in Zafar et al. (2019), with the
  following exceptions:

  \itemize{

    \item the variable \code{duration} has been dropped in order to learn as
      realistic predictive model;
    \item the variable \code{pdays} has been dropped because it is not defined
      for the vast majority of samples;
    \item observations where \code{loan} is \code{"unknown"} have been dropped
      because the corresponding regression coefficient estimated by \code{glm()}
      is \code{NA};
    \item the three observations where \code{default} is \code{"yes"} have been
      dropped to avoid errors in cross-validation (if all those three
      observations are in the test fold it is impossible to compute predictions
      from them).

  }

  In that paper, \code{subscribed} is the response variable, \code{age} is the
  sensitive attribute and the remaining variables are used as predictors.

  The data contains the following variables:

  \itemize{

    \item \code{age} as a numeric variable;
    \item \code{job}, a factor with 12 levels ranging from \code{"blue-collar"}
      to \code{"services"};
    \item \code{marital}, a factor with levels \code{"divorced"},
      \code{"married"}, \code{"single"} and \code{"unknown"};
    \item \code{education}, a factor with 8 levels ranging from
      \code{"basic.4y"} to \code{"university.degree"};
    \item \code{default}, a factor with levels \code{"no"} and \code{"unknown"};
    \item \code{housing}, a factor with levels \code{"yes"} and \code{"no"};
    \item \code{loan}, a factor with levels \code{"yes"} and \code{"no"};
    \item \code{contact}, a factor with levels \code{"cellular"} and
      \code{"telephone"};
    \item \code{month}, a factor with 12 levels for the months of the year;
    \item \code{day_of_week}, a factor with 7 levels for the days of the week;
    \item \code{campaign}, the number of contacts performed during this
      campaign;
    \item \code{previous}, the number of contacts performed before this
      campaign;
    \item \code{poutcome}, a factor with levels \code{"failure"},
      \code{"nonexistent"} and \code{"success"};
    \item \code{emp_var_rate}, the (numeric) quarterly employment variation
      rate;
    \item \code{cons_price_idx}, the (numeric) monthly consumer price index;
    \item \code{cons_conf_idx}, the (numeric) monthly consumer confidence index;
    \item \code{euribor3m}, the (numeric) euribor 3-month rate;
    \item \code{nr_employed}, a numeric variable with the number of employees
      in the company in that quarter;
    \item \code{subscribed}, a factor with levels \code{"yes"} and \code{"no"}.

  }

}
\references{

  UCI Machine Learning Repository. \cr
  \code{https://archive.ics.uci.edu/ml/datasets/bank+marketing}

}
\examples{
data(bank)

# remove loans with unknown status, the corresponding coefficient is NA in glm().
bank = bank[bank$loan != "unknown", ]

# short-hand variable names.
r = bank[, "subscribed"]
s = bank[, c("age")]
p = bank[, setdiff(names(bank), c("subscribed", "age"))]

m = zlrm(response = r, sensitive = s, predictors = p, unfairness = 0.05)
summary(m)
}
\keyword{datasets}

