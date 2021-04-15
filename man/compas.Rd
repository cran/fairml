\name{compas}
\docType{data}
\alias{compas}
\title{Criminal Offenders Screened in Florida}
\description{

  A collection of criminal offenders screened in Florida (US) during 2013-14.

}
\usage{
data(compas)
}
\format{

  The data contains 5855 observations and the following variables:

  \itemize{

    \item \code{age}, a continuous variable containing the age (in years) of the
      person;
    \item \code{juv_fel_count}, a continuous variable containing the number of
      juvenile felonies;
    \item \code{decile_score}, a continuous variable, the decile of the COMPAS
      score;
    \item \code{juv_misd_count}, a continuous variable containing the number of
      juvenile misdemeanors;
    \item \code{juv_other_count}, a continuous variable containing the number
      of prior juvenile convictions that are not considered either felonies or
      misdemeanors;
    \item \code{v_decile_score}, a continuous variable containing the predicted
      decile of the COMPAS score;
    \item \code{priors_count}, a continuous variable containing the number of
      prior crimes committed;
    \item \code{sex}, a factor with levels \code{"Female"} and \code{"Male"};
    \item \code{two_year_recid}, a factor with two levels \code{"Yes"} and
      \code{"No"} (if the person has recidivated within two years);
    \item \code{race}, a factor encoding the race of the person;
    \item \code{c_jail_in}, a numeric variable containing the date in which the
      person entered jail (normalized between 0 and 1);
    \item \code{c_jail_out}, a numeric variable containing the date in which the
      person was released from jail (normalized between 0 and 1);
    \item \code{c_offense_date}, a numeric variable containing the date the
      offense was committed;
    \item \code{screening_date}, a numeric variable containing the date in which
      the person was screened (normalized between 0 and 1);
    \item \code{in_custody}, a numeric variable containing the date in which the
      person was placed in custody (normalized between 0 and 1);
    \item \code{out_custody}, a numeric variable containing the date in which
      the person was released from custody (normalized between 0 and 1);

  }

}
\note{

  The data set has been pre-processed as in Komiyama et al. (2018), with the
  following exceptions:

  \itemize{

    \item the \code{race} variable has not been reduced to a binary factor with
      levels \code{"African-American"} and \code{"not African-American"};
    \item the variables \code{type_of_assessment}, \code{v_type_of_assessment}
      have been dropped from the analysis because they take the same value for
      all observations;
    \item variables like \code{c_jail_in} and \code{c_jail_out} that encode
      dates have been jointly rescaled to preserve the temporal ordering of
      events.

  }


  In that paper, \code{two_year_recid} is the response variable, \code{sex} and
  \code{race} are the sensitive attributes and the remaining variables are
  used as predictors.

}
\references{

  Angwin J, Larson J, Mattu S, Kirchner L (2016). "Machine Bias: Theres Software
    Used Around the Country to Predict Future Criminals."
    https://www.propublica.org.

}
\examples{
data(compas)

# convert the response back to a numeric variable.
compas$two_year_recid = as.numeric(compas$two_year_recid) - 1

# short-hand variable names.
r = compas[, "two_year_recid"]
s = compas[, c("sex", "race")]
p = compas[, setdiff(names(compas), c("two_year_recid", "sex", "race"))]

m = nclm(response = r, sensitive = s, predictors = p, unfairness = 0.05)
summary(m)

m = frrm(response = r, sensitive = s, predictors = p, unfairness = 0.05)
summary(m)
}
\keyword{datasets}

