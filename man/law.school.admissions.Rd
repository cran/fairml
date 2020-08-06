\name{law.school.admissions}
\docType{data}
\alias{law.school.admissions}
\title{Law School Admission Council data}
\description{

  Survey among students attending law school in the U.S. in 1991.

}
\usage{
data(law.school.admissions)
}
\format{

  The data contains 20800 observations and the following variables:

  \itemize{

    \item \code{age}, a continuous variable containing the student's age in
      years;
    \item \code{decile1}, a continuous variable containing the student's decile
      in the school given his grades in Year 1;
    \item \code{decile3}, a continuous variable containing the student's decile
      in the school given his grades in Year 3;
    \item \code{fam_inc}, a continuous variable containing student's family
      income bracket (from \code{1} to \code{5});
    \item \code{lsat}, a continuous variable containing the student's LSAT
      score;
    \item \code{ugpa}, a continuous variable containing the student's
      undergraduate GPA;
    \item \code{gender}, a factor with levels \code{"female"} and \code{"male"};
    \item \code{race1}, a factor with levels \code{"asian"}, \code{"black"},
      \code{"hisp"}, \code{"other"} and \code{"white"};
    \item \code{cluster}, a factor with levels \code{"1"}, \code{"2"},
      \code{"3"}, \code{"4"}, \code{"5"} and \code{"6"} encoding the tiers of
      law school prestige;
    \item \code{fulltime}, a factor with levels \code{"FALSE"} and
      \code{"TRUE"}, whether the student will work full-time or part-time;
    \item \code{bar}, a factor with levels \code{"FALSE"} and \code{"TRUE"},
      whether the student passed the bar exam on the first try.

  }

}
\note{

  The data set has been pre-processed as in Komiyama et al. (2018), with the
  following exceptions:

  \itemize{

    \item \code{DOB_yr}, the year of birth, has been dropped because it is
      (nearly) perfectly collinear with \code{age}, and thus it is redundant;
    \item \code{decile1b} has been dropped because it is (nearly) perfectly
      collinear with \code{decile1}, and thus it is redundant.

  }

  In that paper, \code{ugpa} is the response variable, \code{age} and
  \code{race1} are the sensitive attributes and the remaining variables are
  used as predictors.

}
\references{

  Sander RH (2004). "A Systemic Analysis of Affirmative Action in American Law
  Schools". Stanford Law Review, 57:367--483.

}
\examples{
data(law.school.admissions)

# short-hand variable names.
ll = law.school.admissions

m = nclm(response = ll[, "ugpa"],
      sensitive = ll[, c("age", "race1")],
      predictors = ll[, setdiff(names(ll), c("ugpa", "age", "race1"))],
      epsilon = 0.05)

summary(m)
}
\keyword{datasets}

