\name{drug.consumption}
\docType{data}
\alias{drug.consumption}
\title{Drug Consumption}
\description{

  Predict drug consumption based on psychological scores and demographics.

}
\usage{
data(drug.consumption)
}
\format{

  The data contains 1885 observations and 31 variables. See the UCI Machine
  Learning Repository for details.

}
\note{

  The data set has been minimally pre-processed following the instructions on
  the UCI Machine Learning Repository to re-encode the variables. Categorical
  variables are stored as factors and the psychological scores are stored as
  numeric variables on their original scales.

  Any of the drug use variables can be used as the response variable
  (with 7 different levels); \code{Age}, \code{Gender} and \code{Race} are the
  sensitive attributes. The remaining variables are used as predictors.

  The data contain the following variables:

  \itemize{

    \item \code{Age}, a factor with 6 10-years age brackets;
    \item \code{Gender}, as a factor;
    \item \code{Education}, a factor with 9 levels from \code{"Left school
      before 16"} to \code{"Doctorate degree"};
    \item \code{Country}, a factor with 7 different levels for \code{"USA"},
      \code{"New Zealand"}, \code{"Other"}, \code{"Australia"},
      \code{"Republic of Ireland" "Canada"} and \code{"UK"};
    \item \code{Race} a factor with 7 levels comprising mixed backgrounds as
      well;
    \item \code{Nscore}, \code{Escore}, \code{Oscore}, \code{Ascore},
      \code{Cscore}, numeric scores from the five-factor model for
      personality traits;
    \item \code{Impulsive}, a numeric score for impulsivity;
    \item \code{SS}, a numeric score for sensation seeking;
    \item \code{Alcohol}, \code{Amphet}, \code{Amyl}, \code{Benzos},
      \code{Caff}, \code{Cannabis}, \code{Choc}, \code{Coke}, \code{Crack},
      \code{Ecstasy}, \code{Heroin}, \code{Ketamine}, \code{Legalh},
      \code{LSD}, \code{Meth}, \code{Mushrooms}, \code{Nicotine}, \code{Semer}
      and \code{VSA}: factors with 7 levels ranging from \code{"Never Used"} to
      \code{"Used in Last Day"}.
  }

}
\references{

  UCI Machine Learning Repository. \cr
  \code{https://archive-beta.ics.uci.edu/dataset/373/}

}
\examples{
data(drug.consumption)

# short-hand variable names.
r = drug.consumption[, "Meth"]
s = drug.consumption[, c("Age", "Gender", "Race")]
p = drug.consumption[, c("Education", "Nscore", "Escore", "Oscore", "Ascore",
                         "Cscore", "Impulsive", "SS")]

# collapse levels with low observed frequencies.
levels(p$Education) =
  c("at.most.18y", "at.most.18y", "at.most.18y", "at.most.18y", "university",
    "diploma", "bachelor", "master", "phd")

\dontrun{
m = fgrrm(response = r, sensitive = s, predictors = p, ,
      family = "multinomial", unfairness = 0.05)
summary(m)

HH = drug.consumption$Heroin
levels(HH) = c("Never Used", "Used", "Used", "Used", "Used Recently",
               "Used Recently", "Used Recently")

m = fgrrm(response = HH, sensitive = s, predictors = p, ,
      family = "multinomial", unfairness = 0.05)
summary(m)
}}
\keyword{datasets}

