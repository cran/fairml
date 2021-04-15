\name{german.credit}
\docType{data}
\alias{german.credit}
\title{German Credit Data}
\description{

  A credit scoring data set that can be used to predict defaults on consumer
  loans in the German market.

}
\usage{
data(german.credit)
}
\format{

  The data contains 1000 observations (700 good loans, 300 bad loans) and the
  following variables:

  \itemize{

   \item \code{Account_status}: a factor with four levels representing the
     amount of money in the account or \code{"no chcking account"}.
   \item \code{Duration}: a continuous variable, the duration in months.
   \item \code{Credit_history}: a factor with five levels representing possible
     credit history backgrounds.
   \item \code{Purpose}: a factor with ten levels representing possible reasons
     for taking out a loan.
   \item \code{Credit_amount}: a continuous variable.
   \item \code{Savings_bonds}: a factor with five levels representing amount of
     money available in savings and bonds or
     \code{"unknown / no savings account"}.
   \item \code{Present_employment_since}: a factor with five levels representing
     the length of tenure in the current employment or \code{"unemployed"}.
   \item \code{Installment_rate}: a continuous variable, the installment rate in
     percentage of disposable income.
   \item \code{Other_debtors_guarantors}: a factor with levels \code{"none"},
     \code{"co-applicant"} and \code{"guarantor"}.
   \item \code{Resident_since}: a continuous variable, number of years in the
     current residence.
   \item \code{Property}: a factor with four levels describing the type of
     property to be bought or \code{"unknown / no property"}.
   \item \code{Age}: a continuous variable, the age in years.
   \item \code{Other_installment_plans}: a factor with levels \code{"bank"},
     \code{"none"} and \code{"stores"}.
   \item \code{Housing}: a factor with levels \code{"rent"}, \code{"own"} and
     \code{"for free"}.
   \item \code{Existing_credits}: a continuous variable, the number of existing
     credit lines at this bank.
   \item \code{Job}: a factor with four levels for different job descriptions.
   \item \code{People_maintenance_for}: a continuous variable, the number of
     people being liable to provide maintenance for.
   \item \code{Telephone}: a factor with levels \code{"none"} and \code{"yes"}.
   \item \code{Foreign_worker}: a factor with levels \code{"no"} and
     \code{"yes"}.
   \item \code{Credit_risk}: a factor with levels \code{"BAD"} and
     \code{"GOOD"}.
   \item \code{Gender}: a factor with levels \code{"Male"} and
     \code{"Female"}.

  }

}
\note{

  The variable "Personal status and sex" in the original data has been
  transformed into \code{Gender} by dropping the personal status information.

}
\references{

  UCI Machine Learning Repository: \cr
  https://archive.ics.uci.edu/ml/datasets/statlog+(german+credit+data)

}
\keyword{datasets}

