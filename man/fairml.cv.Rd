\name{fairml.cv}
\alias{fairml.cv}
\alias{loss}
\title{Cross-Validation for Fair Models}
\description{

  Cross-validation for the models in the \pkg{fairml} package.

}
\usage{
fairml.cv(response, predictors, sensitive, method = "k-fold", ...,
  epsilon, model, model.args = list())

loss(x)
}
\arguments{
  \item{response}{a numeric vector, the response variable.}
  \item{predictors}{a numeric matrix or a data frame containing numeric and
    factor columns; the predictors.}
  \item{sensitive}{a numeric matrix or a data frame containing numeric and
    factor columns; the sensitive attributes.}
  \item{method}{a character string, either \code{k-fold}, \code{custom-folds}
    or \code{hold-out}. See below for details.}
  \item{\dots}{additional arguments for the cross-validation \code{method}.}
  \item{epsilon}{a positive number in [0, 1], the proportion of the explained
    variance that can be attributed to \code{S}.}
  \item{model}{a character string, the label of the model. Currently only
    \code{"nclm"} is available.}
  \item{model.args}{additional arguments passed to the model.}
  \item{x}{an object of class \code{fair.kcv} or \code{fair.kcv.list}.}
}
\details{

  The following cross-validation methods are implemented:
  \itemize{

    \item \emph{k-fold}: the data are split in \code{k} subsets of equal size.
      For each subset in turn, \code{model} is fitted on the other \code{k - 1}
      subsets and the loss function is then computed using that subset. Loss
      estimates for each of the \code{k} subsets are then combined to give an
      overall loss for data.

    \item \emph{custom-folds}: the data are manually partitioned by the user
      into subsets, which are then used as in k-fold cross-validation. Subsets
      are not constrained to have the same size, and every observation must be
      assigned to one subset.

    \item \emph{hold-out}: \code{k} subsamples of size \code{m} are sampled
      independently without replacement from the data. For each subsample,
      \code{model} is fitted on the remaining \code{m - length(response)}
      samples and the loss function is computed on the \code{m} observations in
      the subsample. The overall loss estimate is the average of the \code{k}
      loss estimates from the subsamples.

  }

  Cross-validation methods accept the following optional arguments:
  \itemize{

    \item \code{k}: a positive integer number, the number of groups into which
      the data will be split (in k-fold cross-validation) or the number of times
      the data will be split in training and test samples (in hold-out
      cross-validation).

    \item \code{m}: a positive integer number, the size of the test set in
      hold-out cross-validation.

    \item \code{runs}: a positive integer number, the number of times
      k-fold or hold-out cross-validation will be run.

    \item \code{folds}: a list in which element corresponds to one fold and
      contains the indices for the observations that are included to that fold;
      or a list with an element for each run, in which each element is itself a
      list of the folds to be used for that run.

  }

  If cross-validation is used with multiple \code{runs}, the overall loss is the
  average of the loss estimates from the different runs.

  The predictive performance of the models is measured using the mean square
  error as the loss function.

}
\value{

  \code{fairml.cv()} returns an object of class \code{fair.kcv.list} if
  \code{runs} is at least 2, an object of class \code{fair.kcv} if \code{runs}
  is equal to 1.

  \code{loss} returns a numeric vectors containing the values of the loss
  function computed for each run of cross-validation.

}
\examples{
kcv = fairml.cv(response = vur.test$y, predictors = vur.test$X,
        sensitive = vur.test$S, epsilon = 0.10, model = "nclm",
        method = "k-fold", k = 10, runs = 10)
kcv
loss(kcv)
}
\author{Marco Scutari}
\keyword{regression}
\keyword{model selection}
