
# check the function computing the covariance matrix estimates.
check.covariance.function = function(covfun) {

  if (missing(covfun))
    covfun = cov
  else {

    if (!is.function(covfun))
      stop("'covfun' must be a function.")

  }#ELSE

  return(covfun)

}#CHECK.COVARIANCE.FUNCTION

# check the covariance matrix produced by the function above.
check.covariance.matrix = function(covmat, nvars, what) {

  if (!is.matrix(covmat))
    stop("the covariance matrix of ", what, " must be a matrix.")

  if (any(is.infinite(covmat)) || any(is.na(covmat)))
    stop("the covariance matrix of ", what, " contains infinite/missing values.")

  # check the dimensions.
  dims = dim(covmat)

  if (dims[1] != dims[2])
    stop("the covariance matrix of ", what, " must be a square matrix.")
  if (dims[1] != nvars)
    stop("the covariance matrix of ", what,
         " should be a square matrix with dimension ", nvars,
         " but it has dimension ", dims[1], "x", dims[2], ".")

  # check symmetry and Cauchy-Schwarz.
  if (!isSymmetric.matrix(covmat))
    stop("the covariance matrix of ", what, " must be symmetric.")
  if (!all(covmat <= sqrt(outer(diag(covmat), diag(covmat)))))
    stop("the covariance matrix of ", what, " does not satisfy Cauchy-Schwarz.")

}#CHECK.COVARIANCE.MATRIX

# check the unfairness tuning parameter.
check.fairness.level = function(unfairness, scalar = TRUE) {

  if (scalar) {

    if (missing(unfairness) || !is.probability(unfairness))
      stop("'unfairness' should be a single number between 0 and 1.")

  }#THEN
  else {

    if (missing(unfairness))
      unfairness = seq(from = 0.00, to = 1, by = 0.02)
    else if (!is.probability.vector(unfairness) || (length(unfairness) < 2))
      stop("'unfairness' should be a vector of numbers between 0 and 1.")

  }#ELSE

  # make sure the values are sorted, to make it easier to use them to draw
  # profile plots.
  return(sort(unfairness))

}#CHECK.FAIRNESS.LEVEL

# check the lambda penalty coefficient used for regularisation.
check.ridge.penalty = function(lambda) {

    if (missing(lambda))
      lambda = 0
    else if (!is.non.negative(lambda))
      stop('lambda should be a non-negative number.')

    return(lambda)

}#CHECK.RIDGE.PENALTY
