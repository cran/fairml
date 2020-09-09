
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
    stop("the covatiance matrix of ", what,
         " should be a square matrix with dimension ", nvars,
         " but it has dimension ", dims[1], "x", dims[2], ".")

  # check symmetry and Cauchy-Schwarz.
  if (!isSymmetric.matrix(covmat))
    stop("the covariance matrix of ", what, " must be symmetric.")
  if (!all(covmat <= sqrt(outer(diag(covmat), diag(covmat)))))
    stop("the covariance matrix of ", what, " does not satisfy Cauchy-Schwarz.")

}#CHECK.COVARIANCE.MATRIX

# check the proportion of variance explained by sensitive attributes.
check.epsilon = function(epsilon, scalar = TRUE) {

  if (scalar) {

    if (missing(epsilon) || !is.probability(epsilon))
      stop("'epsilon' should be a single number between 0 and 1.")

  }#THEN
  else {

    if (missing(epsilon))
      epsilon = seq(from = 0.00, to = 1, by = 0.02)
    else if (!is.probability.vector(epsilon))
      stop("'epsilon' should be a vector of numbers between 0 and 1.")

  }#ELSE

  return(epsilon)

}#CHECK.EPSILON
