
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

# check the fairness definition used for the constraint.
check.fairness.constraint = function(definition, model, family) {

  if (missing(definition))
    stop("no fairness definition has been specified.")
  else if (is.function(definition)) {

    # custom fairness definition passed as a function.
    fun.arguments = names(formals(definition))
    if (!identical(fun.arguments, c("model", "y", "S", "U", "family")))
      stop("the custom score function must have signature function(model, y, S, U, family).")

  }#THEN
  else {

    # built-in fairness definitions identified by a label.
    check.label(definition, available.fairness.definitions,
      "definition of fairness")
    check.label(definition, fairness.definitions.for.model[[model]],
      "definition of fairness")

  }#THEN

}#CHECK.FAIRNESS.CONSTRAINT

# check the unfairness proportion tuning parameter.
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

# check an absolute correlation coefficient fairness constraint.
check.absolute.covariance = function(unfairness) {

  if (missing(unfairness) || !is.non.negative(unfairness))
    stop("'max.abs.cov' should be a single non-negative number.")

}#CHECK.ABSOLUTE.COVARIANCE

# check the lambda penalty coefficient used for regularisation.
check.ridge.penalty = function(lambda) {

    if (missing(lambda))
      lambda = 0
    else if (!is.non.negative(lambda))
      stop('lambda should be a non-negative number.')

    return(lambda)

}#CHECK.RIDGE.PENALTY

# check the generalized linear model family the model belongs to.
check.family = function(family) {

  if (missing(family) || is.null(family))
    family = "gaussian"
  else
    check.label(family, available.families, "family")

  return(family)

}#CHECK.FAMILY
