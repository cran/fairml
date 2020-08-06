
# extract the coefficients.
coef.fair.model = function(object, ...) {

  if (!is(object, "fair.model"))
    stop("'object' must be a 'fair.model' object.")

  check.unused.args(list(...), character(0))

  return(object$main$coefficients)

}#COEF.FAIR.MODEL

# extract the residuals.
residuals.fair.model = function(object, ...) {

  if (!is(object, "fair.model"))
    stop("'object' must be a 'fair.model' object.")

  check.unused.args(list(...), character(0))

  return(object$main$residuals)

}#RESIDUALS.FAIR.MODEL

# extract the fitted values.
fitted.fair.model = function(object, ...) {

  if (!is(object, "fair.model"))
    stop("'object' must be a 'fair.model' object.")

  check.unused.args(list(...), character(0))

  return(object$main$fitted)

}#FITTED.FAIR.MODEL

# extract the training sample size.
nobs.fair.model = function(object, ...) {

  if (!is(object, "fair.model"))
    stop("'object' must be a 'fair.model' object.")

  check.unused.args(list(...), character(0))

  return(length(object$main$fitted))

}#NOBS.FAIR.MODEL

# extract the estimated standard deviation of the errors.
sigma.fair.model = function(object, ...) {

  if (!is(object, "fair.model"))
    stop("'object' must be a 'fair.model' object.")

  check.unused.args(list(...), character(0))

  resid.df = nobs(object, ...) - sum(!is.na(coef(object)))

  return(sqrt(deviance(object, ...) / resid.df))

}#SIGMA.FAIR.MODEL
