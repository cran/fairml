
# extract the coefficients.
coef.fair.model = function(object, ...) {

  if (!inherits(object, "fair.model"))
    stop("'object' must be a 'fair.model' object.")

  check.unused.args(list(...), character(0))

  return(object$main$coefficients)

}#COEF.FAIR.MODEL

# extract the residuals.
residuals.fair.model = function(object, ...) {

  if (!inherits(object, "fair.model"))
    stop("'object' must be a 'fair.model' object.")

  check.unused.args(list(...), character(0))

  return(object$main$residuals)

}#RESIDUALS.FAIR.MODEL

# extract the fitted values.
fitted.fair.model = function(object, type = "response", ...) {

  if (!inherits(object, "fair.model"))
    stop("'object' must be a 'fair.model' object.")

  # check the type of fitted values.
  if (inherits(object, fair.regressions))
    check.label(type, c("response"), "fitted value type")
  else if (inherits(object, fair.classifiers))
    check.label(type, c("response", "class", "link"), "fitted value type")
  else if (inherits(object, fair.family)) {

    if (object$main$family == "gaussian")
      check.label(type, c("response"), "fitted value type")
    else if (object$main$family %in% c("binomial", "multinomial"))
      check.label(type, c("response", "class", "link"), "fitted value type")
    else if (object$main$family %in% c("poisson", "cox"))
      check.label(type, c("response", "link"), "fitted value type")

  }#THEN

  check.unused.args(list(...), character(0))

  if (type == "response") {

    # the fitted values produced by glm() are "obtained by transforming the
    # linear predictors by the inverse of the link function", so they are on the
    # scale of the response; for coxph() it's the other way round.
    if (object$main$family == "binomial")
      fitted = linpred2prob(object$main$fitted)
    else if (object$main$family == "multinomial")
      fitted = linpred2mprob(object$main$fitted)
    else if (object$main$family == "poisson")
      fitted = exp(object$main$fitted)
    else if (object$main$family == "cox")
      fitted = exp(-object$main$fitted)
    else if (object$main$family == "gaussian")
      fitted = noattr(object$main$fitted)

  }#THEN
  else if (type == "link") {

    # take the fitted value and apply the link function, which gives the fitted
    # values on the scale of the linear predictor; fitted values for coxph() are
    # fine as they are.
    if (object$main$family == "binomial")
      fitted = object$main$fitted
    else if (object$main$family == "poisson")
      fitted = object$main$fitted
    else if (object$main$family == "cox")
      fitted = object$main$fitted
    else if (object$main$family == "multinomial")
      fitted = object$main$fitted

  }#THEN
  else if (type == "class") {

    if (object$main$family == "binomial") {

      fitted = linpred2class(object$main$fitted,
                 labels = object$data$response$levels[["response"]])

    }#THEN
    else if (object$main$family == "multinomial") {

      fitted = linpred2mclass(object$main$fitted,
                 labels = colnames(object$main$fitted))

    }#THEN

  }#THEN

  return(fitted)

}#FITTED.FAIR.MODEL

# extract the training sample size.
nobs.fair.model = function(object, ...) {

  if (!inherits(object, "fair.model"))
    stop("'object' must be a 'fair.model' object.")

  check.unused.args(list(...), character(0))

  return(length(object$main$fitted))

}#NOBS.FAIR.MODEL

# extract the estimated standard deviation of the errors.
sigma.fair.model = function(object, ...) {

  if (!inherits(object, "fair.model"))
    stop("'object' must be a 'fair.model' object.")

  check.unused.args(list(...), character(0))

  resid.df = nobs(object, ...) - sum(!is.na(coef(object)))

  return(sqrt(deviance(object, ...) / resid.df))

}#SIGMA.FAIR.MODEL

# deviance for fair models.
deviance.fair.model = function(object, ...) {

  if (!inherits(object, "fair.model"))
    stop("'object' must be an 'fair.model' object.")

  check.unused.args(list(...), character(0))

  return(object$main$deviance)

}#DEVIANCE.FAIR.MODEL

logLik.fair.model = function(object, ...) {

  if (!inherits(object, "fair.model"))
    stop("'object' must be an 'fair.model' object.")

  check.unused.args(list(...), character(0))

  value = object$main$loglik
  coefs = object$main$coefficients

  if (object$main$family == "cox")
    nobs = sum(object$main$y[, "status"])
  else
    nobs = length(object$main$y)

  if (is.nan(value))
    df = NA_real_
  else if (object$main$family == "gaussian")
    df = length(coefs[coefs != 0]) + 1
  else if (object $main$family == "multinomial")
    df = nrow(coefs) * (ncol(coefs) - 1) - sum(coefs[, -1] == 0)
  else
    df = length(coefs[coefs != 0])

  return(structure(value, nobs = nobs, df = df, class =  "logLik"))

}#LOGLIK.FAIR.MODEL
