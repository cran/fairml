
# create a summary with key information from a fair model.
summary.fair.model = function(object, ...) {

  if (!is(object, "fair.model"))
    stop("'object' must be a 'fair.model' object.")

  check.unused.args(list(...), character(0))

  structure(list(model = object), class = "summary.fair.model")

}#SUMMARY.FAIR.MODEL

# create a summary for the models in Komiyama et al. (2018).
summary.nclm = function(object, ...) {

  if (!is(object, "nclm"))
    stop("'object' must be an 'nclm' object.")

  check.unused.args(list(...), character(0))

  r2 = 1 - var(residuals(object)) / var(fitted(object) + residuals(object))

  performance = list(
    "Ridge penalty" = object$main$arguments$lambda,
    "Custom covariance matrix" = !identical(object$main$arguments$covfun, cov),
    "Residual standard error" = sigma(object),
    "Multiple R-squared" = r2,
    c("Komiyama's R-squared" = object$fairness$value,
      "with bound" = object$fairness$bound)
  )

  structure(list(model = object,
                 banner = paste("Method:", fair.models.labels["nclm"]),
                 performance = performance),
    class = "summary.fair.model")

}#SUMMARY.NCLM

# create a summary for the fair ridge regression model.
summary.frrm = function(object, ...) {

  if (!is(object, "frrm"))
    stop("'object' must be an 'frrm' object.")

  check.unused.args(list(...), character(0))

  r2 = 1 - var(residuals(object)) / var(fitted(object) + residuals(object))

  performance = list(
    c("Ridge penalty (sensitive attributes)" = object$main$arguments$lr,
      "(predictors)" = object$main$arguments$lambda),
    "Residual standard error" = sigma(object),
    "Multiple R-squared" = r2
  )

  if (object$fairness$definition == "sp-komiyama") {

    performance = c(performance, list(
      c("R-squared (statistical parity)" = object$fairness$value,
        "with bound" = object$fairness$bound)
    ))

  }#THEN
  else if (object$fairness$definition == "eo-komiyama") {

    performance = c(performance, list(
      c("R-squared (equality of opportunity)" = object$fairness$value,
        "with bound" = object$fairness$bound)
    ))

  }#THEN

  structure(list(model = object,
                 banner = paste("Method:", fair.models.labels["frrm"]),
                 performance = performance),
    class = "summary.fair.model")

}#SUMMARY.FRRM

# create a summary for Zafar's logistic regression.
summary.zlrm = function(object, ...) {

  if (!is(object, "zlrm"))
    stop("'object' must be an 'zlrm' object.")

  check.unused.args(list(...), character(0))

  performance = list(
    "Bound on the correlations" = object$fairness$bound
  )

  structure(list(model = object,
                 banner = paste("Method:", fair.models.labels["zlrm"]),
                 performance = performance),
    class = "summary.fair.model")

}#SUMMARY.ZLRM

