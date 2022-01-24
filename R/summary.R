
# create a summary with key information from a fair model.
summary.fair.model = function(object, ...) {

  if (!inherits(object, "fair.model"))
    stop("'object' must be a 'fair.model' object.")

  check.unused.args(list(...), character(0))

  # find out what type of model this is.
  model = class(object)[1]
  # find out how fairness was enforced.
  fair = object$fairness
  # extract optional argument names and values.
  args = object$main$arguments
  argnames = names(args)
  args.summary = list()

  # linear regresssion models.
  if ((model %in% fair.regressions) ||
      (model %in% fair.family) && (object$main$family == "gaussian")) {

    r2 = 1 - var(residuals(object)) / var(fitted(object) + residuals(object))

    if ("lr" %in% argnames) {

      args.summary = c(args.summary,
        list(c("Ridge penalty (sensitive attributes)" = args$lr,
               "(predictors)" = args$lambda)))

    }#THEN
    else if ("lambda" %in% argnames) {

      args.summary = c(args.summary, list("Ridge penalty" = args$lambda))

    }#THEN

    if ("covfun" %in% argnames)
      args.summary = c(args.summary,
        list("Custom covariance matrix" = !identical(args$covfun, cov)))

    perf.summary = list(
      "Log-likelihood" = logLik(object),
      "Residual standard error" = sigma(object),
      "Multiple R^2" = r2
    )

  }#THEN
  else if ((model %in% fair.classifiers) ||
           (model %in% fair.family) && (object$main$family == "binomial")) {

    if ("lr" %in% argnames) {

      args.summary = c(args.summary,
        list(c("Ridge penalty (sensitive attributes)" = args$lr,
               "(predictors)" = args$lambda)))

    }#THEN
    else if ("lambda" %in% argnames) {

      args.summary = c(args.summary, list("Ridge penalty" = args$lambda))

    }#THEN

    perf.summary = list("Log-likelihood" = logLik(object))

  }#THEN
  else {

    stop("no summary() method for model ", q(model), ".")

  }#ELSE

  if (length(fair$value) == 1) {

    fair.summary = list(
      structure(c(fair$value, fair$bound),
        names = c(fairness.definitions.labels[fair$definition], "with bound"))
    )

  }#THEN
  else {

    fair.summary = structure(list(fair$value, fair$bound),
        names = c(fairness.definitions.labels[fair$definition], "with bound")
    )

  }#ELSE

  structure(list(model = object,
                 banner = paste("Method:", fair.models.labels[model]),
                 info = c(args.summary, perf.summary, fair.summary)),
    class = "summary.fair.model")

}#SUMMARY.FAIR.MODEL

