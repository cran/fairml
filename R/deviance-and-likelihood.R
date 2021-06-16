
# deviance for models from Komiyama et al. (2018).
deviance.nclm = function(object, ...) {

  if (!is(object, "nclm"))
    stop("'object' must be an 'nclm' object.")

  check.unused.args(list(...), character(0))

  return(sum(object$main$residuals^2))

}#DEVIANCE.NCLM

# deviance for the fair ridge regression model.
deviance.frrm = function(object, ...) {

  if (!is(object, "frrm"))
    stop("'object' must be an 'frrm' object.")

  check.unused.args(list(...), character(0))

  return(sum(object$main$residuals^2))

}#DEVIANCE.FRRM

# deviance for Zafar's logistic regression.
deviance.zlrm = function(object, ...) {

  if (!is(object, "zlrm"))
    stop("'object' must be an 'zlrm' object.")

  check.unused.args(list(...), character(0))

  return(object$main$deviance)

}#DEVIANCE.ZLRM

