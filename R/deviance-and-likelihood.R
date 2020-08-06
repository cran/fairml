
# deviance for models from Komiyama et al. (2018).
deviance.nclm = function(object, ...) {

  if (!is(object, "nclm"))
    stop("'object' must be an 'nclm' object.")

  check.unused.args(list(...), character(0))

  return(sum(object$main$residuals^2))

}#DEVIANCE.NCLM
