
confint.fair.model = function(object, parm, level = 0.95, method = "boot",
   method.args = list(), ...) {

  if (!inherits(object, "fair.model"))
    stop("'object' must be a 'fair.model' object.")

  # which coefficients should we return the confidence intervals of?
  coefs = coef(object)

  if (is.matrix(coefs))
    coef.names = rownames(coefs)
  else
    coef.names = names(coefs)

  if (missing(parm))
    parm = coef.names
  else {

    if (!is.string.vector(parm))
      stop("'parm' must be a vector of character strings, the names of the coefficients.")
    if (anyDuplicated(parm))
      stop("the elements of 'parm' must be unique.")
    if (any(parm %!in% coef.names))
      stop("unknown coefficients in 'parm'.")

  }#ELSE

  # how wide should the bands be?
  if (!is.probability(level))
    stop("'level' must be a number between 0 and 1.")

  bands = c((1 - level) / 2, 1 - (1 - level) / 2)

  # which method should we use to compute the confidence intervals?
  check.label(method, available.confint, "method")
  # ignore any optional arguments to confint().
  check.unused.args(list(...), character(0))
  # sanitize any optional arguments to the method.
  method.args = check.confint.args(method, method.args, object)

  if (method == "boot") {

    # produce the bootstrapped coefficients.
    boot.coefs =
      ci.bootstrap(object, rr = method.args$response,
        ss = method.args$sensitive, pp = method.args$predictors,
        m = method.args$m, R = method.args$R)
    # compute the quantiles to get the desierd coverage.
    if (all(sapply(boot.coefs, is.matrix))) {

      # collapse the list into a three-dimensional matrix...
      boot.coefs = simplify2array(boot.coefs)
      # ... compute the quantiles...
      ci = apply(boot.coefs, 1:2, quantile, probs = bands)
      # ... reorder the dimensions so that the quantiles are the columns...
      ci = aperm(ci, c(2, 1, 3))
      # ... and keep the desired coefficients.
      ci = ci[parm, , , drop = FALSE]

    }#THEN
    else {

      # collapse the list into a two-dimensional matrix...
      boot.coefs = simplify2array(boot.coefs)
      # ... compute the quantiles...
      ci = t(apply(boot.coefs, 1, quantile, probs = bands))
      # ... and keep the desired coefficients.
      ci = ci[parm, , drop = FALSE]

    }#ELSE

  }#THEN

  # add the class and the attributes.
  class(ci) = c("fair.confint", class(ci))
  if (is.matrix(coefs))
    attr(ci, "value") = coefs[parm, , drop = FALSE]
  else
    attr(ci, "value") = coefs[parm, drop = FALSE]
  attr(ci, "sensitive") = attr(coefs, "sensitive")[parm]

  return(ci)

}#CONFINT.FAIR.MODEL

# basic confidence interval computed from non-parametric bootstrap.
ci.bootstrap = function(object, rr, ss, pp, m, R) {

  models = lapply(seq(R), function(r) {

    # resample with replacement.
    boot.ids = sample(sample.size(rr), m, replace = TRUE)
    if (is.matrix(rr))
      boot.rr = rr[boot.ids, , drop = FALSE]
    else
      boot.rr = rr[boot.ids]
    boot.ss = ss[boot.ids, , drop = FALSE]
    boot.pp = pp[boot.ids, , drop = FALSE]

    # replace the data in the call with the bootstrapped data.
    call = object$main$call
    call[["response"]] = boot.rr
    call[["sensitive"]] = boot.ss
    call[["predictors"]] = boot.pp
    # evaluate the call in the original environment to make sure any variables
    # passed as arguments in the original call are within scope.
    boot.model = eval(call, envir = object$main$env)

    return(boot.model)

  })

  return(lapply(models, coef))

}#CONFINT.BOOTSTRAP

