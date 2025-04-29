
# check the optional arguments to confidence interval methods.
check.confint.args = function(method, method.args, object) {

  # check the response variable.
  if (has.argument(method, "response", confint.extra.args))
    method.args[["response"]] =
      check.response(method.args[["response"]],
        model = class(object)[[1]],
        family = object$main$family)

  # check the predictors.
  if (has.argument(method, "predictors", confint.extra.args))
    method.args[["predictors"]] =
      check.data(method.args[["predictors"]], varletter = "X")

  # check the sensitive attributes.
  if (has.argument(method, "sensitive", confint.extra.args))
    method.args[["sensitive"]] =
      check.data(method.args[["sensitive"]], varletter = "S")

  # check the number of bootstrap replicates.
  if (has.argument(method, "R", confint.extra.args))
    method.args[["R"]] = check.replicates(method.args[["R"]])

  # check the size of each bootstrap sample.
  if (has.argument(method, "m", confint.extra.args))
    method.args[["m"]] =
      check.bootsize(method.args[["m"]], n = sample.size(method.args$response))

  return(method.args)

}#CHECK.CONFINT.ARGS

# check the number of bootstrap replicates.
check.replicates = function(R) {

  if (missing(R) || is.null(R))
    R = 200
  else if (!is.positive.integer(R))
    stop("the number of bootstrap replicates must be a positive integer.")

  return(R)

}#CHECK.REPLICATES

# check the size of bootstrap replicates.
check.bootsize = function(m, n) {

  if (missing(m) || is.null(m))
    m = n
  else if (!is.positive.integer(m))
    stop("bootstrap sample size must be a positive integer.")

  return(m)

}#CHECK.BOOTSIZE
