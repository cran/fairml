
# compare twoobjects of class fair.model.
all.equal.fair.model = function(target, current, ...) {

  if (!is(target, "fair.model"))
    stop("'object' must be a 'fair.model' object.")
  if (!is(current, "fair.model"))
    stop("'object' must be a 'fair.model' object.")

  # do not warn about these two arguments, they are set when all.equal() methods
  # call each other in a cascade and thus they result in pointless warnings.
  check.unused.args(list(...), c("check.attributes", "use.names"))

  differences = character(0)

  # both models should be regression models.
  target.is.regression = is.numeric(fitted(target))
  current.is.regression = is.numeric(fitted(current))

  if (target.is.regression && !current.is.regression)
    differences = "'target' is a regression model, 'current' is not."
  else if (!target.is.regression && current.is.regression)
    differences = "'current' is a regression model, 'target' is not."

  # coefficients may be different.
  diff = all.equal(coef(target), coef(current))
  if (!isTRUE(diff))
    differences = c(differences, paste(diff, "(coefficients)."))

  # standard error may be different.
  diff = all.equal(coef(target), coef(current))
  if (!isTRUE(diff))
    differences = c(differences, paste(diff, "(standard error)."))

  if (length(differences) != 0)
    return(differences)
  else
    return(TRUE)

}#ALL.EQUAL.FAIR.MODEL
