
# print a few key facts of the fair model.
print.fair.model = function(x, digits, ...) {

  if (!is(x, "fair.model"))
    stop("'x' must be a 'fair.model' object.")

  if (missing(digits))
    digits = max(3L, getOption("digits") - 3L)
  else {

    if (!is.non.negative.integer(digits))
      stop("'digits' should be a single non-negative integer number.")

  }#ELSE

  check.unused.args(list(...), character(0))

  # print the function call.
  cat("\nCall:\n", paste(deparse(x$main$call), sep = "\n", collapse = "\n"),
    "\n\n", sep = "")
  # print the regression coefficients.
  cat("Coefficients:\n")
  print.default(format(coef(x), digits = digits), print.gap = 2L, quote = FALSE)

  invisible(x)

}#PRINT.FAIR.MODEL

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

  value = object$main$r.squared.S /
            (object$main$r.squared.S + object$main$r.squared.U)
  r2 = 1 - var(residuals(object)) / var(fitted(object) + residuals(object))

  performance = list(
    "Residual standard error" = sigma(object),
    "Multiple R-squared" = r2,
    c("Komiyama's R-squared" = value, "with bound" = object$main$bound)
  )

  structure(list(model = object, banner = "Method: Komiyama et al. (2018)",
                 performance = performance),
    class = "summary.fair.model")

}#SUMMARY.NCLM

# print the summary of a fair model.
print.summary.fair.model = function(x, digits, ...) {

  if (!is(x, "summary.fair.model"))
    stop("'x' must be a 'fair.model' object.")

  if (missing(digits))
    digits = max(3L, getOption("digits") - 3L)
  else {

    if (!is.non.negative.integer(digits))
      stop("'digits' should be a single non-negative integer number.")

  }#ELSE

  check.unused.args(list(...), character(0))

  # banner.
  cat("\nFair Linear Regression Model\n\n")
  cat(x$banner, "\n")

  # model's parameters.
  print.fair.model(x$model, digits = digits)
  cat("\n")

  # performance measures.
  for (i in seq_along(x$performance)) {

    perf = unlist(x$performance[i])
    for (j in seq_along(perf))
      cat(names(perf[j]), ": ", format(perf[j], digits = digits), " ", sep = "")
    cat("\n")

  }#FOR

}#PRINT.SUMMARY.FAIR.MODEL
