
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

  # print various pieces of informations on the performance and fairness of the
  # model.
  for (i in seq_along(x$info)) {

    # for vector arguments: if they have a name, print them individually;
    # otherwise, put them all on the same line.
    if ((names(x$info[i]) != "") && (length(x$info[[i]]) > 1)) {

      cat(names(x$info[i]), ":\n", sep = "")
      print(x$info[[i]], digits = digits)

    }#THEN
    else {

      perf = unlist(x$info[i])
      for (j in seq_along(perf))
        cat(names(perf[j]), ": ",
            format(perf[j], digits = digits), " ", sep = "")
      cat("\n")

    }

  }#FOR

}#PRINT.SUMMARY.FAIR.MODEL

# print method for a single cross-validation run.
print.fair.kcv = function(x, print.loss = TRUE, ...) {

  a = attributes(x)

  # warn about unused arguments.
  check.unused.args(list(...), character(0))

  cat("\n ", a$method,  "cross-validation for fair models\n\n")

  wcat("  model:                                ", fair.models.labels[a$model])

  if (a$method == "k-fold") {

    wcat("  number of folds:                      ", length(x))

  }#THEN
  else if (a$method == "hold-out") {

    wcat("  number of splits:                     ", length(x))
    wcat("  size of the test subset:              ", length(x[[1]]$test))

  }#ELSE

  if (print.loss) {

    cat("  expected loss:\n")
    formatted.losses = format(a$loss)
    formatted.names = paste0(names(a$loss), ":")
    for (i in seq_along(formatted.losses))
      wcat(sprintf("    %-34s  ", formatted.names[i]), formatted.losses[i])

    cat("\n")

  }#THEN

  invisible(x)

}#PRINT.FAIR.KCV

# print method for a list containing multiple cross-validation runs.
print.fair.kcv.list = function(x, ...) {

  losses = sapply(x, function(x) attr(x, "loss"))

  if (is.matrix(losses)) {

    formatted.losses = format(rowMeans(losses))
    formatted.stderr = format(apply(losses, 1, sd))
    formatted.names = paste0(rownames(losses), ":")

  }#THEN
  else {

    formatted.losses = format(mean(losses))
    formatted.stderr = format(sd(losses))
    formatted.names = paste0(names(losses)[1], ":")

  }#ELSE

  print.fair.kcv(x[[1]], print.loss = FALSE)
  wcat("  number of runs:                       ", length(x))
  cat("  average loss over the runs:\n")
  for (i in seq_along(formatted.losses))
    wcat(sprintf("    %-34s  ", formatted.names[i]), formatted.losses[i])
  cat("  standard deviation of the loss:\n")
  for (i in seq_along(formatted.losses))
    wcat(sprintf("    %-34s  ", formatted.names[i]), formatted.stderr[i])
  cat("\n")

  invisible(x)

}#PRINT.FAIR.KCV.LIST

