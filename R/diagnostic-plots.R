
# default plotting method for fair.model objects.
plot.fair.model = function(x, diagonal = FALSE, regression = FALSE, ...) {

  # diagnostic plots are lattice plots.
  check.and.load.package("lattice")
  # check the arguments enabling the reference lines.
  check.logical(diagonal)
  check.logical(regression)

  # this function does not really have additional arguments, the dots are there
  # because they are in the method's deifnition.
  check.unused.args(list(...), character(0))

  # extract the fitted and observed values from the model.
  fit = fitted(x)
  obs = fit + residuals(x)

  lattice::xyplot(obs ~ fit,
    xlab = "fitted values", ylab = "observed values",
    panel = function(x, y, ...) {

      lattice::panel.xyplot(x = x, y = y, ..., alpha = 0.33, pch = 19)
      if (diagonal)
        lattice::panel.abline(c(0, 1), col = "forestgreen", lwd = 2)
      if (regression)
        lattice::panel.abline(coef(lm(y ~ x)), col = "tomato", lwd = 2)

    })

}#PLOT.FAIR.MODEL
