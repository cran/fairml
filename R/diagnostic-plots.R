
# default plotting method for fair.model objects.
plot.fair.model = function(x, support = FALSE, regression = FALSE, ncol = 2,
    ...) {

  # diagnostic plots are lattice plots, arranged in panels.
  check.and.load.package("lattice")
  check.and.load.package("gridExtra")
  # check the arguments enabling the reference lines.
  check.logical(support)
  check.logical(regression)
  # check the layout.
  if (!is.positive.integer(ncol))
    stop("the number of columns should be a positive integer number.")

  # this function does not really have additional arguments, the dots are there
  # because they are in the method's deifnition.
  check.unused.args(list(...), character(0))

  # extract the fitted and observed values from the model.
  fit = fitted(x)
  resid = residuals(x)
  obs = x$main$y

  if (x$main$family == "poisson") {

    fit = log(fit + 1)
    obs = log(obs + 1)

  }#THEN
  else if (x$main$family == "cox") {

    fit = fitted(x, type = "link")
    obs = -log(obs[, "time"])

  }#THEN
  else if (x$main$family %in% c("binomial", "multinomial")) {

    fit = fitted(x, type = "class")

  }#THEN

  if (x$main$family %in% c("gaussian", "poisson", "cox")) {

    p1 = lattice::xyplot(obs ~ fit,
      xlab = "fitted values", ylab = "observed values",
      panel = function(x, y, ...) {

        lattice::panel.xyplot(x = x, y = y, ..., alpha = 0.33, pch = 19)
        if (support)
          lattice::panel.abline(c(0, 1), col = "forestgreen", lwd = 2)
        if (regression)
          lattice::panel.abline(coef(lm(y ~ x)), col = "tomato", lwd = 2)

      })

    p2 = lattice::xyplot(resid ~ fit,
      xlab = "fitted values", ylab = "deviance residuals",
      panel = function(x, y, ...) {

        lattice::panel.xyplot(x = x, y = y, ..., alpha = 0.33, pch = 19)
        if (support)
          lattice::panel.abline(h = 0, col = "forestgreen", lwd = 2)
        if (regression)
          lattice::panel.abline(coef(lm(y ~ x)), col = "tomato", lwd = 2)

      })

    p4 = NULL

  }#THEN
  else if (x$main$family %in% c("binomial", "multinomial")) {

    # produce a heatmap from the confusion matrix...
    confusion.matrix = table(fit, obs)
    # ... using a 3-step gradient palette with the lattice dafault colour.
    col.l = grDevices::colorRampPalette(c('azure', '#0072B2'))(30)
    p1 = lattice::levelplot(confusion.matrix,
      ylab = "observed values", xlab = "fitted values",
      col.regions = col.l, colorkey = FALSE,
      at = seq(from = 0, to = length(fit), length.out = length(col.l)),
      scales = list(x = list(rot = 90)),
      panel = function(y, x, z, ...) {

        lattice::panel.levelplot(y = y, x = x, z = z, ...)
        lattice::ltext(x = x, y = y, labels = z, cex = 0.8)

      })

    p2 = lattice::xyplot(resid ~ fit,
      xlab = "fitted values", ylab = "deviance residuals",
      panel = function(x, y, ...) {

        lattice::panel.xyplot(x = x, y = y, ..., alpha = 0.33, pch = 19,
          jitter.x = TRUE)
        if (support)
          lattice::panel.abline(h = 0, col = "forestgreen", lwd = 2)
        if (regression)
          lattice::panel.abline(coef(lm(y ~ x)), col = "tomato", lwd = 2)

      })

    # multi-class ROC curve, each class against all others.
    if (nlevels(obs) == 2) {

      roc = data.frame(true = (obs == levels(obs)[2]) + 0L,
                       prob = fitted(x, type = "response"),
                       level = rep(levels(obs)[2], length(obs)))

    }#THEN
    else {

      roc = data.frame(true = numeric(0), prob = numeric(0), level = character(0))

      for (l in levels(obs))
        roc = rbind(roc, data.frame(true = (obs == l) + 0L,
                                    prob = fitted(x, type = "response")[, l],
                                    level = l))

    }#ELSE

    p4 = lattice::xyplot(true ~ prob, data = roc, groups = roc$level,
      xlim = c(0, 1), ylim = c(0, 1),
      xlab = "1 - specificity", ylab = "sensitivity",
      scales = list(x = list(at = c(0, 0.20, 0.40, 0.60, 0.80, 1)),
                    y = list(at = c(0, 0.20, 0.40, 0.60, 0.80, 1))),
      panel = lattice::panel.superpose,
      panel.groups = function(x, y, type, ...) {

        DD = table(-x, y)
        sens = cumsum(DD[, 2]) / sum(DD[, 2])
        mspec = cumsum(DD[, 1]) / sum(DD[, 1])
        lattice::panel.xyplot(mspec, sens, type = "l", ...)
        if (support)
          lattice::panel.abline(c(0, 1), col = "darkgrey", lwd = 2)
    })

  }#THEN

  p3 = lattice::qqmath(~ resid,
    xlab = "normal quantiles", ylab = "deviance residuals",
    panel = function(x, ...) {

      lattice::panel.qqmath(x, ...,  alpha = 0.33, pch = 19)
      if (support)
        lattice::panel.qqmathline(x, col = "forestgreen", lwd = 2)

    })

  if (is.null(p4))
    gridExtra::grid.arrange(p1, p2, p3, ncol = ncol)
  else
    gridExtra::grid.arrange(p1, p2, p3, p4, ncol = ncol)

}#PLOT.FAIR.MODEL
