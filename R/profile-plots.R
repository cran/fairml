
# plot a profile plot of the coefficients.
fairness.profile.plot = function(response, predictors, sensitive, unfairness,
    legend = FALSE, type = "coefficients", model = "nclm",
    model.args = list()) {

  valid.types = c("coefficients", "variance")

  # diagnostic plots are lattice plots.
  check.and.load.package("lattice")

  # check response, predictors and sensitive attributes.
  response = check.response(response)
  predictors = check.data(predictors, nobs = length(response), varletter = "X")
  sensitive = check.data(sensitive, nobs = length(response), varletter = "S")
  unfairness = check.fairness.level(unfairness, scalar = FALSE)

  # check the type and legend of the plot.
  check.logical(legend)
  check.label(type, valid.types, "type")

  # check the model to be fitted.
  check.label(model, fair.models, "model")
  # remove optional arguments that do not belong after warning.
  check.unused.args(model.args, fair.models.extra.args[[model]])
  model.args =
    model.args[names(model.args) %in% fair.models.extra.args[[model]]]

  # fit the model for each fairness level.
  fit = function(level) {
    do.call(model, c(list(response = response,
                          predictors = predictors,
                          sensitive = sensitive,
                          unfairness = level),
                     model.args))
  }

  models = lapply(unfairness, fit)

  if (type == "coefficients") {

    # extract the coefficients from the models.
    coefs = lapply(models, coef)

    # arrange the coefficients (except the intercept, which is not meaningful
    # in this context) in a data frame and a formula.
    coefs = as.data.frame(do.call(rbind, coefs))
    coefs = coefs[, names(coefs) != "(Intercept)"]
    lhs = paste0("`", names(coefs), "`", collapse = " + ")
    coefs[, "unfairness"] = unfairness
    ff = formula(paste(lhs, "~ unfairness"))

    if (legend) {

      # make sure the graphic device is initialized, otherwise it is impossible
      # to compute the grphical width of the labels.
      if (is(try(strwidth(""), silent = TRUE), "try-error"))
        plot.new()

      label.width = strwidth(setdiff(names(coefs), "unfairness"))
      x.axis.range = range(coefs$unfairness) + c(-1, 1) * 0.05
      xlim = c(x.axis.range[1], x.axis.range[2] + label.width)
      x.at = pretty(x.axis.range)
      x.at = x.at[(x.at >= x.axis.range[1]) & (x.at <= x.axis.range[2])]

    }#THEN
    else {

      label.width = 0
      xlim = range(coefs$unfairness) + c(-1, 1) * 0.05
      x.at = pretty(xlim)
      x.at = x.at[(x.at >= xlim[1]) & (x.at <= xlim[2])]

    }#ELSE

    # disable clipping to be able to draw the axis.
    suppressWarnings(lattice::trellis.par.set("clip", list(panel = "off")))

    # plot and return (relying on autoprint to display the plot).
    lattice::xyplot(ff, data = coefs,
      xlab = "unfairness", ylab = "coefficients", type = "l",
      scales = list(tck = c(1, 0), x = list(at = NULL)), xlim = xlim,
      panel = function(y, x, ...) {

        lattice::panel.xyplot(y = y, x = x, ...)
        lattice::panel.abline(h = 0, col = "lightgray", lty = "dashed")
        if (legend)
          lattice::panel.text(x = max(x) + 0.01, y = y[x == max(x)], pos = 4,
                     labels = setdiff(names(coefs), "unfairness"))

        lattice::panel.axis(side = "bottom", at = x.at, outside = TRUE)

      })

  }#THEN
  else if (type == "variance") {

    # graphical parameters.
    lwd = 1.5
    col = c("tomato", "darkblue", "forestgreen")

    # extract the proportions of explained variance for predictors and
    # sensitive attributes.
    models = lapply(models, `[[`, "main")
    vars = lapply(models, `[`, c("r.squared.U", "r.squared.S"))
    vars = lapply(vars, unlist)
    # arrange the proportions in a data frame.
    vars = data.frame(unfairness = unfairness, do.call(rbind, vars))
    # add the R^2 defined in Komiyama et al.
    vars$r.squared = vars$r.squared.S / (vars$r.squared.U + vars$r.squared.S)

    # create the legend.
    if (legend) {

      key = list(text = list(c("decorrelated predictors",
                               "sensitive attributes",
                               "proportion (sensitive attributes)")),
                 lines = list(col = col, lwd = lwd),
                 corner = c(0.05, 0.925))

    }#THEN
    else {

      key = NULL

    }#ELSE

    # plot and return (relying on autoprint to display the plot).
    lattice::xyplot(r.squared.U + r.squared.S + r.squared ~ unfairness,
      data = vars, xlab = "unfairness", ylab = "explained variance",
      col = col, type = "l", lwd = lwd,
      ylim = c(-0.05, 1.05), scales = list(tck = c(1, 0)), key = key,
      panel = function(y, x, ...) {

        lattice::panel.segments(x1 = 0, y1 = 0, x2 = 1, y2 = 1,
                                col = "lightgray", lty = "dashed")
        lattice::panel.xyplot(y = y, x = x, ...)
        lattice::panel.abline(h = c(0, 1), col = "lightgray", lty = "dashed")

      })

  }#THEN

}#FAIRNESS.PROFILE.PLOT
