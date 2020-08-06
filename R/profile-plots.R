
# plot a profile plot of the coefficients.
fairness.profile.plot = function(response, predictors, sensitive, epsilon,
    legend = FALSE, type = "coefficients", ...) {

  valid.types = c("coefficients", "variance")

  # diagnostic plots are lattice plots.
  check.and.load.package("lattice")

  # check response, predictors and sensitive attributes.
  response = check.response(response)
  predictors = check.data(predictors, nobs = length(response), varletter = "X")
  sensitive = check.data(sensitive, nobs = length(response), varletter = "S")

  if (missing(epsilon))
    epsilon = seq(from = 0.00, to = 1, by = 0.02)
  else if (!is.probability.vector(epsilon))
      stop("'epsilon' should be a vector of numbers between 0 and 1.")

  if (missing(legend))
    legend = FALSE
  else
    check.logical(legend)

  if (missing(type))
    type = "coefficients"
  else if (!is.string(type) || !(type %in% valid.types))
    stop("'type' should be one of ", q(valid.types), ".")

  # fit the model for each epsilon.
  models = lapply(epsilon, function(e) {
                    nclm(response, predictors, sensitive, epsilon = e, ...)
           })

  if (type == "coefficients") {

    # extract the coefficients from the models.
    coefs = lapply(models, coef)

    # arrange the coefficients in a data frame and a formula.
    coefs = as.data.frame(do.call(rbind, coefs))
    names(coefs)[names(coefs) == "(Intercept)"] = "Intercept"
    lhs = paste0("`", names(coefs), "`", collapse = " + ")
    coefs[, "epsilon"] = epsilon
    ff = formula(paste(lhs, "~ epsilon"))

    if (legend) {

      # make sure the graphic device is initialized, otherwise it is impossible
      # to compute the grphical width of the labels.
      if (is(try(strwidth(""), silent = TRUE), "try-error"))
        plot.new()

      label.width = strwidth(setdiff(names(coefs), "epsilon"))
      x.axis.range = range(coefs$epsilon) + c(-1, 1) * 0.05
      xlim = c(x.axis.range[1], x.axis.range[2] + label.width)
      x.at = pretty(x.axis.range)
      x.at = x.at[(x.at >= x.axis.range[1]) & (x.at <= x.axis.range[2])]

    }#THEN
    else {

      label.width = 0
      xlim = range(coefs$epsilon) + c(-1, 1) * 0.05
      x.at = pretty(xlim)
      x.at = x.at[(x.at >= xlim[1]) & (x.at <= xlim[2])]

    }#ELSE

    # disable clipping to be able to draw the axis.
    lattice::trellis.par.set("clip", list(panel = "off"))

    # plot and return (relying on autoprint to display the plot).
    lattice::xyplot(ff, data = coefs,
      xlab = "epsilon", ylab = "coefficients", type = "l",
      scales = list(tck = c(1, 0), x = list(at = NULL)), xlim = xlim,
      panel = function(y, x, ...) {

        lattice::panel.xyplot(y = y, x = x, ...)
        lattice::panel.abline(h = 0, col = "lightgray", lty = "dashed")
        if (legend)
          lattice::panel.text(x = max(x) + 0.01, y = y[x == max(x)], pos = 4,
                     labels = setdiff(names(coefs), "epsilon"))

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
    vars = data.frame(epsilon = epsilon, do.call(rbind, vars))
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
    lattice::xyplot(r.squared.U + r.squared.S + r.squared ~ epsilon,
      data = vars, xlab = "epsilon", ylab = "explained variance",
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
