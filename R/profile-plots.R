
# plot a profile plot of the coefficients.
fairness.profile.plot = function(response, predictors, sensitive, unfairness,
    legend = FALSE, type = "coefficients", model, model.args = list(), cluster) {

  # diagnostic plots are lattice plots.
  check.and.load.package("lattice")

  # check the model to be fitted.
  check.label(model, fair.models, "model")

  # check response, predictors and sensitive attributes.
  response = check.response(response, model = model)
  predictors = check.data(predictors, nobs = length(response), varletter = "X")
  sensitive = check.data(sensitive, nobs = length(response), varletter = "S")
  unfairness = check.fairness.level(unfairness, scalar = FALSE)

  # check the type and legend of the plot.
  check.logical(legend)
  check.label(type, available.profile.plots, "type")
  # check whether the model can be plotted with this type of plot.
  if (!(model %in% models.for.plot[[type]]))
    stop("model ", q(model), " is not supported by profile plot ", q(type), ".")

  # remove optional arguments that do not belong after warning.
  check.unused.args(model.args, fair.models.extra.args[[model]])
  model.args =
    model.args[names(model.args) %in% fair.models.extra.args[[model]]]

  # check the cluster.
  cluster = check.cluster(cluster)

  # fit the model for each fairness level.
  model.fit = function(level) {
    do.call(model, c(list(response = response,
                          predictors = predictors,
                          sensitive = sensitive,
                          unfairness = level),
                     model.args))
  }

  fitted = smartSapply(cluster, unfairness, model.fit)

  if (type == "coefficients") {

    coefficients.plot(models = fitted, unfairness = unfairness,
      response = response, legend = legend)

  }#THEN
  else if (type == "constraints") {

    constraints.plot(models = fitted, unfairness = unfairness,
      response = response, legend = legend)

  }#THEN
  else if (type == "precision-recall") {

    if (model %in% fair.family) {

      family = fitted[[1]]$main$family

      if (family != "binomial")
        stop("model ", q(model), " with family ", q(family),
             " is not supported by profile plot ", q(type), ".")

    }#THEN

    precision.recall.plot(models = fitted, unfairness = unfairness,
      response = response, legend = legend)

  }#THEN
  else if (type == "rmse") {

    if (model %in% fair.family) {

      family = fitted[[1]]$main$family

      if (family != "gaussian")
        stop("model ", q(model), " with family ", q(family),
             " is not supported by profile plot ", q(type), ".")

    }#THEN

    rmse.plot(models = fitted, unfairness = unfairness, response = response,
      legend = legend)

  }#THEN

}#FAIRNESS.PROFILE.PLOT

coefficients.plot = function(models, unfairness, response, legend) {

  # extract the coefficients from the models.
  coefs = lapply(models, coef)

  # arrange the coefficients (except the intercept, which is not meaningful
  # in this context) in a data frame and a formula.
  coefs = as.data.frame(do.call(rbind, coefs))
  coefs = coefs[, names(coefs) != "(Intercept)", drop = FALSE]
  lhs = paste0("`", names(coefs), "`", collapse = " + ")
  coefs[, "unfairness"] = unfairness
  formula = formula(paste(lhs, "~ unfairness"))

  if (legend) {

    # make sure the graphic device is initialized, otherwise it is impossible
    # to compute the grphical width of the labels.
    if (inherits(try(strwidth(""), silent = TRUE), "try-error"))
      plot.new()

    label.width = strwidth(setdiff(names(coefs), "unfairness"))
    x.axis.range = extended.range(coefs$unfairness)
    xlim = c(x.axis.range[1], x.axis.range[2] + label.width)
    x.at = pretty(x.axis.range)
    x.at = x.at[(x.at >= x.axis.range[1]) & (x.at <= x.axis.range[2])]

  }#THEN
  else {

    label.width = 0
    xlim = extended.range(coefs$unfairness)
    x.at = pretty(xlim)
    x.at = x.at[(x.at >= xlim[1]) & (x.at <= xlim[2])]

  }#ELSE

  # disable clipping to be able to draw the axis.
  suppressWarnings(lattice::trellis.par.set("clip", list(panel = "off")))

  # plot and return (relying on autoprint to display the plot).
  lattice::xyplot(formula, data = coefs,
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

}#COEFFICIENTS.PLOT

constraints.plot = function(models, unfairness, response, legend) {

  # get the definition of fairness to find out what constraints to plot.
  definition = models[[1]]$fairness$definition

  if (definition %in% c("sp-komiyama", "eo-komiyama")) {

    # constraints on variance components.
    variance.constraints.plot(definition = definition, models = models,
      unfairness = unfairness, response = response, legend = legend)

  }#THEN
  else {

    # constraints on individual sensitive attributes.
    variables.constraints.plot(definition = definition, models = models,
      unfairness = unfairness, response = response, legend = legend)

  }#ELSE

}#CONSTRAINTS.PLOT

variance.constraints.plot = function(definition, models, unfairness, response,
    legend) {

  if (definition == "sp-komiyama") {

    # extract the proportions of explained variance for predictors and
    # sensitive attributes, and pair them with the unfairness.
    models = lapply(models, `[[`, "fairness")
    vars = data.frame(
      unfairness = unfairness,
      r.squared.U = sapply(models, function(x) x$other$r.squared.U),
      r.squared.S = sapply(models, function(x) x$other$r.squared.S),
      r2.statistical.parity = sapply(models, `[[`, "value")
    )

    # color palette for xyplot().
    col = c("tomato", "darkblue", "forestgreen")
    # formula for xyplot().
    formula = r.squared.U + r.squared.S + r2.statistical.parity ~ unfairness

    # create the legend.
    if (legend) {

      key = list(text = list(c("decorrelated predictors",
                               "sensitive attributes",
                               "proportion (sensitive attributes)")),
                 lines = list(col = col, lwd = 1.5),
                 corner = c(0.05, 0.925))

    }#THEN
    else {

      key = NULL

    }#ELSE

  }#THEN
  else if (definition == "eo-komiyama") {

    # extract the proportions of variance for the observed response and the
    # sensitive attributes, and pair them with the unfairness.
    models = lapply(models, `[[`, "fairness")
    vars = data.frame(
      unfairness = unfairness,
      r.squared.S = sapply(models, function(x) x$other$r.squared.S),
      r.squared.y = sapply(models, function(x) x$other$r.squared.y),
      r2.equality.of.opportunity = sapply(models, `[[`, "value")
    )

    # color palette for xyplot().
    col = c("tomato", "darkblue", "forestgreen")
    # formula for xyplot().
    formula = r.squared.y + r.squared.S + r2.equality.of.opportunity ~ unfairness

    # create the legend.
    if (legend) {

      key = list(text = list(c("observed response",
                               "sensitive attributes",
                               "proportion (sensitive attributes)")),
                 lines = list(col = col, lwd = 1.5),
                 corner = c(0.05, 0.925))

    }#THEN
    else {

      key = NULL

    }#ELSE

  }#THEN

  # plot and return (relying on autoprint to display the plot).
  lattice::xyplot(formula, data = vars, col = col, type = "l", lwd = 1.5,
    xlab = "unfairness", ylab = "explained variance",
    xlim = extended.range(unfairness), ylim = c(-0.05, 1.05),
    scales = list(tck = c(1, 0)), key = key,
    panel = function(y, x, ...) {

      lattice::panel.segments(x1 = 0, y1 = 0, x2 = 1, y2 = 1,
                              col = "lightgray", lty = "dashed")
      lattice::panel.xyplot(y = y, x = x, ...)
      lattice::panel.abline(h = c(0, 1), col = "lightgray", lty = "dashed")

    })

}#VARIANCE.CONSTRAINTS.PLOT

variables.constraints.plot = function(definition, models, unfairness, response,
    legend) {

  # extract the constraints from the models.
  constraints = lapply(models, function(x) x$fairness$value)
  constraints = data.frame(do.call(rbind, constraints))
  lhs = paste0("`", names(constraints), "`", collapse = " + ")
  constraints$unfairness = unfairness
  # formula for xyplot().
  formula = formula(paste(lhs, "~ unfairness"))

  if (legend) {

    # make sure the graphic device is initialized, otherwise it is impossible
    # to compute the grphical width of the labels.
    if (inherits(try(strwidth(""), silent = TRUE), "try-error"))
      plot.new()

    label.width = strwidth(setdiff(names(constraints), "unfairness"))
    x.axis.range = extended.range(constraints$unfairness)
    xlim = c(x.axis.range[1], x.axis.range[2] + label.width)
    x.at = pretty(x.axis.range)
    x.at = x.at[(x.at >= x.axis.range[1]) & (x.at <= x.axis.range[2])]

  }#THEN
  else {

    label.width = 0
    xlim = extended.range(constraints$unfairness)
    x.at = pretty(xlim)
    x.at = x.at[(x.at >= xlim[1]) & (x.at <= xlim[2])]

  }#ELSE

  # disable clipping to be able to draw the axis.
  suppressWarnings(lattice::trellis.par.set("clip", list(panel = "off")))

  # plot and return (relying on autoprint to display the plot).
  lattice::xyplot(formula, data = constraints,
    xlab = "unfairness", ylab = "|correlations|", type = "l",
    xlim = xlim, ylim = c(-0.05, 1.05),
    scales = list(tck = c(1, 0), x = list(at = NULL)),
    panel = function(y, x, ...) {

      lattice::panel.xyplot(y = y, x = x, ...)
      lattice::panel.abline(h = c(0, 1), col = "lightgray", lty = "dashed")
      lattice::panel.segments(x1 = 0, y1 = 0, x2 = 1, y2 = 1,
                              col = "lightgray", lty = "dashed")
      if (legend)
        lattice::panel.text(x = max(x) + 0.01, y = y[x == max(x)], pos = 4,
                   labels = setdiff(names(constraints), "unfairness"))

      lattice::panel.axis(side = "bottom", at = x.at, outside = TRUE)

    })

}#VARIABLES.CONSTRAINTS.PLOT

precision.recall.plot = function(models, unfairness, response, legend) {

  # graphical parameters.
  lwd = 1.5
  col = c("tomato", "darkblue", "forestgreen")

  # extract the fitted values from the models.
  models = lapply(models, `[[`, "main")
  fitted = lapply(models, `[[`, "fitted.values")
  fitted = lapply(fitted, prob2class, labels = levels(response))

  # compute precision and recall.
  pr = data.frame(unfairness = unfairness, precision = 0, recall = 0)

  for (i in seq_along(unfairness)) {

    # compute the confusion matrix, which is what all performance measures in
    # classification are computed from.
    confusion.matrix = table(fitted[[i]], response)

    tp = confusion.matrix[1, 1] + confusion.matrix[2, 2]
    fp = confusion.matrix[2, 1]
    fn = confusion.matrix[1, 2]

    pr[i, c("precision", "recall")] = c(1 - fp / (fp + tp), tp / (tp + fn))

  }#FOR

  # add the F1 measure.
  pr$f1 = 2 * (pr$precision * pr$recall) / (pr$precision + pr$recall)

  # create the legend.
  if (legend) {

    key = list(text = list(c("precision",
                             "recall",
                             "F1 measure")),
               lines = list(col = col, lwd = lwd),
               corner = c(0.90, 0.075))

  }#THEN
  else {

    key = NULL

  }#ELSE

  # plot and return (relying on autoprint to display the plot).
  lattice::xyplot(precision + recall + f1 ~ unfairness,
    data = pr, xlab = "unfairness", ylab = "goodness of fit",
    col = col, type = "l", lwd = lwd,
    xlim = extended.range(unfairness), ylim = c(-0.05, 1.05),
    scales = list(tck = c(1, 0)), key = key,
    panel = function(y, x, ...) {

      lattice::panel.xyplot(y = y, x = x, ...)
      lattice::panel.abline(h = c(0, 1), col = "lightgray", lty = "dashed")

    })

}#PRECISION.RECALL.PLOT

rmse.plot = function(models, unfairness, response, legend) {

  # graphical parameters.
  lwd = 1.5
  col = "darkblue"

  # extract the fitted values from the models.
  models = lapply(models, `[[`, "main")
  rmse = sapply(models, sigma)

  # create the legend.
  if (legend) {

    key = list(text = list(c("RMSE")),
               lines = list(col = col, lwd = lwd),
               corner = c(0.90, 0.075))

  }#THEN
  else {

    key = NULL

  }#ELSE

  # plot and return (relying on autoprint to display the plot).
  lattice::xyplot(rmse ~ unfairness,
    xlab = "unfairness", ylab = "RMSE",
    col = col, type = "l", lwd = lwd,
    xlim = extended.range(unfairness),
    scales = list(tck = c(1, 0)), key = key,
    panel = function(y, x, ...) {

      lattice::panel.xyplot(y = y, x = x, ...)
      lattice::panel.abline(h = c(0, 1), col = "lightgray", lty = "dashed")

    })

}#RMSE.PLOT
