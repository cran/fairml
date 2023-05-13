# fair ridge regression.
fgrrm = function(response, predictors, sensitive, unfairness,
         definition = "sp-komiyama", family = "binomial", lambda = 0,
         save.auxiliary = FALSE) {

  fitted = two.stage.regression(model = "fgrrm", family = family,
             response = response, predictors = predictors, sensitive = sensitive,
             unfairness = unfairness, definition = definition,
             covfun = NULL, lambda = lambda, save.auxiliary = save.auxiliary)

  # save the function call for the print() method.
  fitted$main$call = match.call()

  return(fitted)

}#FGRRM

fgrrm.glmnet = function(y, S, U, unfairness, definition, family = "binomial",
    lambda = 0) {

  # choose the right function for the definition of fairness (and avoid having
  # to pass it the data explicitly, for brevity).
  if (is.function(definition))
    r2 = function(model) {

      fairness = definition(model, y, S, U, family)["value"]

      if (!is.probability(fairness))
        stop("the custom fairness function should return a single number between 0 and 1.")

      return(fairness)

    }#R2
  else if (definition == "sp-komiyama")
    r2 = function(model) fgrrm.sp.komiyama(model, y, S, U, family)["value"]
  else if (definition == "eo-komiyama")
    r2 = function(model) fgrrm.eo.komiyama(model, y, S, U, family)["value"]
  else if (definition == "if-berk")
    r2 = function(model) fgrrm.if.berk(model, y, S, U, family)["value"]

  # shorthand for the function that fits the model.
  ridge = function(lambda, lr)
            fair.ridge(y, S, U, lambda = lambda, lr = lr, family = family)

  build.return.value = function(model, lr) {

    if (is.matrix(model$coefficients))
      sensitive = rownames(model$coefficients) %in% colnames(S)
    else
      sensitive = names(model$coefficients) %in% colnames(S)

    coefs = structure(model$coefficients, sensitive = sensitive)

    if (is.function(definition)) {

      constraint = definition(model, y, S, U, family)
      other = list()

    }#THEN
    else if (definition == "sp-komiyama") {

      constraint = fgrrm.sp.komiyama(model, y, S, U, family)
      other = as.list(constraint[c("r.squared.S", "r.squared.U")])

    }#THEN
    else if (definition == "eo-komiyama") {

      constraint = fgrrm.eo.komiyama(model, y, S, U, family)
      other = as.list(constraint[c("r.squared.S", "r.squared.y")])

    }#THEN
    else if (definition == "if-berk") {

      constraint = fgrrm.if.berk(model, y, S, U, family)
      other = list()

    }#THEN

    return(list(main = list(
                  coefficients = coefs,
                  residuals = model$residuals,
                  fitted.values = model$fitted,
                  y = y,
                  family = family,
                  deviance = model$deviance,
                  loglik = model$loglik,
                  arguments = list(
                    lr = lr,
                    lambda = lambda,
                    definition = definition
                )),
           fairness = list(
                  definition = definition,
                  value = as.vector(constraint["value"]),
                  bound = unfairness,
                  other = other
                )))

  }#BUILD.RETURN.VALUE

  # first check whether any unfairness is allowed at all: if not, special-case
  # the model and drop all sensitive attributes.
  if (unfairness <= sqrt(.Machine$double.eps)) {

    completely.fair = ridge(lambda = lambda, lr = Inf)
    return(build.return.value(completely.fair, lr = Inf))

  }#THEN

  # then check whether the proportion of variance explained by S is already
  # smaller than the required unfairness level; return an OLS regression for
  # lambda(r) = 0 if that is the case.
  ols = ridge(lambda = lambda, lr = 0)

  if (r2(ols) <= unfairness)
    return(build.return.value(ols, lr = 0))

  # establish an upper bound for lambda(r), to pass to optimize().
  m = 1
  for (m in 1:50) {

    tentative.unfairness = r2(ridge(lambda = lambda, lr = 10^m))
    if (tentative.unfairness <= unfairness)
      break

  }#FOR

  # find the lambda(r) that gives the required R^2 == unfairness.
  value = optimize(f = function(lr) {

    ridge = ridge(lambda = lambda, lr = lr)
    return(abs(r2(ridge) - unfairness))

  }, interval = c(0, 10^m), tol = sqrt(.Machine$double.eps))

  fit = ridge(lambda = lambda, lr = value$minimum)

  return(build.return.value(fit, lr = value$minimum))

}#FGRRM.GLMNET

# glmnet wrapper to make code simpler.
fair.ridge = function(y, S, U, family, lambda, lr) {

  # we want a vector of ridge penalties with elements equal to lr (for the
  # sensitive attributes) and to lambda (for the predictors) which in glmnet()
  # passed as the lambda * penalty.factors arguments.
  if ((lr == 0) && (lambda == 0)) {

    # the penalty factors cannot be all zeros: set lambda to zero instead, and
    # use the default penalty factors (the values does not really matter).
    pf = rep(1, ncol(S) + ncol(U))
    adjusted.lr = 0

  }#THEN
  else if (is.infinite(lr)) {

    # infinte weights just remove the variables from the model, which is why ...
    pf = c(rep(Inf, ncol(S)), rep(lambda, ncol(U)))
    # ... the normalization of lr and pf uses ncol(U) instead of length(pf) like
    # the general case below.
    scaling = ifelse(lambda == 0, 1, ncol(U) / (lambda * ncol(U)))
    adjusted.lr = 1 / scaling
    pf = pf * scaling

  }#THEN
  else {

    # use different penalty factors for the sensitive attributes and the
    # predictors, so that they are assigned independent ridge penalties.
    pf = c(rep(lr, ncol(S)), rep(lambda, ncol(U)))
    # glmnet() scales penalty factors to sum up to the number of variables,
    # but it does not (inverse) scale lambda by the same amount: do that
    # manually.
    scaling = length(pf) / sum(pf)
    adjusted.lr = 1 / scaling
    pf = pf * scaling

  }#ELSE

  rr = try(glmnet(y = y, x = cbind(S, U), family = family, alpha = 0,
             lambda = adjusted.lr, penalty.factor = pf), silent = TRUE)

  # glmnet can misleadingly return an empty model (instead of an error) when it
  # fails to converge for the only value of lambda it has, or fail internally
  # because of unhandled errors.
  if (is(rr, "try-error") || (rr$jerr != 0))
    stop("glmnet() failed to estimate the model for lr = ", lr, ".")

  return(glmnet.stats(rr, y = y, S = S, U = U, family = family, lambda = lambda,
           lr = lr))

}#FAIR.RIDGE

# proportion of deviance explained by the sensitive attributes, from the
# constraint in Komiyama et al. (2018) and the definition of statistical parity.
fgrrm.sp.komiyama = function(model, y, S, U, family = "gaussian") {

  coefs = model$coefficients

  if (family == "gaussian") {

    S = S[, colnames(S) != "(Intercept)", drop = FALSE]
    U = U[, colnames(U) != "(Intercept)", drop = FALSE]
    explained.S = var(S %*% coefs[colnames(S)])
    explained.U = var(U %*% coefs[colnames(U)])
    explained.all = var(cbind(S, U) %*% coefs[names(coefs) != "(Intercept)"])

    return(c(value = explained.S / explained.all,
             r.squared.S = explained.S / var(y),
             r.squared.U = explained.U / var(y)))

  }#THEN
  else if (family == "cox") {

    # re-fit the models to get the fitted values.
    mab = glmnet.stats(model, y = y, S = S, U = U, family = family)
    ma0 = model
    ma0$coefficients[colnames(U)] = 0
    ma0 = glmnet.stats(ma0, y = y, S = S, U = U, family = family)
    m0b = model
    m0b$coefficients[colnames(S)] = 0
    m0b = glmnet.stats(m0b, y = y, S = S, U = U, family = family)

    # compute the variance components as for a linear regression.
    explained.S = var(fitted(ma0))
    explained.U = var(fitted(m0b))
    explained.all = var(fitted(mab))

    return(c(value = explained.S / explained.all,
             r.squared.S = explained.S / var(-log(y[, "time"])),
             r.squared.U = explained.U / var(-log(y[, "time"]))))

  }#THEN
  else if (family %in% c("binomial", "multinomial", "poisson")) {

    mab = glmnet.stats(model, y = y, S = S, U = U, family = family)
    m0b = zero.coefficients(model, colnames(S))
    m0b = glmnet.stats(m0b, y = y, S = S, U = U, family = family)
    ma0 = zero.coefficients(model, colnames(U))
    ma0 = glmnet.stats(ma0, y = y, S = S, U = U, family = family)
    m00 = zero.coefficients(model, c(colnames(S), colnames(U)))
    m00 = glmnet.stats(m00, y = y, S = S, U = U, family = family)

    # compute the deviance components using the formula in the paper.
    Dab = mab$deviance
    Da0 = ma0$deviance
    D0b = m0b$deviance
    D00 = m00$deviance

    return(c(value = as.vector((Dab - D0b) / (Dab - D00)),
             r.squared.S = as.vector((Da0 - D00) / (Dab - D00)),
             r.squared.U = as.vector((D0b - D00) / (Dab - D00))))

  }#ELSE

}#FGRRM.SP.KOMIYAMA

# proportion of the variance of the fitted value explained by the sensitive
# attributes that is not explained by the original response, as per the
# definition of equal opportunity.
fgrrm.eo.komiyama = function(model, y, S, U, family = "gaussian") {

  coefs = model$coefficients
  yhat = glmnet.stats(model, y = y, S = S, U = U, family = family)$fitted

  if (family == "gaussian") {

    # fit an auxiliary model on yhat and compute the proportion of deviance
    # explained by S over the total explained deviance.
    a = anova(lm(yhat ~ S + y))
    eo = proportion(a["S", "Sum Sq"], sum(a[c("S", "y"), "Sum Sq"]))
    rS = proportion(a["S", "Sum Sq"], sum(a[, "Sum Sq"]))
    ry = proportion(a["y", "Sum Sq"], sum(a[, "Sum Sq"]))

  }#THEN
  else if (family == "cox") {

    y = -log(y[, "time"])
    a = anova(lm(yhat ~ S + y))
    eo = proportion(a["S", "Sum Sq"], sum(a[c("S", "y"), "Sum Sq"]))
    rS = proportion(a["S", "Sum Sq"], sum(a[, "Sum Sq"]))
    ry = proportion(a["y", "Sum Sq"], sum(a[, "Sum Sq"]))

  }#THEN
  else if (family == "multinomial") {

    yhat = droplevels(linpred2mclass(yhat, labels = levels(y)))
    keep = intersect(rownames(coefs)[rowSums(coefs) != 0], colnames(S))

    # unfortunately, variables with zero coefficients still explain some degree
    # of deviance because the link function makes things nonlinear: special-case
    # perfect fairness to get the function to return zero.
    if (length(keep) > 0) {

      S = S[, keep, drop = FALSE]
      dmat = design.matrix(data.frame(S, y), intercept = FALSE)
      full = glmnet(y = yhat, x = dmat, family = family, lambda = 0)
      null = glmnet(y = yhat, x = dmat, family = family, lambda = Inf)
      withS = glmnet(y = yhat, x = dmat, family = family, lambda = 0,
                penalty.factor = c(rep(Inf, ncol(S)), rep(0, nlevels(y) - 1)))
      withy = glmnet(y = yhat, x = dmat, family = family, lambda = 0,
                penalty.factor = c(rep(0, ncol(S)), rep(Inf, nlevels(y) - 1)))

      eo = proportion(deviance(null) - deviance(withy),
                      deviance(null) - deviance(full))
      rS = proportion(deviance(null) - deviance(withy),
                      deviance(null))
      ry = proportion(deviance(withy) - deviance(full),
                      deviance(null))

    }#THEN
    else {

      dmat = design.matrix(data.frame(y), intercept = FALSE)
      full = glmnet(y = yhat, x = dmat, family = family, lambda = 0)
      null = glmnet(y = yhat, x = dmat, family = family, lambda = Inf)

      eo = 0
      rS = 0
      ry = proportion(deviance(null) - deviance(full),
                      deviance(null))

    }#ELSE

  }#THEN
  else if (family %in% c("binomial", "poisson")) {

    if (family == "binomial")
      yhat = linpred2class(yhat, labels = levels(y))
    else if (family == "poisson")
      yhat = exp(yhat)

    keep = intersect(names(coefs)[coefs != 0], colnames(S))

    # unfortunately, variables with zero coefficients still explain some degree
    # of deviance because the link function makes things nonlinear: special-case
    # perfect fairness to get the function to return zero.
    if (length(keep) > 0) {

      S = S[, keep, drop = FALSE]
      a = anova(glm(yhat ~ S + y, family = family))

      # the NULL model does not have a "Deviance" entry, so use the "Residual
      # Deviance" entry instead.
      eo = proportion(a["S", "Deviance"], sum(a[c("S", "y"), "Deviance"]))
      rS = proportion(a["S", "Deviance"], a["NULL", "Resid. Dev"])
      ry = proportion(a["y", "Deviance"], a["NULL", "Resid. Dev"])

    }#THEN
    else {

      a = anova(glm(yhat ~ y, family = family))

      eo = 0
      rS = 0
      ry = proportion(a["y", "Deviance"], a["NULL", "Resid. Dev"])

    }#ELSE

  }#THEN

  return(c(value = eo, r.squared.S = rS, r.squared.y = ry))

}#FGRRM.EO.KOMIYAMA

# individual fairness, measuring the (normalized) difference in the response
# weighted by the distance in the part of the fitted values that depends on
# the sensitive attributes.
fgrrm.if.berk = function(model, y, S, U, family = "gaussian") {

  coefs = model$coefficients
  if (family == "cox")
    S = S[, colnames(S) != "(Intercept)", drop = FALSE]
  individual = c(S %*% coefs[colnames(S)])
  fFRRM = fOLS = 0

  # weights of the difference in response, based on the sensitive attributes.
  weights = function(individual)
              outer(individual, individual, function(a, b) (a - b)^2)

  if (family %in% c("gaussian", "binomial", "poisson")) {

    # if the response is categorical, all classes are equidistant.
    if (is.factor(y))
      delta = outer(y, y, function(a, b) (a != b))
    else
      delta = outer(y, y, function(a, b) abs(a - b))

    fFRRM = mean(delta * weights(individual))

    # the model without any fairness constraint provides the upper bound.
    unrestricted.model = glm(y ~ ., data = data.frame(U, S), family = family)
    individual = c(S %*% coef(unrestricted.model)[colnames(S)])

    fOLS = mean(delta * weights(individual))

  }#THEN
  else if (family == "cox") {

    delta = outer(y[, "time"], y[, "time"], function(a, b) abs(a - b)) *
            outer(y[, "status"], y[, "status"], function(a, b) abs(a - b))

    fFRRM = mean(delta * weights(individual))

    # the model without any fairness constraint provides the upper bound.
    unrestricted.model = fair.ridge(y = y, S = S, U = U, family = family,
      lambda = model$lambda, lr = 0)
    individual = c(S %*% unrestricted.model$coefficients[colnames(S)])

    fOLS = mean(delta * weights(individual))


  }#THEN
  else if (family == "multinomial") {

    # the response is always categorical.
    delta = outer(y, y, function(a, b) (a != b))

    # there is one set of coefficients for each level of the response variable:
    # handle them individually (as if they came from separate models) and pool
    # their contributions.
    individual = S %*% coefs[colnames(S), ]
    for (i in seq(ncol(individual)))
      fFRRM = fFRRM + mean(delta * weights(individual[, i]))

    # the model without any fairness constraint provides the upper bound.
    S = S[, colnames(S) != "(Intercept)", drop = FALSE]
    unrestricted.model = fair.ridge(y = y, S = S, U = U, family = family,
      lambda = 0, lr = 0)
    individual = S %*% unrestricted.model$coefficients[colnames(S), ]
    for (i in seq(ncol(individual)))
      fOLS = fOLS + mean(delta * weights(individual[, i]))

  }#THEN

  return(c(value = fFRRM / fOLS))

}#FGRRM.IF.BERK
