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
  if (definition == "sp-komiyama")
    r2 = function(model) fgrrm.sp.komiyama(model, y, S, U, family)["value"]
  else if (definition == "eo-komiyama")
    r2 = function(model) fgrrm.eo.komiyama(model, y, S, U, family)["value"]

  # shorthand for the function that fits the model.
  ridge = function(lambda, lr)
            split.ridge(y, S, U, lambda = lambda, lr = lr, family = family)

  build.return.value = function(model, lr) {

    # refit the model with glm() to compute all the quantities that make up the
    # return value.
    final.model = glm(y ~ - 1, family = family,
                    offset = model["(Intercept)"] +
                             cbind(S, U) %*% model[c(colnames(S), colnames(U))])

    coefs = structure(model, sensitive = names(model) %in% colnames(S))

    if (definition == "sp-komiyama") {

      constraint = fgrrm.sp.komiyama(model, y, S, U, family)
      other = as.list(constraint[c("r.squared.S", "r.squared.U")])

    }#THEN
    else if (definition == "eo-komiyama") {

      constraint = fgrrm.eo.komiyama(model, y, S, U, family)
      other = as.list(constraint[c("r.squared.S", "r.squared.y")])

    }#THEN

    return(list(main = list(
                  coefficients = coefs,
                  residuals = as.vector(residuals(final.model)),
                  fitted.values = as.vector(fitted(final.model)),
                  family = family,
                  deviance = deviance(final.model),
                  loglik = logLik(final.model),
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

  while (r2(ridge(lambda = lambda, lr = 10^m)) >= unfairness)
    m = m + 1

  # find the lambda(r) that gives the required R^2 == unfairness.
  value = optimize(f = function(lr) {

    ridge = ridge(lambda = lambda, lr = lr)
    return(abs(r2(ridge) - unfairness))

  }, interval = c(0, 10^m))

  fit = ridge(lambda = lambda, lr = value$minimum)

  return(build.return.value(fit, lr = value$minimum))

}#FGRRM.GLMNET

# glmnet wrapper to make code simpler.
split.ridge = function(y, S, U, family, lambda, lr) {

  # we want a vector of ridge penalties with elements equal to lr (for the
  # sensitive attributes) and to lambda (for the predictors) which in glmnet()
  # passed as the lambda * penalty.factors arguments.
  if ((lr == 0) && (lambda == 0)) {

    # the penalty factors cannot be all zeros: set lambda to zero instead, and
    # use the default penalty factors (the values does not really matter).
    pf = rep(1, ncol(S) + ncol(U))
    lr = 0

  }#THEN
  else if (is.infinite(lr)) {

    # infinte weights just remove the variables from the model, which is why ...
    pf = c(rep(Inf, ncol(S)), rep(lambda, ncol(U)))
    # ... the normalization of lr and pf uses ncol(U) instead of length(pf) like
    # the general case below.
    scaling = ifelse(lambda == 0, 1, ncol(U) / (lambda * ncol(U)))
    lr = 1 / scaling
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
    lr = 1 / scaling
    pf = pf * scaling

  }#ELSE

  rr = glmnet(y = y, x = cbind(S, U), family = family,
           alpha = 0, lambda = lr, penalty.factor = pf)

  # glmnet can misleadingly return an empty model (instead of an error) when it
  # fails to converge for the only value of lambda it has, make all errors
  # fatal.
  if (rr$jerr != 0)
    stop("glmnet() failed to estimate the model for lr = ", lr, ".")

  coefficients = coef(rr)
  return(structure(as.numeric(coef(rr)), names = rownames(coefficients)))

}#SPLIT.RIDGE

# proportion of deviance explained by the sensitive attributes, from the
# constraint in Komiyama et al. (2018) and the definition of statistical parity.
fgrrm.sp.komiyama = function(model, y, S, U, family = "gaussian") {

  if (family == "gaussian") {

    S = S[, colnames(S) != "(Intercept)", drop = FALSE]
    U = U[, colnames(U) != "(Intercept)", drop = FALSE]
    explained.S = var(S %*% model[colnames(S)])
    explained.U = var(U %*% model[colnames(U)])
    explained.all = var(cbind(S, U) %*% model[names(model) != "(Intercept)"])

    return(c(value = explained.S / explained.all,
             r.squared.S = explained.S / var(y),
             r.squared.U = explained.U / var(y)))

  }#THEN
  else {

    intercept = model["(Intercept)"]
    fitted.S = S %*% model[colnames(S)]
    fitted = intercept + fitted.S + U %*% model[colnames(U)]
    residuals = residuals(glm(y ~ -1, offset = fitted, family = family))

    # this is the extension of the aVa / (aVa + bVb) definition from Komiyama et
    # al. (2018): D(a, b) is the equivalent of var(fitted.S + fitted.U), and
    # D(0, b) is the equivalent of var(fitted.S).
    # note that if the constraint is active, computing deviance components in the
    # usual way (by difference from the residual deviance) would be wrong because
    # deviance components do not add up as expected.
    Dab = deviance(glm(y ~ -1, offset = intercept + residuals, family = family))
    D0b = deviance(glm(y ~ -1, offset = intercept + fitted.S + residuals,
                     family = family))
    D00 = deviance(glm(y ~ 1, family = family))

    return(c(value = as.vector((Dab - D0b) / Dab),
             r.squared.S = as.vector((Dab - D0b) / D00),
             r.squared.U = as.vector(D0b / D00)))

  }#ELSE

}#FGRRM.SP.KOMIYAMA

# proportion of the variance of the fitted value explained by the sensitive
# attributes that is not explained by the original response, as per the
# definition of equal opportunity.
fgrrm.eo.komiyama = function(model, y, S, U, family = "gaussian") {

  if (family == "gaussian") {

    S = S[, colnames(S) != "(Intercept)", drop = FALSE]
    U = U[, colnames(U) != "(Intercept)", drop = FALSE]
    yhat = model["(Intercept)"] +
             cbind(S, U) %*% model[names(model) != "(Intercept)"]
    a = anova(lm(yhat ~ S + y))
    eo = a["S", "Sum Sq"] / sum(a[c("S", "y"), "Sum Sq"])

    return(c(value = eo,
             r.squared.S = a["S", "Sum Sq"] / sum(a[, "Sum Sq"]),
             r.squared.y = a["y", "Sum Sq"] / sum(a[, "Sum Sq"])))

  }#THEN
  else {

    # construct the linear part of the response, and the response on the natural
    # scale for the family.
    yhat = model["(Intercept)"] +
             cbind(S, U) %*% model[names(model) != "(Intercept)"]
    yhat = fitted(glm(y ~ -1, offset = yhat, family = family))
    if (family == "binomial")
      yhat = prob2class(yhat, labels = levels(y))

    # fit an auxiliary model on yhat and compute the proportion of deviance
    # explained by S over the total explained deviance.
    a = anova(glm(yhat ~ S + y, family = family))
    eo = a["S", "Deviance"] / sum(a[c("S", "y"), "Deviance"])

    # two deviance components for S and y are scaled by the null deviance, in a
    # direct extension of what frrm.eo() does.
    return(c(value = eo,
             r.squared.S = a["S", "Deviance"] / a["NULL", "Resid. Dev"],
             r.squared.y = a["y", "Deviance"] / a["NULL", "Resid. Dev"]))

  }#ELSE

}#FGRRM.EO.KOMIYAMA

