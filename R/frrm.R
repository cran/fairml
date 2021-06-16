# fair ridge regression.
frrm = function(response, predictors, sensitive, unfairness,
         definition = "sp-komiyama", lambda = 0, save.auxiliary = FALSE) {

  fitted = two.stage.regression(model = "frrm", response = response,
             predictors = predictors, sensitive = sensitive,
             unfairness = unfairness, definition = definition,
             covfun = NULL, lambda = lambda, save.auxiliary = save.auxiliary)

  # save the function call for the print() method.
  fitted$main$call = match.call()

  return(fitted)

}#FRRM

frrm.glmnet = function(y, S, U, unfairness, definition, lambda = 0) {

  # choose the right function for the definition of fairness (and avoid having
  # to pass it the data explicitly, for brevity).
  if (definition == "sp-komiyama")
    r2 = function(model) frrm.komiyama(model, y, S, U)
  else if (definition == "eo-komiyama")
    r2 = function(model) frrm.eo(model, y, S, U)["value"]

  build.return.value = function(model, lr) {

    coefs = structure(model, sensitive = names(model) %in% colnames(S))
    fitted.S = as.numeric(S %*% coefs[colnames(S)])
    fitted.U = as.numeric(U %*% coefs[colnames(U)])
    fitted = coefs["(Intercept)"] + fitted.S + fitted.U
    resid = y - fitted
    r2.S = var(fitted.S) / var(y)
    r2.U = var(fitted.U) / var(y)

    if (definition == "sp-komiyama")
      other = list(r.squared.S = r2.S, r.squared.U = r2.U)
    else if (definition == "eo-komiyama")
      other = as.list(frrm.eo(model, y, S, U)[c("r.squared.S", "r.squared.y")])

    return(list(main = list(
                  coefficients = coefs,
                  residuals = resid,
                  fitted.values = fitted,
                  r.squared.S = r2.S,
                  r.squared.U = r2.U,
                  arguments = list(
                    lr = lr,
                    lambda = lambda,
                    definition = definition
                )),
           fairness = list(
                  definition = definition,
                  value = r2(model),
                  bound = unfairness,
                  other = other
                )))

  }#BUILD.RETURN.VALUE

  # first check whether any unfairness is required at all: if not, special-case
  # the model and drop all sensitive attributes.
  if (unfairness <= sqrt(.Machine$double.eps)) {

    completely.fair = split.ridge(y, S, U, lambda = lambda, lr = Inf)
    return(build.return.value(completely.fair, lr = Inf))

  }#THEN

  # then check whether the proportion of variance explained by S is already
  # smaller than the required unfairness level; return an OLS regression for
  # lambda(r) = 0 if that is the case.
  ols = split.ridge(y, S, U, lambda = lambda, lr = 0)

  if (r2(ols) <= unfairness)
    return(build.return.value(ols, lr = 0))

  # establish an upper bound for lambda(r), to pass to optimize().
  m = 1

  while (r2(split.ridge(y, S, U, lambda = lambda, lr = 10^m)) >= unfairness)
    m = m + 1

  # find the lambda(r) that gives the required R^2 == unfairness.
  value = optimize(f = function(lr) {

    ridge = split.ridge(y, S, U, lambda = lambda, lr = lr)
    return(abs(r2(ridge) - unfairness))

  }, interval = c(0, 10^m))

  fit = split.ridge(y, S, U, lambda = lambda, lr = value$minimum)

  return(build.return.value(fit, lr = value$minimum))

}#FRRM.GLMNET

# glmnet wrapper to make code simpler.
split.ridge = function(y, S, U, lambda, lr) {

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

    pf = c(rep(Inf, ncol(S)), rep(lambda, ncol(U)))
    lr = 1

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

  rr = glmnet(y = y, x = cbind(S, U), family = "gaussian",
           alpha = 0, lambda = lr, penalty.factor = pf)

  coefficients = coef(rr)
  return(structure(as.numeric(coefficients), names = rownames(coefficients)))

}#SPLIT.RIDGE

# proportion of variance explained by the sensitive attributes, from the
# constraint in Komiyama et al. (2018) and the definition of statistical parity.
frrm.komiyama = function(model, y, S, U) {

  var(S %*% model[colnames(S)]) /
     var(cbind(S, U) %*% model[names(model) != "(Intercept)"])

}#FRRM.KOMIYAMA

# proportion of the variance of the fitted value explained by the sensitive
# attributes that is not explained by the original response, as per the
# definition of equal opportunity.
frrm.eo = function(model, y, S, U) {

  yhat = model["(Intercept)"] +
           cbind(S, U) %*% model[names(model) != "(Intercept)"]
  a = anova(lm(yhat ~ S + y))
  eo = a["S", "Sum Sq"] / sum(a[c("S", "y"), "Sum Sq"])

  return(c(value = eo, r.squared.S = a["S", "Sum Sq"] / sum(a[, "Sum Sq"]),
           r.squared.y = a["y", "Sum Sq"] / sum(a[, "Sum Sq"])))

}#FRRM.EO

