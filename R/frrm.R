# fair ridge regression.
frrm = function(response, predictors, sensitive, unfairness,
         definition = "sp-komiyama", lambda = 0, save.auxiliary = FALSE) {

  fitted = two.stage.regression(model = "frrm", family = NULL,
             response = response, predictors = predictors, sensitive = sensitive,
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
    r2 = function(model) fgrrm.sp.komiyama(model, y, S, U)["value"]
  else if (definition == "eo-komiyama")
    r2 = function(model) fgrrm.eo.komiyama(model, y, S, U)["value"]

  # shorthand for the function that fits the model.
  ridge = function(lambda, lr)
    split.ridge(y, S, U, lambda = lambda, lr = lr, family = "gaussian")

  build.return.value = function(model, lr) {

    coefs = structure(model, sensitive = names(model) %in% colnames(S))
    fitted.S = as.numeric(S %*% coefs[colnames(S)])
    fitted.U = as.numeric(U %*% coefs[colnames(U)])
    fitted = coefs["(Intercept)"] + fitted.S + fitted.U
    resid = y - fitted

    if (definition == "sp-komiyama") {

      constraint = fgrrm.sp.komiyama(model, y, S, U)
      other = as.list(constraint[c("r.squared.S", "r.squared.U")])

    }#THEN
    else if (definition == "eo-komiyama") {

      constraint = fgrrm.eo.komiyama(model, y, S, U)
      other = as.list(constraint[c("r.squared.S", "r.squared.y")])

    }#THEN

    return(list(main = list(
                  coefficients = coefs,
                  residuals = resid,
                  fitted.values = fitted,
                  family = "gaussian",
                  deviance = sum(resid^2),
                  loglik = linear.model.loglikelihood(resid, length(coefs)),
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

    model = ridge(lambda = lambda, lr = lr)
    return(abs(r2(model) - unfairness))

  }, interval = c(0, 10^m))

  fit = ridge(lambda = lambda, lr = value$minimum)

  return(build.return.value(fit, lr = value$minimum))

}#FRRM.GLMNET

