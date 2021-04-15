# fair ridge regression.
frrm = function(response, predictors, sensitive, unfairness) {

  fitted = two.stage.regression(model = "frrm", response = response,
             predictors = predictors, sensitive = sensitive,
             unfairness = unfairness, covfun = NULL, lambda = NULL)

  # save the function call for the print() method.
  fitted$main$call = match.call()

  return(fitted)

}#FRRM

frrm.glmnet = function(y, S, U, unfairness) {

  # glment wrappers to make code simpler.
  fit.the.ridge.regression = function(lambda) {

    glmnet(y = y, x = cbind(S, U), family = "gaussian",
           alpha = 0, lambda = lambda,
           penalty.factor = c(rep(1, ncol(S)), rep(0, ncol(U))))

  }#FIT.THE.RIDGE.REGRESSION

  glmnet.r2 = function(model) {

    var(S %*% model$beta[1:ncol(S)]) /
       var(cbind(S, U) %*% model$beta[1:nrow(model$beta)] )

  }#GLMNET.R2

  build.return.value = function(model) {

    coefs = coef(model)
    coefs = structure(as.numeric(coefs), names = rownames(coefs),
              sensitive = rownames(coefs) %in% colnames(S))
    fitted.S = as.numeric(S %*% coefs[colnames(S)])
    fitted.U = as.numeric(U %*% coefs[colnames(U)])
    fitted = coefs["(Intercept)"] + fitted.S + fitted.U
    resid = y - fitted
    r2.S = var(fitted.S) / var(y)
    r2.U = var(fitted.U) / var(y)

    return(list(coefficients = coefs, residuals = resid, fitted.values = fitted,
             r.squared.S = r2.S, r.squared.U = r2.U))

  }#BUILD.RETURN.VALUE

  # first check whether the proportion of variance explained by S is already
  # smaller than the required unfairness level; return an OLS regression for
  # lambda = 0 if that is the case.
  ols = fit.the.ridge.regression(lambda = 0)

  if (glmnet.r2(ols) <= unfairness)
    return(build.return.value(ols))

  # find the lambda that gives the required R^2 == unfairness.
  value = optimize(f = function(lambda) {

    ridge = fit.the.ridge.regression(lambda = lambda)
    return(abs(glmnet.r2(ridge) - unfairness))

  }, interval = c(0, 1000))

  fit = fit.the.ridge.regression(lambda = value$minimum)

  return(build.return.value(fit))

}#FRRM.GLMNET

