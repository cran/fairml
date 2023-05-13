
# compute all the bits and pieces that are missing from a glmnet object.
glmnet.stats = function(model, y, S, U, family = "gaussian", lambda, lr) {

  if (inherits(model, "glmnet"))
    update = FALSE
  else {

    update = TRUE
    lambda = model$lambda
    lr = model$lr

  }#ELSE

  # compute all quantities from just the coefficients and the data, to allow for
  # model updates, for instance, after zeroing some of the coefficients.
  coefs = coef(model)

  if (family == "gaussian") {

    if (!update)
      coefs = structure(as.numeric(coefs), names = rownames(coefs))

    fitted = coefs["(Intercept)"] +
               cbind(S, U) %*% coefs[c(colnames(S), colnames(U))]

    fitted = as.numeric(fitted)
    resid = y - fitted
    loglik = lm.loglikelihood(resid)
    deviance = sum(resid^2)

  }#THEN
  else if (family == "binomial") {

    if (!update)
      coefs = structure(as.numeric(coefs), names = rownames(coefs))

    if ("(Intercept)" %in% colnames(S))
      fitted = cbind(S, U) %*% coefs[c(colnames(S), colnames(U))]
    else
      fitted = coefs["(Intercept)"] +
                 cbind(S, U) %*% coefs[c(colnames(S), colnames(U))]

    fitted = as.numeric(fitted)
    probs = linpred2prob(fitted)
    yy = (as.numeric(y) - 1)
    resid = sign(yy - probs) *
              sqrt(-2 * (yy * log(probs) + (1 - yy) * log(1 - probs)))
    loglik = sum(dbinom(yy, size = 1, prob = probs, log = TRUE))
    deviance = sum(resid^2)

  }#THEN
  else if (family == "poisson") {

    if (!update)
      coefs = structure(as.numeric(coefs), names = rownames(coefs))

    fitted = as.numeric(coefs["(Intercept)"] +
               cbind(S, U) %*% coefs[c(colnames(S), colnames(U))])

    mu = exp(fitted)
    resid = 2 * (y * log(ifelse(y == 0, 1, y / mu)) - (y - mu))
    resid = sqrt(resid) * ifelse(y > mu, 1, -1)
    loglik = sum(dpois(y, mu, log = TRUE))
    deviance = sum(resid^2)

  }#THEN
  else if (family == "cox") {

    # Cox regression models have no intercept.
    S = S[, colnames(S) != "(Intercept)", drop = FALSE]
    U = U[, colnames(U) != "(Intercept)", drop = FALSE]

    if (update)
      coefs = structure(as.numeric(coefs), names = names(coefs))
    else
      coefs = structure(as.numeric(coefs), names = rownames(coefs))

    fitted = as.vector(cbind(S, U) %*% coefs[c(colnames(S), colnames(U))])
    yy = survival::Surv(time = y[, "time"], event = y[, "status"])
    ph.model =
      try(survival::coxph(yy ~ cbind(S, U),
            init = coefs[c(colnames(S), colnames(U))],
            control = survival::coxph.control(iter.max = 0)), silent = TRUE)

    if (!is(ph.model, "try-error")) {

      resid = residuals(ph.model, type = "deviance")
      loglik = logLik(ph.model)

    }#THEN
    else {

      resid = rep(NA_real_, sample.size(y))
      loglik = NA_real_

    }#ELSE

    # despite the fact that residuals are deviance residuals, they do not sum up
    # to the deviance itself; compute it from the log-likelihood instead.
    deviance = as.numeric(-2 * loglik)

  }#THEN
  else if (family == "multinomial") {

    if (!update) {

      coefs = as.matrix(do.call(cbind, coefs))
      rownames(coefs)[1] = "(Intercept)"
      colnames(coefs) = names(coef(model))

    }#THEN

    if ("(Intercept)" %in% colnames(S))
      fitted = cbind(S, U) %*% coefs
    else
      fitted = cbind(1, S, U) %*% coefs

    mprobs = linpred2mprob(fitted)

    resid = c(-1, 1)[(y != levels(y)[1]) + 1L] *
      sqrt(-2 * rowSums(class.ind(y) * log(mprobs)))
    loglik = sum(sapply(seq_along(y), function(i) log(mprobs[i, y[i]])))
    deviance = sum(resid^2)

  }#THEN

  return(list(coefficients = coefs, deviance = deviance, loglik = loglik,
              fitted = fitted, residuals = resid, lambda = lambda, lr = lr))

}#GLMNET.STATS

# set coefficients to zero in an object returned by glmnet.stats().
zero.coefficients = function(model, names) {

  if (is.matrix(model$coefficients))
    model$coefficients[names, ] = 0
  else
    model$coefficients[names] = 0

  return(model)

}#ZERO.COEFFICIENTS
