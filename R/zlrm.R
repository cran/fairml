
# fair logistic regression from Zafar et al. (2019).
zlrm = function(response, predictors, sensitive, unfairness) {

  model = zlrm.shared(response = response, predictors = predictors,
            sensitive = sensitive, unfairness = unfairness,
            type = "correlation")

  # save the function call for the print() method.
  model$main$call = match.call()

  return(model)

}#ZLRM

# fair logistic regression from Zafar et al. (2019) with the original
# constraint.
zlrm.orig =  function(response, predictors, sensitive, max.abs.cov) {

  model = zlrm.shared(response = response, predictors = predictors,
            sensitive = sensitive, unfairness = max.abs.cov,
            type = "covariance")

  # save the function call for the print() method.
  model$main$call = match.call()

  return(model)

}#ZLRM.ORIG

zlrm.shared = function(response, predictors, sensitive, unfairness, type) {

  # the optimization is implemented using CVXR.
  check.and.load.package("CVXR")

  build.return.value  = function(model, coefs) {

    # if glm() fits the model just from the offsets, without estimating the
    # regression coefficients themselves, it does not contain their values: pass
    # them manually.
    if (missing(coefs))
      coefs = structure(coef(model), names = colnames(predictors))
    else
      coefs = structure(coefs, names = colnames(predictors))

    # the model contains the classification probabilities, not the linear
    # predictors.
    fitted = predictors %*% coefs

    return(structure(list(
      auxiliary = NULL,
      main = list(
        call = call,
        coefficients = coefs,
        residuals = as.vector(residuals(model)),
        fitted.values = as.vector(fitted),
        y = response,
        family = "binomial",
        deviance = model$deviance,
        loglik = logLik(model)
      ),
      fairness = list(
        definition = "sp-zafar-disparate-impact",
        value = abs(safe.cor(as.vector(fitted(model)), sensitive))[1, ],
        bound = unfairness
      ),
      data = list(response = response.info,
                  predictors = predictors.info,
                  sensitive = sensitive.info)
    ), class = c("zlrm", "fair.model")))

  }#BUILD.RETURN.VALUE

  # save the function call for the print() method.
  call = match.call()

  # check the variables.
  response = check.response(response, model = "zlrm")
  predictors =
    check.data(predictors, nobs = sample.size(response), varletter = "X")
  sensitive =
    check.data(sensitive, nobs = sample.size(response), varletter = "S")
  # check the fairness constraint.
  if (type == "correlation")
    check.fairness.level(unfairness)
  else if (type == "covariance")
    check.absolute.covariance(unfairness)

  # save some information on the data, to be used e.g. in predict().
  response.info = get.data.info(data.frame(response = response))
  predictors.info = get.data.info(predictors)
  sensitive.info = get.data.info(sensitive)

  # encode factors with constrasts and add the intercept.
  predictors = design.matrix(predictors, intercept = TRUE)
  sensitive = design.matrix(sensitive, intercept = FALSE)

  # first check whether any unfairness is allowed at all: if not, special-case
  # the model and drop all the predictors that are correlated with at least one
  # sensitive attribute.
  if (unfairness <= sqrt(.Machine$double.eps)) {

    # find out which predictors we can use (the intercept is always allowed).
    overall.cov = rowSums(cov(predictors, sensitive))
    allowed.predictors = predictors[, overall.cov == 0, drop = FALSE]
    # fit a completely fair model and return it.
    completely.fair.model =
      glm(response ~ allowed.predictors - 1, family = "binomial")
    # build a vector of coefficients that includes zero coefficients for the
    # predictors we have dropped.
    coefs = structure(rep(0, length(overall.cov)), names = names(overall.cov))
    coefs[colnames(allowed.predictors)] = coef(completely.fair.model)

    return(build.return.value(completely.fair.model, coefs))

  }#THEN

  # then fit an unconstrained logistic regression, to check whether the
  # constraints are active.
  unconstrained.model = glm(response ~ predictors - 1, family = "binomial")

  if (anyNA(coef(unconstrained.model))) {

    NAcoefs = names(which(is.na(coef(unconstrained.model))))
    stop("the coefficient(s) ", q(NAcoefs),
         " are NA in the unconstrained model.")

  }#THEN

  unconstrained.fitted = predictors %*% coef(unconstrained.model)

  if (type == "correlation") {

    correlations = cor(sensitive, unconstrained.fitted)
    if (all(abs(correlations) < unfairness))
      return(build.return.value(unconstrained.model))

  }#THEN
  else if (type == "covariance") {

    covariances = cov(sensitive, unconstrained.fitted)
    if (all(abs(covariances) < unfairness))
      return(build.return.value(unconstrained.model))

  }#THEN

  # perform the constrained optimization.
  coefs = constrained.logistic(response = response, predictors = predictors,
             sensitive = sensitive, unfairness = unfairness, type = type,
             max.covariance = max(abs(cov(sensitive, unconstrained.fitted))))

  # sensitive attributes do not have coefficients in this model, they only
  # appear in the constraints.
  attr(coefs, "sensitive") = rep(FALSE, length(coefs))
  # fit the logistic regression with the given coefficients, computing all the
  # quantities we are going to return in the process.
  final.model = glm(response ~ - 1, offset = predictors %*% coefs,
                    family = "binomial")

  return(build.return.value(final.model, coefs))

}#ZLRM.SHARED

# CVXR wrapper to make the code simpler.
constrained.logistic = function(response, predictors, sensitive, unfairness,
  type, max.covariance) {

  # transform the response to get coefficient signs that are coherent with those
  # from glm().
  yy  = 2 - as.numeric(response)
  n = length(response)

  # center sensitive attributes, to avoid doing that in the optimizer.
  sensitive = scale(sensitive, center = TRUE, scale = FALSE)
  # precompute the cross-products.
  xts = t(sensitive) %*% predictors

  # estimate the regression coefficients subject to a bound on the covariance,
  # in a literal implementation of Zafar et al. (2019) that follows along the
  # lines of their CVXPy code.
  optimization = function(cov.bound) {

    # define the variables of the optimization problem.
    coefs = CVXR::Variable(rows = ncol(predictors), cols = 1)
    # define the objective function, the negated log-likelihood of logistic
    # regression.
    obj = -sum(CVXR::logistic(-predictors[yy == 0, ] %*% coefs)) -
           sum(CVXR::logistic(predictors[yy == 1, ] %*% coefs))
    # define the constraints on the covariances between the sensitive attributes
    # and the fitted values.
    constraints = list(abs(xts %*% coefs) / (n - 1) <= cov.bound)
    # formulate the constrained optimization problem.
    prob = CVXR::Problem(CVXR::Maximize(obj), constraints = constraints)
    # solve it.
    result = CVXR::solve(prob, ignore_dcp = TRUE)

    if (result$status %in% c("optimal", "optimal_inaccurate"))
      return(result$getValue(coefs))

    # try #2: if the default solver fails, try again with a different one (which
    # is # much slower but seems to fail less often).
    result = CVXR::solve(prob, solver = "SCS", ignore_dcp = TRUE)

    if (result$status %in% c("optimal", "optimal_inaccurate"))
      return(result$getValue(coefs))

    # try #3: add some slack to the constraint to get a slightly-invalid
    # solution that still looks like a valid one.
    constraints = list(abs(xts %*% coefs) / (n - 1) <= cov.bound * 1.01)
    prob = CVXR::Problem(CVXR::Minimize(obj), constraints = constraints)
    result = CVXR::solve(prob, ignore_dcp = TRUE)

    if (!(result$status %in% c("optimal", "optimal_inaccurate")))
      stop("CVXR failed to find a solution (", result$status, ").")

    return(result$getValue(coefs))

  }#OPTIMIZATION

  # remap the constraint on the covariances to the constraint on the marginal
  # correlations used in fairml.
  constraint.mapping = function(cov.bound) {

    coefs = optimization(cov.bound)

    # compute the correlations between the sensitive attributes and the
    # estimated regression coefficients, dropping the sign.
    cc = abs(cor(sensitive, predictors %*% coefs))

    return(abs(max(cc) - unfairness))

  }#CONSTRAINT.MAPPING

  if (type == "correlation") {

    # find the bound on the covariances that satisfies the bound on the
    # correlations (that is, the unfairness) using the maximum covariance from
    # the unconstrained model (+ some slack) to limit the search interval.
    bound = optimize(f = constraint.mapping,
                  interval = c(0, max.covariance * 1.1))$minimum

  }#THEN
  else if (type == "covariance") {

    # unfairness is already expressed with a bound on the covariances.
    bound = unfairness

  }#THEN

  # estimate and rename the regression coefficients.
  coefs = structure(as.vector(optimization(bound)),
            names = colnames(predictors))

  return(coefs)

}#CONSTRAINED.LOGISTIC

