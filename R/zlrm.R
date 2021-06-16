
# fair logistic regression from Zafar et al. (2019).
zlrm = function(response, predictors, sensitive, unfairness) {

  build.return.value  = function(model, coefs) {

    # if glm() fits the model just from the offsets, without estimating the
    # regression coefficients themselves, it does not contain their values: pass
    # them manually.
    if (missing(coefs))
      coefs = structure(coef(model), names = colnames(predictors))
    else
      coefs = structure(coefs, names = colnames(predictors))

    return(structure(list(
      auxiliary = NULL,
      main = list(
        call = call,
        coefficients = coefs,
        residuals = as.vector(residuals(model)),
        fitted.values = as.vector(fitted(model)),
        deviance = model$deviance
      ),
      fairness = list(
        definition = "sp-disparate-impact",
        value = abs(cor(as.vector(fitted(model)), sensitive))[1, ],
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
  response = check.response(response, type = "binary")
  predictors = check.data(predictors, nobs = length(response), varletter = "X")
  sensitive = check.data(sensitive, nobs = length(response), varletter = "S")
  # check the fairness constraint.
  check.fairness.level(unfairness)

  # save some information on the data, to be used e.g. in predict().
  response.info = get.data.info(data.frame(response = response))
  predictors.info = get.data.info(predictors)
  sensitive.info = get.data.info(sensitive)

  # encode factors with constrasts and add the intercept.
  predictors = design.matrix(predictors, intercept = TRUE)
  sensitive = design.matrix(sensitive, intercept = FALSE)

  # fit an unconstrained logistic regression first, to check whether the
  # constraint is active.
  unconstrained.model = glm(response ~ predictors - 1, family = "binomial")

  if (anyNA(coef(unconstrained.model))) {

    NAcoefs = names(which(is.na(coef(unconstrained.model))))
    stop("the coefficient(s) ", q(NAcoefs),
         " are NA in the unconstrained model.")

  }#THEN

  unconstrained.fitted = predictors %*% coef(unconstrained.model)
  correlations = cor(sensitive, unconstrained.fitted)

  if (all(abs(correlations) < unfairness))
    return(build.return.value(unconstrained.model))

  # perform the constrained optimization.
  coefs = constrained.logistic(response = response, predictors = predictors,
             sensitive = sensitive, unfairness = unfairness,
             max.covariance = max(abs(cov(sensitive, unconstrained.fitted))))

  # sensitive attributes do not have coefficients in this model, they only
  # appear in the constraints.
  attr(coefs, "sensitive") = rep(FALSE, length(coefs))
  # fit the logistic regression with the given coefficients, computing all the
  # quantities we are going to return in the process.
  final.model = glm(response ~ - 1, offset = predictors %*% coefs,
                    family = "binomial")

  return(build.return.value(final.model, coefs))

}#ZLRM

# CVXR wrapper to make the code simpler.
constrained.logistic = function(response, predictors, sensitive, unfairness,
  max.covariance) {

  # transform the response to get coefficient signs that are coherent with those
  # from glm().
  yy  = 2 - as.numeric(response)
  n = length(response)

  # center sensitive attributes, to avoid doing that in the optimizer.
  sensitive = scale(sensitive, center = TRUE, scale = FALSE)

  # estimate the regression coefficients subject to a bound on the covariance,
  # in a literal implementation of Zafar et al. (2019) that follows along the
  # lines of their CVXPy code.
  optimization = function(cov.bound) {

    # define the variables of the optimization problem.
    coefs = Variable(rows = ncol(predictors), cols = 1)
    # define the objective function, the negated loglikelihood of logistic
    # regression.
    obj = -sum(logistic(-predictors[yy == 0, ] %*% coefs)) -
           sum(logistic(predictors[yy == 1, ] %*% coefs))
    # define the constraints on the covariances between the sensitive attributes
    # and the fitted values.
    constraints =
      list(t(sensitive) %*% (predictors %*% coefs) / (n - 1) <= cov.bound,
           t(sensitive) %*% (predictors %*% coefs) / (n - 1) >= -cov.bound)
    # formulate the constrained optimization problem.
    prob = Problem(Maximize(obj), constraints = constraints)
    # solve it.
    result = solve(prob)

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

  # find the bound on the covariances that satisfies the bound on the
  # correlations (that is, the unfairness) using the maximum covariance from
  # the unconstrained model (+ some slack) to limit the search interval.
  cor.bound = optimize(f = constraint.mapping,
                interval = c(0, max.covariance * 1.1))$minimum

  # estimate and rename the regression coefficients,
  coefs = structure(as.vector(optimization(cor.bound)),
            names = colnames(predictors))

  return(coefs)

}#CONSTRAINED.LOGISTIC

