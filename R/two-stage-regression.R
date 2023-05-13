# compute the decorrelated predictors and set up a fair model.
two.stage.regression = function(model, family, response, predictors, sensitive,
    unfairness, definition, covfun, lambda, save.auxiliary = TRUE) {

  # check the model to be fitted.
  check.label(model, fair.models, "model")
  family = check.family(family)
  # check method-specific arguments (so that we check the family before using
  # it to check other stuff).
  if (model == "nclm")
    covfun = check.covariance.function(covfun)
  else if (model %in% c("frrm", "fgrrm"))
    check.fairness.constraint(definition, model = model, family = family)

  # check the arguments common to all methods.
  response = check.response(response, model = model, family = family)
  predictors =
    check.data(predictors, nobs = sample.size(response), varletter = "X")
  sensitive =
    check.data(sensitive, nobs = sample.size(response), varletter = "S")
  check.fairness.level(unfairness)
  check.logical(save.auxiliary)
  lambda = check.ridge.penalty(lambda)

  # check that there are no duplicate variable names in the original data.
  duplicate.names = intersect(colnames(predictors), colnames(sensitive))
  if (length(duplicate.names) != 0)
    stop("duplicate variable names in the data: ", q(duplicate.names), ".")

  # save some information on the data, to be used e.g. in predict().
  response.info = get.data.info(data.frame(response = response))
  predictors.info = get.data.info(predictors)
  sensitive.info = get.data.info(sensitive)

  # encode factors with constrasts, but:
  # 1) remove the intercept from the predictors (it will be added later in the
  #      main model);
  # 2) remove the intercept from the sensitive attributes (it will be added
  #      later in the auxiliary model).
  predictors = design.matrix(predictors, intercept = FALSE)
  sensitive = design.matrix(sensitive, intercept = FALSE)
  # check that there are no duplicate names in the design matrices.
  duplicate.names = intersect(colnames(predictors), colnames(sensitive))
  if (length(duplicate.names) != 0)
    stop("duplicate variable names in the design matrices: ",
         q(duplicate.names), ".")
  # regress the predictors against the sensitive attributes to contruct U, the
  # matrix of non-sensitive variables.
  auxiliary.model = lm(predictors ~ ., data = data.frame(sensitive))
  U = predictors - fitted(auxiliary.model)
  # remove the intercept from the sensitive attributes, it is no longer needed.
  sensitive = sensitive[, colnames(sensitive) != "(Intercept)", drop = FALSE]

  if (model == "nclm") {

    if (unfairness == 0) {

      fit = nclm.zero.sensitive(y = response, S = sensitive, U = U,
              covfun = covfun, lambda = lambda)

    }#THEN
    else {

      fit = nclm.optiSolve(y = response, S = sensitive, U = U,
              epsilon = unfairness, covfun = covfun, lambda = lambda)

    }#ELSE

  }#THEN
  else if (model == "fgrrm") {

    fit = fgrrm.glmnet(y = response, S = sensitive, U = U, family = family,
            unfairness = unfairness, definition = definition, lambda = lambda)

  }#THEN

  # collect all the information about the model...
  auxiliary.coefs = coef(auxiliary.model)

  if (ncol(predictors) == 1) {

    auxiliary.coefs =
      matrix(auxiliary.coefs, ncol = 1,
             dimnames = list(names(auxiliary.coefs), colnames(predictors)))
  }#THEN

  auxiliary.info = list(call = auxiliary.model$call,
                        coefficients = auxiliary.coefs)

  # ... and optionally on the data that will be fed to the main model.
  if (save.auxiliary) {

    auxiliary.info = c(auxiliary.info, list(
                       fitted.values = fitted(auxiliary.model), residuals = U))

  }#THEN

  retval = list(
    auxiliary = auxiliary.info,
    main = c(call = NA, fit$main),
    fairness = fit$fairness,
    data = list(response = response.info, predictors = predictors.info,
                sensitive = sensitive.info)
  )

  return(structure(retval, class = c(model, "fair.model")))

}#TWO.STAGE.REGRESSION

