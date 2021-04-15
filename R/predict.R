
# predict new observations for a generic fair model.
predict.fair.model = function(object, ...) {

  if (!is(object, "fair.model"))
    stop("'object' must be a 'fair.model' object.")

  check.unused.args(list(...), character(0))

  stop("predict.fair.model() not implemented.")

}#PREDICT.FAIR.MODELS

predict.two.stages = function(object, new.predictors, new.sensitive) {

  # check the predictors and the sensitive attributes.
  new.predictors = check.data(new.predictors, min.nobs = 1, varletter = "X")
  new.sensitive = check.data(new.sensitive, nobs = nrow(new.predictors),
                             min.nobs = 1, varletter = "S")
  # check that both have the same structure as the data used to train the model.
  new.predictors = check.data.vs.info(new.predictors, object$data$predictors)
  new.sensitive = check.data.vs.info(new.sensitive, object$data$sensitive)

  # use the auxiliary model fitted on the training data to construct the U
  # matrix for the test data, the decorrelated predictors.
  new.predictors = design.matrix(new.predictors, intercept = FALSE)
  new.sensitive = design.matrix(new.sensitive)

  auxiliary.coefs =
    object$auxiliary$coefficients[colnames(new.sensitive), , drop = FALSE]
  new.fitted = new.sensitive %*% auxiliary.coefs
  newU = new.predictors - new.fitted[, colnames(new.predictors), drop = FALSE]

  # merge sensitive attributes and decorrelated predictors.
  newdata = as.matrix(cbind(new.sensitive, newU))

  # extract the coefficients of the main model.
  coefs = coef(object)

  # check that they match with the variables, and that they appear in the same
  # order.
  if (!setequal(colnames(newdata), names(coefs)))
    stop("'new.predictors' or 'new.sensitive' have different variables than the model.")
  if (any(colnames(newdata) != names(coefs)))
    newdata = newdata[, names(coefs)]

  return(as.vector(newdata %*% coefs))

}#PREDICT.TWO.STAGES

# predict new observations for the fair regression in Komiyama et al. (2018).
predict.nclm = function(object, new.predictors, new.sensitive, ...) {

  if (!is(object, "nclm"))
    stop("'object' must be an 'nclm' object.")

  check.unused.args(list(...), character(0))

  predict.two.stages(object = object, new.predictors = new.predictors,
    new.sensitive = new.sensitive)

}#PREDICT.NCLM

# predict new observations for the fair ridge regression.
predict.frrm = function(object, new.predictors, new.sensitive, ...) {

  if (!is(object, "frrm"))
    stop("'object' must be an 'frrm' object.")

  check.unused.args(list(...), character(0))

  predict.two.stages(object = object, new.predictors = new.predictors,
    new.sensitive = new.sensitive)

}#PREDICT.FRRM
