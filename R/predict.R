
# predict new observations for a generic fair model.
predict.fair.model = function(object, ...) {

  if (!inherits(object, "fair.model"))
    stop("'object' must be a 'fair.model' object.")

  check.unused.args(list(...), character(0))

  stop("predict.fair.model() not implemented.")

}#PREDICT.FAIR.MODELS

two.stages.prediction = function(object, new.predictors, new.sensitive, type) {

  # extract the coefficients of the main model and their labels.
  coefs = coef(object)
  if (is.matrix(coefs))
    coef.names = rownames(coefs)
  else
    coef.names = names(coefs)

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
  new.sensitive =
    design.matrix(new.sensitive, intercept = ("(Intercept)" %in% coef.names))

  auxiliary.coefs =
    object$auxiliary$coefficients[colnames(new.sensitive), , drop = FALSE]
  new.fitted = new.sensitive %*% auxiliary.coefs
  newU = new.predictors - new.fitted[, colnames(new.predictors), drop = FALSE]

  # merge sensitive attributes and decorrelated predictors.
  newdata = as.matrix(cbind(new.sensitive, newU))

  # check that they match with the variables, and that they appear in the same
  # order.
  if (!setequal(colnames(newdata), coef.names))
    stop("'new.predictors' or 'new.sensitive' have different variables than the model.")
  if (any(colnames(newdata) != coef.names))
    newdata = newdata[, coef.names]

  # check the type of prediction.
  if (inherits(object, fair.regressions))
    check.label(type, c("response", "link"), "prediction type")
  else if (inherits(object, fair.classifiers))
    check.label(type, c("response", "class", "link"), "prediction type")
  else if (inherits(object, fair.family)) {

    if (object$main$family == "gaussian")
      check.label(type, c("response", "link"), "prediction type")
    else if (object$main$family == "binomial")
      check.label(type, c("response", "class", "link"), "prediction type")
    else if (object$main$family %in% c("poisson", "cox"))
      check.label(type, c("response", "link"), "prediction type")

  }#THEN

  if (is.matrix(coefs))
    linear.predictor = noattr(newdata %*% coefs)
  else
    linear.predictor = as.vector(newdata %*% coefs)

  if (inherits(object, fair.regressions)) {

    return(linear.predictor)

  }#THEN
  else if (inherits(object, fair.classifiers)) {

    return(classifier.prediction(linear.predictor, type = type,
             labels = object$data$response$levels[["response"]]))

  }#THEN
  else if (inherits(object, fair.family)) {

    if (object$main$family == "gaussian") {

      return(linear.predictor)

    }#THEN
    else if (object$main$family %in% c("binomial", "multinomial")) {

      return(classifier.prediction(linear.predictor, type = type,
               labels = object$data$response$levels[["response"]]))

    }#THEN
    else if (object$main$family == "poisson") {

      if (type == "link")
        return(linear.predictor)
      else if (type == "response")
        return(exp(linear.predictor))

    }#THEN
    else if (object$main$family == "cox") {

      if (type == "link")
        return(linear.predictor)
      else if (type == "response")
        return(exp(-linear.predictor))

    }#THEN

  }#THEN

}#TWO.STAGES.PREDICTION

# produce different types of predictions from classifiers.
classifier.prediction = function(linear.predictor, type, labels) {

  if (type == "link") {

    # predict on the scale of the linear component of the model.
    return(linear.predictor)

  }#THEN
  else if (type == "response") {

    # predict the probability of success.
    if (is.matrix(linear.predictor))
      return(linpred2mprob(linear.predictor))
    else
      return(linpred2prob(linear.predictor))

  }#THEN
  else if (type == "class") {

    # predict the class label.
    if (is.matrix(linear.predictor))
      return(linpred2mclass(linear.predictor, labels = labels))
    else
      return(linpred2class(linear.predictor, labels = labels))

  }#THEN

}#CLASSIFIER.PREDICTION

# predict new observations for the fair regression in Komiyama et al. (2018).
predict.nclm = function(object, new.predictors, new.sensitive,
    type = "response", ...) {

  if (!inherits(object, "nclm"))
    stop("'object' must be an 'nclm' object.")

  check.unused.args(list(...), character(0))

  two.stages.prediction(object = object, new.predictors = new.predictors,
    new.sensitive = new.sensitive, type = type)

}#PREDICT.NCLM

# predict new observations for the fair ridge regression.
predict.frrm = function(object, new.predictors, new.sensitive,
    type = "response", ...) {

  if (!inherits(object, "frrm"))
    stop("'object' must be an 'frrm' object.")

  check.unused.args(list(...), character(0))

  two.stages.prediction(object = object, new.predictors = new.predictors,
    new.sensitive = new.sensitive, type = type)

}#PREDICT.FRRM

# predict new observations for the fair generalized ridge regression.
predict.fgrrm = function(object, new.predictors, new.sensitive,
    type = "response", ...) {

  if (!inherits(object, "fgrrm"))
    stop("'object' must be an 'fgrrm' object.")

  check.unused.args(list(...), character(0))

  two.stages.prediction(object = object, new.predictors = new.predictors,
    new.sensitive = new.sensitive, type = type)

}#PREDICT.FGRRM

# predict new observations for Zafar's logistic regression.
predict.zlrm = function(object, new.predictors, type = "response", ...) {

  if (!inherits(object, "zlrm"))
    stop("'object' must be a 'zlrm' object.")

  # check the type of fitted values.
  check.label(type, c("response", "class", "link"), "prediction type")

  check.unused.args(list(...), character(0))

  # check the predictors, in themselves and against the data the model was
  # fitted from.
  new.predictors = check.data(new.predictors, min.nobs = 1, varletter = "X")
  new.predictors = check.data.vs.info(new.predictors, object$data$predictors)
  new.predictors = design.matrix(new.predictors)

  # extract the coefficients of the main model.
  coefs = coef(object)

  # check that they match with the variables, and that they appear in the same
  # order.
  if (!setequal(colnames(new.predictors), names(coefs)))
    stop("'new.predictors' have different variables than the model.")
  if (any(colnames(new.predictors) != names(coefs)))
    new.predictors = new.predictors[, names(coefs)]

  # compute the linear component of the model.
  linear.predictor = as.vector(new.predictors %*% coefs)

  classifier.prediction(linear.predictor, type = type,
    labels = object$data$response$levels[["response"]])

}#PREDICT.ZLRM

# predict new observations for Zafar's linear regression.
predict.zlm = function(object, new.predictors, type = "response", ...) {

  if (!inherits(object, "zlm"))
    stop("'object' must be a 'zlm' object.")

  # check the type of fitted values.
  check.label(type, c("response", "link"), "prediction type")

  check.unused.args(list(...), character(0))

  # check the predictors, in themselves and against the data the model was
  # fitted from.
  new.predictors = check.data(new.predictors, min.nobs = 1, varletter = "X")
  new.predictors = check.data.vs.info(new.predictors, object$data$predictors)
  new.predictors = design.matrix(new.predictors)

  # extract the coefficients of the main model.
  coefs = coef(object)

  # check that they match with the variables, and that they appear in the same
  # order.
  if (!setequal(colnames(new.predictors), names(coefs)))
    stop("'new.predictors' have different variables than the model.")
  if (any(colnames(new.predictors) != names(coefs)))
    new.predictors = new.predictors[, names(coefs)]

  linear.predictor = as.vector(new.predictors %*% coefs)

  return(linear.predictor)

}#PREDICT.ZLM
