
# cross-validating fair models.
fairml.cv = function(response, predictors, sensitive, method = "k-fold", ...,
                     unfairness, model, model.args = list(), cluster) {

  # check the model to be fitted.
  check.label(model, fair.models, "model")

  # check arguments common to all models.
  response = check.response(response, model = model)
  n = length(response)
  predictors = check.data(predictors, nobs = n, varletter = "X")
  sensitive = check.data(sensitive, nobs = n, varletter = "S")
  check.fairness.level(unfairness)

  # remove optional arguments that do not belong after warning.
  check.unused.args(model.args, fair.models.extra.args[[model]])
  model.args = model.args[names(model.args) %in% fair.models.extra.args[[model]]]

  # check the cross-validation method.
  check.label(method, available.cv.methods, 'method')
  # check the extra arguments for the cross-validation method.
  extra.args = check.cv.args(method, list(...), n = n)

  # check the cluster.
  cluster = check.cluster(cluster)

  # allocate and populate the return value.
  runs = ifelse(is.null(extra.args$runs), 1, extra.args$runs)
  result = structure(vector(runs, mode = "list"), class = "fair.kcv.list")

  for (r in seq(runs)) {

    if (method == "k-fold") {

      # shuffle the data to get unbiased splits (do not warn about fold size).
      kcv = suppressWarnings(split(sample(n), seq_len(extra.args$k)))
      # store the length of each test set.
      kcv.length = sapply(kcv, length)

    }#THEN
    else if (method == "hold-out") {

      # sample m observations without replacement.
      kcv = lapply(seq(extra.args$k), function(x) sample(n, extra.args$m))
      # all test sets have the same length, a dummy works just fine.
      kcv.length = rep(extra.args$m, extra.args$k)

    }#THEN
    else if (method == "custom-folds") {

      # the folds are the custom folds specified by the user.
      kcv = extra.args$folds[[r]]
      # store the length of each test set.
      kcv.length = sapply(kcv, length)

    }#THEN

    kcv = smartSapply(cluster, kcv, compute.loss.from.split,
            response = response, predictors = predictors, sensitive = sensitive,
            unfairness = unfairness, model = model, model.args = model.args)

    definition = kcv[[1]]$fitted$fairness$definition
    family = kcv[[1]]$fitted$main$family

    # match predicted and observed values.
    pred = unlist(lapply(kcv, "[[", "predicted"))
    obs = unlist(lapply(kcv, "[[", "observed"))

    if (method == "hold-out") {

      # for hold-out cross-validation, it does not make sense to pool the test
      # folds to compute the loss because their union does not make up the whole
      # sample; compute the average loss instead.
      fold.loss = sapply(kcv, `[[`, "loss")
      overall.loss = weighted.mean(fold.loss, kcv.length)
      names(overall.loss) = names(fold.loss)[1]

    }#THEN
    else {

      # recompute the loss function on the pooled data.
      overall.loss =
        family.loss(observed = obs, predicted = pred, family = family)

    }#ELSE

    overall.unfairness =
      all.unfairness(kcv = kcv, kcv.length = kcv.length, sensitive = sensitive,
        predictors = predictors, definition = definition)

    # reset the names of the elements of the return value.
    names(kcv) = NULL
    # add some useful attributes to the renurn value.
    kcv = structure(kcv, class = "fair.kcv", loss = overall.loss,
            unfairness = overall.unfairness, method = method, model = model)

    result[[r]] = kcv

  }#FOR

  # return a fair.kcv object (for a single run) or a fair.kcv.list object (for
  # multiple runs).
  if (runs == 1)
    return(result[[1]])
  else
    return(result)

}#FAIRML.CV

compute.loss.from.split = function(test, response, predictors, sensitive,
    unfairness, model, model.args) {

  # create the training and test sets.
  train.response = response[-test]
  train.predictors = predictors[-test, , drop = FALSE]
  train.sensitive = sensitive[-test, , drop = FALSE]
  test.response = response[test]
  test.predictors = predictors[test, , drop = FALSE]
  test.sensitive = sensitive[test, , drop = FALSE]

  # learn the model from the training set.
  fitted = do.call(model, c(list(response = train.response,
                                 predictors = train.predictors,
                                 sensitive = train.sensitive,
                                 unfairness = unfairness),
                            model.args))

  # find out what family the model belongs to.
  family = fitted$main$family

  if (family == "gaussian")
    type = "response"
  else if (family == "binomial")
    type = "class"

  # predict the values on the test set.
  if (any(attr(fitted$main$coefficients, "sensitive"))) {

    predicted = predict(fitted, new.predictors = test.predictors,
                  new.sensitive = test.sensitive, type = type)

  }#THEN
  else {

    predicted = predict(fitted, new.predictors = test.predictors, type = type)

  }#ELSE

  loss =
    family.loss(observed = test.response, predicted = predicted, family = family)

  unfairness =
    fold.unfairness(fitted = fitted, predictors = predictors,
      sensitive = sensitive, response = response, test = test)

  return(list(test = test, fitted = fitted, loss = loss,
           unfairness = unfairness, predicted = predicted,
           observed = test.response))

}#COMPUTE.LOSS.FROM.SPLIT

# unfairness measured on the test set/folds.
fold.unfairness = function(fitted, predictors, sensitive, response, test) {

  definition = fitted$fairness$definition
  family = fitted$main$family

  if (definition %in% c("sp-komiyama", "eo-komiyama")) {

    # create the uncorrelated predictors and the design matrix of the sensitive
    # attributes, as well as subsetting the response.
    predictors.design =
      design.matrix(predictors, intercept = FALSE)[test, , drop = FALSE]
    sensitive.design = design.matrix(sensitive)[test, , drop = FALSE]
    decorrelated.predictors =
      predictors.design - sensitive.design %*% fitted$auxiliary$coefficients
    test.response = response[test]

    if (definition == "sp-komiyama") {

      unfairness =
        fgrrm.sp.komiyama(model = coef(fitted), y = test.response,
          S = sensitive.design, U = decorrelated.predictors,
          family = family)["value"]

    }#THEN
    else if (definition == "eo-komiyama") {

      unfairness =
        fgrrm.eo.komiyama(model = coef(fitted), y = test.response,
          S = sensitive.design, U = decorrelated.predictors,
          family = family)["value"]

    }#THEN

  }#THEN
  else if (definition == "sp-zafar-disparate-impact") {

    sensitive.design = design.matrix(sensitive)[test, , drop = FALSE]
    pred = predict(fitted, new.predictors = predictors, type = "link")[test]

    unfairness = safe.cor(pred, sensitive.design)[1, ]
    # make sure to replace NA correlations arising from variables with
    # variance equal to zero.
    unfairness[is.na(unfairness)] = 0

  }#THEN

  return(structure(unfairness, names = definition))

}#FOLD.UNFAIRNESS

# unfairness measured on the whole data set.
all.unfairness = function(kcv, kcv.length, sensitive, predictors, definition) {

  if (definition %in% c("sp-komiyama", "eo-komiyama")) {

    fold.unfairness = sapply(kcv, `[[`, "unfairness")
    unfairness = weighted.mean(fold.unfairness, kcv.length)
    names(unfairness) = definition

  }#THEN
  else if (definition == "sp-zafar-disparate-impact") {

    # indexes of the observations in test folds, to map the predictions
    # correctly to the sensitive attributes.
    index = lapply(kcv, "[[", "test")
    sens = design.matrix(sensitive, intercept = FALSE)
    sens = sens[unlist(index), , drop = FALSE]
    linpred = rep(0, nrow(sens))

    # compute predictions on the linear scale.
    for (fold in seq_along(kcv)) {

      linpred[index[[fold]]] =
        predict(kcv[[fold]]$fitted,
                new.predictors = predictors[index[[fold]], ],
                type = "link")

    }#FOR

    # unfairness is defined as the correlation between each sensitive
    # attribute and the predictions.
    unfairness = safe.cor(linpred, sens)[1, ]
    # make sure to replace NA correlations arising from variables with
    # variance equal to zero.
    unfairness[is.na(unfairness)] = 0

  }#THEN

  return(unfairness)

}#ALL.UNFAIRNESS

# extract predictive loss values from fair.kcv and fair.kcv.list objects.
cv.loss = function(x) {

  if (inherits(x, "fair.kcv"))
    values = attr(x, "loss")
  else if (inherits(x, "fair.kcv.list"))
    values = sapply(x, function(x) attr(x, "loss"))
  else
    stop("'x' must be an object of class 'fair.kcv' or 'fair.kcv.list'.")

  # if the loss is a vector, the return value is matrix: make sure that the rows
  # correspond to the runs and that the columns correspond to the losses.
  if (is.matrix(values))
    values = t(values)

  return(values)

}#CV.LOSS

# extract predictive fairness values from fair.kcv and fair.kcv.list objects.
cv.unfairness = function(x) {

  if (inherits(x, "fair.kcv"))
    values = attr(x, "unfairness")
  else if (inherits(x, "fair.kcv.list"))
    values = sapply(x, function(x) attr(x, "unfairness"))
  else
    stop("'x' must be an object of class 'fair.kcv' or 'fair.kcv.list'.")

  # if the unfairness is a vector, the return value is matrix: make sure that
  # the rows correspond to the runs and that the columns correspond to the
  # unfairness measures.
  if (is.matrix(values))
    values = t(values)

  return(values)

}#CV.UNFAIRNESS

# extract the indexes of the observations in each fold.
cv.folds = function(x) {

  if (inherits(x, "fair.kcv")) {

    folds = lapply(x, `[[`, "test")

  }#THEN
  else if (inherits(x, "fair.kcv.list")) {

    folds = vector(length(x), mode = "list")
    for (i in seq_along(x))
      folds[[i]] = lapply(x[[i]], `[[`, "test")

  }#THEN
  else
    stop("'x' must be an object of class 'fair.kcv' or 'fair.kcv.list'.")

  return(folds)

}#CV.FOLDS

# pick the right loss function for the model's family.
family.loss = function(observed, predicted, family) {

  if (family == "gaussian")
    rmse.loss(observed = observed, predicted = predicted)
  else if (family == "binomial")
    pr.loss(observed = observed, predicted = predicted)

}#FAMILY.LOSS

# residuals mean square error loss.
rmse.loss = function(observed, predicted) {

  return(c(RMSE = mean((observed - predicted)^2)))

}#RMSE.LOSS

# precision and recall loss.
pr.loss = function(observed, predicted) {

  # compute the confusion matrix, which is what all performance measures in
  # classification are computed from.
  confusion.matrix = table(observed, predicted)

  # by convention, the "positive" class is the second level of the response
  # variable, and the first is the "negative" class
  tp = confusion.matrix[2, 2]
  fp = confusion.matrix[1, 2]
  fn = confusion.matrix[2, 1]

  # the loss functions are precision and recall.
  return(c(precision = 1 - fp / (fp + tp), recall = tp / (tp + fn)))

}#PR.LOSS
