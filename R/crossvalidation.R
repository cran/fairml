
# cross-validating fair models.
fairml.cv = function(response, predictors, sensitive, method = "k-fold", ...,
                     unfairness, model, model.args = list(), cluster) {

  # check the model to be fitted.
  check.label(model, fair.models, "model")

  # check arguments common to all models.
  if (model %in% fair.regressions)
    response = check.response(response, type = "continuous")
  else if (model %in% fair.classifiers)
    response = check.response(response, type = "binary")
  n = length(response)
  predictors = check.data(predictors, nobs = length(response), varletter = "X")
  sensitive = check.data(sensitive, nobs = length(response), varletter = "S")
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

    if (model %in% fair.regressions) {

      # match predicted and observed values.
      pred = unlist(lapply(kcv, "[[", "predicted"))
      obs = unlist(lapply(kcv, "[[", "observed"))

      # the loss function is the residuals mean square error.
      overall.loss = c(RMSE = mean((obs - pred)^2))

      # compute the aggregate fairness.
      fold.unfairness = sapply(kcv, `[[`, "unfairness")
      overall.unfairness = weighted.mean(fold.unfairness, kcv.length)

    }#THEN
    else if (model %in% fair.classifiers) {

      # match predicted and observed values.
      pred = unlist(lapply(kcv, "[[", "predicted"))
      obs = unlist(lapply(kcv, "[[", "observed"))

      # compute the confusion matrix, which is what all performance measures in
      # classification are computed from.
      confusion.matrix = table(pred, obs)

      tp = confusion.matrix[1, 1] + confusion.matrix[2, 2]
      fp = confusion.matrix[2, 1]
      fn = confusion.matrix[1, 2]

      # the loss functions are precision and recall.
      overall.loss = c(precision = 1 - fp / (fp + tp),
                       recall = tp / (tp + fn))

      # compute the overall unfairness.
      definition = kcv[[1]]$fitted$fairness$definition

      if (definition == "sp-disparate-impact") {

        # indexes of the observations in test folds, to map the predictions
        # correctly to the sensitive attributes.
        index = lapply(kcv, "[[", "test")
        sens = sensitive[unlist(index), ]
        linpred = rep(0, n)

        # compute predictions on the linear scale.
        for (fold in seq_along(kcv)) {

          linpred[index[[fold]]] =
            predict(kcv[[fold]]$fitted,
                    new.predictors = predictors[index[[fold]], ],
                    type = "link")

        }#FOR

        # unfairness is defined as the correlation between each sensitive
        # attribute and the predictions.
        overall.unfairness = cor(linpred, sens)[1, ]

      }#THEN

    }#THEN

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

  if (model %in% fair.regressions) {

    # predict the values on the test set.
    if (any(attr(fitted$main$coefficients, "sensitive"))) {

      predicted.best = predict(fitted, new.predictors = test.predictors,
                         new.sensitive = test.sensitive)

    }#THEN
    else {

      predicted.best = predict(fitted, new.predictors = test.predictors)

    }#ELSE

    if (fitted$fairness$definition == "sp-komiyama") {

      # recompute the loss function after neutering the sensitive attributes in
      # order to estimate how unfair the predictions are.
      is.sensitive = attr(fitted$main$coefficients, "sensitive")
      fitted$main$coefficients[is.sensitive] = 0

      # unfairness is measured as the proportion of the variance of the
      # predictions that is explained by the sensitive attributes.
      predicted.fair = predict(fitted, new.predictors = test.predictors,
                         new.sensitive = test.sensitive)
      obs.unfairness = 1 - var(predicted.fair) / var(predicted.best)

    }#THEN
    else if (fitted$fairness$definition == "eo-komiyama") {

      # regress the predicted values against the sensitive attributes and the
      # observed response.
      vars = anova(lm(predicted.best ~ test.sensitive + test.response))

      # unfairness is measured as the proportion of variance that is explained
      # by the sensitive attributes conditional on the observed response.
      obs.unfairness =
        vars["test.sensitive", "Sum Sq"] /
          (vars["test.sensitive", "Sum Sq"] + vars["test.response", "Sum Sq"])

    }#THEN

  }#THEN
  else if (model %in% fair.classifiers) {

    # predict the values on the test set.
    if (any(attr(fitted$main$coefficients, "sensitive"))) {

      predicted.best = predict(fitted, new.predictors = test.predictors,
                         new.sensitive = test.sensitive, type = "class")

    }#THEN
    else {

      predicted.best = predict(fitted, new.predictors = test.predictors,
                         type = "class")

    }#ELSE

    obs.unfairness = NA

  }#THEN

  return(list(test = test, fitted = fitted,
           unfairness = obs.unfairness, predicted = predicted.best,
           observed = test.response))

}#COMPUTE.LOSS.FROM.SPLIT

# extract predictive loss values from fair.kcv and fair.kcv.list objects.
cv.loss = function(x) {

  if (is(x, "fair.kcv"))
    values = attr(x, "loss")
  else if (is(x, "fair.kcv.list"))
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

  if (is(x, "fair.kcv"))
    values = attr(x, "unfairness")
  else if (is(x, "fair.kcv.list"))
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

  if (is(x, "fair.kcv")) {

    folds = lapply(x, `[[`, "test")

  }#THEN
  else if (is(x, "fair.kcv.list")) {

    folds = vector(length(x), mode = "list")
    for (i in seq_along(x))
      folds[[i]] = lapply(x[[i]], `[[`, "test")

  }#THEN
  else
    stop("'x' must be an object of class 'fair.kcv' or 'fair.kcv.list'.")

  return(folds)

}#CV.FOLDS
