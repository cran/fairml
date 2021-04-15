
# cross-validating fair models.
fairml.cv = function(response, predictors, sensitive, method = "k-fold", ...,
                     unfairness, model, model.args = list()) {

  # check arguments common to all models.
  response = check.response(response)
  n = length(response)
  predictors = check.data(predictors, nobs = length(response), varletter = "X")
  sensitive = check.data(sensitive, nobs = length(response), varletter = "S")
  check.fairness.level(unfairness)

  # check the model to be fitted.
  check.label(model, fair.models, "model")
  # remove optional arguments that do not belong after warning.
  check.unused.args(model.args, fair.models.extra.args[[model]])
  model.args = model.args[names(model.args) %in% fair.models.extra.args[[model]]]

  # check the cross-validation method.
  check.label(method, available.cv.methods, 'method')
  # check the extra arguments for the cross-validation method.
  extra.args = check.cv.args(method, list(...), n = n)

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

    kcv = lapply(kcv, compute.loss.from.split, response = response,
            predictors = predictors, sensitive = sensitive,
            unfairness = unfairness, model = model, model.args = model.args)

    # compute the aggregate loss.
    fold.losses = sapply(kcv, `[[`, "loss")
    mean.loss = weighted.mean(fold.losses, kcv.length)
    # compute the aggregate fairness.
    fold.unfairness = sapply(kcv, `[[`, "unfairness")
    mean.unfairness = weighted.mean(fold.unfairness, kcv.length)
    # reset the names of the elements of the return value.
    names(kcv) = NULL
    # add some useful attributes to the renurn value.
    kcv = structure(kcv, class = "fair.kcv", mean.loss = mean.loss,
            mean.unfairness = mean.unfairness, method = method, model = model)

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

  # compute the loss function on the test set.
  predicted.best = predict(fitted, new.predictors = test.predictors,
                     new.sensitive = test.sensitive)
  obs.loss = mean((test.response - predicted.best)^2)

  # recompute the loss function after neutering the sensitive attributes.
  is.sensitive = attr(fitted$main$coefficients, "sensitive")
  fitted$main$coefficients[is.sensitive] = 0

  predicted.fair = predict(fitted, new.predictors = test.predictors,
                     new.sensitive = test.sensitive)
  obs.unfairness = 1 - var(predicted.fair) / var(predicted.best)

  return(c(list(test = test, fitted = fitted), loss = obs.loss,
           unfairness = obs.unfairness))

}#COMPUTE.LOSS.FROM.SPLIT

# extract predictive loss values from fair.kcv and fair.kcv.list objects.
cv.loss = function(x) {

  if (is(x, "fair.kcv"))
    values = attr(x, "mean.loss")
  else if (is(x, "fair.kcv.list"))
    values = sapply(x, function(x) attr(x, "mean.loss"))
  else
    stop("'x' must be an object of class 'fair.kcv' or 'fair.kcv.list'.")

  return(values)

}#CV.LOSS


# extract predictive fairness values from fair.kcv and fair.kcv.list objects.
cv.unfairness = function(x) {

  if (is(x, "fair.kcv"))
    values = attr(x, "mean.unfairness")
  else if (is(x, "fair.kcv.list"))
    values = sapply(x, function(x) attr(x, "mean.unfairness"))
  else
    stop("'x' must be an object of class 'fair.kcv' or 'fair.kcv.list'.")

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
