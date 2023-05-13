# fair ridge regression.
frrm = function(response, predictors, sensitive, unfairness,
         definition = "sp-komiyama", lambda = 0, save.auxiliary = FALSE) {

  fitted = two.stage.regression(model = "fgrrm", family = "gaussian",
             response = response, predictors = predictors, sensitive = sensitive,
             unfairness = unfairness, definition = definition,
             covfun = NULL, lambda = lambda, save.auxiliary = save.auxiliary)

  # save the function call for the print() method.
  fitted$main$call = match.call()
  # reset the main class to frrm.
  class(fitted)[1] = "frrm"

  return(fitted)

}#FRRM

