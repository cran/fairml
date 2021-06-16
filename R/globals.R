
# fair model labels and optional arguments.
fair.regressions = c("nclm", "frrm")
fair.classifiers = c("zlrm")
fair.models = c(fair.regressions, fair.classifiers)

fair.models.labels = c(
  "nclm" = "Komiyama et al. (2018)",
  "frrm" = "Fair Ridge Regression",
  "zlrm" = "Zafar's Logistic Regression"
)

fair.models.extra.args = list(
  "nclm" = c("lambda", "covfun"),
  "frrm" = c("lambda", "definition"),
  "zlrm"  = character(0)
)

# fairness definition labels.
available.fairness.definitions = c("sp-komiyama", "eo-komiyama",
  "sp-disparate-impact")

fairness.definitions.labels = c(
  "sp-komiyama" = "statistical parity",
  "eo-komiyama" = "equality of opportunity",
  "sp-disparate-impact" = "disparate impact"
)

fairness.definitions.for.model = list(
  "nclm" = "sp-komiyama",
  "frrm" = c("sp-komiyama", "eo-komiyama"),
  "zlrm" = "sp-disparate-impact"
)

# cross-validation method labels and optional arguments.
available.cv.methods = c("k-fold", "hold-out", "custom-folds")

cv.extra.args = list(
  "k-fold" = c("k", "runs"),
  "hold-out" = c("k", "m", "runs"),
  "custom-folds" = c("folds")
)

# types of profile plots and what models they apply to.
available.profile.plots = c("coefficients", "constraints", "precision-recall")

models.for.plot  = list(
  "coefficients" = c("nclm", "frrm", "zlrm"),
  "constraints" = c("nclm", "frrm", "zlrm"),
  "precision-recall" = c("zlrm")
)
