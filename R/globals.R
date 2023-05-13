
# fair model labels and optional arguments.
fair.regressions = c("nclm", "frrm", "zlm")
fair.classifiers = c("zlrm")
fair.family = c("fgrrm")
fair.models = c(fair.regressions, fair.classifiers, fair.family)

fair.models.labels = c(
  "nclm" = "Komiyama et al. (2018)",
  "frrm" = "Fair Ridge Regression",
  "fgrrm" = "Fair Generalized Ridge Regression",
  "zlm" = "Zafar's Linear Regression",
  "zlrm" = "Zafar's Logistic Regression"
)

fair.models.extra.args = list(
  "nclm" = c("lambda", "covfun"),
  "frrm" = c("lambda", "definition"),
  "fgrrm" = c("family", "lambda", "definition"),
  "zlm" = character(0),
  "zlrm" = character(0)
)

# generalized linear model families.
available.families = c("gaussian", "binomial", "poisson", "cox", "multinomial")

# fairness definition labels.
available.fairness.definitions = c("sp-komiyama", "eo-komiyama",
  "sp-zafar-disparate-impact", "if-berk")

fairness.definitions.labels = c(
  "sp-komiyama" = "Komiyama's R^2 (statistical parity)",
  "eo-komiyama" = "Komiyama's R^2 (equality of opportunity)",
  "sp-zafar-disparate-impact" = "Marginal correlation (disparate impact)",
  "if-berk" = "Berk's individual fairness"
)

fairness.definitions.for.model = list(
  "nclm" = "sp-komiyama",
  "frrm" = c("sp-komiyama", "eo-komiyama", "if-berk"),
  "fgrrm" = c("sp-komiyama", "eo-komiyama", "if-berk"),
  "zlm" = "sp-zafar-disparate-impact",
  "zlrm" = "sp-zafar-disparate-impact"
)

# cross-validation method labels and optional arguments.
available.cv.methods = c("k-fold", "hold-out", "custom-folds")

cv.extra.args = list(
  "k-fold" = c("k", "runs"),
  "hold-out" = c("k", "m", "runs"),
  "custom-folds" = c("folds")
)

# types of profile plots and what models they apply to.
available.profile.plots = c("coefficients", "constraints", "precision-recall",
  "rmse")

models.for.plot  = list(
  "coefficients" = c("nclm", "frrm", "fgrrm", "zlm", "zlrm"),
  "constraints" = c("nclm", "frrm", "fgrrm", "zlm", "zlrm"),
  "precision-recall" = c("zlrm", "fgrrm"),
  "rmse" = c("nclm", "frrm", "fgrrm", "zlm")
)
