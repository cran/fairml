
# fair model labels and optional arguments.
fair.models = c("nclm")

fair.models.labels = c(
  "nclm" = "Komiyama et al. (2018)"
)

fair.models.extra.args = list(
  "nclm" = c("lambda", "covfun")
)

# cross-validation method labels and optional arguments.
available.cv.methods = c("k-fold", "hold-out", "custom-folds")

cv.extra.args = list(
  "k-fold" = c("k", "runs"),
  "hold-out" = c("k", "m", "runs"),
  "custom-folds" = c("folds")
)

