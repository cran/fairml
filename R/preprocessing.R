
# transform a data set into the corresponding design matrix.
design.matrix = function(data, intercept = TRUE) {

  # gather all the variables (model.matrix() does that internally if we do not).
  if (is.data.frame(data))
    frame = model.frame(~ ., data = data)
  else
    frame = model.frame(~ ., data = data.frame(data))

  # encode factors with contrasts.
  design = model.matrix(~ ., data = frame)

  # ensure column names are syntactically valid.
  colnames(design)[colnames(design) != "(Intercept)"] =
    make.names(colnames(design)[colnames(design) != "(Intercept)"])

  # optionally, remove the intercept term; this is not the same as not including
  # in the first place because contrasts would be different.
  if (!intercept)
    design = design[, colnames(design) != "(Intercept)", drop = FALSE]

  return(design)

}#DESIGN.MATRIX
