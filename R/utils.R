# advanced cat which correctly handles ini-like lines and short line widths.
wcat = function(header, value) {

  # get the number of available columns.
  columns = options("width")

  # blatantly ignore any line width shorter than 45, trying to support
  # that case is a losing proposition.
  if ((columns >= nchar(header) + nchar(value) + 1) || (columns < 45)) {

    # if there are enough columns print the string as is.
    cat(paste(header, value, sep = " "), "\n")

  }#THEN
  else {

    # if there are not enough columns print the header on one row
    # (left-aligned) and the value on the following line (right-aligned).
    cat(header, "\n", sprintf(paste("%", columns, "s", sep = ""), value), "\n")

  }#ELSE

}#WCAT

# remove extraneous attributes.
noattr = function(x, ok) {

  if (missing(ok))
    if (is.matrix(x))
      ok = c("dim", "dimnames")
    else if (is.factor(x))
      ok = c("class", "levels")
    else
      ok = character(0)

  x.attr = attributes(x)
  attributes(x) = x.attr[names(x.attr) %in% ok]

  return(x)

}#NOATTR

# transform the linear predictor into a classification probability.
linpred2prob = function(linear.predictor) {

  as.numeric(1 / (1 + exp(-linear.predictor)))

}#LINPRED2PROB

# transform the linear predictor into a binary-class factor.
linpred2class = function(linear.predictor, labels) {

  probs = linpred2prob(linear.predictor)

  factor(probs > 0.5, levels = c("FALSE", "TRUE"), labels = labels)

}#LINPRED2CLASS

# transform a linear predictor into multinomial probabilities.
linpred2mprob = function(linear.predictor) {

  probs = exp(linear.predictor)
  normalizing.constant = rowSums(probs)
  for (i in seq(ncol(probs)))
    probs[, i] = probs[, i] / normalizing.constant

  return(probs)

}#LINPRED2MPROB

# transform a linear predictor into a multiple-class factor.
linpred2mclass = function(linear.predictor, labels) {

  probs = linpred2mprob(linear.predictor)
  best.class = apply(probs, 1, which.max)
  factor(labels[best.class], levels = labels)

}#LINPRED2MCLASS

# range of values to plot over, with a bit of white space on either side.
extended.range = function(values, by = 0.05) {

  range(values) + c(-1, 1) * 0.05 * diff(range(values))

}#EXTENDED.RANGE

# log-likelihood of a linear regression model.
lm.loglikelihood = function(residuals) {

  # this is what logLik.lm() does.
  nobs = length(residuals)
  value = 0.5 * ( - nobs * (log(2 * pi) + 1 - log(nobs) +
                              log(sum(residuals^2))))

  return(value)

}#LM.LOGLIKELIHOOD

# any proportion of nothing is nothing.
proportion = function(x, tot) {

  if (tot == 0)
    return(0)
  else
    return(x / tot)

}#PROPORTION

# wrap cor() so that it handles zero-variance variables.
safe.cor = function(x, y) {

  suppressWarnings(cor(x, y))

}#SAFE.COR

# count the number of observations in a vector or in tabular data.
sample.size = function(sample) {

  if (is.null(dim(sample)))
    return(length(sample))
  else if (is.matrix(sample) || is.data.frame(sample))
    return(nrow(sample))

}#SAMPLE.SIZE

# explode a factor into a matrix of indicators, one column per leve.
class.ind = function(cl) {

  n = length(cl)
  indicators = matrix(0, n, nlevels(cl))
  indicators[seq(n) + n * (unclass(cl) - 1)] = 1

  return(indicators)

}#CLASS.IND

