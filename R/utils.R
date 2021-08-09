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

# transform a classification probability into a binary class factor.
prob2class = function(prob, labels) {

  factor(prob > 0.5, levels = c("FALSE", "TRUE"), labels = labels)

}#PROB2CLASS

# transform a classification probability into the linear component of the model.
prob2link = function(prob) {

  log(prob / (1 - prob))

}#PROB2LINK

# range of values to plot over, with a bit of white space on either side.
extended.range = function(values, by = 0.05) {

  range(values) + c(-1, 1) * 0.05 * diff(range(values))

}#EXTENDED.RANGE

# loglikelihood of a linear regression model.
linear.model.loglikelihood = function(residuals, p) {

  nobs = length(residuals)

  # this is what logLik.lm() does.
  value = 0.5 * ( - nobs * (log(2 * pi) + 1 - log(nobs) + log(sum(residuals^2))))

  return(structure(value, nobs = nobs, df = p + 1, class = "logLik"))

}#LINEAR.MODEL.LOGLIKELIHOOD

# wrap cor() so that it handles zero-variance variables.
safe.cor = function(x, y) {

  suppressWarnings(cor(x, y))

}#SAFE.COR
