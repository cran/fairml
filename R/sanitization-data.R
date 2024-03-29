
# check the response variable.
check.response = function(response, model, family, min.nobs = 2) {

  if (missing(response))
    stop("'response' is missing.")

  if (model %in% fair.regressions) {

    # make sure the response is a real vector.
    response = as.vector(response)
    if (!is.real.vector(response))
      stop("'response' should be a numeric vector.")

    # do not allow a zero-variance response if later we try to standardize it.
    if (min.nobs > 1)
      if (var(response) < sqrt(.Machine$double.eps))
        stop("'response' has variance zero, it cannot be standardized.")

  }#THEN
  else if (model %in% fair.classifiers) {

    if (!is.factor(response) || (nlevels(response) != 2))
      stop("'response' should be a factor with two levels.")

  }#THEN
  else if (model %in% fair.family) {

    if (missing(family))
      stop("'family' is missing.")

    if (family == "gaussian") {

      response = as.vector(response)
      if (!is.real.vector(response))
        stop("'response' should be a numeric vector.")

      # do not allow a zero-variance response if later we try to standardize it.
      if (min.nobs > 1)
        if (var(response) < sqrt(.Machine$double.eps))
          stop("'response' has variance zero, it cannot be standardized.")

    }#THEN
    else if (family == "binomial") {

      if (!is.factor(response) || (nlevels(response) != 2))
        stop("'response' should be a factor with two levels.")
      if (!all(table(response) > 0))
        stop("'response' only takes one of its two possible values.")

    }#THEN
    else if (family == "poisson") {

      response = as.vector(response)
      if (!is.non.negative.vector(response))
        stop("'response' should be a non-negative vector.")

      # make sure the respone is integer.
      response = round(response)

    }#THEN
    else if (family == "cox") {

      # make sure that the response is a two-column matrix with column names.
      if (is.data.frame(response))
        response = as.matrix(response)
      if (!is.matrix(response)) {

        if (!identical(length(dim(response)), 2) || (ncol(response) != 2))
          stop("'response' should be a matrix with columns 'time' and 'status'.")
        if (is.null(colnames(response)))
          colnames(response) = c("time", "status")
        else if (!identical(colnames(response), c("time", "status")))
          stop("'response' should be a matrix with columns 'time' and 'status'.")

      }#THEN

      # make sure that the times are positive.
      if (!is.positive.vector(response[, "time"]))
        stop("the 'time' in 'response' should be strictly positive.")
      # make sure that the censoring indicators are binary.
      if (!is.binary.vector(response[, "status"]))
        stop("the 'status' in 'response' should be a binary {0, 1} vector.")
      # make sure that not all observations are censored.
      if (all(response[, "status"] == 0))
        stop("all the observations are censored ('status' equal to zero).")

    }#THEN
    else if (family == "multinomial") {

      if (!is.factor(response) || (nlevels(response) < 2))
        stop("'response' should be a factor with at least two levels.")
      if (!all(table(response) > 0))
        stop("not all values of 'response' are observed.")

    }#THEN

  }#THEN

  # no missing values in the response.
  if (anyNA(response))
    stop("'response' contains missing values.")

  # check the minimum sample size.
  if (sample.size(response) < min.nobs)
    stop("'response' should contain at least ", min.nobs, " observations.")

  return(response)

}#CHECK.RESPONSE

# check data in tabular form.
check.data = function(data, nobs, min.nobs = 2, varletter) {

  argname = as.character(match.call()[[2]])

  if (missing(data))
    stop(q(argname), " is missing.")

  if (is.vector(data, mode = "numeric") || is.factor(data))
    data = structure(data.frame(V = data), names = paste0(varletter, "1"))
  if (!is.matrix(data) && !is.data.frame(data))
    stop(q(argname), " must be a matrix or a data frame.")
  if (!missing(nobs) && (nrow(data) != nobs))
    stop(q(argname), " contains ", nrow(data),
      " observations, but ", nobs, " were expected.")
  if (anyNA(data))
    stop(q(argname), " contains missing values.")

  # check the minimum sample size.
  if (nrow(data) < min.nobs)
    stop(q(argname), " should contain at least ", min.nobs, " observations.")

  if (is.matrix(data)) {

    if (!is.numeric(data))
      stop(q(argname), " is a matrix, but its values are not numeric.")
    # make sure the variables have names, which is not a given if they are
    # stored in a matrix.
    if (is.null(colnames(data)))
      colnames(data) = paste0(varletter, seq(ncol(data)))

    if (min.nobs > 1) {

      # do not allow variables that are effectively constants.
      singular = (apply(data, 2, var) < sqrt(.Machine$double.eps))
      if (any(singular))
        stop("variables ", q(colnames(data)[singular]),
             " in ", q(argname), " have variance zero.")

    }#THEN

  }#THEN
  else if (is.data.frame(data)) {

    # make sure all variables are numeric or factors.
    invalid = sapply(data,
                function(x) !inherits(x, "numeric") && !inherits(x, "factor") || inherits(x, "integer"))
    if (any(invalid))
      stop("variables ", q(names(which(invalid))),
           " in ", q(argname), " should be numeric or factor(s).")

    isf = names(which(sapply(data, is.factor)))
    for (f in isf)
      data[, f] = droplevels(data[, f])

    if (min.nobs > 1) {

      # do not allow variables that are effectively constants.
      is.numvar = sapply(data, is.numeric)
      numeric.vars = data[, is.numvar, drop = FALSE]
      singular = (sapply(numeric.vars, var) < sqrt(.Machine$double.eps))
      if (any(singular))
        stop("variables ", q(names(which(singular))),
             " in ", q(argname), " have variance zero.")

      # do not allow factors with a single level, lm() breaks down trying to
      # create contrasts.
      nlvls = sapply(data[, isf, drop = FALSE], nlevels)
      if (any(nlvls == 1))
        stop("variables ", q(names(which(nlvls == 1))),
             " in ", q(argname), " only have a single level (each).")

    }#THEN

  }#THEN

  return(data)

}#CHECK.DATA.SET

# extract some structural information from a data set.
get.data.info = function(data) {

  if (is.matrix(data)) {

    # can only be "numeric".
    classes = structure(rep("numeric", ncol(data)), names = colnames(data))
    levels = structure(vector("list", ncol(data)), names = colnames(data))

  }#THEN
  else if (is.data.frame(data)) {

    # can be "numeric" or "factor", in which case we need the levels as well.
    var.class = function(x) ifelse(is.factor(x), "factor", "numeric")
    classes = sapply(data, var.class)
    levels = structure(vector("list", ncol(data)), names = colnames(data))
    levels[classes == "factor"] =
      lapply(data[, classes == "factor", drop = FALSE], levels)

  }#THEN

  return(list(classes = classes, levels = levels))

}#GET.DATA.INFO

# check that new data have the expected characteristics.
check.data.vs.info = function(data, info) {

  argname = deparse(substitute(data))
  new.info = get.data.info(data)

  # the number of variables must be the same.
  nvars = length(info$classes)
  new.nvars = length(new.info$classes)
  if (nvars != new.nvars)
    stop("found ", new.nvars, " variables, expected ", nvars, " in '",
         argname, "'.")

  # the names of the variables must be the same.
  varnames = names(info$classes)
  new.varnames = names(new.info$classes)
  names.not.in.new = setdiff(varnames, new.varnames)
  if (length(names.not.in.new) != 0)
    stop("variables ", q(names.not.in.new),
         " are present in the model but not in ", q(argname), "." )

  names.not.in.orig = setdiff(new.varnames, varnames)
  if (length(names.not.in.orig) != 0)
    warning("variables ", q(names.not.in.orig),
            " are present in ", q(argname), " but not in the model." )

  # subset the data and reorder the variables to match the model.
  data = data[, varnames, drop = FALSE]
  new.info = get.data.info(data)

  # the classes of the variables must be the same.
  diff.class = info$class != new.info$class
  if (any(diff.class))
    stop("variables ", q(names(which(diff.class))),
         " have different classes in ", q(argname), " and in the model.")

  # for discrete variables, there should be no level that has not been observed
  # when learning the model.
  for (v in names(which(new.info$class == "factor"))) {

    # level sets are identical.
    if (setequal(info$levels[[v]], new.info$levels[[v]])) {

      # reorder the levels if needed.
      if (all(info$levels[[v]] == new.info$levels[[v]]))
        next
      else
        data[, v] = factor(data[, v], levels = info$level[[v]])

    }#THEN

    # not all levels are observed in the data, re-encode the factor using the
    # levels in the model.
    if (all(new.info$levels[[v]] %in% info$levels[[v]]) &&
        !all(info$levels[[v]] %in% new.info$levels[[v]]))
      data[, v] = factor(data[, v], levels = info$level[[v]])

    # there are levels in the data that are unknown to the model, give up.
    if (!all(new.info$levels[[v]] %in% info$levels[[v]]))
      stop("variable '", v, "' has levels in ", q(argname),
           " that are not in the model.")

  }#FOR

  return(data)

}#CHECK.DATA.VS.INFO
