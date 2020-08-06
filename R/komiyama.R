# non-convex fair regression from Komiyama et al. (2018).
nclm = function(response, predictors, sensitive, epsilon) {

  # save the function call for the print() method.
  cl = match.call()

  # check the arguments.
  response = check.response(response)
  predictors = check.data(predictors, nobs = length(response), varletter = "X")
  sensitive = check.data(sensitive, nobs = length(response), varletter = "S")
  if (missing(epsilon) || !is.probability(epsilon))
    stop("'epsilon' should be a number between 0 and 1.")

  # save some information on the data, to be used e.g. in predict().
  predictors.info = get.data.info(predictors)
  sensitive.info = get.data.info(sensitive)

  # encode factors with constrasts, but:
  # 1) remove the intercept from the predictors (it will be added later in the
  #      main model);
  # 2) remove the intercept from the sensitive attributes (it will be added
  #      later in the auxiliary model).
  predictors = design.matrix(predictors, intercept = FALSE)
  sensitive = design.matrix(sensitive, intercept = FALSE)
  # regress the predictors against the sensitive attributes to contruct U, the
  # matrix of non-sensitive variables.
  auxiliary.model = lm(predictors ~ ., data = data.frame(sensitive))
  U = predictors - fitted(auxiliary.model)
  # remove the intercept from the sensitive attributes, it is no longer needed.
  sensitive = sensitive[, colnames(sensitive) != "(Intercept)", drop = FALSE]

  # check that there are no duplicate names.
  duplicate.names = intersect(colnames(U), colnames(sensitive))
  if (length(duplicate.names) != 0)
    stop("duplicate variable names: ", paste(duplicate.names, collapse = " "), ".")

  if (epsilon == 0)
    fit = nclm.zero.sensitive(y = response, S = sensitive, U = U)
  else
    fit = nclm.optiSolve(y = response, S = sensitive, U = U, epsilon = epsilon)

  # collect all the information about the model and the input data.
  retval = list(
    auxiliary = list(call = auxiliary.model$call,
                     coefficients = coef(auxiliary.model),
                     fitted.value = fitted(auxiliary.model),
                     residuals = residuals(auxiliary.model)),
    main = c(call = cl, fit, bound = epsilon),
    data = list(predictors = predictors.info, sensitive = sensitive.info)
  )

  return(structure(retval, class = c("nclm", "fair.model")))

}#NCLM

# particular case, excluding sensitive attributes.
nclm.zero.sensitive = function(y, S, U, epsilon) {

  res = lm(y ~ ., data = data.frame(U))

  # make sure the regression coefficients are returned in the same order as
  # in nclm.optiSolve().
  coefs = structure(rep(0, ncol(S) + ncol(U) + 1),
            names = c("(Intercept)", colnames(S), colnames(U)))
  coefs[names(coef(res))] = coef(res)

  return(list(coefficients = coefs, residuals = residuals(res),
           fitted.values = fitted(res), r.squared.S = 0,
           r.squared.U = 1 - var(residuals(res)) / var(y)))

}#NCLM.ZERO.SENSITIVE

# general case, with quadratic constraints.
nclm.optiSolve = function(y, S, U, epsilon) {

  nS = ncol(S)
  nU = ncol(U)

  # standardize all variables, to make the life easier for the optimizer.
  Ss = scale(S, center = TRUE, scale = TRUE)
  Us = scale(U, center = TRUE, scale = TRUE)
  # standardize the response only if it has variance smaller than 1, this
  # seems to produce coefficients that are closer to those from lm() when
  # epsilon = 1 while preserving predictive accuracy.
  ys = scale(y, center = TRUE, scale = ifelse(sd(y) < 1, TRUE, 1))

  # the objective function contains just gamma, all the other parameters are
  # zeroed by the respective coefficients.
  labels = c("gamma", colnames(Ss), colnames(Us))
  objective.function = linfun(a = c(1, rep(0, nS), rep(0, nU)), id = labels)

  # first constraint, quadratic term.
  Q1 = diag(0, 1 + nS + nU)
  Q1[2:(nS + 1), 2:(nS + 1)] = cov(Ss)
  Q1[(nS + 2):(nS + nU + 1), (nS + 2):(nS + nU + 1)] = cov(Us)
  # first constraint, linear term.
  qs = t(ys) %*% Ss / length(ys)
  qu = t(ys) %*% Us / length(ys)
  a1 = -2 * c(1/2, qs, qu)
  # create the first constraint .
  first.constraint = quadcon(Q = Q1, a = a1, dir = "<=", val = 0, id = labels)

  # second constraint, quadratic term.
  Q2 = diag(0, 1 + nS + nU)
  Q2[2:(nS + 1), 2:(nS + 1)] = cov(Ss) / epsilon
  # second constraint, linear term (same as in the previous constraint).
  a2 = a1
  # create the second constraint.
  second.constraint = quadcon(Q = Q2, a = a2, dir = "<=", val = 0, id = labels)

  # linear optimization problem, quadratic constraints: call the optimizer.
  mycop = cop(f = objective.function,
              qc = first.constraint, qc2 = second.constraint)
  res = solvecop(mycop, solver = "cccp", quiet = TRUE, maxiters = 50000L)

  # validate the solution provided by the solver.
  check = validate(mycop, res, quiet = TRUE)

  if (check$info$valid == FALSE)
    stop("failed to find an optimal solution for the constrained optimization.")
  if (check$info$status != "optimal")
    warning("found suboptimal solution, convergence possibly failed.")

  # compute relevant quantities for later use.
  # regression coefficients (that is, all parameters apart from gamma), scaled
  # back into their unstandardized form.
  coefs = res$x[names(res$x) != "gamma"]
  coefs = coefs * attr(ys, "scaled:scale") /
            c(attr(Ss, "scaled:scale"), attr(Us, "scaled:scale"))
  # add the intercept back.
  coefs = c("(Intercept)" = 0, coefs)
  coefs["(Intercept)"] =
    as.vector(attr(ys, "scaled:center") -
              attr(Ss, "scaled:center") %*% coefs[colnames(S)] -
              attr(Us, "scaled:center") %*% coefs[colnames(U)])
  # fitted values and residuals.
  fitted.U = U %*% coefs[colnames(U)]
  fitted.S = S %*% coefs[colnames(S)]
  fitted = as.vector(coefs["(Intercept)"] + fitted.S + fitted.U)
  resid = as.vector(y - fitted)
  # proportions of response variance explained by S and U.
  r2.S = var(fitted.S) / attr(ys, "scaled:scale")^2
  r2.U =  var(fitted.U) / attr(ys, "scaled:scale")^2

  return(list(coefficients = coefs, residuals = resid, fitted.values = fitted,
           r.squared.S = r2.S, r.squared.U = r2.U))

}#NCLM.OPTISOLVE

