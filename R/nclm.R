# non-convex fair regression from Komiyama et al. (2018).
nclm = function(response, predictors, sensitive, unfairness, covfun, lambda = 0,
         save.auxiliary = FALSE) {

  fitted = two.stage.regression(model = "nclm", response = response,
             predictors = predictors, sensitive = sensitive,
             unfairness = unfairness, definition = "sp-komiyama",
             covfun = covfun, lambda = lambda, save.auxiliary = save.auxiliary)

  # save the function call for the print() method.
  fitted$main$call = match.call()

  return(fitted)

}#NCLM

# particular case, excluding sensitive attributes.
nclm.zero.sensitive = function(y, S, U, covfun, lambda) {

  nU = ncol(U)
  nS = ncol(S)
  nobs = length(y)

  # standardize all variables, to make the life easier for the optimizer.
  Us = scale(U, center = TRUE, scale = TRUE)
  # standardize the response only if it has variance smaller than 1, this
  # seems to produce coefficients that are closer to those from lm() when
  # epsilon = 1 while preserving predictive accuracy.
  ys = scale(y, center = TRUE, scale = ifelse(sd(y) < 1, TRUE, 1))

  # compute the covariance matrices with the specified estimator.
  covmatU = covfun(Us)
  check.covariance.matrix(covmatU, nvars = nU, what = "predictors")

  # regularization with a ridge penalty.
  if (lambda > 0)
    covmatU = covmatU + diag(lambda / nobs, nrow = nU, ncol = nU)

  # build the objective function (no gamma in this simple particular case).
  qu = t(ys) %*% Us / nobs
  objective.function = quadfun(Q = covmatU, a = -2 * qu, id = colnames(Us))

  # linear optimization problem, quadratic constraints: call the optimizer.
  mycop = cop(f = objective.function)
  res = solvecop(mycop, solver = "cccp", quiet = TRUE, maxiters = 50000L)

  # validate the solution provided by the solver.
  check = validate(mycop, res, quiet = TRUE)

  # compute relevant quantities for later use.
  # regression coefficients (that is, all parameters apart from gamma), scaled
  # back into their unstandardized form.
  coefs = res$x * attr(ys, "scaled:scale") / attr(Us, "scaled:scale")
  # add the intercept back.
  coefs = c("(Intercept)" = 0, coefs)
  coefs["(Intercept)"] =
    as.vector(attr(ys, "scaled:center") -
              attr(Us, "scaled:center") %*% coefs[colnames(U)])
  # fitted values and residuals.
  fitted = as.vector(coefs["(Intercept)"] + U %*% coefs[colnames(U)])
  resid = as.vector(y - fitted)
  # proportions of response variance explained by S and U: scale by var(y)
  # instead of using the "scaled:scale" attribute because scale() is often
  # called with a hard-coded scale factor of 1.
  r2.S = 0
  r2.U = var(fitted) / var(y)

  # make sure the coefficients are returned in the same order as in
  # nclm.optiSolve().
  coefs = c(coefs["(Intercept)"], structure(rep(0, nS), names = colnames(S)),
            coefs[colnames(Us)])
  # mark coefficients corresponding to sensitive attributes.
  attr(coefs, "sensitive") = names(coefs) %in% colnames(S)

  return(list(main = list(
                coefficients = coefs,
                residuals = resid,
                fitted.values = fitted,
                r.squared.S = r2.S,
                r.squared.U = r2.U,
                r2.statistical.parity = r2.S / (r2.S + r2.U),
                arguments = list(
                  lambda = lambda,
                  covfun = covfun
              )),
         fairness = list(
                definition = "sp-komiyama",
                value = r2.S / (r2.S + r2.U),
                bound = 0,
                other = list(
                  r.squared.S = r2.S,
                  r.squared.U = r2.U
                )
              )))

}#NCLM.ZERO.SENSITIVE

# general case, with quadratic constraints.
nclm.optiSolve = function(y, S, U, epsilon, covfun, lambda) {

  nS = ncol(S)
  nU = ncol(U)
  nobs = length(y)

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

  # compute the covariance matrices with the specified estimator.
  covmatS = covfun(Ss)
  check.covariance.matrix(covmatS, nvars = nS, what = "sensitive attributes")
  covmatU = covfun(Us)
  check.covariance.matrix(covmatU, nvars = nU, what = "predictors")

  # regularization with a ridge penalty.
  if (lambda > 0) {

    covmatS = covmatS + diag(lambda / nobs, nrow = nS, ncol = nS)
    covmatU = covmatU + diag(lambda / nobs, nrow = nU, ncol = nU)

  }#THEN

  # first constraint, quadratic term.
  Q1 = diag(0, 1 + nS + nU)
  Q1[2:(nS + 1), 2:(nS + 1)] = covmatS
  Q1[(nS + 2):(nS + nU + 1), (nS + 2):(nS + nU + 1)] = covmatU
  # first constraint, linear term.
  qs = t(ys) %*% Ss / nobs
  qu = t(ys) %*% Us / nobs
  a1 = -2 * c(1/2, qs, qu)
  # create the first constraint .
  first.constraint = quadcon(Q = Q1, a = a1, dir = "<=", val = 0, id = labels)

  # second constraint, quadratic term.
  Q2 = diag(0, 1 + nS + nU)
  Q2[2:(nS + 1), 2:(nS + 1)] = covmatS / epsilon
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
  # mark coefficients corresponding to sensitive attributes.
  attr(coefs, "sensitive") = names(coefs) %in% colnames(S)
  # fitted values and residuals.
  fitted.U = U %*% coefs[colnames(U)]
  fitted.S = S %*% coefs[colnames(S)]
  fitted = as.vector(coefs["(Intercept)"] + fitted.S + fitted.U)
  resid = as.vector(y - fitted)
  # proportions of response variance explained by S and U: scale by var(y)
  # instead of using the "scaled:scale" attribute because scale() is often
  # called with a hard-coded scale factor of 1.
  r2.S = var(fitted.S) / var(y)
  r2.U =  var(fitted.U) / var(y)

  return(list(main = list(
                coefficients = coefs,
                residuals = resid,
                fitted.values = fitted,
                r.squared.S = r2.S,
                r.squared.U = r2.U,
                r2.statistical.parity = r2.S / (r2.S + r2.U),
                arguments = list(
                  lambda = lambda,
                  covfun = covfun
              )),
         fairness = list(
                definition = "sp-komiyama",
                value = r2.S / (r2.S + r2.U),
                bound = epsilon,
                other = list(
                  r.squared.S = r2.S,
                  r.squared.U = r2.U
                )
              )))

}#NCLM.OPTISOLVE

