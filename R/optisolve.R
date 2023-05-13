
# this file contains a very minimal wrapper around cccp, with a similar
# interface to optiSolve.
quadcon = function(Q, a = rep(0, nrow(Q)), d = 0, val, id = 1:nrow(Q)) {

  a = c(a)
  val = c(val)

  rownames(Q) = colnames(Q) = names(a) = id

  return(structure(list(Q = Q, a = a, d = c(d), dir = "<=", val = val, id = id,
           name = "quadratic", use = TRUE), class = "quadCon"))

}#QUADCON

quadfun = function(Q, a = rep(0, nrow(Q)), d = 0, id = 1:nrow(Q)) {

  a = c(a)
  rownames(Q) = colnames(Q) = names(a) = id

  return(structure(list(Q = Q, a = a, d = c(d), id = id, name = "quad.fun"),
           class =  "quadFun"))

}#QUADFUN

linfun = function(a, d = 0, id = 1:length(a)){

  a = c(a)
  names(a) = id

  return(structure(list(a = a, d = c(d), id = id, name = "lin.fun"),
           class = "linFun"))

}#LINFUN

cop = function(f, ...) {

  ids = f$id
  x = structure(rep(NA_real_, length(ids)), names = ids)

  return(list(f = f, max = FALSE, qc = list(...), x = x, id = ids))

}#COP

validate = function(op, sol, tol = 1e-4) {

  x = sol$x

  if (is(op$f, "linFun"))
    value = c(t(op$f$a) %*% x + op$f$d)
  if (is(op$f, "quadFun"))
    value = c(t(x) %*% op$f$Q %*% x + t(op$f$a) %*% x + op$f$d)

  valid = TRUE

  for (i in seq_along(op$qc)) {

    name = op$qc[[i]]$name
    value = c(t(x) %*% (op$qc[[i]]$Q) %*% x + t(op$qc[[i]]$a) %*% x +
              (op$qc[[i]]$d))
    dir = op$qc[[i]]$dir
    cval = op$qc[[i]]$val
    isOK = (value <= cval + tol)
    isOK = !is.na(isOK) & isOK
    valid = valid & isOK

  }#FOR

  return(list(valid = valid, status = sol$status))

}#VALIDATE

solvecop = function(op, ...) {

  if (class(op$f) %in% c("linFun", "quadFun"))
    op$f$d = 0 * op$f$d

  for (i in seq_along(op$qc)) {

    op$qc[[i]]$val = op$qc[[i]]$val - op$qc[[i]]$d
    op$qc[[i]]$d = 0 * op$qc[[i]]$d

  }#FOR

  res = call_cccp(op, ...)
  res$x = replace(op$x, names(res$x), res$x)

  return(res)

}#SOLVECOP

call_cccp = function(op, abstol = (1e-06)*(10^length(op$qc)), feastol = 1e-05,
  trace = FALSE, stepadj = 0.90, maxiters = 100L, reltol = 1e-06, beta = 0.5) {

  optctrl = cccp::ctrl(abstol = abstol, feastol = feastol, trace = trace,
              stepadj = stepadj, maxiters = maxiters, reltol = reltol,
              beta = beta)

  op = qc2socc(op)

  if (is(op$f, "linFun")) {

    P = NULL
    q = op$f$a

  }#THEN

  if (is(op$f, "quadFun")) {

    P = 2 * op$f$Q
    q = op$f$a

  }#THEN

  cList = NULL
  soccNumber = length(op$socc)
  if (soccNumber > 0) {

    cList = vector("list", soccNumber)
    for(i in seq_along(op$socc))
      cList[[i]] = do.call(cccp::socc, op$socc[[i]][c("F", "g", "d", "f")])

  }#THEN

  suppressWarnings({
    res = cccp::cccp(P = P, q = c(q), A = op$eqlc$A, b = op$eqlc$val, cList = cList,
               optctrl = optctrl)
  })

  if (length(cccp::getx(res)) == 0)
    x = structure(rep(NA, length(op$id)), names = op$id)
  else
    x = structure(c(cccp::getx(res)), names = op$id)

  return(list(x = x, solver = "cccp", status = res$status))

}#CALL_CCCP

qc2socc = function(op) {

  nqc = length(op$qc)
  if(nqc == 0)
    return(op)

  op$socc = append(vector("list", nqc), op$socc)
  for(i in 1:nqc) {

    op$qc[[i]]$Q = make.definite(op$qc[[i]]$Q)
    F = as.matrix(chol(op$qc[[i]]$Q))
    g = 0.5 * c(solve(t(F)) %*% (op$qc[[i]]$a))
    f = c(sqrt(op$qc[[i]]$val + sum(g * g)))
    d = 0 * g
    op$socc[[i]] =
      structure(list(F = F, g = g, f = f, d = d, id = op$qc[[i]]$id),
                class = "socCon")

  }#FOR

  op$qc = NULL

  return(op)

}#QC2SOCC

make.definite = function(Q) {

  oldQ = Q
  diagQ = diag(Q)
  epsilon = max(abs(diagQ)) * 1e-4

  if (any(diagQ < epsilon)) {

    diagQ[diagQ < epsilon] = epsilon
    diag(Q) = diagQ

  }#THEN

  x = eigen(Q, only.values = TRUE)$values

  if (min(x) < epsilon) {

    x = eigen(Q, only.values = FALSE)
    v = x$values
    v[v < epsilon] = epsilon
    Q = x$vectors %*% diag(v) %*% t(x$vectors)

  }#THEN

  return(Q)

}#MAKE.DEFINITE

