# check whether the cluster is running.
isClusterRunning = function(cl) {

  tryCatch(any(unlist(parallel::clusterEvalQ(cl, TRUE))),
    error = function(err) { FALSE })

}#ISCLUSTERRUNNING

# check the status of the snow/parallel cluster.
check.cluster = function(cluster) {

  if (missing(cluster) || is.null(cluster))
    return(NULL)
  if (!inherits(cluster, "cluster"))
    stop("cluster is not a valid cluster object.")
  check.and.load.package("parallel")
  if (!isClusterRunning(cluster))
    stop("the cluster is stopped.")

  return(cluster)

}#CHECK.CLUSTER

# smart parSapply() that falls back to standard sapply(), but with defaults to
# simplify = FALSE.
smartSapply = function(cl, ..., simplify = FALSE, USE.NAMES = TRUE) {

  if (is.null(cl))
    sapply(..., simplify = simplify, USE.NAMES = USE.NAMES)
  else
    parallel::parSapplyLB(cl = cl, ..., simplify = simplify, USE.NAMES = USE.NAMES)

}#SMARTSAPPLY

