
# check whether a package can be loaded.
check.and.load.package = function(pkg) {

  # silence all warnings while looking for suggested packages.
  warning.level  = as.numeric(options("warn"))
  options("warn" = -1)
  on.exit(options("warn" = warning.level))

  if (!requireNamespace(pkg))
    stop("this function requires the ", pkg, " package.")

}#CHECK.AND.LOAD.PACKAGE

# warn about unused arguments.
check.unused.args = function(dots, used.args) {

  if (inherits(dots, "list"))
    args = names(dots)
  else
    args = dots

  unused = !(args %in% used.args)

  if (any(unused))
    warning("unused argument(s):", paste0(" '", args[unused], "'"), ".")

}#CHECK.UNUSED.ARGS

# compact quoting function.
q = function(strings) {

  paste(sQuote(strings, FALSE), collapse = ", ")

}#Q

# check labels for various arguments.
check.label = function(arg, choices, argname) {

  if (missing(arg) || !is.string(arg))
    stop("the ", argname, " must be a single character string.")

  if (arg %in% choices)
    return(invisible(NULL))

  stop("valid ", argname, "(s) are ", q(choices), ".")

}#CHECK.LABEL
