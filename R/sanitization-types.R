
# is x a vector of real number?
is.real.vector = function(x) {

  is.numeric(x) && !is.integer(x) && all(is.finite(x))

}#IS.REAL.VECTOR

# is x a non-negative number?
is.non.negative = function(x) {

  is.numeric(x) && !is.integer(x) && (length(x) == 1) && is.finite(x) &&
    (x >= 0)

}#IS.NON.NEGATIVE

# is x a non-negative integer?
is.non.negative.integer = function(x) {

  is.numeric(x) && (length(x) == 1) && is.finite(x) && (x >= 0) &&
    ((x %/% 1) == x)

}#IS.NON.NEGATIVE.INTEGER

# is x a probability or a proportion?
is.probability = function(x) {

  is.numeric(x) && !is.integer(x) && (length(x) == 1) && is.finite(x) &&
    (x >= 0) && (x <= 1)

}#IS.PROBABILITY

# is x a vector of probabilities?
is.probability.vector = function(x, zero = FALSE) {

  is.numeric(x) && !is.integer(x) && all(is.finite(x)) && all(x >= 0) &&
    all(x <= 1) && (zero || any(x > 0))

}#IS.PROBABILITY.VECTOR

# is x a single character string?
is.string = function(x) {

  is.character(x) && (length(x) == 1) && !any(is.na(x)) && any(x != "")

}#IS.STRING

# check logical flags.
check.logical = function(bool) {

  if (!is.logical(bool) || is.na(bool) || (length(bool) != 1))
    stop(sprintf("%s must be a logical value (TRUE/FALSE).",
           deparse(substitute(bool))))

}#CHECK.LOGICAL

