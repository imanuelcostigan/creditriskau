
`%||%` <- function (x, y) {
  if (is.null(x)) y else x
}

`%<>%` <- function(x, y) {
  if (is.na(x)) y else x
}

default_on_na <- function(x, to) {
  if (length(to) > 1) {
    x[is.na(x)] <- to[is.na(x)]
  } else {
    x[is.na(x)] <- to
  }
  x
}

`%=>%` <- function(from, dictionary) {
  indices <- match(from, names(dictionary))
  unname(dictionary[indices])
}

are_equal_lengths <- function(...) {
  ll <- vapply(list(...), length, integer(1))
  abs(max(ll) - min(ll)) < 1L # ll will all be ints
}

# Determines the discount factor per APS 180, Att. A, para. 17(a)
discount_factor <- function(M) {
  assertthat::assert_that(all(M > 0))
  (1 - exp(-0.05 * M)) / 0.05 / M
}
