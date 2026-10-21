validate_knapsack <- function(x, W, discrete = FALSE) {
  if (!is.data.frame(x) || ncol(x) != 2L ||
      !setequal(names(x), c("w", "v"))) {
    stop("x must be a data.frame with exactly two columns named w and v", call. = FALSE)
  }
  if (!is.numeric(x$w) || !is.numeric(x$v) ||
      anyNA(x$w) || anyNA(x$v) ||
      any(!is.finite(x$w)) || any(!is.finite(x$v)) ||
      any(x$w <= 0) || any(x$v <= 0)) {
    stop("w and v must contain positive finite numeric values", call. = FALSE)
  }
  if (!is.numeric(W) || length(W) != 1L || is.na(W) ||
      !is.finite(W) || W < 0) {
    stop("W must be a nonnegative finite number", call. = FALSE)
  }
  if (discrete) {
    if (W != floor(W) || any(x$w != floor(x$w))) {
      stop("W and weights must be whole-number integers", call. = FALSE)
    }
    if (W > 1000000 || W >= .Machine$integer.max) {
      stop("W is too large", call. = FALSE)
    }
  }
  invisible(TRUE)
}

.make_answer <- function(x, ids) {
  ids <- as.integer(ids)
  list(value = as.numeric(sum(x$v[ids])), elements = ids)
}
