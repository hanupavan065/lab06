#' Knapsack solution using dynamic programming
#'
#' Uses a capacity table to remember the best value for the first i items.
#' Supports integer weights and integer W, and recovers the chosen row indices.
#'
#' @param x A data.frame with two columns `w` 
#' @param W capacity
#' @return A list with `value` and `elements`.
#' @export
knapsack_dynamic <- function(x, W) {
  validate_knapsack(x, W, discrete = TRUE)
  possible <- which(x$w <= W)
  n <- length(possible)
  if (!n) return(list(value = 0, elements = integer(0)))
  w <- as.integer(x$w[possible])
  v <- x$v[possible]
  capacity <- as.integer(W)
  cells <- (as.double(n) + 1) * (as.double(capacity) + 1)
  if (cells > 15000000) {
    stop("dynamic table would be too large for memory", call. = FALSE)
  }
  dp <- matrix(0, nrow = n + 1L, ncol = capacity + 1L)
  take <- matrix(FALSE, nrow = n, ncol = capacity + 1L)
  for (i in seq_len(n)) {
    prev <- dp[i, ]
    dp[i + 1L, ] <- prev  
    wi <- w[i]
    if (wi > capacity) next
    cols <- seq.int(wi + 1L, capacity + 1L)
    candidate <- prev[cols - wi] + v[i]
    improve <- candidate > prev[cols]
    if (any(improve)) {
      dp[i + 1L, cols[improve]] <- candidate[improve]
      take[i, cols[improve]] <- TRUE
    }
  }
  remaining <- capacity
  chosen <- integer(0)
  for (i in rev(seq_len(n))) {
    if (take[i, remaining + 1L]) {
      chosen <- c(possible[i], chosen)
      remaining <- remaining - w[i]
    }
  }
  .make_answer(x, chosen)
}
