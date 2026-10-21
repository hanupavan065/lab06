#' Greedy approximation for knapsack
#'
#' Sorts items by descending (value / weight),then tries each one in turn.
#'  also considers the best single item and returns the best of the 
#'  two solutions.
#'
#' @param x A data.frame columns `w` and `v`.
#' @param W capacity.
#' @param fast default `FALSE`. If `TRUE`, use the compiled
#'   Rcpp/C++.
#' @return A list with `value` and `elements`.
#' @export
greedy_knapsack <- function(x, W, fast = FALSE) {
  validate_knapsack(x, W)
  if (!is.logical(fast) || length(fast) != 1L || is.na(fast)) {
    stop("fast must be TRUE or FALSE", call. = FALSE)
  }
  if (!nrow(x) || W == 0 || !any(x$w <= W)) {
    return(list(value = 0, elements = integer(0)))
  }
  if (fast) {
    return(.make_answer(x, greedy_cpp(x$w, x$v, as.numeric(W))))
  }
  ord <- order(-(x$v / x$w), -x$v, seq_len(nrow(x)))
  chosen <- integer(length(ord))
  n_chosen <- 0L
  total_weight <- 0
  for (id in ord) {
    wi <- x$w[id]
    if (wi <= W - total_weight) {
      n_chosen <- n_chosen + 1L
      chosen[n_chosen] <- id
      total_weight <- total_weight + wi
    }
  }
  chosen <- chosen[seq_len(n_chosen)]
  chosen_value <- sum(x$v[chosen])
  feasible <- which(x$w <= W)
  best_single <- feasible[which.max(x$v[feasible])]
  if (x$v[best_single] > chosen_value) {
    chosen <- best_single
  }
  .make_answer(x, chosen)
}
