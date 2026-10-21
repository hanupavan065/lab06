#' Knapsack solution using brute force
#'
#' Tries every possible subset of valid items. returns the maximum
#' value without exceeding capacity. This is only suitable for small
#' numbers of items due to exponential running time.
#'
#' @param x A data.frame with two columns weights `w`
#'  and values `v`.
#' @param W capacity.
#' @return A list with `value` and  `elements`.
#' @details more than 23 eligible objects are rejected to avoid impractically 
#' long runs. 
#' @export
brute_force_knapsack <- function(x, W) {
  validate_knapsack(x, W)
  possible <- which(x$w <= W)
  n <- length(possible)
  if (!n) return(list(value = 0, elements = integer(0)))
  if (n > 23L) {
    stop("Brute force is limited to only 23 objects", call. = FALSE)
  }
  weights <- x$w[possible]
  values <- x$v[possible]
  best_value <- 0
  best_ids <- integer(0)
  for (mask in seq.int(0, 2^n - 1)) {
    selection <- which(as.integer(intToBits(as.integer(mask))[seq_len(n)]) == 1L)
    if (!length(selection)) next
    if (sum(weights[selection]) > W) next
    current_value <- sum(values[selection])
    if (current_value > best_value) {
      best_value <- current_value
      best_ids <- possible[selection]
    }
  }
  .make_answer(x, best_ids)
}
