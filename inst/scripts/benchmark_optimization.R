# This compares a simple scalar-loop DP baseline with vectorized lab6 DP.
# Run this from the root after devtools::install().
if (!requireNamespace("lab6knapsack", quietly=TRUE)) {
  stop("Install the package first")
}

# Baseline; intentionally avoids the vectorized optimized row update.
dp_scalar_baseline <- function(x, W) {
  n <- nrow(x)
  dp <- matrix(0, nrow = n + 1L, ncol = W + 1L)
  for (i in seq_len(n)) {
    wi <- as.integer(x$w[i])
    vi <- x$v[i]
    for (cap in 0:W) {
      old <- dp[i, cap + 1L]
      if (wi <= cap) {
        old <- max(old, vi + dp[i, cap - wi + 1L])
      }
      dp[i + 1L, cap + 1L] <- old
    }
  }
  dp[n+1L, W+1L]
}

suppressWarnings(RNGversion(min(as.character(getRversion()), "3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
items <- data.frame(w=sample(1:4000, 2000L, replace=TRUE),
                    v=runif(2000L,0,10000))
items <- items[1:250, , drop=FALSE]
W <- 3500L
# Verify equal numerical results FIRST.
baseline_value <- dp_scalar_baseline(items, W)
optimized_value <- lab6knapsack::knapsack_dynamic(items, W)$value
stopifnot(isTRUE(all.equal(baseline_value, optimized_value)))

measure <- function(expr) unname(system.time(force(expr))[["elapsed"]])
# Measure both with identical inputs and warm-up.
base_seconds <- measure(dp_scalar_baseline(items, W))
optimized_seconds <- measure(lab6knapsack::knapsack_dynamic(items, W))
result <- data.frame(algorithm=c("DP scalar baseline", "DP vectorized"),
                     n=250L, W=W, elapsed_seconds=c(base_seconds, optimized_seconds),
                     value=c(baseline_value, optimized_value))
print(result)
write.csv(result, "inst/extdata/optimization_results.csv", row.names=FALSE)
cat(sprintf("DP optimization gain: %.3f x\n", base_seconds/optimized_seconds))
