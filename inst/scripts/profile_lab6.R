# Run from project root after devtools::install() and install.packages('profvis').
if (!requireNamespace("profvis", quietly=TRUE)) stop("Install profvis")
if (!requireNamespace("lab6knapsack", quietly=TRUE)) stop("Install lab6knapsack")
set.seed(42)
x <- data.frame(w=sample(1:4000, 50000L, replace=TRUE),
                v=runif(50000L, 0, 10000))
# Inspect the allocation/sorting/iteration costs in the pure-R version.
profvis::profvis({
  lab6knapsack::greedy_knapsack(x, W=3500, fast=FALSE)
})
# Using C++ compiled code moves operations outside the R-level profiler;
# time the compiled path separately rather than claiming profvis sees C++ internals.
print(system.time(lab6knapsack::greedy_knapsack(x, 3500, fast=TRUE)))
