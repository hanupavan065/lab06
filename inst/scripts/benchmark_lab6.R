# Run from the ROOT of the package project after devtools::install().
# This benchmarks exactly the sizes specified in Lab 6, plus the 1st star.
# It records *measured* elapsed times, NOT guessed values.
if (!requireNamespace("lab6knapsack", quietly = TRUE)) {
  stop("Install the project first using devtools::install()")
}

run_lab6_benchmarks <- function() {
  old_kind <- RNGkind()
  seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (seed_exists) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  suppressWarnings(RNGversion(min(as.character(getRversion()), "3.5.3")))
  set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
  n <- 2000L
  knapsack_objects <- data.frame(
    w = sample(1:4000, size = n, replace = TRUE),
    v = runif(n = n, 0, 10000)
  )

  measure <- function(name, size, capacity, version, expr) {
    gc()
    result <- NULL
    timing <- system.time(result <- force(expr))
    data.frame(algorithm = name, n = size, W = capacity,
               version = version, elapsed_seconds = unname(timing[["elapsed"]]),
               value = as.numeric(result$value),
               items = length(result$elements))
  }

  # Questions 1.1.2 and 1.1.3, same seeded PDF dataset.
  measurements <- list(
    measure("brute_force", 16L, 3500, "R",
            lab6knapsack::brute_force_knapsack(knapsack_objects[1:16, ], 3500)),
    measure("dynamic", 500L, 3500, "R",
            lab6knapsack::knapsack_dynamic(knapsack_objects[1:500, ], 3500))
  )

  # Question 1.1.4 asks for ONE MILLION objects. Just as the PDF says,
  # reset the same seed and change n, not the weight/value distributions.
  set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
  n <- 1000000L
  big <- data.frame(
    w = sample(1:4000, size = n, replace = TRUE),
    v = runif(n = n, 0, 10000)
  )
  # Warm up compiled code once; it must already have been installed.
  invisible(lab6knapsack::greedy_knapsack(big[1:100, ], 3500, fast=TRUE))
  measurements[[3]] <- measure("greedy", n, 3500, "R",
    lab6knapsack::greedy_knapsack(big, 3500, fast=FALSE))
  measurements[[4]] <- measure("greedy", n, 3500, "Rcpp",
    lab6knapsack::greedy_knapsack(big, 3500, fast=TRUE))

  results <- do.call(rbind, measurements)
  if (!dir.exists("inst/extdata")) {
    stop("Run the script from the package root: inst/extdata is missing")
  }
  write.csv(results, "inst/extdata/benchmark_results.csv", row.names=FALSE)
  print(results)
  tR <- results$elapsed_seconds[results$algorithm == "greedy" & results$version == "R"]
  tCpp <- results$elapsed_seconds[results$algorithm == "greedy" & results$version == "Rcpp"]
  cat(sprintf("Rcpp speedup: %.3f x (R elapsed / C++ elapsed)\n", tR/tCpp))
  if (!isTRUE(all.equal(results$value[3], results$value[4], tolerance=1e-8))) {
    warning("R and Rcpp greedy versions disagree in value; investigate")
  }
  invisible(results)
}

run_lab6_benchmarks()
