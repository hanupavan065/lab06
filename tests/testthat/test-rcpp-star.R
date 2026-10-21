test_that("", {
  set.seed(117)
  for (n in c(2L, 8L, 100L, 400L)) {
    x <- data.frame(w = sample(1:100, n, replace = TRUE),
                    v = runif(n, 0.1, 1000))
    for (capacity in c(30, 100, 250)) {
      expect_equal(greedy_knapsack(x, capacity, fast = FALSE),
                   greedy_knapsack(x, capacity, fast = TRUE),
                   tolerance = 1e-8)
    }
  }
})

test_that("", {
  x <- data.frame(w = c(2, 2, 4, 5), v = c(20, 20, 40, 1))
  expect_equal(greedy_knapsack(x, 4, FALSE),
               greedy_knapsack(x, 4, TRUE))
  expect_equal(greedy_knapsack(x, 1, TRUE), list(value=0, elements=integer(0)))
})
