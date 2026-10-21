test_that("", {
  x <- data.frame(w = c(1, 2), v = c(2, 3))
  for (solver in list(brute_force_knapsack, knapsack_dynamic, greedy_knapsack)) {
    expect_error(solver(data.frame(weight = 1, value = 2), 5), "columns")
    expect_error(solver(data.frame(w = -1, v = 2), 5), "positive")
    expect_error(solver(data.frame(w = NA_real_, v = 2), 5), "positive")
    expect_error(solver(x, -1), "nonnegative")
    expect_error(solver(x, NA_real_), "nonnegative")
  }
  expect_error(knapsack_dynamic(data.frame(w=1.5, v=10), 5), "whole-number")
  expect_error(knapsack_dynamic(x, 2.5), "whole-number")
  expect_error(greedy_knapsack(x, 5, fast = NA), "fast")
  expect_error(greedy_knapsack(x, 5, fast = 1), "fast")
})


test_that( "",{
  x <- data.frame(w = rep(1, 20), v = rep(1, 20))
  expect_error(knapsack_dynamic(x, 1000000), "too large")
})
