test_that("", {
  x <- data.frame(w=c(5,6,7),v=c(10,20,30))
  for (solver in list(brute_force_knapsack, knapsack_dynamic, greedy_knapsack)) {
    out <- solver(x,2)
    expect_equal(out$value,0)
    expect_length(out$elements,0)
  }
})

test_that( "",{
  x <- data.frame(w=c(1.5,2.5), v=c(10,20))
  expect_equal(brute_force_knapsack(x,4)$value,30)
  expect_equal(greedy_knapsack(x,4)$value,30)
  expect_equal(greedy_knapsack(x,4,fast=TRUE)$value,30)
})
