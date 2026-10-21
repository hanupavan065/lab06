test_that("", {
  x <- data.frame(w=c(2,3,4,1), v=c(30,50,65,15))
  expect_equal(greedy_knapsack(x,5), greedy_knapsack(x,5,fast=TRUE))
  expect_equal(greedy_knapsack(x,0)$value, 0)
  expect_equal(greedy_knapsack(data.frame(w=10,v=20),3)$value,0)
  set.seed(6)
  for (n in c(1L, 3L, 30L, 100L)) {
    x <- data.frame(w=sample(1:100,n,replace=TRUE),v=runif(n,1,1000))
    expect_equal(greedy_knapsack(x,100,fast=FALSE),
                 greedy_knapsack(x,100,fast=TRUE), tolerance=1e-8)
  }
})

test_that("", {
  x <- data.frame(w=c(10,20,30), v=c(60,100,120))
  optimum <- brute_force_knapsack(x,50)$value
  for (fast in c(FALSE, TRUE)) {
    out <- greedy_knapsack(x,50,fast=fast)
    expect_lte(sum(x$w[out$elements]), 50)
    expect_equal(sum(x$v[out$elements]), out$value)
    expect_gte(out$value, optimum/2)
  }
})
