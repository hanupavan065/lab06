test_that("", {
  x <- data.frame(w = c(2,3,4,1), v = c(30,50,65,15))
  ans <- brute_force_knapsack(x, 5)
  expect_equal(ans$value, 80)
  expect_true(sum(x$w[ans$elements]) <= 5)
  expect_equal(sum(x$v[ans$elements]), ans$value)
  expect_equal(brute_force_knapsack(x, 0), list(value=0, elements=integer(0)))
  expect_error(brute_force_knapsack(data.frame(w=rep(1,24),v=rep(1,24)), 24), "23")
})

test_that("",{
  set.seed(41)
  for (n in c(3L, 5L, 8L, 10L)) {
    x <- data.frame(w = sample(1:10,n,replace=TRUE),
                    v = runif(n,1,30))
    bf <- brute_force_knapsack(x, 15)
    dp <- knapsack_dynamic(x, 15)
    expect_equal(bf$value, dp$value, tolerance=1e-8)
    expect_lte(sum(x$w[bf$elements]), 15)
  }
})
