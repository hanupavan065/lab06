test_that("", {
  x <- data.frame(w = c(10,20,30), v = c(60,100,120))
  ans <- knapsack_dynamic(x, 50)
  expect_equal(ans$value, 220)
  expect_setequal(ans$elements, c(2L, 3L))
  expect_equal(knapsack_dynamic(x, 0)$value, 0)
  expect_equal(knapsack_dynamic(data.frame(w=numeric(),v=numeric()), 5)$value, 0)
})

test_that("", {
  x <- data.frame(w=c(2,3,4,1), v=c(30,50,65,15))
  ans <- knapsack_dynamic(x,5)
  expect_equal(ans$value,80)
  expect_lte(sum(x$w[ans$elements]),5)
  expect_equal(sum(x$v[ans$elements]),ans$value)
})
