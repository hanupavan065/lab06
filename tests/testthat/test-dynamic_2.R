
test_that("", {
  
  # Step 1: Create a data frame
  x <- data.frame(
    w = c(2, 3, 4, 1),
    v = c(30, 50, 65, 15)
  )
  
  # Step 2: Set the maximum capacity
  W <- 5
  
  # Step 3: Run brute force
  brute <- brute_force_knapsack(x, W)
  
  # Step 4: Run dynamic programming
  dynamic <- knapsack_dynamic(x, W)
  
  # Step 5: Check the maximum value
  expect_equal(dynamic$value, 80)
  
  # Step 6: Compare both algorithms
  expect_equal(dynamic$value, brute$value)
  
  # Step 7: Check total selected weight
  expect_true(
    sum(x$w[dynamic$elements]) <= W
  )
  
  # Step 8: Check the returned value
  expect_equal(
    sum(x$v[dynamic$elements]),
    dynamic$value
  )
  
})


test_that("", {
  
  x <- data.frame(
    w = c(2, 3, 4, 1),
    v = c(30, 50, 65, 15)
  )
  
  for (W in c(3, 5, 7)) {
    
    brute <- brute_force_knapsack(x, W)
    dynamic <- knapsack_dynamic(x, W)
    
    expect_equal(
      dynamic$value,
      brute$value
    )
    
  }
  
})
