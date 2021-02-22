test_that("Can deposit and withdraw money from SafeAccount", {
  safe_account <- SafeAccount$new()
  safe_account$deposit <- 2000
  safe_account$withdraw <- 200

  expect_equal(safe_account$deposit, 1800)
  expect_equal(safe_account$withdraw, 1800)
})
