test_that("Can deposit and withdraw money from GiroAccount", {
  giro_account <- GiroAccount$new(limit = 100)
  giro_account$deposit(2000)

  expect_error(giro_account$withdraw(200))
  expect_equal(giro_account$withdraw(100)$balance, 1900)

  giro_account$balance <- -10
  expect_equal(giro_account$withdraw(10)$balance, -30)
})
