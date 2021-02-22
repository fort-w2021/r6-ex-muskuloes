test_that("Can deposit and withdraw money from Account", {
  account <- Account$new()
  account$deposit(2000)
  account$withdraw(200)

  expect_equal(account$balance, 1800)
})
