context("SA-CVA")

test_that("cva_capital returns the correct value for single counterparty", {
  expect_equal(cva_capital(2, 1, 0, 0.8), 0)
  expect_equal(cva_capital(2, 1, 1e6, 0.8), 2.33 * 0.008 * 1 * 0.8 * 1e6)
  expect_equal(cva_capital(2, 2, 1e6, 0.8), 2.33 * 0.008 * 2 * 0.8 * 1e6)
  expect_equal(cva_capital(NA, 2, 1e6, 0.8), 2.33 * 0.02 * 2 * 0.8 * 1e6)
  expect_equal(cva_capital(3, 2, 1e6, 0.8), 2.33 * 0.01 * 2 * 0.8 * 1e6)
  expect_equal(cva_capital(3, 2, 1e6, 1), 2.33 * 0.01 * 2 * 1 * 1e6)
  expect_error(cva_capital(8, 2, 1e6, 1))
  expect_error(cva_capital(3, 0, 1e6, 1))
  expect_error(cva_capital(3, 2, 1e6, 1.1))
  expect_error(cva_capital(3, 2, -0.01, 1))
})

test_that("cva_capital correct for multiple counterparties and no hedges", {
  s <- 0.02 * 1 * discount_factor(1) * 100 +
    0.03 * 0.5 * discount_factor(0.5) * 8000
  ss <- (0.02 * 1 * discount_factor(1) * 100) ^ 2 +
    (0.03 * 0.5 * discount_factor(0.5) * 8000) ^ 2
  res <- 2.33 * sqrt(0.25 * s ^ 2 + 0.75 * ss)
  expect_equal(cva_capital(c(4, 5), c(1, 0.5), c(100, 8000)), res)
})

test_that("cva_capital correct for multiple counterparties and hedges", {
  s <- 0.02 * (1 * discount_factor(1) * 100  -
      1 * discount_factor(1) * 50) +
    0.03 * (0.5 * discount_factor(0.5) * 8000 -
        0.5 * discount_factor(0.5) * 1000)
  cva_s <- 0.01 * 1 * discount_factor(1) * 2000
  ss <- (0.02 * (1 * discount_factor(1) * 100  -
      1 * discount_factor(1) * 50)) ^ 2 +
    (0.03 * (0.5 * discount_factor(0.5) * 8000 -
        0.5 * discount_factor(0.5) * 1000)) ^ 2
  res <- 2.33 * sqrt((0.5 * s - cva_s) ^ 2 + 0.75 * ss)
  out <- cva_capital(c(4, 5), c(1, 0.5), c(100, 8000), NULL,
    c(1, 0.5), c(50, 1000), NULL, 3, 1, 2000, NULL)
  expect_equal(out, res)
})
