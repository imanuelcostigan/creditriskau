context("IRB")

test_that("Non-retail capital ratios are correct", {
  expect_equal(non_retail_capital(c(0.20, 0.02, 0.01), 0.45, 50, 1, FALSE),
    c(0.1783729463, 0.0766165594, 0.0586227053))
  # TODO: add more comprehensive test cases
})

test_that("Retail capital ratios are correct", {
  lgds <- rep(c(0.2, 0.7, 0.4), each = 3)
  pds <- rep(c(0.01, 0.1, 0.999), 3)
  sub_classes <- rep(c("mortgage", "qrr", "other"), each = 3)
  # From irb.xlsx
  kk <- c(0.0200529513109492, 0.072679289475851, 0.0001996680404597,
    0.0214345101785415, 0.104400546577197, 0.00064614706087279,
    0.0325494930426509, 0.0537193288676191, 0.0003535564621715)
  expect_equal(retail_capital(pds, lgds, sub_classes), kk)
  expect_equal(retail_capital(pds[1], lgds[1], sub_classes[1], c(mortgage = 0.15)),
    0.0200529513109492)
})

