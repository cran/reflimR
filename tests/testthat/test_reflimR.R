library(testthat)

context("adjust_digits.R")
context("bowley.R")
context("conf_int.R")
context("iBoxplot.R")
context("lognorm.R")
context("permissible_uncertainty.R")
context("reflim.R")
context("ri_hist.R")
context("truncated_qqplot.R")

dataset1  <- livertests$ALB
dataset2  <- 5.4321
dataset_false1 <- "A"

test_that("Checking for correct results",{

  expect_equal(adjust_digits(dataset2)$x.round, 5.43)
  expect_equal(adjust_digits(dataset2)$digits, 2)

  expect_equal(bowley(dataset1), 0.01945525)
})

test_that("Checking the shape of the result",{

  expect_is(adjust_digits(dataset2)$x.round, "numeric")
  expect_is(adjust_digits(dataset2)$digits, "numeric")

  expect_is(bowley(dataset1), "numeric")

  expect_is(conf_int95(n = 250, lower.limit = 10, upper.limit = 50)[1], "numeric")
  expect_is(conf_int95(n = 250, lower.limit = 10, upper.limit = 50)[2], "numeric")
  expect_is(conf_int95(n = 250, lower.limit = 10, upper.limit = 50)[3], "numeric")
  expect_is(conf_int95(n = 250, lower.limit = 10, upper.limit = 50)[4], "numeric")

  expect_is(permissible_uncertainty(lower.limit = 10, upper.limit = 50)[1], "numeric")
  expect_is(permissible_uncertainty(lower.limit = 10, upper.limit = 50)[2], "numeric")
  expect_is(permissible_uncertainty(lower.limit = 10, upper.limit = 50)[3], "numeric")
  expect_is(permissible_uncertainty(lower.limit = 10, upper.limit = 50)[4], "numeric")
})

test_that("Checking error messages",{

  expect_error(adjust_digits(dataset_false))

  expect_error(bowley(dataset_false))

  expect_error(conf_int95(dataset_false))

  expect_error(iboxplot(dataset_false))

  expect_error(lognorm(dataset_false))

  expect_error(permissible_uncertainty(dataset_false))

  expect_error(reflim(dataset_false))

  expect_error(ri_hist(dataset_false))

  expect_error(truncated_qqplot(dataset_false))
})
