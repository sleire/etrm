context("Error handling")
library(etrm)

test_that("cppi returns appropriate errors for input validation", {

  # test data
  q <- 100
  tdate <- seq(as.Date("2020-02-02"), by = "day", length.out = 10)
  f <- abs(rnorm(10))*15
  tper <- 0.1

  # missing arguments
  expect_error( cppi(tdate, f, tper) )
  expect_error( cppi(q, f, tper) )
  expect_error( cppi(q, tdate, tper) )
  expect_error( cppi(q, tdate, f) )

  # invalid arguments
  expect_error( cppi(q,tdate, f, tper, tcost = -1) )
  expect_error( cppi(q, head(tdate, 2), f, tper) )
  expect_error( cppi(q, tdate, f,  tper = 0) )
  expect_error( cppi(q = -1, tdate, f, tper) )

})
