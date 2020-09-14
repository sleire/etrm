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

test_that("dppi returns appropriate errors for input validation", {

  # test data
  q <- 100
  tdate <- seq(as.Date("2020-02-02"), by = "day", length.out = 10)
  f <- abs(rnorm(10))*15
  tper <- 0.1

  # missing arguments
  expect_error( dppi(tdate, f) )
  expect_error( dppi(q, f, tper) )
  expect_error( dppi(q, tdate, tper) )

  # invalid arguments
  expect_error( dppi(q,tdate, f, tper, tcost = -1) )
  expect_error( dppi(q,tdate, f, tper, rper = -0.1) )
  expect_error( dppi(q, head(tdate, 2), f, tper) )
  expect_error( dppi(q, tdate, f,  tper = 0) )
  expect_error( dppi(q = -1, tdate, f, tper) )

})

test_that("obpi returns appropriate errors for input validation", {

  # test data
  q <- 100
  tdate <- seq(as.Date("2020-02-02"), by = "day", length.out = 10)
  f <- abs(rnorm(10))*15
  vol <- 0.2
  daysleft <- length(tdate)

  # missing arguments
  expect_error( obpi(tdate, f, vol, daysleft) )
  expect_error( obpi(q, f, vol, daysleft) )
  expect_error( obpi(q, tdate, vol, daysleft) )
  expect_error( obpi(q, tdate, f, vol) )

  # invalid arguments
  expect_error( obpi(q, tdate, f, vol = -0.1, daysleft) )
  expect_error( obpi(q, tdate, f, vol, daysleft = -1) )
  expect_error( obpi(q, tdate, f, vol, daysleft, tcost = -1) )
  expect_error( obpi(q, tdate, f, vol, daysleft, k = -1) )
  expect_error( obpi(q, head(tdate, 2), f, vol, daysleft) )

})

test_that("shpi returns appropriate errors for input validation", {

  # test data
  q <- 100
  tdate <- seq(as.Date("2020-02-02"), by = "day", length.out = 10)
  f <- abs(rnorm(10))*15
  daysleft <- length(tdate)

  # missing arguments
  expect_error( shpi(tdate, f, daysleft) )
  expect_error( shpi(q, f, daysleft) )
  expect_error( shpi(q, tdate, f) )

  # invalid arguments
  expect_error( shpi(q, head(tdate, 2), f, daysleft) )
  expect_error( shpi(q, tdate, f, daysleft = -1) ) # ok
  expect_error( shpi(q, tdate, f, daysleft, tcost = -1) ) #ok
  expect_error( shpi(q, tdate, f, daysleft, tper = 0) )
  expect_error( shpi(q, head(tdate, 2), f, daysleft) )

})

test_that("slpi returns appropriate errors for input validation", {

  # test data
  q <- 100
  tdate <- seq(as.Date("2020-02-02"), by = "day", length.out = 10)
  f <- abs(rnorm(10))*15
  daysleft <- length(tdate)

  # missing arguments
  expect_error( slpi(tdate, f) )
  expect_error( slpi(q, f) )
  expect_error( slpi(q, tdate) )

  # invalid arguments
  expect_error( slpi(q, had(tdate, 2), f) )
  expect_error( slpi(q, tdate, f = 1) )
  expect_error( slpi(q, tdate, f, daysleft, tcost = -1) )
  expect_error( slpi(q, tdate, f, daysleft = -1) )

})
