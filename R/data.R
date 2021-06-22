
#' Closing prices for power futures contracts at trading date 2013-05-13
#'
#' A synthetic dataset containing the closing prices and other attributes of 38
#'  power futures contracts.
#'
#' @format A data frame with 38 rows and 5 columns:
#' \describe{
#'   \item{Include}{boolean variable to determine if contract should be included in forward curve calculation}
#'   \item{Contract}{the name of the futures contract}
#'   \item{Start}{delivery start date for the futures contract}
#'   \item{End}{delivery start date for the futures contract}
#'   \item{Closing}{the futures contract closing price}
#' }
"powfutures130513"


#' Example priors at trading date 2015-05-13
#'
#' An example of two simple priors for forward market price to be used with powfutures130513
#'
#' @format A data frame with 3885 rows and 3 columns:
#' \describe{
#'   \item{Date}{vector of dates ranging from 2013-05-13 to final end date of contracts in powfutures130513}
#'   \item{trig.prior}{a simple smooth trigonometric prior describing power price seasonality}
#'   \item{mod.prior}{a trigonometric prior adjusted for typical calendar effects}
#' }
"powpriors130513"


#' Historical daily closing prices for 11 calendar year power futures contracts
#'
#' A synthetic dataset containing the closing prices and other attributes of 11
#'  power futures contracts for calendar year delivery for 2006 - 2016.
#'
#' @format A data frame with 3253 rows and 12 columns:
#' \describe{
#'   \item{Date}{the trading date}
#'   \item{CAL-06}{the closing price for the 2006 futures contract}
#'   \item{CAL-07}{the closing price for the 2007 futures contract}
#'   \item{CAL-08}{the closing price for the 2008 futures contract}
#'   \item{CAL-09}{the closing price for the 2009 futures contract}
#'   \item{CAL-10}{the closing price for the 2010 futures contract}
#'   \item{CAL-11}{the closing price for the 2011 futures contract}
#'   \item{CAL-12}{the closing price for the 2012 futures contract}
#'   \item{CAL-13}{the closing price for the 2013 futures contract}
#'   \item{CAL-14}{the closing price for the 2014 futures contract}
#'   \item{CAL-15}{the closing price for the 2015 futures contract}
#'   \item{CAL-16}{the closing price for the 2016 futures contract}
#' }
"powcal"
