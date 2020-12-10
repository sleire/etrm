

# simple trigonometric prior function describing seasonality in power prices
trigprior <- function(tdate, edate, prior_par = c(30, 0.03, 2.437, 4.366, 2)){

  # get day of year for tdate
  yr <- format(tdate, "%Y")
  doyr <- length(seq(as.Date(paste(yr, "01", "01", sep = "-")), tdate, by = "day"))
  x <- doyr:(doyr + length(seq(tdate, edate, by = "day")) - 1)

  # default prior is zero
  pri <-  prior_par[1] * exp(prior_par[2]/365 * x) +
    prior_par[3] * sin(prior_par[5] * x * pi/365) +
    prior_par[4] * cos(prior_par[5] * x * pi/365)
  data.frame(Date = seq(tdate, edate, by = "day"), prior = pri)
}

################################################################################


# forward curve from spreads data set
fwd_spreads <- msfc(tdate = as.Date("2016-11-03"),
                    include = spreads161230$Include,
                    contract = spreads161230$Contract,
                    sdate = spreads161230$Start,
                    edate = spreads161230$End,
                    f = spreads161230$Closing)

# inspect msfc object and methods
str(fwd_spreads)
summary(fwd_spreads)
head(show(fwd_spreads))
plot(fwd_spreads)


# forward curve from power futures data set at 2013-05-13
fwd_fut_nopri <- msfc(tdate = as.Date("2013-05-13"),
                  include = powfutures130513$Include,
                  contract = powfutures130513$Contract,
                  sdate = powfutures130513$Start,
                  edate = powfutures130513$End,
                  f = powfutures130513$Closing,
                  prior = 0)

# inspect msfc object and methods
str(fwd_fut_nopri)
summary(fwd_fut_nopri)
head(show(fwd_fut_nopri))
plot(fwd_fut_nopri)


# forward curve from power futures data set at 2013-05-13 with trigonometric prior
fwd_fut_wpri <- msfc(tdate = as.Date("2013-05-13"),
               include = powfutures130513$Include,
               contract = powfutures130513$Contract,
               sdate = powfutures130513$Start,
               edate = powfutures130513$End,
               f = powfutures130513$Closing,
               prior = trigprior(as.Date("2013-05-13"), max(powfutures130513$End))$prior)

# inspect msfc object and methods
str(fwd_fut_wpri)
summary(fwd_fut_wpri)
head(show(fwd_fut_wpri))
plot(fwd_fut_wpri)
