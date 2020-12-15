
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
fwd_fut_wpri1 <- msfc(tdate = as.Date("2013-05-13"),
               include = powfutures130513$Include,
               contract = powfutures130513$Contract,
               sdate = powfutures130513$Start,
               edate = powfutures130513$End,
               f = powfutures130513$Closing,
               prior = powpriors130513$trig.prior)

# inspect msfc object and methods
str(fwd_fut_wpri1)
summary(fwd_fut_wpri1)
head(show(fwd_fut_wpri1))
plot(fwd_fut_wpri1)


# forward curve from power futures data set at 2013-05-13 with trigonometric prior
fwd_fut_wpri2 <- msfc(tdate = as.Date("2013-05-13"),
                     include = powfutures130513$Include,
                     contract = powfutures130513$Contract,
                     sdate = powfutures130513$Start,
                     edate = powfutures130513$End,
                     f = powfutures130513$Closing,
                     prior = powpriors130513$mod.prior)

# inspect msfc object and methods
str(fwd_fut_wpri2)
summary(fwd_fut_wpri2)
head(show(fwd_fut_wpri2))
plot(fwd_fut_wpri2)


