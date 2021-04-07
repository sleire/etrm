
# forward curve from power futures data set at 2013-05-13 without prior
fwd.fut.npri <- msfc(tdate = as.Date("2013-05-13"),
                  include = powfutures130513$Include,
                  contract = powfutures130513$Contract,
                  sdate = powfutures130513$Start,
                  edate = powfutures130513$End,
                  f = powfutures130513$Closing,
                  prior = 0)

# inspect msfc object and methods
str(fwd.fut.npri)
summary(fwd.fut.npri)
head(show(fwd.fut.npri))
plot(fwd.fut.npri)


# forward curve from power futures data set at 2013-05-13 with trigonometric prior
fwd.fut.tpri <- msfc(tdate = as.Date("2013-05-13"),
               include = powfutures130513$Include,
               contract = powfutures130513$Contract,
               sdate = powfutures130513$Start,
               edate = powfutures130513$End,
               f = powfutures130513$Closing,
               prior = powpriors130513$trig.prior)

# inspect msfc object and methods
str(fwd.fut.tpri)
summary(fwd.fut.wpri1)
head(show(fwd.fut.tpri))
plot(fwd.fut.tpri)


# forward curve from power futures data set at 2013-05-13 with model prior
fwd.fut.mpri <- msfc(tdate = as.Date("2013-05-13"),
                     include = powfutures130513$Include,
                     contract = powfutures130513$Contract,
                     sdate = powfutures130513$Start,
                     edate = powfutures130513$End,
                     f = powfutures130513$Closing,
                     prior = powpriors130513$mod.prior)

# inspect msfc object and methods
str(fwd.fut.mpri)
summary(fwd.fut.mpri)
head(show(fwd.fut.mpri))
plot(fwd.fut.mpri)


