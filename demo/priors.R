library(ggplot2)

trigprior <- function(tdate, edate, prior_par = c(0,0,0,0,0)){

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

# use the function
tdate <- as.Date("2013-05-13")
edate <- as.Date("2018-12-31")
viz_par <- c(15, 0.03, 2.437, 4.366, 2)

pr <- trigprior(tdate, edate, viz_par)
ggplot(pr, aes(x = Date, y = prior)) + geom_line()
