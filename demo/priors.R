library(lubridate)

# period
From <- as.Date("2014-05-01")
To <- as.Date("2020-12-31")
Date<-seq(From, To,by="day")

# start numeric date vector on From date's day number
tfrom <-lubridate::yday(From)
tt <- tfrom:(tfrom + length(Date) -1)

viz_par <- c(15, 0.03, 2.437, 4.366, 2)

trigprior <- function(x, prior_par = c(0,0,0,0,0)){
  # default prior is zero
  pri <-  prior_par[1] * exp(prior_par[2]/365 * x) +
          prior_par[3] * sin(prior_par[5] * x * pi/365) +
          prior_par[4] * cos(prior_par[5] * x * pi/365)
  pri
}

pt <- trigprior(tt,prior_par = viz_par)
pt0 <- trigprior(tt)
plot(Date,pt, type = "l")


