ov <- powfutures130513[c(21,22,23,24,29), ]


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

viz_par <- c(15, 0.03, 2.437, 4.366, 2)

################################################################################

debug_fwdov <- function(){
  fwdov <- msfc(tdate = as.Date("2013-05-13"),
                include = c(FALSE,TRUE,FALSE,TRUE,TRUE),
                contract = ov$Contract,
                sdate = ov$Start,
                edate = ov$End,
                f = ov$Closing,
                #prior = 0
                #prior = c(1, 2)
                prior = trigprior(as.Date("2013-05-13"), max(ov$End), viz_par)$prior
                )
}

#################

B <- c(0, 0, 0, 0, 0, 0, 0, 0,  0,  0,  0,  0,  0,  8.233151,  9.536301, 36.330192)
m <- 3
n <- 5
pri_dat <- data.frame(tv = fwdov@CalcDat$tv, prior = fwdov@PriorFunc)
tcs <- fwdov@BenchSheet$tcs
tce <- fwdov@BenchSheet$tce

con_pri <- NULL
for (c in 1:m){
  con_pri <- c(con_pri, mean(pri_dat[pri_dat$tv >= tcs[c] & pri_dat$tv <= tce[c],]$prior))
}
con_pri <- c(rep(0,3*(n-1)),0,con_pri)
B <- B - con_pri

#####################

debug_fwd1 <- function(){
  fwd <- msfc(tdate = as.Date("2016-11-03"),
              include = spreads161230$Include,
              contract = spreads161230$Contract,
              sdate = spreads161230$Start,
              edate = spreads161230$End,
              f = spreads161230$Closing)
              #prior = trigprior(as.Date("2016-1-03"), max(spreads161230$End), viz_par)$prior)
}

debug_fwd2 <- function(){
fwd2 <- msfc(tdate = as.Date("2013-05-13"),
            include = powfutures130513$Include,
            #include = c(powfutures130513$Include[1:33], rep(TRUE, 5)),
            contract = powfutures130513$Contract,
            sdate = powfutures130513$Start,
            edate = powfutures130513$End,
            f = powfutures130513$Closing)
}

debug_fwd3 <- function(){
fwd3 <- msfc(tdate = as.Date("2014-09-09"),
             include = powfutures140909$Include,
             contract = powfutures140909$Contract,
             sdate = powfutures140909$Start,
             edate = powfutures140909$End,
             f = powfutures140909$Closing,
             prior = trigprior(as.Date("2014-09-09"), max(powfutures140909$End), viz_par)$prior)
}

shdat <- head(powfutures140909, 17)

debug_fwd4 <- function(){
  fwd4 <- msfc(tdate = as.Date("2014-09-09"),
               include = shdat$Include,
               contract = shdat$Contract,
               sdate = shdat$Start,
               edate = shdat$End,
               f = shdat$Closing)
}

# calc for  MOCT-14  is incorrect, check calc with alt splines
#subset(shdat, End %in% shdat$End[duplicated(shdat$End)])

