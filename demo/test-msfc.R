

debug_fwd1 <- function(){
fwd <- msfc(tdate = as.Date("2016-11-03"),
            include = spreads161230$Include,
            contract = spreads161230$Contract,
            sdate = spreads161230$Start,
            edate = spreads161230$End,
            f = spreads161230$Closing)
}

debug_fwd2 <- function(){
fwd2 <- msfc(tdate = as.Date("2013-05-13"),
            #include = powfutures130513$Include,
            include = c(powfutures130513$Include[1:33], rep(TRUE, 5)),
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
             f = powfutures140909$Closing)
}

shdat <- head(powfutures140909, 17)

debug_fwd4 <- function(){
  fwd4 <- msfc(tdate = as.Date("2014-09-09"),
               #include = shdat$Include,
               include = rep(TRUE, 17),
               contract = shdat$Contract,
               sdate = shdat$Start,
               edate = shdat$End,
               f = shdat$Closing)
}
