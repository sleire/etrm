# packages
library(ggplot2)
library(reshape2)

# MSFC source
source("../etrm/R/msfc_class.R")
source("../etrm/R/msfc.R")

# data
load("../etrm/data/Nasdaq130513.Rda")
load("../etrm/data/Nasdaq090914.Rda")
load("../etrm/data/contracts.Rda")

# for testing with the Nasdaq bench sheets
# bench <- contracts
# tdate <- as.Date("2016-11-03")
 bench <- Nasdaq130513
 tdate <- as.Date("2013-05-13")

#bench <-Nasdaq090914
#bench <- bench[18:25,]
#bench$Include <- TRUE
# tdate <- as.Date("2014-09-09")
#tdate <- as.Date("2014-09-30")

# inputs to msfc
include <- bench$Include
f <- bench$Closing
sdate <- bench$Start
edate <- bench$End

# create MSFC
res <- msfc(tdate,include,sdate,edate,f)
res@Results$MSFC[length(res@Results$MSFC)] <- NA

# plot res
plot(res, legent="")

# summary res
summary(res)

# show res
# show(res)

