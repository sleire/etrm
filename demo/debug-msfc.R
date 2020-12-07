
# test overlapping
ov <- powfutures130513[c(21,22,23,24,29), ]


fwdov <- msfc(tdate = as.Date("2013-05-13"),
              include = c(FALSE,TRUE,FALSE,TRUE,TRUE),
              contract = ov$Contract,
              sdate = ov$Start,
              edate = ov$End,
              f = ov$Closing)


    # all splines to be used in comp for all contracts
    dat <- fwdov@CalcDat[fwdov@CalcDat$tv %in% fwdov@KnotPoints,]
    m <- 3
    tcs <- fwdov@BenchSheet$tcs
    tce <- fwdov@BenchSheet$tce

    # find subset for contract c from tcs and tce
    comp <- NULL
    for (c in 1:m){
      cdat <- dat[dat$tv >= tcs[c] & dat$tv <= tce[c], ]
      rows <- dim(cdat)[1]
      tcs_ <- cdat$tv[1:(rows - 1)]
      tce_ <- cdat$tv[2:rows]

      c_comp <- NULL
      for (i in 1:length(tcs_)){

        # pick spline
        c_spline <- cdat[(i + 1), 5:9]

        # computed price from spline i
        cc <-
          as.numeric((c_spline[1]/5*(tce_[i]**5-tcs_[i]**5)
           +c_spline[2]/4*(tce_[i]**4-tcs_[i]**4)
           +c_spline[3]/3*(tce_[i]**3-tcs_[i]**3)
           +c_spline[4]/2*(tce_[i]**2-tcs_[i]**2)
           +c_spline[5]*(tce_[i]-tcs_[i]))) #/(tce_[i]-tcs_[i])

        c_comp <- c(c_comp, cc)
        print(c_comp)
      }

      c_comp <- sum(c_comp)/(tce[c] - tcs[c])
      comp <- c(comp, c_comp)
    }
    comp


# 0.0000000 0.6383562 0.8849315 1.1315068 1.3863014 1.6356164

v1 <- c(1,3,2,4)
v2 <- c(1,3,2,)
v3 <- c(1,34)
vlist <- list(v1, v2, v3)















#
#
# debug_fwdov2 <- function(){
#   fwdov2 <- msfc(tdate = as.Date("2013-05-13"),
#                 include = ov$Include,
#                 contract = ov$Contract,
#                 sdate = ov$Start,
#                 edate = ov$End,
#                 f = ov$Closing)
# }
#
# #mean(fwdov@Results$MSFC[fwdov@Results$Date>= as.Date('2014-01-01') & fwdov@Results$Date<=as.Date('2014-12-31')])
#
# # find overlapping contracts
# ov <- powfutures130513[c(21:24,29), c(2, 3, 4)]
#
# get_overlaps <- function(dat){
#   dat$ndays <- dat$End - dat$Start + 1
#   dat <- dat[order(- dat$ndays), ]
#   #candidates <- subset(dat, dat$)
#
# }
#
#
# unique(ov$End)
# subset(ov, End %in% unique(ov$End)[4])
#
# subset(powfutures130513, End %in% powfutures130513$End[duplicated(powfutures130513$End)])
#
# subset(powfutures140909, End %in% powfutures140909$End[duplicated(powfutures140909$End)])



