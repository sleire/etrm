# test overlapping
ov <- powfutures130513[c(21,22,23,24,29), ]
ov$Include <- TRUE

debug_fwdov <- function(){
  fwdov <- msfc(tdate = as.Date("2013-05-13"),
                include = ov$Include,
                contract = ov$Contract,
                sdate = ov$Start,
                edate = ov$End,
                f = ov$Closing)
}

#mean(fwdov@Results$MSFC[fwdov@Results$Date>= as.Date('2014-01-01') & fwdov@Results$Date<=as.Date('2014-12-31')])
