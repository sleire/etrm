# evaluate an implementation of the hedging strategies on the supplied powcal data set

head(powcal)

# example parameter settings for players active last 500 trading days prior
# to expiry of the yearly base load contracts in powcal
q <- 30
tper <- 0.1
vol <- 0.2
tdays <- 250
daysleft <- 500

# run portfolios for market participants with long and short exposure, store in list
portf_results <- list()
for (j in 2:ncol(powcal)){

  d <- tail(powcal[,1][!is.na(powcal[,j])], daysleft)
  f <- tail(powcal[,j][!is.na(powcal[,j])], daysleft)

  # long
  long_list <- list(
    shpi_seller = shpi(-q, f = f, tdate = d, tper =  - tper, daysleft = length(f)),
    slpi_seller = slpi(-q, f, tdate = d, tper = -  tper),
    cppi_seller = cppi(-q, f, tdate = d, tper = -  tper),
    dppi_seller = dppi(-q, f, tdate = d, tper = -  tper),
    obpi_seller = obpi(-q, f, tdate = d, vol = vol, daysleft = length(f))
  )

  # short
  short_list <- list(
    shpi_buyer = shpi(q, f, tdate = d, tper = tper, daysleft = length(f)),
    slpi_buyer = slpi(q, f, tdate = d, tper = tper,),
    cppi_buyer = cppi(q, f, tdate = d, tper = tper),
    dppi_buyer = dppi(q, f, tdate = d, tper = tper),
    obpi_buyer = obpi(q, f, tdate = d, vol = vol, daysleft = length(f))
  )

  # clean up
  portf_results <- c(portf_results, long_list, short_list)
  rm(short_list, long_list, f, d, j)
}

# update portfolio names with contract names, assuming number of short portfolios = number of long portfolios
names(portf_results) <- paste(
  rep(colnames(powcal[, 2:ncol(powcal)]), each= length(portf_results)/ncol(powcal[, 2:ncol(powcal)])),
  names(portf_results),
  sep = "_")

# the backtested strategies
length(portf_results)
names(portf_results)

# inspect one of the 110 strategy objects in the list and the generic class methods "plot", "summary", "show"
str(portf_results[[10]])

plot(portf_results[[10]], title = "", legend = "right")
summary(portf_results[[10]])
head(show(portf_results[[10]]))
tail(show(portf_results[[10]]))

