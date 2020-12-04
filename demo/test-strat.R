# parameters
q <- 100
tper <- 0.1
vol <- 0.2
tdays <- 500

# run portfolios
portf_results <- list()
for (j in 2:ncol(powcal)){

  d <- tail(powcal[,1][!is.na(powcal[,j])], tdays)
  f <- tail(powcal[,j][!is.na(powcal[,j])], tdays)

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

# use the generic class methods "plot", "summary", "show"
plot(portf_results[[5]])
summary(portf_results[[1]])
show(portf_results[[1]])


################

# test strategies on series still traded

daysleft <- tdays
traded_days <- 30

# run portfolios
portf_results_active <- list()
for (j in 2:ncol(powcal)){

  d <- head(powcal[,1][!is.na(powcal[,j])], traded_days)
  f <- head(powcal[,j][!is.na(powcal[,j])], traded_days)

  # long
  long_list <- list(
    shpi_seller = shpi(-q, f = f, tdate = d, tper =  - tper, daysleft = daysleft),
    slpi_seller = slpi(-q, f, tdate = d, tper = -  tper),
    cppi_seller = cppi(-q, f, tdate = d, tper = -  tper),
    dppi_seller = dppi(-q, f, tdate = d, tper = -  tper),
    obpi_seller = obpi(-q, f, tdate = d, vol = vol, daysleft = daysleft)
  )

  # short
  short_list <- list(
    shpi_buyer = shpi(q, f, tdate = d, tper = tper, daysleft = daysleft),
    slpi_buyer = slpi(q, f, tdate = d, tper = tper,),
    cppi_buyer = cppi(q, f, tdate = d, tper = tper),
    dppi_buyer = dppi(q, f, tdate = d, tper = tper),
    obpi_buyer = obpi(q, f, tdate = d, vol = vol, daysleft = daysleft)
  )

  # clean up
  portf_results_active <- c(portf_results_active, long_list, short_list)
  rm(short_list, long_list, f, d, j)
}

