#' Maximum Smoothness Forward Curve (MSFC)
#'
#' Creates a smooth forward curve from futures prices for a flow delivery
#' @param tdate trading date
#' @param include logical vector to determine if contracts should be included in calculation
#' @param contract vector with contract names
#' @param sdate date vector with contract delivery start dates
#' @param edate date vector with contract delivery end dates
#' @param f numeric vector with futures contract prices
#' @param prior numeric vector with prior forward price curve
#' @return instance of the MSFC class
#' @importFrom utils head
#' @importFrom methods new
#' @export
#' @examples
#' # calculate forward curve for synthetic futures contracts, without prior
#'
#' # date for curve calculation and contract information
#'tdate <- as.Date("2021-06-17")
#'include <- rep(TRUE, 10)
#'contract <- c("JUL-21", "AUG-21", "SEP-21", "OCT-21", "NOV-21", "DEC-21",
#'"Q1-22", "Q2-22", "Q3-22", "Q4-22")
#'
#'sdate <- as.Date(c("2021-07-01", "2021-08-01", "2021-09-01", "2021-10-01",
#'"2021-11-01", "2021-12-01", "2022-01-01", "2022-04-01", "2022-07-01", "2022-10-01"))
#'
#'edate <- as.Date(c("2021-07-30", "2021-08-31", "2021-09-30", "2021-10-31",
#'"2021-11-30", "2021-12-31", "2022-03-31", "2022-06-30", "2022-09-30", "2022-12-31"))
#'
#'f <- c(32.55, 32.50, 32.50, 32.08, 36.88, 39.80, 39.40, 25.20, 21.15, 29.50)
#'
#'fwd_curve <- msfc(tdate = tdate,
#'include = include,
#' contract = contract,
#' sdate = sdate,
#' edate = edate,
#' f = f)

msfc <- function(
  tdate,
  include,
  contract,
  sdate,
  edate,
  f,
  prior = 0
  ){

  # validation of arguments

  # missing arguments
  if (missing(tdate))
    stop("No trading date specified")

  if (missing(include))
    stop("No include vector specified")

  if (missing(contract))
    stop("No contract vector specified")

  if (missing(sdate))
    stop("No sdate vector specified")

  if (missing(edate))
    stop("No edate vector specified")

  if (missing(f))
    stop("No price vector f specified")

  # invalid arguments
  if (length(tdate) != 1)
    stop("Trading date tdate must be single date")

  if (class(tdate) != "Date")
    stop("Trading date tdate must be of type date")

  vlist <- list(include, contract, sdate, edate, f)
  if (length(unique(lengths(vlist))) != 1)
    stop("Vectors include, contract, sdate, edate, and f must be of equal length")

  if(any(class(sdate)!="Date"))
    stop("Elements in vector sdate must be of type date")

  if(any(class(edate)!="Date"))
    stop("Elements in vector edate must be of type date")



  BenchSheet <- data.frame(
    Include = include,
    Contract = contract,
    From = sdate,
    To = edate,
    Price = f
  )

  bench <- subset(BenchSheet,BenchSheet$Include==TRUE)

  sdate <- bench$From
  edate <- bench$To
  f <- bench$Price

  # do not allow contracts that have edate-sdate < 1
  if(any(as.numeric(edate-sdate) < 1))
    stop("Contract edate cannot be smaller than sdate")

  # date vector, time vector (in years) and knots
  Date <- seq(tdate,max(edate),by="day")
  tv <- as.numeric((Date-tdate)/365)

  # prior function evaluation
  if (length(prior) > 1 & length(prior) < length(tv)){
    # return error when prior is not constant and prior length < necessary time period for calculation
    stop("Prior vector cannot be shorter than time interval [tdate, max(edate)] for included contracts")

  } else if (length(prior) > 1 & length(prior) >= length(tv)){
    # select subset of provided prior relevant for contracts that are included in calculation
    prior = head(prior, length(tv))

  } else {
    # constant prior, ex. default = 0
    prior = rep(prior, length(tv))

  }

  # TODO: consider moving k below tcs, tce, tc (depend on those)
  k <- as.numeric(sort((c((sdate-tdate),(edate-tdate))))/365)
  k <- k[!duplicated(k)]
  #k[1] <-0
  k <- c(0, k)

  # contract start/ end point and length in years
  # TODO: evaluate tc vs tc+1
  tcs <- as.numeric((sdate-tdate)/365)
  tce <- as.numeric((edate-tdate)/365) #+ 0.00000027397259581841
  tc <- as.numeric((edate-sdate)/365)

  #####################
  # for daily contracts
  # TODO: consider removing
  #tc <- ifelse(tc==0, 0.00000027397259581841, tc)
  #k <- sort(c(tcs, tce))
  #k[1] <- 0 # alternatively k <- c(0, k)
  ########################

  # number of polynomials (n) and contracts (m)
  n <- length(k) - 1
  m <- length(f)

  # build (5nx5n) matrix H
  H <- matrix(0,nrow=5*n,ncol=5*n)

  # create n hj matrices
  ix <- 1
  for (j in 1:n) {
    hj <- matrix(c(28.8*(k[j+1]-k[j])**5,
                 18*(k[j+1]-k[j])**4,
                 8*(k[j+1]-k[j])**3,
                 0,
                 0,
                 18*(k[j+1]-k[j])**4,
                 12*(k[j+1]-k[j])**3,
                 6*(k[j+1]-k[j])**2,
                 0,
                 0,
                 8*(k[j+1]-k[j])**3,
                 6*(k[j+1]-k[j])**2,
                 4*(k[j+1]-k[j]),
                 0,0,0,0,0,0,0,0,0,0,0,0),nrow=5,ncol=5)
    # insert hj's at h(5j-4,5j-4) in H
    H[ix:(ix+5-1),ix:(ix+5-1)] <- hj
    ix <- ix + 5
  }

  # build (3n+m-2)x5n matrix A
  A <- matrix(0,nrow=3*n+m-2,ncol=5*n)

  # insert knot constraints into A
  co <- 1
  ro <- 1
  for (j in 2:n) {
    # continuity of the function at the knots
    A[ro,co:(co+9)] <- c(-k[j]**4, -k[j]**3, -k[j]**2, -k[j], -1,
                         k[j]**4,  k[j]**3, k[j]**2, k[j], 1)

    # continuity of the first derivative at the knots
    A[(ro+1),co:(co+9)] <- c(-4*k[j]**3, -3*k[j]**2, -2*k[j], -1, 0,
                             4*k[j]**3, 3*k[j]**2, 2*k[j], 1, 0)

    # continuity of the second derivative at the knots
    A[(ro+2),co:(co+9)] <- c(-12*k[j]**2, -6*k[j], -2, 0, 0,
                             12*k[j]**2, 6*k[j], 2, 0, 0)
    co <- co + 5
    ro <- ro + 3
  }

  # insert vector with end constraint for last element in k into A
  A[(3*(n-1)+1),] <- c(rep(0,5*(n-1)), 4*k[length(k)]**3, 3*k[length(k)]**2,
                       2*k[length(k)], 1, 0)

  # insert m price constraints into A
  ix <- 3*(n-1)+2
  for (i in 1:m){
    for (j in 2:(n+1)){
      if (tce[i] > k[j-1] & tcs[i] < k[j]){
        # insert price constraints into contract periods only
        t1 <- max(tcs[i], k[j-1])
        t2 <- min(k[j], tce[i])
        A[ix,((j-2)*5+1):((j-2)*5+5)] <- c(1/5*(t2**5-t1**5),
                                           1/4*(t2**4-t1**4),
                                           1/3*(t2**3-t1**3),
                                           1/2*(t2**2-t1**2),
                                           t2-t1)
      }
    }
   ix <- ix + 1
  }

  # build initial (3n+m-2) vector B
  B <- c(rep(0,3*(n-1)),0,f*tc)

  ##############################################################################

  # create pri_dat data frame for prior function calc for contracts to adjust B
  pri_dat <- data.frame(tv = tv, prior = prior)

  con_pri <- NULL
  for (c in 1:m){
    con_pri <- c(con_pri, mean(pri_dat[pri_dat$tv >= tcs[c] & pri_dat$tv <= tce[c],]$prior)*tc[c])
  }
  con_pri <- c(rep(0,3*(n-1)),0,con_pri)

  # adjust initial B
  B <- B - con_pri

  ##############################################################################

  # solve equations with Lagrange: x'Hx + a'(Ax-B)
  # |2H A'| |x| = |0|
  # |A  0 | |a|   |B|
  L <- cbind(rbind(2*H,A),rbind(t(A),matrix(0,(3*n+m-2),(3*n+m-2))))
  R <- c(rep(0,5*n),B)
  S <- solve(L,R)
  x <- S[1:(5*n)]
  a <- setdiff(S,x)

  # construct the MSFC with n polynomials
  # time delta dtv, numeric equivalent of 0.0001 day
  # dtv <- as.numeric(Date[1]-(Date[1]+0.0001))/365
  dtv <- 0.00000027397259581841
  tvo <- tv + dtv
  ki <- k*365
  MSFC <- vector(length(Date), mode = "numeric")
  st <- ki[1] + 1
  xi <- 1
  ###################################

  # data frame for daily msfc calculation
  cdat <- data.frame(tv, tvo) #, a = NA, b = NA, c = NA, d = NA, e = NA)

  # add spline coefficient to list
  spli_coef <- list()
  for (s in 1:n) {
    spli_coef[[s]] <- c(x[xi], x[xi+1], x[xi+2], x[xi+3], x[xi+4])
    xi <- xi + 5
  }

  # add spline nr and spline coefficients to cdat
  spline_count <- n
  for (i in length(k):2) {
    cdat$spline <- ifelse(cdat$tv <= k[i], i - 1, cdat$spline)
    cdat$a <- ifelse(cdat$tv <= k[i], spli_coef[[spline_count]][1], cdat$a)
    cdat$b <- ifelse(cdat$tv <= k[i], spli_coef[[spline_count]][2], cdat$b)
    cdat$c <- ifelse(cdat$tv <= k[i], spli_coef[[spline_count]][3], cdat$c)
    cdat$d <- ifelse(cdat$tv <= k[i], spli_coef[[spline_count]][4], cdat$d)
    cdat$e <- ifelse(cdat$tv <= k[i], spli_coef[[spline_count]][5], cdat$e)
    spline_count <- spline_count - 1
  }

  # calculate daily msfc values
  MSFC <- (cdat$a/5 * (cdat$tvo**5 - cdat$tv**5) +
    cdat$b/4 * (cdat$tvo**4 - cdat$tv**4) +
    cdat$c/3 * (cdat$tvo**3 - cdat$tv**3) +
    cdat$d/2 * (cdat$tvo**2 - cdat$tv**2) +
    cdat$e * (cdat$tvo - cdat$tv))/(cdat$tvo - cdat$tv)

  # TODO: evaluate if length date vs msfc is redundant
  if (length(Date > length(MSFC))){
    MSFC <- c(MSFC, rep(NA,(length(Date)-length(MSFC))))
  }

  # add the prior function
  MSFC <- MSFC + prior

  Results <- data.frame(Date,MSFC)

  # add futures contracts to the results data frame
  for (i in 1:m){
    fut <- as.numeric(rep(NA,length(Date)))
    fut[Date >= sdate[i] & Date <= edate[i]] <- f[i]
    Results <- cbind(Results,fut)
  }

  colnames(Results) <- c("Date","MSFC", bench$Contract)
  #colnames(Results) <- c("Date","MSFC",paste0("F",1:m))

  # calculation details to CalcDat data frame
  CalcDat <- cbind(Date = Results$Date, cdat, Results[, 2:ncol(Results)])

  # get computed prices for contracts used in bench
  # all splines to be used in comp for all contracts
  adat <- CalcDat[CalcDat$tv %in% k, ]

  # find subset for contract c from tcs and tce
  Comp <- NULL
  CompAvg <- NULL
  for (c in 1:m){
    cadat <- adat[adat$tv >= tcs[c] & adat$tv <= tce[c], ]
    rows <- dim(cadat)[1]
    tcs_ <- cadat$tv[1:(rows - 1)]
    tce_ <- cadat$tv[2:rows]

    c_comp <- NULL
    for (i in 1:length(tcs_)){

      # pick spline
      c_spline <- cadat[(i + 1), 5:9]

      # computed price from spline i
      cc <-
        as.numeric((c_spline[1]/5*(tce_[i]**5-tcs_[i]**5)
                    +c_spline[2]/4*(tce_[i]**4-tcs_[i]**4)
                    +c_spline[3]/3*(tce_[i]**3-tcs_[i]**3)
                    +c_spline[4]/2*(tce_[i]**2-tcs_[i]**2)
                    +c_spline[5]*(tce_[i]-tcs_[i]))) #/(tce_[i]-tcs_[i])

      c_comp <- c(c_comp, cc)
    }

    # do calculation on all splines for the contract
    c_comp <- sum(c_comp)/(tce[c] - tcs[c])

    # add the prior
    c_comp <- c_comp + mean(pri_dat[pri_dat$tv >= tcs[c] & pri_dat$tv <= tce[c],]$prior)

    Comp <- c(Comp, c_comp)

    cavg <- mean(Results$MSFC[Results$Date >= sdate[c] & Results$Date <= edate[c]])
    CompAvg <- c(CompAvg, cavg)
  }

  # add computed prices to bench
  Comp <- round(Comp, 2)
  bench <- cbind(bench,Comp)

  # create an instance of the MSFC class
  out <- new("MSFC",
             Name="MSFC",
             TradeDate = tdate,
             BenchSheet = bench,
             Polynomials = n,
             PriorFunc = prior,
             Results = Results,
             SplineCoef = spli_coef,
             KnotPoints = k,
             CalcDat = CalcDat
  )

  # return MSFC object
  return(out)
}
