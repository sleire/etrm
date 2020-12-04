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
#' @export


msfc <- function(
  tdate,
  include,
  contract,
  sdate,
  edate,
  f,
  prior = 0
  ){

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

  # date vector, time vector (in years) and knots
  Date <- seq(tdate,max(edate),by="day")
  #Date <- seq(min(sdate),max(edate),by="day")
  tv <- as.numeric((Date-tdate)/365)
  # TODO: consider moving k below tcs, tce, tc (depend on those)
  k <- as.numeric(sort((c((sdate-tdate),(edate-tdate))))/365)
  k <- k[!duplicated(k)]
  k[1] <-0

  # number of polynomials (n) and contracts (m)
  n <- length(k) - 1
  m <- length(f)

  # contract start/ end point and length in years
  # TODO: evaluate tc vs tc+1
  tcs <- as.numeric((sdate-tdate)/365)
  tce <- as.numeric((edate-tdate)/365)
  tc <- as.numeric((edate-sdate)/365)

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

  # build (3n+m-2) vector B
  # TODO: include prior function and reconsider tc length
  B <- c(rep(0,3*(n-1)),0,f*tc)

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
  cdat <- data.frame(tv, tvo, a = NA, b = NA, c = NA, d = NA, e = NA)

  # add spline coefficient to list
  spli_coef <- list()
  for (s in 1:n) {
    spli_coef[[s]] <- c(x[xi], x[xi+1], x[xi+2], x[xi+3], x[xi+4])
    xi <- xi + 5
  }

  # add coefficients to cdat
  spline_count <- n
  for (i in length(k):2) {
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

  Results <- data.frame(Date,MSFC)

  # add futures contracts to the results data frame
  for (i in 1:m){
    fut <- as.numeric(rep(NA,length(Date)))
    fut[Date >= sdate[i] & Date <= edate[i]] <- f[i]
    Results <- cbind(Results,fut)
  }

  colnames(Results) <- c("Date","MSFC",paste0("F",1:m))

  # TODO: remove CalcDat?
  CalcDat <- cbind(Results$Date, cdat, Results[, 2:ncol(Results)])

  # get computed prices for contracts used in bench
  # TODO: evaluate comp, see ns
  #####################
  ns <- (match(tce,k)-1)*5-4
  Comp <- NULL
  CompAvg <- NULL
  for (i in 1:m){
    nsm <- ns[i]
    cc <-
      (x[nsm]/5*(tce[i]**5-tcs[i]**5)
      +x[nsm+1]/4*(tce[i]**4-tcs[i]**4)
      +x[nsm+2]/3*(tce[i]**3-tcs[i]**3)
      +x[nsm+3]/2*(tce[i]**2-tcs[i]**2)
      +x[nsm+4]*(tce[i]-tcs[i]))/(tce[i]-tcs[i])
    Comp <- c(Comp, cc)
    cavg <- mean(Results$MSFC[Results$Date >= sdate[i] & Results$Date <= edate[i]])
    CompAvg <- c(CompAvg, cavg)
  }
  #####################

  # TODO:reconsider rounding of Comp and CompAvg
  Comp <- round(Comp, 2)
  CompAvg <- round(CompAvg, 2)

  bench <- cbind(bench,Comp)
  bench <- cbind(bench,CompAvg)
  bench$CompAvgDiff <- bench$Price - round(bench$CompAvg, 2)

  # create an instance of the MSFC class
  out <- new("MSFC",
             Name="MSFC",
             TradeDate = tdate,
             BenchSheet = bench,
             Polynomials = n,
             PriorFunc = prior,
             Results = Results,
             SplineCoef = spli_coef,
             CalcDat = CalcDat
  )

  # return MSFC object
  return(out)
}
