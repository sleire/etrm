#' Maximum Smoothness Forward Curve (MSFC)
#'
#' Creates a smooth forward curve from futures prices for a flow delivery
#' @param tdate trading date
#' @param include logical vector to determine if contracts should be included in calculation
#' @param sdate date vector with contract delivery start dates
#' @param edate date vector with contract delivery end dates
#' @param f numeric vector with futures contract prices
#' @return instance of the MSFC class
#' @import lubridate
#' @export


msfc <- function(
  tdate,
  include,
  sdate,
  edate,
  f
  # prior_par = c(0,0,0,0,0)
  ){

  BenchSheet <- data.frame(
    Include = include,
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
  k <- sort((c((sdate-tdate),(edate-tdate))))/365
  k <- k[!duplicated(k)]
  k[1] <-0

  # number of polynomials (n) and contracts (m)
  n <- length(k) - 1
  m <- length(f)

  # contract start/ end point and length in years
  tcs <- as.numeric((sdate-tdate)/365)
  tce <- as.numeric((edate-tdate)/365)
  tc <- as.numeric((edate-sdate)/365)

  # # trigonometric prior function
  # trigprior <- function(x, prior_par){
  #   # ex prior_par <- c(35, 0.03, 2.437, 4.366, 2)
  #   pri <-  prior_par[1] * exp(prior_par[2]/365 * x) +
  #     prior_par[3] * sin(prior_par[5] * x * pi/365) +
  #     prior_par[4] * cos(prior_par[5] * x * pi/365)
  #   pri
  # }
  #
  # # start numeric date vector for prior on tdate date's day number
  # tfrom <-lubridate::yday(tdate)
  # tpri <- tfrom:(tfrom + length(Date) -1)
  # prior <- trigprior(tpri, prior_par)

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
        # insert price contraints into contract periods only
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
  # ta med sinuskurven her..
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
  # dtv <- 0.00000027397259581841
  ki <- k*365
  MSFC <- vector(length(Date), mode = "numeric")
  #MSFC <- NULL
  st <- ki[1] +1
  xi <- 1
  for (s in 1:n) {
    MSFC[st:(ki[s+1])] <- (x[xi]*tv[(st):(ki[s+1])]**4
             + x[xi+1]*tv[(st):(ki[s+1])]**3
             + x[xi+2]*tv[(st):(ki[s+1])]**2
             + x[xi+3]*tv[(st):(ki[s+1])]
             + x[xi+4])
    st <- ki[s+1]
    xi <- xi + 5
   # MSFC <- append(MSFC,po)
  }


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

  # get computed prices for contracts used in bench
    Comp <- NULL
    for (i in 1:length(f)){
      c <- mean(Results$MSFC[Results$Date >= sdate[i] & Results$Date <= edate[i]])
      Comp <- append(Comp, c)
    }


  bench <- cbind(bench,Comp)


  # create an instance of the MSFC class
  out <- new("MSFC",
             Name="MSFC",
             TradeDate = tdate,
             BenchSheet = bench,
             Polynomials = n,
             PriorFunc = FALSE,
             Results = Results
  )

  # return MSFC object
  return(out)
}

# s <-1
# po <- (x[1]/5*((tv[(st):(ki[s+1])]+dtv)**5-tv[(st):(ki[s+1])]**5)
#        + x[2]/4*((tv[(st):(ki[s+1])]+dtv)**4-tv[(st):(ki[s+1])]**4)
#        + x[3]/3*((tv[(st):(ki[s+1])]+dtv)**3-tv[(st):(ki[s+1])]**3)
#        + x[4]/2*((tv[(st):(ki[s+1])]+dtv)**2-tv[(st):(ki[s+1])]**2)
#        + x[5]*((tv[(st):(ki[s+1])]+dtv)-tv[(st):(ki[s+1])]))/dtv
#
# ffc <- NULL
# for (i in 2:93){
#   d <- (x[1]/5*((tv[i]+dtv)**5-tv[i]**5) +
#     x[2]/4*((tv[i]+dtv)**4-tv[i]**4) +
#     x[3]/3*((tv[i]+dtv)**3-tv[i]**3) +
#     x[4]/2*((tv[i]+dtv)**2-tv[i]**2) +
#     x[5]*((tv[i]+dtv)-tv[i]))/(dtv)
#   ffc <- append(ffc, d)
#
# }
