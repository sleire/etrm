#' Dynamic Proportion Portfolio Insurance (DPPI)
#'
#' Implements DPPI strategy for commodity price risk management
#' @param q numeric value for quantity to be hedged, either positive (net buyer) or negative (net seller)
#' @param tdate date vector with trading days
#' @param f numeric futures price vector
#' @param tper numeric target price factor, markup/down to the price on the first trading day
#' @param rper numeric risk factor as a percentage of the price on the first trading day
#' @param tcost numeric transaction costs pr unit
#' @param int TRUE/FALSE integer restriction on tradable volume
#' @importFrom methods new
#' @return instance of the DPPI class
#' @export
#' @examples
#'# DPPI for a buyer (seller), where stop loss is set 10% above (below) initial market price.
#'
#' set.seed(5)
#' # GBM price process parameters
#' mu <- 0.2
#' sigma <- 0.1
#' S0 <- 100
#'
#'# time
#'Y <- 2
#'N <- 500
#'delta <- Y/N
#'t <- seq (0, 1, length = N + 1)
#'
#'# price process and date vector
#'W <- c(0, cumsum ( sqrt(delta) * rnorm (N)))
#'f_gbm <- S0 * exp(mu * t + sigma * W)
#'tr_dates <- seq(Sys.Date(), Sys.Date()+500, by = "day")
#'
#'# implement dppi strategy for buyer
#'dppi_b <- dppi(q = 10,
#'tdate = tr_dates,
#'f = f_gbm,
#'tper = 0.1,
#'rper = 0.1,
#'tcost = 0,
#'int = TRUE)
#'
#'# implement dppi strategy for seller
#'dppi_s <- dppi(q = - 10,
#'tdate = tr_dates,
#'f = f_gbm,
#'tper = - 0.1,
#'rper = 0.1,
#'tcost = 0,
#'int = TRUE)
#'

dppi <- function(
  q,
  tdate,
  f,
  tper,
  rper,
  tcost = 0,
  int = TRUE
  ){

  # validation of arguments

  # missing arguments
  if (missing(q))
    stop("No volume specified")

  if (missing(tdate))
    stop("No date vector specified")

  if (missing(f))
    stop("No price vector specified")

  if (missing(tper))
    stop("No target price factor specified")

  if (missing(rper))
    stop("No risk factor specified")

  # invalid arguments
  if (tcost < 0)
    stop("Transaction cost cannot be a negative number")

  if (rper <= 0)
    stop("Risk factor must be a positive number")

  if (tper == 0)
    stop("Target price percentage cannot be zero")

  if (q < 0 & tper > 0)
    stop("A seller cannot set target price above current market")

  if (q > 0 & tper < 0)
    stop("A buyer cannot set target price below current market")

  if (length(tdate) != length(f))
    stop("Date and price vectors must be of equal length")

  # define vectors
  tp<-vector(length(f),mode="numeric")       # target price
  pp<-vector(length(f),mode="numeric")       # portfolio price
  h<-vector(length(f),mode="numeric")        # hedge
  tr<-vector(length(f),mode="numeric")       # transaction
  hper<-vector(length(f),mode="numeric")     # hedge percentage
  exp<-vector(length(f),mode="numeric")      # exposed
  ch<-vector(length(f),mode="numeric")       # hedge cost

  # volume restrictions
  if(int==FALSE){
    digits<-10        # model without tradeable volume restrictions (int=FALSE)
  } else {
    digits<-0         # model with smallest tradeable volume unit = 1 (int=TRUE)
  }

  # test of model selection
  if(length(rper)==1){
    rf<-rep(rper*f[1],length(f))      # CPPI model
  } else {
    stopifnot(length(rper)==length(f))  # check validity of rf for vector for DPPI
    rf <- rper * f
  }

  # expression definitions for positive q (net buyer)
  # and negative q (net seller)
  if(q>0){
    test1<-expression(tp[1]-pp[1])
    test2<-expression(1-(tp[1]-pp[1])/rf[1])
    test3<-expression(tp[t-1]-pp[t-1])
    test4<-expression(1-(tp[t-1]-pp[t-1])/rf[t-1])
  } else {
    test1<-expression(pp[1]-tp[1])
    test2<-expression(1-(pp[1]-tp[1])/rf[1])
    test3<-expression(pp[t-1]-tp[t-1])
    test4<-expression(1-(pp[t-1]-tp[t-1])/rf[t-1])
  }

  # t=1
  pp[1]<-f[1]                           # t=1 initial portfolio price
  tp[1]<-f[1] * (1+tper)

  if(eval(test1)>rf[1]){
    h[1]<-0
  } else {
    h[1]<-round((eval(test2)*q),digits)
  }
  tr[1]<-h[1]
  exp[1]<-q-h[1]
  hper<-abs(h[1]/q)
  ch[1]<-(f[1]+tcost*sign(tr[1]))*tr[1]
  pp[1]<-(f[1]*exp[1]+ch[1])/q

  # t=2,..,T
  for(t in 2:(length(f))){
    if(eval(test3)>rf[t-1]){
      h[t]<-0
    } else if(eval(test3)<0){
      h[t]<-q
    } else {
      h[t]<-round((eval(test4)*q),digits)
    }
    tr[t]<-h[t]-h[t-1]
    exp[t]<-q-h[t]
    hper[t]<-abs(h[t]/q)
    ch[t]<-ch[t-1]+(f[t]+sign(tr[t])*tcost)*tr[t]
    pp[t]<-(f[t]*exp[t]+ch[t])/q
    if (tper<0){
      tp[t]<-max(pp[t]*(1+tper),tp[t-1])
    } else {
      tp[t]<-min(pp[t]*(1+tper),tp[t-1])
    }
  }

  # create an instance of the DPPI class
  out <- new("DPPI",
             Name="DPPI",
             Volume=q,
             TargetPrice=unique(tp),
             RiskFactor=rf,
             TransCost=tcost,
             TradeisInt=int,
             Results=data.frame(
               Date=tdate,
               Market=f,
               Trade=tr,
               Exposed=exp,
               Position=h,
               Hedge=hper,
               Target = tp,
               Portfolio=pp
             )
  )

  # return DPPI object
  return(out)
}
