#' Constant Proportion Portfolio Insurance (CPPI)
#'
#' Implements CPPI strategy for commodity price risk management
#' @param q numeric value for quantity to be hedged, either positive (net buyer) or negative (net seller)
#' @param tdate numeric values as dates
#' @param f numeric futures price vector
#' @param tper numeric target price markup/down to the price on the first trading day
#' @param rper numeric risk factor as a percentage of the price on the first trading day
#' @param tcost numeric transaction costs pr unit
#' @param int TRUE/FALSE integer restriction on tradable volume
#' @return Data frame with strategy results
#' @export

cppi<-function(
  q,
  tdate,
  f,
  tper = 0.1,
  rper = 0.1,
  tcost = 0,
  int = TRUE
  ){

  # define vectors
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

  # expression definitions for positive q (net buyer)
  # and negative q (net seller)
  if(q>0){
    test1<-expression(tp-pp[1])
    test2<-expression(1-(tp-pp[1])/rf[1])
    test3<-expression(tp-pp[t-1])
    test4<-expression(1-(tp-pp[t-1])/rf[t-1])
  } else {
    test1<-expression(pp[1]-tp)
    test2<-expression(1-(pp[1]-tp)/rf[1])
    test3<-expression(pp[t-1]-tp)
    test4<-expression(1-(pp[t-1]-tp)/rf[t-1])
  }

  # define rf and tp
  rf<-f[1]*rper
  tp<-f[1]*(1+tper)

  # test of model selection
  if(length(rf)==1){
    rf<-rep(rf,length(f))                             # CPPI model
  } else {
    stopifnot(length(rf)==length(f))                  # check validity of rf for vector for DPPI
  }

  # t=1
  pp[1]<-f[1]                           # t=1 initial portfolio price

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
  }

  # create an instance of the CPPI class
  out <- new("CPPI",
             Name="CPPI",
             Volume=q,
             TargetPrice=tp,
             RiskFactor=rf,
             TransCost=tcost,
             TradeisInt=int,
             Results=data.frame(
               Date=tdate,
               Price=f,
               Traded=tr,
               Exposed=exp,
               Hedged=h,
               HedgeRate=hper,
               PortfPrice=pp
               )
             )

  # return CPPI object
  return(out)
}
