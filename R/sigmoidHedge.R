##############################################################################
# Sigmoid hedge strategy
# Q       - quantity to be hedged
# P       - price vector
# a       - risk appetite parameter
# tcost   - transaction costs pr unit
# min     - minimum quantity to be hedged at delivery start
# integer - integer restriction on tradable volume TRUE/ FALSE
##############################################################################

sigmoidHedge<-function (Q,P,a,tcost=0,min=Q,integer=TRUE){
  stopifnot(Q>0,Q>=min,P>0,tcost>=0,min>0)

  if (integer==FALSE){
    # model without tradeable volume restrictions (integer=FALSE)
    digits<-10
  }
  else {
    # model with smallest tradeable volume unit = 1 (integer=TRUE)
    digits<-0
  }

  # calculation of percentage deviation from first day's price and sigmoid
  x <- P/P[1]-1
  s <- 1/(1+exp(-a*x))

  # calculation of hedge, trades, exposed volume and portfolio price
  h<-round(Q*s,digits)          # hedged volumes
  h[length(P)]<-min                               # minimum hedge at delivery
  hper<-h/Q                                       # hedged %
  tr<-c((h[1]),diff(h))                           # traded volumes
  exp<-Q-h                                        # exposed volume
  pp<-(cumsum(tr*(P+sign(tr)*tcost))+exp*P)/Q     # portfolio price

  # renaming of variables for presentation
  Price<-P;Traded<-tr;Exposed<-exp;Hedged<-h
  HedgeRate<-hper;PortfPrice<-pp

  # data frame with results
  return(data.frame(Price,Traded,Exposed,Hedged,HedgeRate,PortfPrice))
}
