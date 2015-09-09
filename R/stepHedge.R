##############################################################################
# Simple step hedge strategy
# Q       - quantity to be hedged
# P       - price vector
# tcost   - transaction costs pr unit
# integer - integer restriction on tradable volume TRUE/ FALSE
##############################################################################

stepHedge <- function(Q,P,tcost=0,integer=TRUE){
  stopifnot(Q>0,P>0,tcost>=0)

  if (integer==FALSE){
    # model without tradeable volume restrictions (integer=FALSE)
    digits<-10
  }
  else {
    # model with smallest tradeable volume unit = 1 (integer=TRUE)
    digits<-0
  }

  # calculation of hedge, trades, exposed vol and portfolio price
  h<-round((cumsum(P/P))/(length(P))*Q,digits)    # hedged volumes
  hper<-h/Q                                       # hedged %
  tr<-c((h[1]),diff(h))                           # traded volumes
  exp<-Q-h                                        # exposed volume
  pp<-(cumsum(tr*(P+tcost))+exp*P)/Q              # portfolio price

  # renaming of variables for presentation
  Price<-P;Traded<-tr;Exposed<-exp;Hedged<-h
  HedgeRate<-hper;PortfPrice<-pp

  # data frame with results
  return(data.frame(Price,Traded,Exposed,Hedged,HedgeRate,PortfPrice))
}
