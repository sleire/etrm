##############################################################################
# Black76 delta hedge strategy
# Q       - quantity to be hedged
# P       - price vector
# K       - strike
# vol     - volatility
# R       - interest rate
# tdays   - assumed number of trading days pr year
# tcost   - transaction costs pr unit
# min     - minimum quantity to be hedged at delivery start
# integer - integer restriction on tradable volume TRUE/ FALSE
##############################################################################

deltaHedge<-function (Q,P,K=P[1],vol,R=0,tdays=250,tcost=0,min=Q,integer=TRUE){
  stopifnot(Q>0,Q>=min,P>0,K>=0,vol>0,R>=0,tdays>200,tcost>=0,min>0)

  if (integer==FALSE){
    # model without tradeable volume restrictions (integer=FALSE)
    digits<-10
  }
  else {
    # model with smallest tradeable volume unit = 1 (integer=TRUE)
    digits<-0
  }

  # remaining time in years and d1 from Black76
  t<-(length(P)-cumsum(P/P)+1)/tdays
  d1<-(log(P/K)+(0.5*vol^2*t))/(vol*sqrt(t))

  # calculation of hedge, trades, exposed volume and portfolio price
  h<-round(Q*pnorm(d1)*exp(-R*t),digits)          # hedged volumes
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
