#' Step Hedge Portfolio Insurance (SHPI)
#'
#' Implements SHPI strategy for price risk management
#' @param Q numeric value for quantity to be hedged
#' @param P numeric futures price vector
#' @param tcost numeric transaction costs pr unit
#' @param int TRUE/ FALSE integer restriction on tradable volume
#' @return Data frame with strategy results
#' @export
ShpiStrat <- function(Q,P,tcost=0,int=TRUE){
  stopifnot(Q>0,P>0,tcost>=0)

  if (int==FALSE){
    # model without tradeable volume restrictions (int=FALSE)
    digits<-10
  }
  else {
    # model with smallest tradeable volume unit = 1 (int=TRUE)
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
