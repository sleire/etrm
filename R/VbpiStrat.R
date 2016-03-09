#' VaR Based Portfolio Insurance (VBPI)
#'
#' Implements VBPI strategy for commodity price risk management
#' @param Q numeric value for quantity to be hedged
#' @param P numeric futures price vector
#' @param TP numeric target price to protect
#' @param conf numeric confidence level
#' @param tcost numeric transaction costs pr unit
#' @param min numeric minimum quantity to be hedged at delivery
#' @param int TRUE/ FALSE integer restriction on tradable volume
#' @return Data frame with strategy results
#' @export
VbpiStrat<-function (Q,P,TP,conf=0.95,tcost=0,min=Q,int=TRUE){

  stopifnot(Q>0,Q>=min,TP>P[1]*1.1,P>0,conf>0,tcost>=0,min>0)

  # Mr Pignatelli...

  # renaming of variables for presentation
  Price<-P;Traded<-tr;Exposed<-exp;Hedged<-h
  HedgeRate<-hper;PortfPrice<-pp

  # data frame with results
  return(data.frame(Price,Traded,Exposed,Hedged,HedgeRate,PortfPrice))
}
