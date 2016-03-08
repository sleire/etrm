#' Dynamic Proportion Portfolio Insurance (DPPI)
#'
#' Implements DPPI strategy for price risk management
#' @param Q numeric value for quantity to be hedged
#' @param P numeric futures price vector
#' @param tper numeric target price markup
#' @param RF numeric risk factor vector
#' @param tcost numeric transaction costs pr unit
#' @param min numeric minimum quantity to be hedged at delivery
#' @param int TRUE/ FALSE integer restriction on tradable volume
#' @return Data frame with strategy results
#' @export
DppiStrat<-function (Q,P,tper=1.15,RF=10,tcost=0,min=Q,int=TRUE){

  # test of model selection
  if (length(RF)==1){
    # CPPI model
    RF<-rep(RF,length(P))
  }
  else{
    # check validity of RF vector for DPPI
    stopifnot(length(RF)==length(P))
  }

  stopifnot(Q>0,Q>=min,P>0,tper>=1.1,tcost>=0,min>0)

  if (int==FALSE){
    # model without tradeable volume restrictions (int=FALSE)
    digits<-10
  }
  else {
    # model with smallest tradeable volume unit = 1 (int=TRUE)
    digits<-0
  }

  # define vectors
  TP<-vector(length=length(P),mode="numeric")
  pp<-vector(length=length(P),mode="numeric")
  h <-vector(length=length(P),mode="numeric")
  tr<-vector(length=length(P),mode="numeric")
  hper<-vector(length=length(P),mode="numeric")
  exp<-vector(length=length(P),mode="numeric")
  ch<-vector(length=length(P),mode="numeric")

  pp[1]<-P[1]                           # t=1 initial portfolio price
  TP[1]<-pp[1]*tper                     # t=1 initial TP

  if (TP[1]-pp[1]>RF[1]){
    h[1]<-0                             # t=1 hedge
  } else {
    h[1]<-round((1-(TP[1]-pp[1])/RF[1])*Q,digits)
  }

  tr[1]<-h[1]                           # t=1 transaction
  exp[1]<-Q-h[1]                        # t=1 exposure
  hper<-h[1]/Q                          # t=1 hedged %
  ch[1]<-(P[1]+tcost*sign(tr[1]))*tr[1] # t=1 hedge cost
  pp[1]<-(P[1]*exp[1]+ch[1])/Q          # t=1 portfolio price with tcost
  #TP[1]<-pp[1]*tper                     # t=1 TP with tcost

  for (t in 2:(length(P)-1)){           # t=2,..,(T-1) hedge
    if (TP[t-1]-pp[t-1]>RF[t-1]){
      h[t]<-0
    } else if (TP[t-1]-pp[t-1]<0){
      h[t]<-Q
    } else {
      h[t]<-round((1-(TP[t-1]-pp[t-1])/RF[t-1])*Q,digits)
    }

    tr[t]<-h[t]-h[t-1]                  # t=2,..,(T-1) transaction,
    exp[t]<-Q-h[t]                      # exposure,
    hper[t]<-h[t]/Q                     # hedged %
    ch[t]<-ch[t-1]+(P[t]+sign(tr[t])*tcost)*tr[t]
    pp[t]<-(P[t]*exp[t]+ch[t])/Q        # hedge cost and portfolio price
    TP[t]<-min(pp[t]*tper,TP[t-1])
  }

  h[length(P)]<-min                     # t=T hedge, transaction,
  tr[length(P)]<-h[length(P)]-h[length(P)-1]
  exp[length(P)]<-Q-h[length(P)]        # exposure,
  hper[length(P)]<-h[length(P)]/Q       # hedged % and
  ch[length(P)]<-ch[length(P)-1]+       # hedge cost and portfolio price
    (P[length(P)]+sign(tr[length(P)])*tcost)*tr[length(P)]
  pp[length(P)]<-(P[length(P)]*exp[length(P)]+ch[length(P)])/Q
  TP[length(P)]<-min(pp[length(P)],pp[length(P)-1])

  # renaming of variables for presentation
  Price<-P;Traded<-tr;Exposed<-exp;Hedged<-h
  HedgeRate<-hper;PortfPrice<-pp

  # data frame with results
  return(data.frame(Price,Traded,Exposed,Hedged,HedgeRate,TP,PortfPrice))
}
