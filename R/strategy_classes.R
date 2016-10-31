
######### classes #######

#' An S4 VIRTUAL parent class for the hedging strategy classes in hedgeR
#'
#' @Name Object of type "character" containing hedging strategy name
#' @Volume Object of type "numeric" with quantity to be hedged
#' @TargetPrice Object of type "numeric" with portfolio target price (cap or floor)
#' @TransCost Object of type "numeric" with transaction costs pr unit
#' @TradeisInt Object of type "logical" with integer restriction on tradable volume
#' @Results Object of type "data frame" with strategy results
setClass("GenericStrat",
         contains = "VIRTUAL",
         slots = c(Name = "character",
                   Volume = "numeric",
                   TargetPrice = "numeric",
                   TransCost = "numeric",
                   TradeisInt = "logical",
                   Results = "data.frame")
)

#' An S4 class for the CPPI hedging strategy
#'
#' @RiskFactor Object of type "numeric" with risk factor in CPPI model
setClass("CPPI",
         contains = "GenericStrat",
         slots = c(RiskFactor = "numeric")
)

#' An S4 class for the DPPI hedging strategy
#'
#' @TargetPercent Object of type "numeric" with portfolio target price
#' (cap or floor) set as a percentage of first trading day's market price
#' @RiskFactor Object of type "numeric" with risk factor in CPPI model
setClass("DPPI",
         contains = "GenericStrat",
         slots = c(TargetPercent = "numeric",
                   RiskFactor = "numeric")
)

#' An S4 class for the OBPI hedging strategy
#'
#'@StrikePrice Object of type "numeric" with strike price for the synthetic option
#'@AnnVol Object of type "numeric" with annualized volatility
#'@InterestRate Object of type "numeric" with interest rate
#'@TradingDays Object of type "integer" with assumed number of trading days per year
setClass("OBPI",
         contains = "GenericStrat",
         slots = c(StrikePrice = "numeric",
                   AnnVol = "numeric",
                   InterestRate = "numeric",
                   TradingDays = "numeric")
)

#' An S4 class for the SHPI hedging strategy
#'
setClass("SHPI",
         contains = "GenericStrat"
)

#' An S4 class for the SLPI hedging strategy
#'
setClass("SLPI",
         contains = "GenericStrat"
)

#' An S4 class for the VBPI hedging strategy
#'
#'@ConfLevel Object of type "numeric" with VaR confidence level
#' @TargetPercent Object of type "numeric" with portfolio target price
#' (cap or floor) set as a percentage of first trading day's market price
#'@Mean Object of type "numeric" with mean in price process
#'@StDev Object of type "numeric" with annualized volatility
#'@Horizon Object of type "integer" with trading days left
setClass("VBPI",
         contains = "GenericStrat",
         slots = c(ConfLevel = "numeric",
                   TargetPercent= "numeric",
                   Mean= "numeric",
                   StDev= "numeric",
                   Horizon= "numeric")
)


######### methods #######

#' S4 method for the show generic
#'
#'@export
setMethod("show",
          signature ="GenericStrat",
          definition=function(object) {
            Results <- object@Results
            Results
          })

#' S4 method for the summary generic
#'
#'@export
setMethod("summary",
          signature ="GenericStrat",
          definition=function(object, ...) {

            Description <- paste("Hedging strategy of type",
                                 object@Name,
                                 "and length",
                                 length(object@Results$Price),sep=" ")

            Volume <- object@Volume

            TargetPrice <- object@TargetPrice

            ChurnRate <- sum(abs(object@Results$Traded))/abs(object@Volume)

            Stats <- object@Results[1,2:8]
            Stats <- rbind(Stats, sapply(object@Results[,2:8], max))
            Stats <- rbind(Stats, sapply(object@Results[,2:8], min))
            Stats <- rbind(Stats, tail(object@Results[,2:8],1))
            rownames(Stats) <- c("First","Max","Min","Last")

            list(Description=Description,Volume=Volume,
                 TargetPrice = TargetPrice,
                 ChurnRate=ChurnRate,Stats=Stats)
          })

#' S4 method for the plot generic
#'
#' @import ggplot2
#' @import gridExtra
#' @export
setMethod("plot",
          signature = "GenericStrat",
          definition = function(x, y = NULL, title="",
                                xlab = "", ylab.1 = "Price",
                                ylab.2 = "Hedge %",
                                #ylab.3 = "Return %",
                                legend= "top",...){

            x@Results$xaxis <- 1:length(x@Results$Price)
#            x@Results$Target <- rep(x@TargetPrice,length(x@Results$Price))
#             x@Results$MarRet <- c(0,diff(log(c14@Results$Price)))*100
#             x@Results$PorRet <- c(0,diff(log(x@Results$PortfPrice)))*100


            PricePlot <- ggplot(x@Results, aes(x=xaxis,y=Price)) +
              geom_line(aes(y=Price,colour="Market"),size=0.5) +
              geom_line(aes(y=PortfPrice,colour="Portfolio"),size=0.5) +
              geom_line(aes(y=Target,colour="Target"),size=0.5) +
              theme(legend.position=legend,legend.title=element_blank()) +
              xlab(xlab) +  ylab(ylab.1) +
              theme(plot.margin=unit(c(0.3,1,-0.4,0.2),"cm")) +
              theme(axis.title=element_text(size=12)) +
              theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
              ggtitle(title) +
              theme(legend.background = element_rect(fill = "transparent"),
                    legend.key = element_rect(fill = "transparent",
                                              color = "transparent")
              )

#             ReturnPlot <- ggplot(x@Results, aes(x=xaxis,y=MarRet)) +
#               geom_line(aes(y=MarRet,colour="Market"),size=0.5) +
#               geom_line(aes(y=PorRet,colour="Portfolio"),size=0.5) +
#               theme(legend.position="",legend.title=element_blank()) +
#               xlab(xlab) +  ylab(ylab.3) +
#               theme(plot.margin=unit(c(0,1,0,0),"cm")) +
#               theme(axis.title=element_text(size=12))

            HedgePlot <- ggplot(x@Results, aes(x=Date,y=HedgeRate*100)) +
              geom_area(aes(y=HedgeRate*100),fill="gray75") +
              theme(legend.position="top",legend.title=element_blank()) +
              xlab(xlab) +  ylab(ylab.2) +
              theme(plot.margin=unit(c(0,1,0,0),"cm")) +
              theme(axis.title=element_text(size=12))

            grid.arrange(PricePlot,HedgePlot,ncol=1,heights=c(1.5,0.6))

          })
