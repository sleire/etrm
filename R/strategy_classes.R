
######### classes #######

#' An S4 VIRTUAL parent class for the hedging strategy classes in etrm
#'
#' @Name Object of type "character" containing hedging strategy name
#' @Volume Object of type "numeric" with quantity to be hedged
#' @TargetPrice Object of type "numeric" with portfolio target price (cap or floor)
#' @TransCost Object of type "numeric" with transaction costs pr unit
#' @TradeisInt Object of type "logical" with integer restriction on tradable volume
#' @Results Object of type "data frame" with strategy results
#' @importFrom utils tail
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
                                 length(object@Results$Market),sep=" ")

            Volume <- object@Volume

            Target <- object@TargetPrice

            ChurnRate <- sum(abs(object@Results$Trade))/abs(object@Volume)

            Stats <- object@Results[1,2:8]
            Stats <- rbind(Stats, sapply(object@Results[,2:8], max))
            Stats <- rbind(Stats, sapply(object@Results[,2:8], min))
            Stats <- rbind(Stats, tail(object@Results[,2:8],1))
            rownames(Stats) <- c("First","Max","Min","Last")

            list(Description=Description,
                 Volume=Volume,
                 Target = Target,
                 ChurnRate=ChurnRate,
                 Stats=Stats)
          })


#' S4 method for the plot generic
#'
#' @import ggplot2
#' @import reshape2
#' @importFrom ggplot2 theme_light
#' @export
setMethod("plot",
          signature = "GenericStrat",
          definition = function(x,
                                y = NULL,
                                title="Strategy plot",
                                ylab.1 = "Price",
                                ylab.2 = "Hedge %",
                                pcols = c("#F8766D", "steelblue3", "gray60", "gray80"),
                                legend = "bottom",
                                ...){

            # reshape and add grouping variable
            pdat <- x@Results[c("Date","Market","Hedge","Target","Portfolio")]
            pdat.melt <- melt(pdat, id = "Date")
            pdat.melt$groupvar <- ifelse(pdat.melt$variable == "Hedge", "Hedge", "Price")
            pdat.melt$groupvar <- factor(pdat.melt$groupvar, levels = c("Price", "Hedge"))

            ggplot() +

              # market price plot
              geom_line(data = subset(pdat.melt, variable == "Market"),
                        aes(x = Date, y = value, color = variable)) +

              # portfolio price plot
              geom_line(data = subset(pdat.melt, variable == "Portfolio"),
                        aes(x = Date, y = value, color = variable)) +

              # target price plot
              geom_line(data = subset(pdat.melt, variable == "Target"),
                        aes(x = Date, y = value, color = variable), linetype = "twodash") +

              # set colors
              scale_color_manual(values = pcols[1:3]) +

              # hedge plot
              geom_area(data = subset(pdat.melt, groupvar == "Hedge"),
                        aes(x = Date, y= value*100, fill = variable), show.legend = FALSE) +
              scale_fill_manual(values=pcols[4]) + #CC6666

              # facet wrap
              facet_wrap(groupvar~., scales = "free_y" ,
                         nrow = 2,
                         strip.position = "left",
                         labeller = as_labeller(c(Price = ylab.1, Hedge = ylab.2))) +
              ylab(NULL) +

              # restrict theme for plot
              theme(strip.background = element_blank(),
                    strip.placement = "outside",
                    legend.title = element_blank(),
                    legend.position = legend) +

              ggtitle(title)

          })
