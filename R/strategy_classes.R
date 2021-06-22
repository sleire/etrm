
######### classes #######

#' An S4 VIRTUAL parent class for the hedging strategy classes in etrm
#'
#' @slot Name A string with the portfolio insurance strategy name
#' @slot Volume The quantity to be hedged
#' @slot TargetPrice The target price(s) for the portfolio (cap or floor)
#' @slot TransCost Transaction costs pr unit traded
#' @slot TradeisInt TUE/FALSE integer restriction on tradable volume, TRUE sets smallest transacted unit to 1
#' @slot Results Data frame with strategy results, daily values for market price, transactions, exposure, position, hedge and portfolio price
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
#' @slot RiskFactor The risk factor (cushion) used in the CPPI model
setClass("CPPI",
         contains = "GenericStrat",
         slots = c(RiskFactor = "numeric")
)

#' An S4 class for the DPPI hedging strategy
#'
#' @slot TargetPercent A percentage of first trading day's market price used to set target price (cap or floor)
#' @slot RiskFactor The risk factor (cushion) used in the DPPI model
setClass("DPPI",
         contains = "GenericStrat",
         slots = c(TargetPercent = "numeric",
                   RiskFactor = "numeric")
)

#' An S4 class for the OBPI hedging strategy
#'
#'@slot StrikePrice Strike price for the synthetic option hedging
#'@slot AnnVol Annualized volatility for the contract to be traded
#'@slot InterestRate Risk-free rate of interest
#'@slot TradingDays The number of trading days per year
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

#' S4 method for the show generic for portfolio insurance strategy classes
#'
#'@export
#'@param object instance of a strategy class
#'@return a data frame with daily observations for market price, transactions,
#'exposed volume, forward positions, hedge rate, target price and portfolio price
setMethod("show",
          signature ="GenericStrat",
          definition=function(object) {
            Results <- object@Results
            Results
          })

#' S4 method for the summary generic for portfolio insurance strategy classes
#'
#'@export
#'@param object instance of a strategy class
#'@return a list with five elements. 1) A string describing the type of portfolio
#' insurance trading strategy and number of observations, 2) volume to be hedged,
#' calculated churn rate (numer of times volume to be hedged has been traded) and
#' 5) a data frame with summary statistics for achieved results
setMethod("summary",
          signature ="GenericStrat",
          definition=function(object) {

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


#' S4 method for the plot generic for portfolio insurance strategy classes
#'
#' @import ggplot2
#' @import reshape2
#' @param x instance of the strategy class created by the corresponding strategy function
#' @param y NULL
#' @param title plot title
#' @param xlab label for x-axis
#' @param ylab.1 label for y-axis on price plot in top panel
#' @param ylab.2 label for y-axis on hedge plot in bottom panel
#' @param pcols vector with four color codes for plot
#' @param legend legend position in c("top", "bottom")
#' @export
#' @return a two-panel chart with daily values for (top panel) target price,
#' market price and portfolio price and (bottom) portfolio hedge rate
setMethod("plot",
          signature = "GenericStrat",
          definition = function(x,
                                y = NULL,
                                title="Strategy plot",
                                xlab = "",
                                ylab.1 = "Price",
                                ylab.2 = "Hedge %",
                                pcols = c("#F8766D", "steelblue3", "gray60", "gray80"),
                                legend = "bottom"
                                ){

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

              xlab(xlab) +

              ggtitle(title)

          })
