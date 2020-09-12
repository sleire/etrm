
######### classes #######

#' An S4 VIRTUAL parent class for the hedging strategy classes in etrm
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

# http://stackoverflow.com/questions/37024814/ggplot-generate-facet-grid-plot-with-multiple-series

#' S4 method for the plot generic
#'
#' @import ggplot2
#' @import gridExtra
#' @import reshape2
#' @export
setMethod("plot",
          signature = "GenericStrat",
          definition = function(x, y = NULL, title="",
                                xlab = "", ylab.1 = "Price",
                                ylab.2 = "Hedge %",
                                legend= "",...){

            pdat <- x@Results[c("Date","Price","HedgeRate","Target","PortfPrice")]
            colnames(pdat) <- c("Date","Market","Hedge","Target","Portfolio")
            pdat.melt <- melt(pdat, id = "Date")

            # add new grouping variable for facetting
            pdat.melt$var <- ifelse(pdat.melt$variable == "Hedge", "Hedge", "Price")
            pdat.melt$var <- factor(pdat.melt$var, levels = c("Price", "Hedge"))
            #head(pdat.melt)
# https://ggplot2.tidyverse.org/reference/facet_grid.html
# https://sctyner.github.io/redoing-graphs.html
# https://github.com/sctyner/geomnet

            ggplot() +
              geom_line(data = subset(pdat.melt, var == "Price"),
                        aes(x = Date, y = value, color = variable), size = 0.5) +
              geom_area(data = subset(pdat.melt, var == "Hedge"),
                        aes(x = Date, y= value, fill = variable), size = 1.5) + #, position = "dodge")  +
              scale_fill_manual(values=c("gray")) + #CC6666
              scale_color_manual(values=c("firebrick", "seagreen", "deepskyblue4")) +
              #scale_color_brewer(palette = "Paired") +
              facet_grid(var~., scales = "free_y" ) + ylim(0)

            # p <- ggplot(pdat.melt, aes(Date, value, col=variable, group=variable)) +
            #   geom_line() +
            #   facet_grid(var~., scale='free_y') +
            #   #facet_wrap(~var, nrow = 2, scales = "free_y" ) +
            #   #facet_wrap(var~., scale='free_y') +
            #   #theme(strip.text.y = element_blank())
            #   scale_color_discrete(breaks=c("Market","Target","Portfolio","Hedge"))
            # p

# http://stackoverflow.com/questions/13294952/left-align-two-graph-edges-ggplot

           # x@Results$xaxis <- 1:length(x@Results$Price)
            #            x@Results$Target <- rep(x@TargetPrice,length(x@Results$Price))

            ########################
#
#             PricePlot <- ggplot(x@Results, aes(x=Date,y=Price)) +
#               geom_line(aes(y=Price,colour="Market"),size=0.5) +
#               geom_line(aes(y=PortfPrice,colour="Portfolio"),size=0.5) +
#               geom_line(aes(y=Target,colour="Target"),size=0.5) +
#               theme(legend.position=legend,legend.title=element_blank()) +
#               xlab(xlab) +  ylab(ylab.1) +
#               theme(plot.margin=unit(c(0.3,1,-0.4,0.2),"cm")) +
#               theme(axis.title=element_text(size=8)) +
#               theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
#               ggtitle(title) +
#               theme(legend.background = element_rect(fill = "transparent"),
#                     legend.key = element_rect(fill = "transparent",
#                                               color = "transparent")
#               )
#
#             HedgePlot <- ggplot(x@Results, aes(x=Date,y=HedgeRate*100)) +
#               geom_area(aes(y=HedgeRate*100),fill="gray75") +
#               theme(legend.position="top",legend.title=element_blank()) +
#               xlab(xlab) +  ylab(ylab.2) +
#               #ylim(0,100) +
#               theme(plot.margin=unit(c(0,1,0,0),"cm")) +
#               scale_y_continuous(breaks = c(0,50,100), limits = c(0, 100)) +
#               theme(axis.title=element_text(size=8))
#
#             gP <- ggplotGrob(PricePlot)
#             gH <- ggplotGrob(HedgePlot)
#             #g <- rbind(gP, gH)
#
#             g <-gtable:::rbind_gtable(gP, gH, "first")
#             # panels <- g$layout$t[grep("panel", g$layout$name)]
#             # g$heights[panels] <- unit(c(1,2), "null")
#
#             grid::grid.newpage()
#             grid::grid.draw(g)
#
            ##########################

            #grid.arrange(gP, gH, ncol=1, heights=c(1,0.4))
                         #layout_matrix = rbind(c(1,1), c(1,1)), c(2,2))

          })
