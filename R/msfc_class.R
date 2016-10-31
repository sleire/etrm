
######### MSFC class #######

#' An S4 class for the Maximum Smoothness Forward Curve (MSFC) in hedgeR
#'
#' @Name Object of type "character" containing hedging strategy name
#' @TradeDate object of type "date"
#' @BenchSheet Object of type "data frame" with futures contracts
#' @Polynomials Object of type "numeric" with number of polynomials in the spline
#' @PriorFunc Object of type "logical", prior function or not
#' @Results Object of type "data frame" with MSFC and contracts
setClass("MSFC",
         slots = c(Name = "character",
                   TradeDate ="Date",
                   BenchSheet = "data.frame",
                   Polynomials = "numeric",
                   PriorFunc = "logical",
                   Results = "data.frame")
)


######### methods #######

#' S4 method for the show generic
#'
#'@export
setMethod("show",
          signature ="MSFC",
          definition=function(object) {
            Results <- object@Results
            Results
          })

#' S4 method for the summary generic
#'
#'@export
setMethod("summary",
          signature ="MSFC",
          definition=function(object, ...) {

            Description <- paste(object@Name,
                                 "object of length",
                                 length(object@Results$MSFC),
                                 "built with",
                                 object@Polynomials,
                                 "polynomials at",
                                 "trade date",
                                 object@TradeDate, sep=" ")

            PriorFunc <- object@PriorFunc

            BenchSheet <- object@BenchSheet

            list(Description=Description,PriorFunc=PriorFunc,
                 BenchSheet = BenchSheet)
          })

#' S4 method for the plot generic
#'
#' @import ggplot2
#' @import reshape2
#' @export
setMethod("plot",
          signature = "MSFC",
          definition = function(x, y = NULL, title="",
                                xlab = "", ylab = "Price",
                                legend= "top",...){

            x_melt <- melt(x@Results, id = "Date")
            x_meltNA <- na.omit(x_melt)

            p <- ggplot(x_meltNA, aes(x = Date,y = value, color = variable)) +
            geom_line() +
            xlab(xlab) +
            ylab(ylab) +
            theme(legend.title=element_blank())

            p

          })
