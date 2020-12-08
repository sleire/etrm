
######### MSFC class #######

#' An S4 class for the Maximum Smoothness Forward Curve (MSFC) in etrm
#'
#' @Name Object of type "character" containing forward curve name "MSFC"
#' @TradeDate object of type "date"
#' @BenchSheet Object of type "data frame" with futures contracts
#' @Polynomials Object of type "numeric" with number of polynomials in the spline
#' @PriorFunc Object of type "numeric" with prior function values
#' @Results Object of type "data frame" with MSFC and contracts
#' @SplineCoef List with spline coefficients for the msfc calculation
#' @KnotPoints Vector with spline knot points
#' @CalcDat Data frame with daily values for splines and msfc
setClass("MSFC",
         slots = c(Name = "character",
                   TradeDate ="Date",
                   BenchSheet = "data.frame",
                   Polynomials = "numeric",
                   PriorFunc = "numeric",
                   Results = "data.frame",
                   SplineCoef = "list",
                   KnotPoints = "numeric",
                   CalcDat = "data.frame")
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
                                 "of length",
                                 length(object@Results$MSFC),
                                 "built with",
                                 object@Polynomials,
                                 "polynomials at",
                                 "trade date",
                                 object@TradeDate, sep=" ")

            PriorFunc <- object@PriorFunc # TODO: consider not returning complete prior

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
          definition = function(x,
                                y = NULL,
                                title="",
                                xlab = "",
                                ylab = "Price",
                                legend= "top",
                                ...){

            x_melt <- melt(x@Results, id = "Date")
            x_meltNA <- na.omit(x_melt)

            p <- ggplot(x_meltNA, aes(x = Date,y = value, color = variable)) +
            geom_line() +
            xlab(xlab) +
            ylab(ylab) +
            theme(legend.title=element_blank())

            p

          })
