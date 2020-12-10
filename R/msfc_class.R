
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

            PriorFunc <- head(object@PriorFunc) # TODO: consider not returning complete prior

            BenchSheet <- object@BenchSheet

            list(Description=Description,PriorFunc=PriorFunc,
                 BenchSheet = BenchSheet)
          })

#' S4 method for the plot generic
#'
#' @import ggplot2
#' @import reshape2
#' @importFrom stats na.omit
#' @param x instance of the MSFC class created by the msfc function
#' @param plot.prior TRUE/FALSE for incuding prior function in plot
#' @param title plot title
#' @param xlab x-axis title
#' @param ylab y-axis title
#' @param legend position of legend, as implemented in ggplot2
#' @param ggtheme ggplot2 theme to be used in plot
#' @export
setMethod("plot",
          signature = "MSFC",
          definition = function(x,
                                y = NULL,
                                plot.prior = FALSE,
                                title="",
                                xlab = "",
                                ylab = "Price",
                                legend= "right",
                                ...){

            if (plot.prior){
              x@Results$Prior <- x@PriorFunc
            }

            # reshape
            x_melt <- melt(x@Results, id = "Date")
            x_meltNA <- na.omit(x_melt)

            ggplot(x_meltNA, aes(x = Date,y = value, color = variable)) +

            geom_line() +

            xlab(xlab) +

            ylab(ylab) +

            # restrict theme for plot
            theme(legend.title=element_blank(),
                  legend.position = legend) +

            ggtitle(title)


          })
