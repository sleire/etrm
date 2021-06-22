
######### MSFC class #######

#' An S4 class for the Maximum Smoothness Forward Curve (MSFC) in etrm
#'
#' @slot Name A string with the acronym for Maximum Smoothness Forward Curve, "MSFC"
#' @slot TradeDate The trading date
#' @slot BenchSheet A data frame with futures contracts selected for calculation with MSFC computed prices
#' @slot Polynomials The number of polynomials in the MSFC spline
#' @slot PriorFunc A numeric vector with the prior function values
#' @slot Results A data frame with daily values for the calculated MSFC and contracts in "BenchSheet"
#' @slot SplineCoef List with coefficients for the polynomials in the MSFC spline
#' @slot KnotPoints Vector with spline knot points
#' @slot CalcDat Data frame extending "Results" with daily values for time vectors and polynomial coefficients used in calculation
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

#' S4 method for the show generic for class "MSFC"
#'
#'@export
#'@importFrom methods show
#'@param object instance of the MSFC class
#'@return data frame with daily values for forward curve and forward contracts used in calculation
setMethod("show",
          signature ="MSFC",
          definition=function(object) {
            Results <- object@Results
            Results
          })

#' S4 method for the summary generic for class "MSFC"
#'
#'@export
#'@param object instance of the MSFC class
#'@return a list with three elements. 1) A string describing length of forward
#'curve, number of polynomials used in spline and trading date, 2) a vector with a
#'sample of the prior used via head(prior) and 3) a data frame with all forward
#'contracts used in the calculation along with computed forward curve prices
setMethod("summary",
          signature ="MSFC",
          definition=function(object) {

            Description <- paste(object@Name,
                                 "of length",
                                 length(object@Results$MSFC),
                                 "built with",
                                 object@Polynomials,
                                 "polynomials at",
                                 "trade date",
                                 object@TradeDate, sep=" ")

            PriorFunc <- head(object@PriorFunc)

            BenchSheet <- object@BenchSheet

            list(Description=Description,PriorFunc=PriorFunc,
                 BenchSheet = BenchSheet)
          })

#' S4 method for the plot generic for class "MSFC"
#'
#' @import ggplot2
#' @import reshape2
#' @importFrom stats na.omit
#' @param x instance of the MSFC class created by the msfc function
#' @param y NULL
#' @param plot.prior TRUE/FALSE for incuding prior function in plot
#' @param title plot title
#' @param xlab x-axis title
#' @param ylab y-axis title
#' @param legend position of legend, as implemented in ggplot2
#' @export
#' @return a chart with daily values for the forward curve and contracts used in calculation
setMethod("plot",
          signature = "MSFC",
          definition = function(x,
                                y = NULL,
                                plot.prior = FALSE,
                                title="",
                                xlab = "",
                                ylab = "Price",
                                legend= "right"
                                ){

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
