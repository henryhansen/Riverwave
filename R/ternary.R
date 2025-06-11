#' ternary
#'
#' @param data dataframe of measurements
#' @param coefv coefficient of variation
#' @param rbi richard baker index
#' @param bfi baseflow index
#' @param lat latitude
#' @param cat_size catchment size
#'
#' @returns a ternary plot with coefv, rbi, and bfi axes
#' @export
#'
#'
ternary <- function(data, coefv, rbi, bfi, lat, cat_size) {
    #scale all values between 0 and 1
    data$coefv <- 1-scales::rescale(data[,coefv])
    data$rbi <- scales::rescale(data[,rbi])
    data$bfi <- 1-scales::rescale(data[,bfi])

    stndata <- c(coefv,bfi,rbi)
    labs <- c("Flood Timing", "Baseflow Stability", "Flashiness")
    nPoints <- nrow(data)

    rowCol <- hcl.colors(nPoints, palette = "mako")

    par(mar = c(0, 0, 0, 0),
        mai = c(0, 0, 0, 0))

    Ternary::TernaryPlot(alab = labs[1],
                blab = labs[2],
                clab = labs[3],
                clockwise = T)

    PlotTools::SpectrumLegend(palette = rowCol,
                              legend = c(paste("Latitude:",round(max(data[,lat]))),
                                         rep(" ",3),
                                         paste("Latitude:",round(min(data[,lat])))),
                              "topright",
                              xpd = T,
                              inset = c(-0.1,0),
                              cex = 1,
                              bty = "n",
                              lwd = 50,
                              seg.len = 1,
                              pt.cex = 100)


    size <- c(1,2,3,4,5)
    scale <- 2
    legend(-0.90,1.05,
           title = "Log10(Catchment Size)",
           title.cex = 1.5,
           cex = 1,
           legend = size,
           pt.cex = size / scale,
           pch = 1,
           xpd = T,
           inset = c(-0.3,0),
           bty = "n")

    Ternary::TernaryPoints(data[,stndata],
                  pch = 21,
                  col = "black",
                  bg = rowCol[findInterval(data[,lat], sort(data[,lat]))],
                  cex = log10(as.numeric(data[,cat_size]))/scale)

    # test dataset
    # test <- data.frame(coefv = rnorm(50, mean = 20),
    #                   rbi = runif(50, min = 0.001, max = 0.999),
    #                   bfi = runif(50, min = 0.001, max = 0.999),
    #                   latorder = runif(50, min = 45, max = 50),
    #                   Catchment_size = rexp(50, 0.0001))
}






