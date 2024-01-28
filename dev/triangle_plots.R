# load libraries ----------------------------------------------------------
library(tidyverse)
library(Ternary)


# load data ---------------------------------------------------------------
final <- read_rds("data/final_dataset.rds")

final <- final[order(as.numeric(final$Catchment_size), decreasing = T),]

# Make Ternary plot with indices ------------------------------------------
# labels
stndata <- c("max_prop","skew","rbi")
labs <- c("Average Max Qmax", "Skew", "Flashyness Index")
nPoints <- nrow(final)

rowCol <- hcl.colors(nPoints, palette = "mako")

par(mar = c(0, 0, 0, 0),
    mai = c(0, 0, 0, 0))

TernaryPlot(alab = labs[1],
            blab = labs[2],
            clab = labs[3])

PlotTools::SpectrumLegend(palette = rowCol,
                          legend = c("Latitude 68.44","Latitude 55.45"),
                          "topright",
                          xpd = T,
                          inset = c(-0.1,0),
                          #title.cex = 0.75,
                          cex = 0.75,
                          bty = "n")

size <- c(1,2,3,4,5)
scale <- 2
legend("topleft",
       title = "Log10(Catchment Size)",
       #title.cex = 0.75,
       cex = 1,
       legend = size,
       pt.cex = size / scale,
       pch = 1,
       xpd = T,
       inset = c(-0.3,0),
       bty = "n")

final$latorder <- findInterval(final$lat, sort(final$lat))

TernaryPoints(final[,stndata],
              pch = 21,
              col = "black",
              bg = rowCol[final$latorder],
              cex = log10(as.numeric(final[,"Catchment_size"]))/scale)

TernaryPoints(final[,stndata],
              pch = 21)


