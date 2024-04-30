#' Get DOY Summary Stats Graph
#'
#' @param data A data.frame with date and flow columns.
#' @param value_name One unquoted expression for flow column name, e.g. vettenforing_m3_s.
#' @param q1 Numeric. Peakflow for 1 year recurrence interval.
#' @param q2 Numeric. Peakflow for 2 year recurrence interval.
#' @param q5 Numeric. Peakflow for 5 year recurrence interval.
#' @param wy_month A numeric for what month to use as start of water year, 10 (default).
#'
#' @return A \code{ggplot} with percentiles, proportion to wy max and Q1, Q2, Q5 flood frequency values.
#' @importFrom ggplot2 aes
#' @export
#'

riverwave_percentiles_plot <- function(data, value_name,q1, q2, q5,  wy_month = 10) {


        ff_vals <- dplyr::tibble(q1 = q1, q2 = q2, q5 = q5)

        data <- data %>% summary_stats_doy({{value_name}}, wy_month)

        data %>%
        dplyr::group_by(wy_doy) %>%
        dplyr::slice(1) %>%
        tidyr::pivot_longer(c("p0_va", "p75_va", "p50_va", 'p25_va', 'p95_va', 'p10_va'), names_to = 'Percentiles') %>%
        dplyr::mutate(Percentiles = forcats::fct_recode(Percentiles, 'Lowest Flow' = 'p0_va',
                                                 '10%' = 'p10_va',
                                                 "25%" = 'p25_va',
                                                 'Median Flow' = 'p50_va',
                                                 '75%' = 'p75_va',
                                                 '95%' = 'p95_va'),
               Percentiles = factor(Percentiles, levels = c('Lowest Flow', '10%', '25%', 'Median Flow', '75%', '95%'))) %>%
        ggplot2::ggplot() +
        ggplot2::geom_line(aes(wy_doy, value, group = Percentiles, color = Percentiles), size = 1.5) +
        ggplot2::scale_color_manual(values = grDevices::hcl.colors(n = 6, palette = 'Viridis'))  +
        ggplot2::geom_line(data = data %>% dplyr::filter(wy_doy < 366), aes(wy_doy,
                                                                     {{value_name}},
                                                                     group = wy), alpha = 0.15, linewidth = 0.25) +
        geomtextpath::geom_labelhline(yintercept = ff_vals$q1, linetype = 3, label = paste0('Q1: ', round(ff_vals$q1, 2), ' (cms)'), fontface = 'bold') +
        geomtextpath::geom_labelhline(yintercept = ff_vals$q2, linetype = 2, label = paste0('Q2: ', round(ff_vals$q2, 2), ' (cms)'),  fontface = 'bold') +
        geomtextpath::geom_labelhline(yintercept = ff_vals$q5, linetype = 1, label = paste0('Q5: ', round(ff_vals$q5, 2), ' (cms)'), fontface = 'bold') +
        ggplot2::labs(x = "Day of Year (DOY)", y = 'Discharge (cms)')


}

#' Riverwave Rastergraph
#'
#' @param data A data.frame with date and flow columns.
#' @param value_name One unquoted expression for flow column name, e.g. vettenforing_m3_s.
#' @param q1 Numeric. Peakflow for 1 year recurrence interval.
#' @param q2 Numeric. Peakflow for 2 year recurrence interval.
#' @param wy_month A numeric for what month to use as start of water year, 10 (default).
#'
#' @return
#' @export
#'
riverwave_rastergraph <- function(data, value_name, q1, q2, wy_month = 10) {

        if (!requireNamespace("ggfx")) {

            tile <- ggplot2::geom_tile(ggplot2::aes(fill = cut_value))

        } else {

            tile <- ggfx::with_outer_glow(ggplot2::geom_tile(ggplot2::aes(fill = cut_value)))

        }


        ff_vals <- dplyr::tibble(q1 = q1, q2 = q2)

        data <- prep_flow(data, {{value_name}}, wy_month)

        xbreaks <- data %>%
            dplyr::group_by(month) %>%
            dplyr::slice_min(day, with_ties = F) %>%
            dplyr::pull(wy_doy)

        dup_labels <- function(x) data$month_abb[match(x, data$wy_doy)]

        data %>%
            dplyr::mutate(cut_value = ifelse({{value_name}} < ff_vals$q1, 'Riverine Productivity',
                                      ifelse({{value_name}} >= ff_vals$q1 & {{value_name}} < ff_vals$q2, 'River Continuum', 'Flood Pulse')),
                   cut_value = factor(cut_value, levels = c('Riverine Productivity', 'River Continuum', 'Flood Pulse'))) %>%
            ggplot2::ggplot(ggplot2::aes(wy_doy, wy)) +
            ggplot2::scale_y_continuous(breaks = seq(min(data$wy),
                                                      max(data$wy),
                                                      by = 10),
                                         name = 'Water Year') +
            ggplot2::scale_x_continuous(breaks = xbreaks,
                                        name = 'Day of Year (DOY)',
                                        sec.axis = ggplot2::dup_axis(labels = dup_labels, name = NULL),
                                        expand = c(0,0)) +
            tile +
            ggplot2::scale_fill_manual(values = c("#33b544","#f5c01a","#ee3b27"),
                                       name = 'Discharge') +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                           plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 8),
                           panel.grid.minor.x = ggplot2::element_blank(),
                           panel.grid.major.y = ggplot2::element_blank(),
                           panel.grid.major.x = ggplot2::element_blank(),
                           panel.background = ggplot2::element_rect(fill = NA),
                           panel.ontop = T,
                           axis.ticks.x = ggplot2::element_blank(),
                           axis.text.x.bottom = ggplot2::element_text(vjust = 5.5,face = 'bold'),
                           axis.text.x.top = ggplot2::element_text(hjust = -.45, vjust = -5.5, face = 'bold'))
}

#' 3D Riverwave Plot
#'
#' @param data A data.frame with date and flow columns.
#' @param value_name One unquoted expression for flow column name, e.g. vettenforing_m3_s.
#' @param q1 Numeric. Peakflow for 1 year recurrence interval.
#' @param q2 Numeric. Peakflow for 2 year recurrence interval.
#' @param wy_month A numeric for what month to use as start of water year, 10 (default).
#' @param ... Arguments to pass to \link[rgl]{view3d}.
#'
#' @return A 3D riverwave plot.
#' @export
#'
#' @examples

riverwave_3d <- function(data, value_name, q1, q2, wy_month = 10, ...) {

    data <- prep_flow(data, {{value_name}}, wy_month)

    data_mat <- data %>%
        dplyr::select(wy_doy, wy, {{value_name}}) %>%
        dplyr::group_by(wy) %>%
        dplyr::mutate(contains_1 = ifelse(any(wy_doy == 1), TRUE, FALSE)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(contains_1) %>%
        dplyr::select(-contains_1) %>%
        tidyr::pivot_wider(names_from = wy_doy, values_from = {{value_name}}, values_fill = NA_real_) %>%
        # dplyr::group_by(wy) %>%
        # dplyr::slice(rep(1:dplyr::n(), each = 3)) %>%
        # dplyr::ungroup() %>%
        dplyr::arrange(dplyr::desc(wy)) %>%
        dplyr::select(-wy)

    data_mat <- as.matrix(data_mat)

    data_mat_rast <- terra::rast(data_mat)

    myColorRamp <- function(colors, values) {
        v <- (values - min(values))/diff(range(values))
        x <- grDevices::colorRamp(colors)(v)
        grDevices::rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
    }


    while (rgl::rgl.cur() > 0) { try(rgl::close3d())}


    myPal <- myColorRamp(c("#33b544","#33b544","#f5c01a","#ee3b27"),0:255)

    max_all_time <- max(data[[paste(rlang::as_string(rlang::ensym(value_name)))]], na.rm = TRUE)

    colour_breaks <- c(0,q1,q2,max_all_time)

    rw_plot3D(data_mat_rast, col = myPal,
                      drape = terra::classify(data_mat_rast, colour_breaks))


    ranges <- getRanges()

    xlim <- ranges$xlim

    ylim <- ranges$ylim

    zlim <- ranges$zlim


    rgl::axes3d(c("x-"), col="black", at = seq(xlim[1], xlim[2], length.out = 10),
                nticks = 10, labels = F)

    rgl::axes3d(c("y+"), col="black", at = seq(ylim[1], ylim[2], length.out = 10),
                nticks = 10, labels = F)

    rgl::axes3d(c("z++"), col="black", at = seq(zlim[1], zlim[2], length.out = 3),
                nticks = 3, labels = F)

    xvalues <- round(seq(min(data$wy_doy, na.rm = T),max(data$wy_doy, na.rm = T), length.out = 10))

    for(i in xvalues){

        rgl::mtext3d(paste0(xvalues), 'x', at = seq(xlim[1], xlim[2], length.out = 10), level = 1)

    }

    yvalues <- round(seq(min(data$wy, na.rm = T),max(data$wy, na.rm = T), length.out = 10))

    for(i in yvalues){

        rgl::mtext3d(paste0(yvalues), 'y+', at = seq(ylim[1], ylim[2], length.out = 10), level = 0,
                     line = 1.5)

    }

    flow <- data[[paste(rlang::as_string(rlang::ensym(value_name)))]]

    zvalues <- round(seq(min(flow, na.rm = T),max(flow, na.rm = T), length.out = 3))

    for(i in zvalues){

        rgl::mtext3d(paste0(zvalues), 'z++', at = seq(zlim[1], zlim[2], length.out = 3),
                     level = 1.5, line = 1)

    }


    rgl::view3d(...)

}


#' Get Ranges
#' @description
#' This function is taken from the rgl package due to it being non-exported.
#'
#' @param expand numeric
#' @param ranges rgl par3d function.
#'
#' @return A range of x, y, and z values.
getRanges <- function (expand = 1.03, ranges = rgl::par3d("bbox")) {
    ranges <- list(xlim = ranges[1:2], ylim = ranges[3:4], zlim = ranges[5:6])
    strut <- FALSE
    ranges <- lapply(ranges, function(r) {
        d <- diff(r)
        if (d > 0)
            return(r)
        strut <<- TRUE
        if (d < 0)
            return(c(0, 1))
        else if (r[1] == 0)
            return(c(-1, 1))
        else return(r[1] + 0.4 * abs(r[1]) * c(-1, 1))
    })
    ranges$strut <- strut
    ranges$x <- (ranges$xlim - mean(ranges$xlim)) * expand +
        mean(ranges$xlim)
    ranges$y <- (ranges$ylim - mean(ranges$ylim)) * expand +
        mean(ranges$ylim)
    ranges$z <- (ranges$zlim - mean(ranges$zlim)) * expand +
        mean(ranges$zlim)
    ranges
}

<<<<<<< HEAD
#' Plot 3D for RW
#'
#' @param x
#' @param maxpixels
#' @param zfac
#' @param drape
#' @param col
#' @param at
#' @param rev
#' @param useLegend
#' @param adjust
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
<<<<<<< Updated upstream

plot3D_rw <- function(x, maxpixels=1e5, zfac=1, drape=NULL,
         col=terrain.colors, at=100, rev=FALSE,
         useLegend=TRUE,
         adjust=TRUE, ...) {
=======
rw_plot3D <-  function(x, maxpixels=1e5, zfac=1, drape=NULL,
                       col=terrain.colors, at=100, rev=FALSE,
                       useLegend=TRUE,
                       adjust=TRUE, ...) {
>>>>>>> Stashed changes
    ## much of the below code was taken from example(surface3d) in the rgl package
    if (requireNamespace("rgl", quietly = TRUE)){

        x <- terra::spatSample(x, size=ncol(x)*nrow(x), method = 'regular',as.raster=TRUE)
        X <- terra::xFromCol(x,1:ncol(x))
        Y <- terra::yFromRow(x, nrow(x):1)
        Z <- t(as.matrix(x, wide = TRUE))

        background <- min(Z, na.rm=TRUE) - 1
        Z[is.na(Z)] <- background

        zlim <- range(Z)
        zlen <- zlim[2] - zlim[1] + 1
        xlen <- max(X) - min(X)
        ylen <- max(Y) - min(Y)
        if (adjust) {
            adj <- 4*zlen/min(ylen,xlen)
            #X <- X * adj
            Y <- Y * adj *1.2
        }

        if (!is.null(drape)){
            x <- terra::spatSample(drape, size=ncol(drape)*nrow(drape),method = 'regular', as.raster=TRUE)
            Zcol <- t(as.matrix(x, wide = TRUE))
            background <- min(Zcol, na.rm=TRUE) - 1
            Zcol[is.na(Zcol)] <- background
            zlim <- range(Zcol)
        } else {
            Zcol <- Z
        }

        if (length(at)==1) at <- lattice::do.breaks(zlim, at)
            if (rev) {
                if (is.function(col)) {
                    col <- rev(col(length(at)))
                } else {
                    col <- rev(col)
                }
            }

        color <- lattice::level.colors(Zcol, at=at, col.regions=col)

        ## Open a device only if there is none active
        if (rgl::cur3d() == 0) rgl::open3d()

        if (background==min(Zcol)) {
            trans <- Zcol
            trans[] <- 1.0
            trans[Zcol==background] <- 0
            rgl::surface3d(X, Y, Z*zfac, color=color, back="lines", alpha=trans, ...)
        } else {
            rgl::surface3d(X, Y, Z*zfac, color=color, back="lines", ...)
        }
    } else stop("to use this function you need to install the 'rgl' package")
}

#' 2D plot of riverwave with breakpoints
#' @description
#' This plotting function returns a ggplot of daily discharge colored by riverwave breakpoints across time
#' @param flowdata dataframe with discharge column, day of year column, year column
#' @param doy day of year column pointer
#' @param discharge discharge column pointer
#' @param year year column pointer
#' @param breakpoints a vector with 3 positions for gradient, 1st pos. is usually 1

riverwave_2d <- function(flowdata, doy, discharge, year, breakpoints) {
    ggplot2::ggplot(flowdata,
            ggplot2::aes(x={{doy}},
               y={{discharge}},
               group={{year}},
               color={{discharge}})) +
        ggplot2::geom_line() +
        ggplot2::scale_color_gradientn(colours = c("#33b544","#f5c01a","#ee3b27"),
                              breaks = c(breakpoints[1],
                                         breakpoints[2],
                                         breakpoints[3]))+
        ggplot2::labs(x = "Day of Year",
             y = "Discharge",
             color = "Discharge")
}

