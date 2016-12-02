#' Plotting debarcoded sample yields as a function of the separation threshold
#'
#' @param bc.res The debarcoding results to plot
#' @param sep.threshold If provided, adds a red line corresponding to the separation
#'  threshold in use
#'
#' @return Returns a ggplot object
#' @export
plot_barcode_separation <- function(bc.res, sep.threshold = NULL) {
    tab <- get_well_abundances(bc.res, seq(0, 1, 0.05))
    tab <- tab[tab$label != "Unassigned", ]

    (p <- ggplot2::ggplot(ggplot2::aes(y = Freq, x = threshold, colour = label, group = label), data = tab)
        + ggplot2::geom_line()
        + ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.1))
    )

    if(!is.null(sep.threshold))
        p <- p + ggplot2::geom_vline(xintercept = sep.threshold, colour = "red", linetype = 2)

    return(p)
}

#' Plotting barcode channels intensities for individual events
#'
#' @param m The data matrix to plot. The value should be \code{asinh} transformed
#' @param bc.channels The barcode channels names
#' @param m.normed A matrix of rescaled intensity values. If provided, they are plotted
#'  next to the values in \code{m}
#'
#' @return Returns a ggplot object
#' @export
plot_barcode_channels_intensities <- function(m, bc.channels, m.normed = NULL) {
    process_matrix <- function(m, bc.channels, data.type) {
        m <- m[, bc.channels]
        m <- data.frame(m, check.names = F)
        m <- cbind(m, Event = 1:nrow(m))
        m <- cbind(m, data.type = data.type)
    }

    m <- process_matrix(m, bc.channels, "Original")
    if(!is.null(m.normed)) {
        m.normed <- process_matrix(m.normed, bc.channels, "Rescaled")
        m <- rbind(m, m.normed)
    }

    m <- reshape::melt.data.frame(m, id.vars = c("Event", "data.type"))

    (p <- ggplot2::ggplot(ggplot2::aes(x = Event, y = value, colour = variable), data = m)
        + ggplot2::geom_point()
        + ggplot2::facet_wrap(~data.type, scales = "free")
    )
    return(p)
}

#' Plotting the distribution of separation values
#'
#' @param bc.results The debarcoding results
#'
#' @return Returns a ggplot object
#'
#' @export
plot_separation_histogram <- function(bc.results) {
    tab <- data.frame(separation = bc.results$deltas)

    (p <- ggplot2::ggplot(ggplot2::aes(x = separation), data = tab)
        + ggplot2::geom_histogram(bins = 100)
        + ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.1))
    )
    return(p)

}


#' Plotting sample yields after debarcoding
#' @inheritParams get_well_abundances
#'
#' @return Returns a ggplot2 object
#'
#' @export
plot_barcode_yields <- function(bc.results, sep.threshold, mahal.threshold = NULL, mahal.dist = NULL) {

    tab <- get_well_abundances(bc.results, sep.threshold, mahal.threshold, mahal.dist)
    perc.assigned <- 1 - (tab[tab$label == "Unassigned", "Freq"] / sum(tab$Freq))
    title.string <- sprintf("Barcode yields with current filters: %1.0f%% assigned", perc.assigned * 100)
    tab <- tab[tab$label != "Unassigned", ]

    (p <- ggplot2::ggplot(ggplot2::aes(x = label, y = Freq), data = tab)
        + ggplot2::geom_bar(stat = "identity")
        + ggplot2::scale_y_continuous("Cell count")
        + ggplot2::scale_x_discrete("Sample")
        + ggplot2::labs(title = title.string)
    )

    return(p)
}


#' Scatteplot matrix of all the barcode channels
#'
#' @param m The matrix of data to plot (\code{asinh} transformed)
#' @param bc.channels The names of the barcode channels
#'
#' @return Returns a ggplot2 object
#'
#' @export
plot_all_barcode_biaxials <- function(m, bc.channels) {
    plotlist <- list()
    mahal.dist <- m[, "mahal.dist"]
    m <- m[, bc.channels]
    m <- cbind(m , mahal.dist = mahal.dist)
    m <- data.frame(m)

    for(i in 1:length(bc.channels))
        for(j in 1:length(bc.channels)) {
            if(i == j) {
                # Plot a histogram
                plot <- (ggplot2::ggplot(ggplot2::aes_string(x = names(m)[i]), data = m)
                         + ggplot2::geom_histogram()
                         + ggplot2::scale_x_continuous("")
                         + ggplot2::scale_y_continuous("")
                )
                if(i == 1)
                    plot <- plot + ggplot2::scale_y_continuous(names(m)[i])
                if(j == length(bc.channels))
                    plot <- plot + ggplot2::scale_x_continuous(names(m)[i])
            }
            else {
                # Plot a biaxial
                plot <- ggplot2::ggplot(ggplot2::aes_string(x = names(m)[j], y = names(m)[i], colour = "mahal.dist"), data = m)
                if(i > j) {
                    plot <- plot + ggplot2::geom_point()
                    plot <- plot + ggplot2::scale_colour_gradientn("", breaks = 0:30, colours = rainbow(3))
                }
                else
                    plot <- (plot + ggplot2::geom_blank()
                                + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                    panel.grid.minor = ggplot2::element_blank(),
                                    panel.border = ggplot2::element_blank(),
                                    panel.background = ggplot2::element_blank())
                    )
                if(j != 1)
                    plot <- plot + ggplot2::scale_y_continuous("")
                if(i != length(bc.channels))
                    plot <- plot + ggplot2::scale_x_continuous("")
            }
            plot <- plot + ggplot2::theme(axis.line = ggplot2::element_blank(),
                                          axis.ticks = ggplot2::element_blank(),
                                          axis.text = ggplot2::element_blank(),
                                          legend.position = "none")
            plotlist <- c(plotlist, list(plot))
        }

    plotlist <- c(plotlist, list(nrow = length(bc.channels)))
    fun <- get("grid.arrange", asNamespace("gridExtra"))
    return(do.call(fun, plotlist))
}
