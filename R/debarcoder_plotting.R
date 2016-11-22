
plot_single_biaxial <- function(m, x.var, y.var, mahal.dist) {
    x.var.idx <- which(colnames(m) == x.var)
    y.var.idx <- which(colnames(m) == y.var)

    m <- data.frame(m)
    m$mahal.dist <- mahal.dist

}

plot_color_coded_biaxial <- function(m, x.var, y.var, color.var, color.breaks) {
    x.var.idx <- which(colnames(m) == x.var)
    y.var.idx <- which(colnames(m) == y.var)
    color.var.idx <- which(colnames(m) == color.var)

    m <- data.frame(m)


    m <- hexbin_downsample(m, x.var.idx, y.var.idx)


    (p <- ggplot2::ggplot(ggplot2::aes_string(x = names(m)[x.var.idx],
                    y =  names(m)[y.var.idx], colour = names(m)[color.var.idx]), data = m)
        + ggplot2::geom_point()
        + ggplot2::scale_colour_gradientn(color.breaks, colours = rainbow(3))
        + ggplot2::theme(legend.position = "top", legend.key.width = ggplot2::unit(0.1, "npc"))
    )
    return(p)

}

plot_barcode_separation <- function(bc.res) {
    tab <- get_well_abundances(bc.res, seq(0, 1, 0.05))
    tab <- tab[tab$label != "Unassigned", ]

    (p <- ggplot2::ggplot(ggplot2::aes(y = Freq, x = threshold, colour = label, group = label), data = tab)
        + ggplot2::geom_line()
        + ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.1))
    )

    return(p)
}

plot_barcode_channels_intensities <- function(m, bc.channels) {
    m <- m[, bc.channels]
    m <- data.frame(m, check.names = F)
    m <- cbind(m, Event = 1:nrow(m))
    m <- reshape::melt.data.frame(m, id.vars = "Event")

    (p <- ggplot2::ggplot(ggplot2::aes(x = Event, y = value, colour = variable), data = m)
        + ggplot2::geom_point()
    )
    return(p)
}

plot_separation_histogram <- function(bc.results) {
    tab <- data.frame(separation = bc.results$deltas)

    (p <- ggplot2::ggplot(ggplot2::aes(x = separation), data = tab)
        + ggplot2::geom_histogram(bins = 100)
        + ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.1))
    )
    return(p)

}

plot_barcode_yields <- function(bc.results, sep.threshold) {

    tab <- get_well_abundances(bc.results, sep.threshold)
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

plot_all_barcode_biaxials <- function(m, bc.channels) {
    plotlist <- list()
    m <- m[, bc.channels]
    m <- data.frame(m)

    for(i in 1:length(bc.channels))
        for(j in 1:length(bc.channels)) {
            plot <- ggplot2::ggplot(ggplot2::aes_string(x = names(m)[i], y = names(m)[j]), data = m)
            if(i <= j) {
                plot <- plot + ggplot2::geom_point()
            }
            else
                plot <- plot + ggplot2::geom_blank()

            plotlist <- c(plotlist, list(plot))
        }

    plotlist <- c(plotlist, list(nrow = length(bc.channels)))
    fun <- get("grid.arrange", asNamespace("gridExtra"))
    return(do.call(fun, plotlist))
}

multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    if (is.null(layout)) {
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }

    if (numPlots == 1) {
        print(plots[[1]])

    } else {
        grid::grid.newpage()
        grid::pushViewport(viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))))

        for (i in 1:numPlots) {
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

            print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                                  layout.pos.col = matchidx$col))
        }
    }
}

