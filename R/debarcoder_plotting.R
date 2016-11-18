


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
