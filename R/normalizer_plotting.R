
plot_beads_medians <- function(tab, out.name) {

    m <- reshape::melt.data.frame(tab, id.vars = c("sample", "type"))

    (p <- ggplot2::ggplot(ggplot2::aes(x = sample, y = asinh(value / 5), color = variable, group = variable), data = m)
        + ggplot2::facet_wrap(~type, ncol = 1)
        + ggplot2::geom_line()
        + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
        + ggplot2::scale_y_continuous("Intensity (asinh transformed)")
    )
    ggplot2::ggsave(out.name, plot = p, width = 11, height = 8.5, units = "in")
}



hexbin_downsample <- function(m, x.var, y.var, ...) {
    hex <- hexbin::hexbin(m[, x.var], m[, y.var], xbins = 250, IDs = T, ...)
    return(m[!duplicated(hex@cID),])
}

#Refactor this to use plot_color_coded_biaxial
#' Biaxial plot color-coded according to the Mahalanobis distance from the beads population
#'
#' @param m The untrasformed matrix of data. Must contain a column named \code{beadDist} containing the
#' Mahalanobis distance
#' @param x.var The variable on the x axis
#' @param y.var The variable on the y axis
plot_distance_from_beads <- function(m, x.var, y.var) {
    x.var.idx <- which(colnames(m) == x.var)
    y.var.idx <- which(colnames(m) == y.var)

    m <- data.frame(m)
    m[, x.var.idx] <- asinh(m[, x.var.idx] / 5)
    m[, y.var.idx] <- asinh(m[, y.var.idx] / 5)

    m <- hexbin_downsample(m, x.var.idx, y.var.idx)

    m$beadDist[m$beadDist > 20] <- 20


    (p <- ggplot2::ggplot(ggplot2::aes_string(x = names(m)[x.var.idx],
                                     y =  names(m)[y.var.idx], colour = "beadDist"), data = m)
        + ggplot2::geom_point()
        + ggplot2::scale_colour_gradientn(breaks = 0:20, colours = rainbow(3))
        + ggplot2::theme(legend.position = "top", legend.key.width = ggplot2::unit(0.1, "npc"))
    )
    return(p)

}
