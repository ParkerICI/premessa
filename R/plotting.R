
plot_beads_medians <- function(tab, out.name) {

    m <- reshape::melt(tab, id.vars = c("sample", "type"))

    (p <- ggplot2::ggplot(ggplot2::aes(x = sample, y = asinh(value / 5), color = variable, group = variable), data = m)
        + ggplot2::facet_wrap(~type, ncol = 1)
        + ggplot2::geom_line()
        + ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
        + ggplot2::scale_y_continuous("Intensity (asinh transformed)")
    )
    ggsave(out.name, plot = p, width = 11, height = 8.5, units = "in")
}



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

    m$beadDist[m$beadDist > 20] <- 20

    (p <- ggplot2::ggplot(ggplot2::aes_string(x = sprintf("asinh(%s / 5)", names(m)[x.var.idx]),
                                     y =  sprintf("asinh(%s / 5)", names(m)[y.var.idx]), colour = "beadDist"), data = m)
        + ggplot2::geom_point()
        + ggplot2::scale_colour_gradientn(breaks = 0:20, colours = rainbow(3))
        + ggplot2::theme(legend.position = "top", legend.key.width = unit(0.1, "npc"))
    )
    return(p)

}
