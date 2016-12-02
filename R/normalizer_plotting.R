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


#' Biaxial plot color-coded according to the Mahalanobis distance from the beads population
#'
#' @param m The untrasformed matrix of data. Must contain a column named \code{beadDist} containing the
#'  Mahalanobis distance
#' @param x.var The variable on the x axis
#' @param y.var The variable on the y axis
plot_distance_from_beads <- function(m, x.var, y.var) {
    m[, x.var] <- asinh(m[, x.var] / 5)
    m[, y.var] <- asinh(m[, y.var] / 5)
    m[m[, "beadDist"] > 20, "beadDist"] <- 20

    ret <- plot_color_coded_biaxial(m, x.var, y.var, "beadDist", 0:20)
    return(ret)
}


