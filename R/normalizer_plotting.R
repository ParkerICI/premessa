plot_beads_over_time <- function(beads.data, beads.normed, beads.cols) {
    beads.data <- data.frame(beads.data, type = "Before", check.names = F, stringsAsFactors =  F)
    beads.normed <- data.frame(beads.normed, type = "After", check.names =  F, stringsAsFactors =  F)
    m <- rbind(beads.data, beads.normed)
    m <- data.frame(m[, beads.cols], Time = m$Time, type = m$type, check.names = F, stringsAsFactors = F)

    plot_beads_medians(m, "Time")
}



plot_beads_medians <- function(tab, x.var = "sample") {

    m <- reshape::melt.data.frame(tab, id.vars = c(x.var, "type"))
    m$value <- asinh(m$value / 5)

    (p <- ggplot2::ggplot(ggplot2::aes_string(x = x.var, y = "value", color = "variable", group = "variable"), data = m)
        + ggplot2::facet_wrap(~type, ncol = 1)
        + ggplot2::geom_line()
        + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
        + ggplot2::scale_y_continuous("Intensity (asinh transformed)")
    )
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


