plot_color_coded_biaxial <- function(m, x.var, y.var, color.var, color.breaks = NULL) {
    x.var.idx <- which(colnames(m) == x.var)
    y.var.idx <- which(colnames(m) == y.var)
    color.var.idx <- which(colnames(m) == color.var)

    m <- data.frame(m)


    m <- hexbin_downsample(m, x.var.idx, y.var.idx)

    if(is.null(color.breaks))
        color.breaks <- ggplot2::waiver()

    (p <- ggplot2::ggplot(ggplot2::aes_string(x = names(m)[x.var.idx],
                                              y =  names(m)[y.var.idx], colour = names(m)[color.var.idx]), data = m)
        + ggplot2::geom_point()
        + ggplot2::scale_colour_gradientn(breaks = color.breaks, colours = rainbow(3))
        + ggplot2::theme(legend.position = "top", legend.key.width = ggplot2::unit(0.1, "npc"))
    )
    return(p)

}


hexbin_downsample <- function(m, x.var, y.var, ...) {
    hex <- hexbin::hexbin(m[, x.var], m[, y.var], xbins = 250, IDs = T, ...)
    return(m[!duplicated(hex@cID),])
}
