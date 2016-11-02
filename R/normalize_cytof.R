slide_function <- function(data, window, fun, step = 1, ...) {
    res <- NULL
    total <- length(data)

    if(total >= window) {
        spots <- seq(from = 1, to = (total - window + 1), by = step)
        res <- vapply(spots, FUN = function(i) {return(median(data[i:(i + window - 1)], ...))}, FUN.VALUE = 1)
    }
    else
        res <- fun(data, ...)

    return(res)
}

smooth_beads <- function(beads.data, window.size = 201) {
    #ret <- apply(beads.data, 2, function(x) {return(slide_function(x, window = window.size, fun = median))})

    ret <- apply(beads.data, 2, runmed, k = window.size)
    return(ret)

}

compute_bead_slopes <- function(beads.data, baseline, beads.col.names) {
    beads.data <- beads.data[, beads.col.names]
    baseline <- baseline[beads.col.names]

    stopifnot(length(baseline) == dim(beads.data)[2])
    m <- t(t(beads.data) * baseline)

    #Minimizing residual sum of squares, assuming a no-intercept model
    ret <- rowSums(m) / rowSums(beads.data ^ 2)
    return(ret)
}


correct_channels <- function(m, beads.data, baseline, beads.col.names, time.col.name = "Time") {
    sm.beads <- smooth_beads(beads.data)
    slopes <- compute_bead_slopes(beads.data, baseline, beads.col.names)

    tt <- beads.data[, time.col.name]
    y <- slopes
    int.slopes <- approx(tt, y, m[, time.col.name], method = "linear", rule = 2)
    return(m * int.slopes$y)
}

identify_beads <- function(m, gates, beads.cols.names, dna.col) {
    sel <- lapply(beads.cols.names, function(n) {
        g <- gates[[n]]
        ret <- (m[,n] > g$x[1]) & (m[,n] < g$x[2]) & (m[,dna.col] > g$y[1]) & (m[,dna.col] < g$y[2])

    })

    return(Reduce("&", sel))
}

load_all_beads <- function(wd, beads.gates, beads.type) {
    lapply(names(beads.gates), function(f.name) {
        fcs <- flowCore::read.FCS(f.name)
        beads.cols <-  beads.cols <- find_bead_channels(fcs, beads.type)
        dna.col <- find_dna_channel(fcs)
        beads.cols.names <- get_parameter_name(fcs, beads.cols)

        m <- exprs(fcs)
        m <- asinh(m / 5)

        sel <- identify_beads(m, beads.gates, beads.col.names, dna.col)
        return(m[sel,])
    })

    browser()
}


normalize_folder <- function(wd, beads.gates, beads.type) {
    beads.data <- load_all_beads(wd, beads.gates, beads.type)


}
