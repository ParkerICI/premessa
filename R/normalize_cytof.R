slide_function <- function(data, window, fun, step = 1, ...) {
    res <- NULL
    total <- length(data)

    if(total >= window) {
        spots <- seq(from = 1, to = (total - window + 1), by = step)
        res <- vapply(spots, FUN = function(i) {return(fun(data[i:(i + window - 1)], ...))}, FUN.VALUE = 1)
    }
    else
        res <- fun(data, ...)

    return(res)
}

smooth_beads <- function(beads.data, window.size = 500) {
    ret <- apply(beads.data, 2, function(x) {return(slide_function(x, window = window.size, fun = median))})
    return(ret)
}

compute_bead_slopes <- function(beads.data, baseline, beads.col.names) {
    beads.data <- beads.data[, beads.col.names]
    baseline <- baseline[, beads.col.names]

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
    int.slopes <- approx(tt, y, m[, time.cole.name], method = "linear", rule = 2)
    return(m * int.slopes)
}


