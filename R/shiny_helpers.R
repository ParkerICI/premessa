find_dna_channel <- function(fcs) {
    return(grep("Ir193", flowCore::parameters(fcs)$name))
}

find_bead_channels <- function(fcs, bead.type = c("Fluidigm", "Beta")) {
    bead.type <- match.arg(bead.type)
    grep.string <- switch(bead.type,
        Fluidigm = "Ce140|Eu151|Eu153|Ho165|Lu175",
        Beta = "La139|Pr141|Tb159|Tm169|Lu175"
    )

    return(grep(grep.string, flowCore::parameters(fcs)$name))
}

get_parameter_name <- function(fcs, i) {
    return(as.vector(unname(flowCore::parameters(fcs)$name[i])))
}

get_initial_beads_gates <- function(fcs, beads.cols) {
    ret <- list()

    col.names <- get_parameter_name(fcs, beads.cols)

    for(x in col.names)
        #ret <- c(ret, setNames(list(list(x = c(0, 3), y = c(0, 5))), x))
        ret[[x]] <- list(x = c(0, 3), y = c(0, 5))

    return(ret)
}


identify_beads <- function(m, gates, dna.col) {
    sel <- lapply(names(gates), function(n) {
        g <- gates[[n]]
        ret <- (m[,n] > g$x[1]) & (m[,n] < g$x[2]) & (m[,dna.col] > g$y[1]) & (m[,dna.col] < g$y[2])

    })

    return(Reduce("&", sel))
}



#' @export
cytofNormalizeR.run <- function(...) {
    shiny::runApp(appDir = file.path(system.file(package = "cytofNormalizeR"), "shinyGUI"), ...)
}
