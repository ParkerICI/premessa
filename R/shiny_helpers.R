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

find_beads_channels_names <-  function(fcs, bead.type = c("Fluidigm", "Beta")) {
    beads.cols <- find_bead_channels(fcs, bead.type)
    return(get_parameter_name(fcs, beads.cols))
}

get_beads_type_from_description <- function(s) {
    ret <- unlist(regmatches(s, regexec("Fluidigm|Beta", s)))
    return(ret)
}


get_initial_beads_gates <- function(fcs) {
    beta.beads <- find_bead_channels(fcs, "Beta")
    fluidigm.beads <- find_bead_channels(fcs, "Fluidigm")
    beads.cols <- union(beta.beads, fluidigm.beads)

    ret <- list()

    col.names <- get_parameter_name(fcs, beads.cols)

    for(x in col.names)
        ret[[x]] <- list(x = c(2, 5), y = c(-1, 2))

    return(ret)
}




#' @export
cytofNormalizeR.run <- function(...) {
    shiny::runApp(appDir = file.path(system.file(package = "cytofNormalizeR"), "shinyGUI"), ...)
}
