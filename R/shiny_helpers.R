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





#' @export
cytofNormalizeR.run <- function(...) {
    shiny::runApp(appDir = file.path(system.file(package = "cytofNormalizeR"), "shinyGUI"), ...)
}
