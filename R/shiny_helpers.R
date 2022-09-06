find_dna_channel <- function(fcs) {
    return(grep("Ir193", flowCore::parameters(fcs)$name))
}

find_bead_channels <- function(fcs, bead.type = c("Fluidigm", "Beta","XT")) {
    bead.type <- match.arg(bead.type)
    grep.string <- switch(bead.type,
        Fluidigm = "Ce140|Eu151|Eu153|Ho165|Lu175",
        Beta = "La139|Pr141|Tb159|Tm169|Lu175",
        XT = "Y89|In115|Ce140|Tb159|Lu175|Bi209"
    )

    return(grep(grep.string, flowCore::parameters(fcs)$name))
}

get_parameter_name <- function(fcs, i) {
    return(as.vector(unname(flowCore::parameters(fcs)$name[i])))
}

find_beads_channels_names <-  function(fcs, bead.type = c("Fluidigm", "Beta","XT")) {
    beads.cols <- find_bead_channels(fcs, bead.type)
    return(get_parameter_name(fcs, beads.cols))
}

get_beads_type_from_description <- function(s) {
    ret <- unlist(regmatches(s, regexec("Fluidigm|Beta|XT", s)))
    return(ret)
}


get_initial_beads_gates <- function(fcs) {
    beta.beads <- find_bead_channels(fcs, "Beta")
    fluidigm.beads <- find_bead_channels(fcs, "Fluidigm")
    xt.beads <- find_bead_channels(fcs,'XT')
    beads.cols <- union(beta.beads, fluidigm.beads)
    beads.cols <- union(beads.cols,xt.beads)

    ret <- list()

    col.names <- get_parameter_name(fcs, beads.cols)

    for(x in col.names)
        ret[[x]] <- list(x = c(2, 5), y = c(-1, 2))

    return(ret)
}



#' Starts the normalizer GUI
#'
#' Upon starting, a file selection window will appear from your R session. You should use
#' this window to navigate to the directory containing the data you want to analyze,
#' and select any file in that directory. The directory itself will then become
#' the working directory for the software.
#' To stop the software simply hit the "ESC" key in your R session.
#'
#' @param launch.browser Whether to open the GUI in a separate browser window
#'   (recommended)
#' @param ... Additional arguments to be passed to \code{shiny::runApp}
#' @export
normalizer_GUI <- function(launch.browser = TRUE, ...) {
    shiny::runApp(appDir = file.path(system.file(package = "premessa"), "normalizer_shinyGUI"),
                  launch.browser = launch.browser, ...)
}

#' Starts the debarcoder GUI
#'
#' To stop the software simply hit the "ESC" key in your R session.
#'
#' @inheritParams normalizer_GUI
#' @export
debarcoder_GUI <- function(launch.browser = TRUE, ...) {
    shiny::runApp(appDir = file.path(system.file(package = "premessa"), "debarcoder_shinyGUI"),
                  launch.browser = launch.browser, ...)
}

#' Starts the panel editor GUI
#'
#' Upon starting, a file selection window will appear from your R session. You should use
#' this window to navigate to the directory containing the data you want to analyze,
#' and select any file in that directory. The directory itself will then become
#' the working directory for the software.
#' To stop the software simply hit the "ESC" key in your R session.
#'
#' @inheritParams normalizer_GUI
#' @export
paneleditor_GUI <- function(launch.browser = TRUE, ...) {
    shiny::runApp(appDir = file.path(system.file(package = "premessa"), "paneleditor_shinyGUI"),
                  launch.browser = launch.browser, ...)
}

