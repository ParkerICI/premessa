#' @export
cytofNormalizeR.run <- function(...) {
    shiny::runApp(appDir = file.path(system.file(package = "cytofNormalizeR"), "shinyGUI"), ...)
}
