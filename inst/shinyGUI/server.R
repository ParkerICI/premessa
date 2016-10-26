render_normalizer_ui <- function(working.directory, ...){renderUI({
    fluidPage(
        fluidRow(
            column(12,
                p("Test")
            )
        )
    )
})}


shinyServer(function(input, output, session) {
    working.directory <- "C:\\Users\\fgherardini\\temp\\bead-normalization\\sample_data"
    output$normalizerUI <- render_normalizer_ui(working.directory, input, output, session)
})
