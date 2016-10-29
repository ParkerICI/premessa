gatePlot <- function (outputId) {
    HTML(paste("<div id=\"", outputId, "\" class=\"shiny-gateplot\"><canvas id=\"gatePlotCanvas\"></canvas></div>", sep=""))
}




render_normalizer_ui <- function(working.directory, ...){renderUI({
    fluidPage(
        fluidRow(
            column(9,
                selectizeInput("normalizerui_beads_type", "Select beads type", multiple = FALSE, width = "100%",
                               choices = c("Fluidigm Beads (140,151,153,165,175)", "Beta Beads (139,141,159,169,175)")),
                selectizeInput("normalizerui_selected_fcs", "Select FCS file",
                            choices = c("", list.files(working.directory, pattern = "*.fcs$")), multiple = FALSE, width = "100%"),
                verbatimTextOutput("dialog")
            )
        )
    )
})}

generate_plot_outputs <- function(n) {renderUI({
    lapply(1:n, function(i) {
        column(2,
            gatePlot(paste("normalizerui_gateplot", i, sep = ""))
        )
    })


})}


shinyServer(function(input, output, session) {
    working.directory <- "C:\\Users\\fgherardini\\temp\\bead-normalization\\sample_data"
    output$normalizerUI <- render_normalizer_ui(working.directory, input, output, session)
    output$normalizerUI_plot_outputs <- generate_plot_outputs(5)


    #fcs <- read.FCS(file.path(working.directory, "20120222_cells_found.fcs"))
    #m <- exprs(fcs)
    #m <- asinh(m / 5)
    #m <- m[1:100000,]

    observe({
        if(!is.null(input$normalizerui_selected_fcs) && input$normalizerui_selected_fcs != "") {
            fcs <- flowCore::read.FCS(file.path(working.directory, input$normalizerui_selected_fcs))
            beads.type <- unlist(regmatches(input$normalizerui_beads_type, regexec("Fluidigm|Beta", input$normalizerui_beads_type)))
            beads.cols <- cytofNormalizeR:::find_bead_channels(fcs, beads.type)
            dna.col <- cytofNormalizeR:::find_dna_channel(fcs)

            m <- exprs(fcs)
            m <- asinh(m / 5)
            if(nrow(m) > 50000)
                m <- m[sample(1:nrow(m), 50000),]
            print(beads.cols)

            for(i in 1:length(beads.cols)) {
                output[[paste("normalizerui_gateplot", i, sep ="")]] <- reactive({
                    list(x = m[, i], y = m[, dna.col], xAxisName = "foo", yAxisName = "dsfsda", file = "pippo.fcs")
                })
                #output[[paste("normalizerui_gateplot", i)]] <- reactive({
                #    ret <- list(x = m[, i], y = m[, 1], xAxisName = "foo", yAxisName = "dsfsda", file = "pippo.fcs")
                #    print(ret)
                #    return(ret)
                #})
            }
        }



    })
})
