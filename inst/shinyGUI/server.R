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
                actionButton("normalizerui_identify_beads", "Identify beads"),
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

    beads.gates <- list()

    get_fcs <- reactive({
        ret <- NULL

        if(!is.null(input$normalizerui_selected_fcs) && input$normalizerui_selected_fcs != "")
            ret <- flowCore::read.FCS(file.path(working.directory, input$normalizerui_selected_fcs))

        return(ret)
    })

    observe({
        fcs <- get_fcs()
        if(!is.null(fcs)) {
            beads.type <- unlist(regmatches(input$normalizerui_beads_type, regexec("Fluidigm|Beta", input$normalizerui_beads_type)))
            beads.cols <- cytofNormalizeR:::find_bead_channels(fcs, beads.type)
            dna.col <- cytofNormalizeR:::find_dna_channel(fcs)

            if(!input$normalizerui_selected_fcs %in% names(beads.gates))
                beads.gates[[input$normalizerui_selected_fcs]] <<- cytofNormalizeR:::get_initial_beads_gates(fcs, beads.cols)


            m <- flowCore::exprs(fcs)
            m <- asinh(m / 5)
            if(nrow(m) > 50000)
                m <- m[sample(1:nrow(m), 50000),]

            #Needs to be in lapply to work
            #see https://github.com/rstudio/shiny/issues/532
            lapply(1:length(beads.cols), function(i) {
                xAxisName <- cytofNormalizeR:::get_parameter_name(fcs, beads.cols[i])
                yAxisName <- cytofNormalizeR:::get_parameter_name(fcs, dna.col)

                output[[paste("normalizerui_gateplot", i, sep ="")]] <- reactive({
                    list(
                        x = m[, beads.cols[i]],
                        y = m[, dna.col],
                        xAxisName = xAxisName,
                        yAxisName = yAxisName,
                        file = input$normalizerui_selected_fcs,
                        channelGates = beads.gates[[input$normalizerui_selected_fcs]][[xAxisName]]
                    )
                })
            })
        }

    })


    observeEvent(input$normalizerui_identify_beads, {
        isolate({
            fcs <- get_fcs()
            dna.col <- cytofNormalizeR:::find_dna_channel(fcs)
            cytofNormalizeR:::identify_beads(fcs, beads.gates[[input$normalizerui_selected_fcs]], dna.col)

        })

    })

    observe({
        if(!is.null(input$normalizerui_gate_selected)) {
            isolate({
                gate.data <- input$normalizerui_gate_selected
                print(input$normalizerui_selected_fcs)
                temp <- beads.gates[[input$normalizerui_selected_fcs]][[gate.data$xAxisName]]

                temp$x <- unlist(gate.data$xLim)
                temp$y <- unlist(gate.data$yLim)

                beads.gates[[input$normalizerui_selected_fcs]][[gate.data$xAxisName]] <<- temp
                print(beads.gates)
            })
        }

    })
})
