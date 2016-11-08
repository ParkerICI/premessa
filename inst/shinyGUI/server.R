gatePlot <- function (outputId) {
    HTML(paste("<div id=\"", outputId, "\" class=\"shiny-gateplot\"><canvas id=\"gatePlotCanvas\"></canvas></div>", sep=""))
}




render_beadremoval_ui <- function(working.directory, ...) {renderUI({
    fluidPage(
        fluidRow(
            column(9,
                   selectizeInput("beadremovalui_beads_type", "Select beads type", multiple = FALSE, width = "100%",
                                  choices = c("Fluidigm Beads (140,151,153,165,175)", "Beta Beads (139,141,159,169,175)")),
                   selectizeInput("beadremovalui_selected_fcs", "Select FCS file",
                                  choices = c("", list.files(file.path(working.directory, "normed"), pattern = "*.fcs$")), multiple = FALSE, width = "100%")
            )
        )
    )


})}


render_normalizer_ui <- function(working.directory, ...){renderUI({
    #Remove this fluidpage?
    fluidPage(
        fluidRow(
            column(9,
                selectizeInput("normalizerui_beads_type", "Select beads type", multiple = FALSE, width = "100%",
                               choices = c("Fluidigm Beads (140,151,153,165,175)", "Beta Beads (139,141,159,169,175)")),
                selectizeInput("normalizerui_selected_fcs", "Select FCS file",
                            choices = c("", list.files(working.directory, pattern = "*.fcs$")), multiple = FALSE, width = "100%"),
                actionButton("normalizerui_identify_beads", "Identify beads"),
                actionButton("normalizerui_normalize_files", "Normalize"),
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
    output$beadremovalUI <- render_beadremoval_ui(working.directory, input, output, session)

    beads.gates <- reactiveValues()


    get_beads_gates_for_current_file <- reactive({
            if(!input$normalizerui_selected_fcs %in% names(beads.gates)) {
                print("Initializing gates")
                fcs <- get_fcs()
                beads.gates[[input$normalizerui_selected_fcs]] <<- cytofNormalizeR:::get_initial_beads_gates(fcs)
            }
            beads.gates[[input$normalizerui_selected_fcs]]
    })


    get_fcs <- reactive({
        ret <- NULL

        if(!is.null(input$normalizerui_selected_fcs) && input$normalizerui_selected_fcs != "")
            ret <- flowCore::read.FCS(file.path(working.directory, input$normalizerui_selected_fcs))

        return(ret)
    })

    get_exprs <- reactive({
        fcs <- get_fcs()
        m <- flowCore::exprs(fcs)
        m <- asinh(m / 5)
        if(nrow(m) > 50000) {
            print("Subsampling")
            m <- m[sample(1:nrow(m), 50000),]
        }

    })

    get_beads_type <- reactive({
        unlist(regmatches(input$normalizerui_beads_type, regexec("Fluidigm|Beta", input$normalizerui_beads_type)))
    })



    do_plot_outputs <- function(sel.beads = NULL) {
        fcs <- get_fcs()
        beads.type <- get_beads_type()
        beads.cols <- cytofNormalizeR:::find_bead_channels(fcs, beads.type)
        dna.col <- cytofNormalizeR:::find_dna_channel(fcs)

        gates <- isolate({get_beads_gates_for_current_file()})

        m <- get_exprs()
        colors <- rep("black", nrow(m))
        if(!is.null(sel.beads))
            colors[sel.beads] <- "red"

        #Needs to be in lapply to work
        #see https://github.com/rstudio/shiny/issues/532
        lapply(1:length(beads.cols), function(i) {
            xAxisName <- cytofNormalizeR:::get_parameter_name(fcs, beads.cols[i])
            yAxisName <- cytofNormalizeR:::get_parameter_name(fcs, dna.col)

            output[[paste("normalizerui_gateplot", i, sep ="")]] <- reactive({
                list(
                    x = m[, beads.cols[i]],
                    y = m[, dna.col],
                    color = colors,
                    xAxisName = xAxisName,
                    yAxisName = yAxisName,
                    file = input$normalizerui_selected_fcs,
                    channelGates = gates[[xAxisName]]
                )
            })
        })
    }

    observe({
        fcs <- get_fcs()
        if(!is.null(fcs)) {
            do_plot_outputs()
        }

    })

    observeEvent(input$normalizerui_normalize_files, {
        beads.type <- get_beads_type()
        cytofNormalizeR::normalize_folder(working.directory, "normed", beads.gates, beads.type)

    })

    observeEvent(input$normalizerui_identify_beads, {
        isolate({
            print("Identifying")
            fcs <- get_fcs()
            m <- get_exprs()
            dna.col <- cytofNormalizeR:::find_dna_channel(fcs)

            gates <- get_beads_gates_for_current_file()
            beads.type <- get_beads_type()
            beads.cols <- cytofNormalizeR:::find_bead_channels(fcs, beads.type)
            beads.cols.names <- cytofNormalizeR:::get_parameter_name(fcs, beads.cols)

            sel <- cytofNormalizeR:::identify_beads(m, gates, beads.cols.names, dna.col)
            do_plot_outputs(sel)
        })

    })

    observe({
        if(!is.null(input$normalizerui_gate_selected)) {
            isolate({
                #Change this to use get_beads_gates_for_current_file ??
                gate.data <- input$normalizerui_gate_selected
                temp <- beads.gates[[input$normalizerui_selected_fcs]][[gate.data$xAxisName]]

                temp$x <- unlist(gate.data$xLim)
                temp$y <- unlist(gate.data$yLim)

                beads.gates[[input$normalizerui_selected_fcs]][[gate.data$xAxisName]] <<- temp

            })
        }

    })
})
