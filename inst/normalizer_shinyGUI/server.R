gatePlot <- function (outputId) {
    HTML(paste("<div id=\"", outputId, "\" class=\"shiny-gateplot\"><canvas id=\"gatePlotCanvas\"></canvas></div>", sep=""))
}

render_concatenate_ui <- function(working.directory, ...) {renderUI({
    fluidPage(
        fluidRow(
            column(12,
                #selectizeInput("concatenateui_selected_files", multiple = T, width = "100%", choices = c()),
                shinyjqui::orderInput("concatenateui_available_files", "Available files", connect = "concatenateui_files_order",
                                      items = list.files(working.directory, pattern = "*.fcs$", ignore.case = TRUE), width = "100%"),
                shinyjqui::orderInput("concatenateui_files_order", "File order", placeholder = "Drag files here",
                                      items = NULL, connect = "concatenateui_available_files", width = "100%"),
                actionButton("concatenateui_concatenate_files", "Concatenate files")

            )
        )
    )




})}


render_beadremoval_ui <- function(working.directory, ...) {renderUI({
    fluidPage(
        fluidRow(
            column(12,
                   selectizeInput("beadremovalui_beads_type", "Select beads type", multiple = FALSE, width = "100%",
                                   choices = c("Fluidigm Beads EQ4 (140,151,153,165,175)",
                                               "Fluidigm Beads EQ6 (89, 115, 140, 159, 175, 209)",
                                               "Beta Beads (139,141,159,169,175)")
                               ),
                   selectizeInput("beadremovalui_selected_fcs", "Select FCS file",
                                  choices = c("", list.files(file.path(working.directory, "normed"), pattern = "*.fcs$", ignore.case = T)), multiple = FALSE, width = "100%"),
                   numericInput("beadremovalui_cutoff", "Cutoff for bead removal", value = 0, min = 0, max = 20),
                   actionButton("beadremovalui_remove_beads", "Remove beads (current file)"),
                   actionButton("beadremovalui_remove_beads_all_files", "Remove beads (all files)")

            )
        )
    )


})}


render_normalizer_ui <- function(working.directory, ...){renderUI({
    fluidPage(
        fluidRow(
            column(12,
                selectizeInput("normalizerui_beads_type", "Select beads type", multiple = FALSE, width = "100%",
                               choices = c("Fluidigm_EQ4 beads (140,151,153,165,175)",
                                           "Fluidigm_EQ6 beads (89,115,140,159,175,209)",
                                           "Beta Beads (139,141,159,169,175)")
                               ),
                selectizeInput("normalizerui_selected_fcs", "Select FCS file",
                            choices = c("", list.files(working.directory, pattern = "*.fcs$", ignore.case = T)), multiple = FALSE, width = "100%"),
                fluidRow(
                    column(6,
                        selectizeInput("normalizerui_baseline", "Select baseline for normalization", multiple = FALSE, width = "100%",
                            choices = c("Current files", "Existing folder of beads files"))
                    ),
                    column(6,
                        conditionalPanel(
                            condition <- "input.normalizerui_baseline != 'Current files'",
                            p("Selected baseline beads folder"),
                            verbatimTextOutput("normalizerui_dialog1")
                        )
                    )
                ),
                p("You have gated beads for the following files (Only these files will be normalized):"),
                verbatimTextOutput("normalizerui_dialog2"),
                actionButton("normalizerui_visualize_beads", "Visualize beads"),
                actionButton("normalizerui_apply_gates_all_files", "Apply current gates to all files"),
                actionButton("normalizerui_normalize_files", "Normalize")
            )
        )
    )
})}

generate_normalizerui_plot_outputs <- function(n) {renderUI({
    lapply(1:n, function(i) {
        column(2,
            gatePlot(paste("normalizerui_gateplot", i, sep = ""))
        )
    })

})}


generate_beadremovalui_plot_outputs <- function(n) {renderUI({
    lapply(1:n, function(i) {
        column(4,
            plotOutput(paste("beadremovalui_plot", i, sep = ""))
        )
    })
})}

shinyServer(function(input, output, session) {
    #options(warn = -1)
    working.directory <- dirname(file.choose())
    normed.dir <- file.path(working.directory, "normed")
    beads.removed.dir <- file.path(normed.dir, "beads_removed")
    beadremovalui.plots.number <- 3

    output$concatenateUI <- render_concatenate_ui(working.directory)
    output$normalizerUI <- render_normalizer_ui(working.directory, input, output, session)
    output$normalizerUI_plot_outputs <- generate_normalizerui_plot_outputs(6)
    output$beadremovalUI <- render_beadremoval_ui(working.directory, input, output, session)
    output$beadremovalUI_plot_outputs <- generate_beadremovalui_plot_outputs(beadremovalui.plots.number)

    #concatenateUI functions

    observe({
        if(!is.null(input$concatenateui_concatenate_files) && input$concatenateui_concatenate_files != 0) {
            input.files <- file.path(working.directory, input$concatenateui_files_order)
            out.file <- file.path(working.directory, gsub(".fcs$", "_concat.fcs", input$concatenateui_files_order[[1]], ignore.case = TRUE))
            showModal(modalDialog(
                title = "Normalizer report",
                "File concatenation started, please wait..."
            ))
            premessa::concatenate_fcs_files(input.files, out.file)
            showModal(modalDialog(
                title = "Normalizer report",
                p("Files concatenated in this order:", br(),
                  lapply(input$concatenateui_files_order, function(x) list(x, br())),
                  br(), "Outuput file: ", basename(out.file)
                )
            ))
            updateSelectizeInput(session, "normalizerui_selected_fcs",
                                 choices = c("", list.files(working.directory, pattern = "*.fcs$", ignore.case = T)))
        }
    })

    #beadremovalUI functions

    get_beadremovalui_fcs <- reactive({
        ret <- NULL

        if(!is.null(input$beadremovalui_selected_fcs) && input$beadremovalui_selected_fcs != "")
            ret <- flowCore::read.FCS(file.path(normed.dir, input$beadremovalui_selected_fcs))

        return(ret)
    })

    observeEvent(input$beadremovalui_remove_beads, {
            isolate({
                dir.create(beads.removed.dir, recursive = T)
                showModal(modalDialog(
                    title = "Normalizer report",
                    "Bead removal started, please wait..."
                ))
                premessa::remove_beads_from_file(file.path(normed.dir, input$beadremovalui_selected_fcs),
                                        input$beadremovalui_cutoff, beads.removed.dir)
                showModal(modalDialog(
                    title = "Normalizer report",
                    sprintf("Beads removed from file: %s", input$beadremovalui_selected_fcs)
                ))
            })
        }
    )

    observeEvent(input$beadremovalui_remove_beads_all_files, {
            isolate({
                dir.create(beads.removed.dir, recursive = T)
                files.list <- list.files(normed.dir, pattern = "*.fcs$", ignore.case = T)
                showModal(modalDialog(
                    title = "Normalizer report",
                    "Bead removal started, please wait..."
                ))
                files.list <- lapply(files.list, function(f.name) {
                    fcs <- flowCore::read.FCS(file.path(normed.dir, f.name))
                    premessa::remove_beads_from_file(file.path(normed.dir, f.name), input$beadremovalui_cutoff, beads.removed.dir)
                    return(f.name)
                })
                showModal(modalDialog(
                    title = "Normalizer report",
                    p("Beads removed from files:", br(),
                        lapply(files.list, function(x) list(x, br()))
                    )
                ))
            })
        }
    )

    observe({
        if(!is.null(input$beadremovalui_selected_fcs) && input$beadremovalui_selected_fcs != "") {
            fcs <- flowCore::read.FCS(file.path(normed.dir, input$beadremovalui_selected_fcs))
            beads.type <-  premessa:::get_beads_type_from_description(input$beadremovalui_beads_type)

            beads.cols.names <- premessa:::find_beads_channels_names(fcs, beads.type)
            combs <- rep(beads.cols.names, length.out = beadremovalui.plots.number * 2)

            m <- flowCore::exprs(fcs)

            lapply(seq(1, length(combs), 2), function(i) {
                plot.idx <- ceiling(i / 2)
                plot.output <- premessa:::plot_distance_from_beads(m, combs[i], combs[i + 1])
                output[[paste("beadremovalui_plot", plot.idx, sep ="")]] <- renderPlot(plot.output)
            })
        }
    })



    #normalizerUI functions

    beads.gates <- reactiveValues()
    normalizerui.baseline.dir <- NULL

    output$normalizerui_dialog1 <- renderText({
        if(!is.null(input$normalizerui_baseline) && length(grep("^Existing", input$normalizerui_baseline)) > 0) {
            normalizerui.baseline.dir <<- dirname(file.choose())
            return(normalizerui.baseline.dir)
        }
    })

    output$normalizerui_dialog2 <- renderText({
        paste(names(beads.gates), collapse = ", ")
    })


    observeEvent(input$normalizerui_apply_gates_all_files, {
        isolate({
            cur.gates <- get_beads_gates_for_current_file()
            files.list <- list.files(working.directory, pattern = "*.fcs$", ignore.case = T)
            lapply(files.list, function(f.name) {
                beads.gates[[f.name]] <- cur.gates
                NULL
            })
        })
    })

    get_beads_gates_for_current_file <- reactive({
            if(!input$normalizerui_selected_fcs %in% names(beads.gates)) {
                fcs <- get_fcs()
                beads.gates[[input$normalizerui_selected_fcs]] <<- premessa:::get_initial_beads_gates(fcs)
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
            m <- m[sample(1:nrow(m), 50000),]
        }
        return(m)

    })

    get_beads_type <- reactive({
        premessa:::get_beads_type_from_description(input$normalizerui_beads_type)
    })



    do_plot_outputs <- function(sel.beads = NULL) {
        fcs <- get_fcs()
        beads.type <- get_beads_type()
        beads.cols <- premessa:::find_bead_channels(fcs, beads.type)
        dna.col <- premessa:::find_dna_channel(fcs)

        gates <- isolate({get_beads_gates_for_current_file()})

        m <- get_exprs()
        colors <- rep("black", nrow(m))
        if(!is.null(sel.beads))
            colors[sel.beads] <- "red"

        #Needs to be in lapply to work
        #see https://github.com/rstudio/shiny/issues/532
        lapply(1:length(beads.cols), function(i) {
            xAxisName <- premessa:::get_parameter_name(fcs, beads.cols[i])
            yAxisName <- premessa:::get_parameter_name(fcs, dna.col)

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
        isolate({
            beads.type <- get_beads_type()
            baseline <- NULL
            if(length(grep("^Existing", input$normalizerui_baseline)) > 0)
                baseline <- normalizerui.baseline.dir

            showModal(modalDialog(
                title = "Normalizer report",
                "Normalization started, please wait..."
            ))
            premessa::normalize_folder(working.directory, "normed",
                                       reactiveValuesToList(beads.gates), beads.type, baseline = baseline)
            updateSelectizeInput(session, input$beadremovalui_selected_fcs,
                                 choices = c("", list.files(normed.dir, pattern = "*normalized.fcs$", ignore.case = T)))
            showModal(modalDialog(
                title = "Normalizer report",
                sprintf("Normalization complete! The output files are located in %s",
                        normed.dir)
            ))
        })

    })

    observeEvent(input$normalizerui_visualize_beads, {
        isolate({
            fcs <- get_fcs()
            m <- get_exprs()
            dna.col <- premessa:::find_dna_channel(fcs)

            gates <- get_beads_gates_for_current_file()
            beads.type <- get_beads_type()
            beads.cols <- premessa:::find_bead_channels(fcs, beads.type)
            beads.cols.names <- premessa:::get_parameter_name(fcs, beads.cols)

            sel <- premessa:::identify_beads(m, gates, beads.cols.names, dna.col)
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
