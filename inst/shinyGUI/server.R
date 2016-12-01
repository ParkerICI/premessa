gatePlot <- function (outputId) {
    HTML(paste("<div id=\"", outputId, "\" class=\"shiny-gateplot\"><canvas id=\"gatePlotCanvas\"></canvas></div>", sep=""))
}




render_beadremoval_ui <- function(working.directory, ...) {renderUI({
    fluidPage(
        fluidRow(
            column(12,
                   selectizeInput("beadremovalui_beads_type", "Select beads type", multiple = FALSE, width = "100%",
                                  choices = c("Fluidigm Beads (140,151,153,165,175)", "Beta Beads (139,141,159,169,175)")),
                   selectizeInput("beadremovalui_selected_fcs", "Select FCS file",
                                  choices = c("", list.files(file.path(working.directory, "normed"), pattern = "*.fcs$")), multiple = FALSE, width = "100%"),
                   numericInput("beadremovalui_cutoff", "Cutoff for bead removal", value = 0, min = 0, max = 20),
                   verbatimTextOutput("beadremovalui_dialog"),
                   actionButton("beadremovalui_remove_beads", "Remove beads (current file)"),
                   actionButton("beadremovalui_remove_beads_all_files", "Remove beads (all files)")

            )
        )
    )


})}


render_debarcoder_ui <- function(working.directory, ...){renderUI({
    fluidPage(
        fluidRow(
            column(4,
                selectizeInput("debarcoderui_selected_fcs", "Select FCS file",
                    choices = c("", list.files(working.directory, pattern = "*.fcs$")), multiple = FALSE, width = "100%"),
                selectizeInput("debarcoderui_selected_key", "Select barcode key",
                               choices = c("", list.files(working.directory, pattern = "*.csv$")), multiple = FALSE, width = "100%"),
                numericInput("debarcoderui_separation_threshold", "Minimum separation", value = 0.3, min = 0, max = 1, step = 0.1, width = "100%"),
                numericInput("debarcoderui_mahal_dist_threshold", "Maxiumum Mahlanobis distance", value = 30, min = 0, max = 30, step = 1, width = "100%"),
                selectizeInput("debarcoderui_plot_type", "Select plot type", multiple = FALSE, width = "100%",
                    choices = c("Separation", "Event", "Single biaxial", "All barcode biaxials")),
                conditionalPanel(
                    condition <- "input.debarcoderui_plot_type != 'Separation'",
                    selectizeInput("debarcoderui_selected_sample", "Select sample", choices = c(""), multiple = FALSE, width = "100%")
                ),
                conditionalPanel(
                    condition <- "input.debarcoderui_plot_type == 'Single biaxial'",
                    selectizeInput("debarcoderui_xaxis", "Select x axis", choices = c(""), multiple = FALSE, width = "100%"),
                    selectizeInput("debarcoderui_yaxis", "Select y axis", choices = c(""), multiple = FALSE, width = "100%")
                ),
                actionButton("debarcoderui_save_files", "Save files")
            ),
            column(8,
                plotOutput("debarcoderui_plot1", height = "200px"),
                plotOutput("debarcoderui_plot2")
            )
        )
    )
})}

render_normalizer_ui <- function(working.directory, ...){renderUI({
    #Remove this fluidpage?
    fluidPage(
        fluidRow(
            column(12,
                selectizeInput("normalizerui_beads_type", "Select beads type", multiple = FALSE, width = "100%",
                               choices = c("Fluidigm Beads (140,151,153,165,175)", "Beta Beads (139,141,159,169,175)")),
                selectizeInput("normalizerui_selected_fcs", "Select FCS file",
                            choices = c("", list.files(working.directory, pattern = "*.fcs$")), multiple = FALSE, width = "100%"),
                selectizeInput("normalizerui_baseline", "Select baseline for normalization", multiple = FALSE, width = "100%",
                            choices = c("Current files", "Existing folder of beads files")),
                p("You have gated beads for the following files (Only these files will be normalized):"),
                verbatimTextOutput("normalizerui_dialog"),
                actionButton("normalizerui_identify_beads", "Identify beads"),
                actionButton("normalizerui_apply_gates_all_files", "Apply current gates to all files"),
                actionButton("normalizerui_normalize_files", "Normalize")
            )
        )
    )
})}

remove_beads_from_file <- function(fcs, cutoff, input.fname, out.dir) {
    if(!is.null(fcs)) {
        beads.dir <- file.path(out.dir, "removed_events")
        dir.create(beads.dir, recursive = T)
        base.fname <- tools::file_path_sans_ext(input.fname)
        data.fname <- paste(base.fname, "beadsremoved.fcs", sep = "_")
        beads.fname <- paste(base.fname, "removedEvents.fcs", sep = "_")

        temp <- cytofNormalizeR::remove_beads_from_fcs(fcs, cutoff)
        flowCore::write.FCS(temp$data.fcs, file.path(out.dir, data.fname))
        flowCore::write.FCS(temp$beads.fcs, file.path(beads.dir, beads.fname))
    }
}

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

    output$normalizerUI <- render_normalizer_ui(working.directory, input, output, session)
    output$normalizerUI_plot_outputs <- generate_normalizerui_plot_outputs(5)
    output$beadremovalUI <- render_beadremoval_ui(working.directory, input, output, session)
    output$beadremovalUI_plot_outputs <- generate_beadremovalui_plot_outputs(beadremovalui.plots.number)
    output$debarcoderUI <- render_debarcoder_ui(working.directory, input, output, session)


    #debarcoderUI functions

    debarcoderui_get_bc_key <- reactive({
        if(!is.null(input$debarcoderui_selected_key) && input$debarcoderui_selected_key != "")
            return(cytofNormalizeR:::read_barcode_key(file.path(working.directory, input$debarcoderui_selected_key)))
    })

    debarcoderui_get_fcs <- reactive({
        if(!is.null(input$debarcoderui_selected_fcs) && input$debarcoderui_selected_fcs != "")
            return(flowCore::read.FCS(file.path(working.directory, input$debarcoderui_selected_fcs)))
    })

    debarcoderui_get_exprs <- reactive({
            fcs <- debarcoderui_get_fcs()
            if(!is.null(fcs)) {
                m <- flowCore::exprs(fcs)
                return(asinh(m / 10))
            }
    })

    debarcoderui_get_mahalanobis_distance <- reactive({
        m <- debarcoderui_get_exprs()
        bc.res <- debarcoderui_get_bc_results()
        if(!is.null(m) && !is.null(bc.res))
            return(cytofNormalizeR:::get_mahalanobis_distance(m, bc.res,
                    input$debarcoderui_separation_threshold))
    })


    debarcoderui_get_bc_results <- reactive({
        bc.key <- debarcoderui_get_bc_key()
        m <- debarcoderui_get_exprs()

        if(!is.null(bc.key) && !is.null(m)) {
            res <- cytofNormalizeR:::debarcode_data(m, bc.key)

            all.labels <- unique(res$labels)
            all.labels <- sort(all.labels[!is.na(all.labels)])
            updateSelectizeInput(session, "debarcoderui_selected_sample", choices = all.labels)

            updateSelectizeInput(session, "debarcoderui_xaxis", choices = res$bc.channels)
            updateSelectizeInput(session, "debarcoderui_yaxis", choices = res$bc.channels)

            return(res)
        }
    })

    output$debarcoderui_plot1 <- renderPlot({
        bc.res <- debarcoderui_get_bc_results()
        if(!is.null(bc.res)) {
            if(input$debarcoderui_plot_type == "Separation")
                return(cytofNormalizeR:::plot_separation_histogram(bc.res))
            else if(input$debarcoderui_plot_type == "Event" || input$debarcoderui_plot_type == "Single biaxial"
                    || input$debarcoderui_plot_type == "All barcode biaxials") {
                mahal.dist <- debarcoderui_get_mahalanobis_distance()
                return(cytofNormalizeR:::plot_barcode_yields(bc.res, input$debarcoderui_separation_threshold,
                                            input$debarcoderui_mahal_dist_threshold, mahal.dist))
            }
        }
    })

    output$debarcoderui_plot2 <- renderPlot({
        bc.res <- debarcoderui_get_bc_results()
        if(!is.null(bc.res)) {
            if(input$debarcoderui_plot_type == "Separation")
                return(cytofNormalizeR:::plot_barcode_separation(bc.res, input$debarcoderui_separation_threshold))
            else {
                m <- debarcoderui_get_exprs()
                mahal.dist <- debarcoderui_get_mahalanobis_distance()
                m <- cbind(m, mahal.dist = mahal.dist)
                sel.rows <- cytofNormalizeR:::get_sample_idx(input$debarcoderui_selected_sample,
                            bc.res, input$debarcoderui_separation_threshold, input$debarcoderui_mahal_dist_threshold, mahal.dist)
                m <- m[sel.rows, ]

                if(input$debarcoderui_plot_type == "Event")
                    return(cytofNormalizeR:::plot_barcode_channels_intensities(m, bc.res$bc.channels, bc.res$m.normed[sel.rows,]))
                else {
                    if(input$debarcoderui_plot_type == "Single biaxial")
                        return(cytofNormalizeR:::plot_color_coded_biaxial(m, input$debarcoderui_xaxis,
                                        input$debarcoderui_yaxis, "mahal.dist"))
                    else if(input$debarcoderui_plot_type == "All barcode biaxials")
                        return(cytofNormalizeR:::plot_all_barcode_biaxials(m, bc.res$bc.channels))
                }
            }
        }
    })


    observeEvent(input$debarcoderui_save_files, {
        isolate({
            fcs <- debarcoderui_get_fcs()
            bc.key <- debarcoderui_get_bc_key()
            if(!is.null(fcs) && !is.null(bc.key)) {
                out.dir <- file.path(working.directory, "debarcoded")
                dir.create(out.dir, recursive = T)
                cytofNormalizeR:::debarcode_fcs(fcs, bc.key, out.dir,
                        tools::file_path_sans_ext(input$debarcoderui_selected_fcs), input$debarcoderui_separation_threshold, input$debarcoderui_mahal_dist_threshold)
            }
        })
    })

    #beadremovalUI functions

    beadremovalui.reactive.values <- reactiveValues(dialog.text = "")

    output$beadremovalui_dialog <- renderText({
        beadremovalui.reactive.values[["dialog.text"]]
    })

    get_beadremovalui_fcs <- reactive({
        ret <- NULL

        if(!is.null(input$beadremovalui_selected_fcs) && input$beadremovalui_selected_fcs != "")
            ret <- flowCore::read.FCS(file.path(normed.dir, input$beadremovalui_selected_fcs))

        return(ret)
    })

    observeEvent(input$beadremovalui_remove_beads, {
            isolate({
                fcs <- get_beadremovalui_fcs()
                dir.create(beads.removed.dir, recursive = T)
                remove_beads_from_file(fcs, input$beadremovalui_cutoff, input$beadremovalui_selected_fcs, beads.removed.dir)
                beadremovalui.reactive.values[["dialog.text"]] <- sprintf("Beads removed from file: %s", input$beadremovalui_selected_fcs)
            })
        }
    )

   observeEvent(input$beadremovalui_remove_beads_all_files, {
            isolate({
                dir.create(beads.removed.dir, recursive = T)
                files.list <- list.files(normed.dir, pattern = "*.fcs$")
                files.list <- lapply(files.list, function(f.name) {
                    fcs <- flowCore::read.FCS(file.path(normed.dir, f.name))
                    remove_beads_from_file(fcs, input$beadremovalui_cutoff, f.name, beads.removed.dir)
                    return(f.name)
                })
                beadremovalui.reactive.values[["dialog.text"]] <- sprintf("Beads removed from files: %s", paste(files.list, collapse = ", "))
            })
        }
    )

    observe({
        if(!is.null(input$beadremovalui_selected_fcs) && input$beadremovalui_selected_fcs != "") {
            fcs <- flowCore::read.FCS(file.path(normed.dir, input$beadremovalui_selected_fcs))
            beads.type <-  cytofNormalizeR:::get_beads_type_from_description(input$beadremovalui_beads_type)

            beads.cols.names <- cytofNormalizeR:::find_beads_channels_names(fcs, beads.type)
            combs <- rep(beads.cols.names, length.out = beadremovalui.plots.number * 2)

            m <- flowCore::exprs(fcs)

            lapply(seq(1, length(combs), 2), function(i) {
                plot.idx <- ceiling(i / 2)
                plot.output <- cytofNormalizeR:::plot_distance_from_beads(m, combs[i], combs[i + 1])
                output[[paste("beadremovalui_plot", plot.idx, sep ="")]] <- renderPlot(plot.output)
            })
        }
    })



    #normalizerUI functions

    beads.gates <- reactiveValues()

    output$normalizerui_dialog <- renderText({
        paste(names(beads.gates), collapse = ", ")
    })


    observeEvent(input$normalizerui_apply_gates_all_files, {
        isolate({
            cur.gates <- get_beads_gates_for_current_file()
            files.list <- list.files(working.directory, pattern = "*.fcs$")
            lapply(files.list, function(f.name) {
                beads.gates[[f.name]] <- cur.gates
                NULL
            })
        })
    })

    get_beads_gates_for_current_file <- reactive({
            if(!input$normalizerui_selected_fcs %in% names(beads.gates)) {
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
            m <- m[sample(1:nrow(m), 50000),]
        }
        return(m)

    })

    get_beads_type <- reactive({
        cytofNormalizeR:::get_beads_type_from_description(input$normalizerui_beads_type)
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
        isolate({
            beads.type <- get_beads_type()
            baseline <- NULL
            if(length(grep("^Existing", input$normalizerui_baseline)) > 0)
                baseline <- dirname(file.choose())

            cytofNormalizeR::normalize_folder(working.directory, "normed", beads.gates, beads.type, baseline = baseline)
            updateSelectizeInput(session, input$beadremovalui_selected_fcs,
                                 choices = c("", list.files(normed.dir, pattern = "*normalized.fcs$")))
        })

    })

    observeEvent(input$normalizerui_identify_beads, {
        isolate({
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
