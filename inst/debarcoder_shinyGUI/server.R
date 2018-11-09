render_debarcoder_ui <- function(...){renderUI({
    fluidPage(
        fluidRow(
            column(4,
                fluidRow(
                    column(8,
                        p("Current barcode key")
                    ),
                    column(4,
                        actionButton("debarcoderui_select_key", "Select key")
                    )
                ),
                verbatimTextOutput("debarcoderui_dialog_selected_key"),
                fluidRow(
                    column(8,
                        p("Current FCS file")
                    ),
                    column(4,
                        actionButton("debarcoderui_select_fcs", "Select FCS")
                    )
                ),
                verbatimTextOutput("debarcoderui_dialog_selected_fcs"),
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
                actionButton("debarcoderui_plot_data",
                                tagList(tags$div(id = "debarcoderui_plot_data_text", "Plot data"),
                                        tags$div(id = "debarcoderui_plot_data_spinner", class = "spinner", style = "visibility: hidden")),
                                style = "position: relative"),
                actionButton("debarcoderui_save_files", "Save files")
            ),
            column(8,
                plotOutput("debarcoderui_plot1", height = "200px"),
                plotOutput("debarcoderui_plot2")
            )
        )
    )
})}


shinyServer(function(input, output, session) {
    #options(warn = -1)

    output$debarcoderUI <- render_debarcoder_ui(input, output, session)


    #debarcoderUI functions

    debarcoderui.reactive.values <- reactiveValues(bc.key.fname = file.path(system.file(package = "premessa"), "Fluidigm_20plex_barcode_key.csv"),
                                        fcs.fname = NULL)


    debarcoderui_get_bc_key <- reactive({
        if(!is.null(debarcoderui.reactive.values$bc.key.fname) && debarcoderui.reactive.values$bc.key.fname != "")
            return(premessa:::read_barcode_key(debarcoderui.reactive.values$bc.key.fname))
    })

    debarcoderui_get_fcs <- reactive({
        ret <- NULL
        if(!is.null(debarcoderui.reactive.values$fcs.fname) && debarcoderui.reactive.values$fcs.fname != "")
            ret <- flowCore::read.FCS(debarcoderui.reactive.values$fcs.fname)
        return(ret)
    })

    debarcoderui_get_exprs <- reactive({
            fcs <- debarcoderui_get_fcs()

            ret <- NULL
            if(!is.null(fcs)) {
                m <- flowCore::exprs(fcs)
                ret <- asinh(m / 10)
            }


            return(ret)
    })

    debarcoderui_get_mahalanobis_distance <- reactive({
        m <- debarcoderui_get_exprs()
        bc.res <- debarcoderui_get_bc_results()
        if(!is.null(m) && !is.null(bc.res))
            return(premessa:::get_mahalanobis_distance(m, bc.res,
                    input$debarcoderui_separation_threshold))
    })


    debarcoderui_get_bc_results <- reactive({
        bc.key <- debarcoderui_get_bc_key()
        m <- debarcoderui_get_exprs()

        if(!is.null(bc.key) && !is.null(m)) {
            res <- premessa:::debarcode_data(m, bc.key)

            all.labels <- unique(res$labels)
            all.labels <- sort(all.labels[!is.na(all.labels)])
            updateSelectizeInput(session, "debarcoderui_selected_sample", choices = all.labels)

            updateSelectizeInput(session, "debarcoderui_xaxis", choices = res$bc.channels)
            updateSelectizeInput(session, "debarcoderui_yaxis", choices = res$bc.channels)

            return(res)
        }
    })

    output$debarcoderui_dialog_selected_key <- renderText({
        debarcoderui.reactive.values$bc.key.fname
    })

    output$debarcoderui_dialog_selected_fcs <- renderText({
        debarcoderui.reactive.values$fcs.fname
    })

    output$debarcoderui_plot1 <- renderPlot({
        if(!is.null(input$debarcoderui_plot_data) && input$debarcoderui_plot_data) {
            session$sendCustomMessage(type = "plot_loading", "none")
            ret <- NULL
            isolate({
                bc.res <- debarcoderui_get_bc_results()
                if(!is.null(bc.res)) {
                    if(input$debarcoderui_plot_type == "Separation")
                        ret <- premessa:::plot_separation_histogram(bc.res)
                    else if(input$debarcoderui_plot_type == "Event" || input$debarcoderui_plot_type == "Single biaxial"
                            || input$debarcoderui_plot_type == "All barcode biaxials") {
                        mahal.dist <- debarcoderui_get_mahalanobis_distance()
                        ret <- premessa:::plot_barcode_yields(bc.res, input$debarcoderui_separation_threshold,
                                                    input$debarcoderui_mahal_dist_threshold, mahal.dist)
                    }
                }
            })
            return(ret)
        }
    })

    output$debarcoderui_plot2 <- renderPlot({
        if(!is.null(input$debarcoderui_plot_data) && input$debarcoderui_plot_data) {
            session$sendCustomMessage(type = "plot_loading", "none")
            ret <- NULL
            isolate({
                bc.res <- debarcoderui_get_bc_results()
                if(!is.null(bc.res)) {
                    if(input$debarcoderui_plot_type == "Separation")
                        ret <- premessa:::plot_barcode_separation(bc.res, input$debarcoderui_separation_threshold)
                    else {
                        m <- debarcoderui_get_exprs()
                        mahal.dist <- debarcoderui_get_mahalanobis_distance()
                        m <- cbind(m, mahal.dist = mahal.dist)
                        sel.rows <- premessa:::get_sample_idx(input$debarcoderui_selected_sample,
                                    bc.res, input$debarcoderui_separation_threshold, input$debarcoderui_mahal_dist_threshold, mahal.dist)
                        m <- m[sel.rows, ]

                        if(input$debarcoderui_plot_type == "Event")
                            ret <- premessa:::plot_barcode_channels_intensities(m, bc.res$bc.channels, bc.res$m.normed[sel.rows,])
                        else {
                            if(input$debarcoderui_plot_type == "Single biaxial")
                                ret <- premessa:::plot_color_coded_biaxial(m, input$debarcoderui_xaxis,
                                                input$debarcoderui_yaxis, "mahal.dist")
                            else if(input$debarcoderui_plot_type == "All barcode biaxials")
                                ret <- premessa:::plot_all_barcode_biaxials(m, bc.res$bc.channels)
                        }
                    }
                }
            })
            return(ret)
        }
    })


    observeEvent(input$debarcoderui_save_files, {
        isolate({
            fcs <- debarcoderui_get_fcs()
            bc.key <- debarcoderui_get_bc_key()
            if(!is.null(fcs) && !is.null(bc.key)) {
                fcs.fname <- debarcoderui.reactive.values$fcs.fname
                out.dir <- file.path(dirname(fcs.fname), "debarcoded")
                dir.create(out.dir, recursive = T)
                showModal(modalDialog(
                    title = "Debarcoder report",
                    "Debarcoding started, please wait..."
                ))
                premessa:::debarcode_fcs(fcs, bc.key, out.dir,
                        tools::file_path_sans_ext(basename(fcs.fname)), input$debarcoderui_separation_threshold, input$debarcoderui_mahal_dist_threshold)
                showModal(modalDialog(
                    title = "Debarcoder report",
                    sprintf("Debarcoding complete! The output files are located in %s",
                            out.dir)
                ))
            }
        })
    })

    observeEvent(input$debarcoderui_select_key, {
        isolate({
            debarcoderui.reactive.values$bc.key.fname <- file.choose()
        })
    })

    observeEvent(input$debarcoderui_select_fcs, {
        isolate({
            debarcoderui.reactive.values$fcs.fname <- file.choose()
        })
    })
})
