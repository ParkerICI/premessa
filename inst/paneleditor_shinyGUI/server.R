


render_paneleditor_ui <- function(working.directory, ...) {renderUI({
    fluidPage(
        fluidRow(
            column(12,
                    rhandsontable::rHandsontableOutput("paneleditorui_panel_table")
            )
        ),
        fluidRow(
            column(12,
                actionButton("paneleditorui_process_files", "Process files")
            )
        )
    )
})}


shinyServer(function(input, output, session) {
    library(rhandsontable)
    working.directory <- "C:/Users/fgherardini/temp/irina/fcs"

    output$paneleditorUI <- render_paneleditor_ui(working.directory)


    files.list <- list.files(working.directory, pattern = "*.fcs", ignore.case = T)

    files.list <- file.path(working.directory, files.list)

    print("Reading FCS parameters...")
    panel.table <- premessa:::read_parameters(files.list)
    print("Done")
    common.names <- premessa:::get_common_names(panel.table)
    #panel.table <- cbind(remove = FALSE, panel.table)



   
    reactive_values <- reactiveValues(problem_idx = NULL)

    observe({
        if(!is.null(input$paneleditorui_panel_table))
            reactive_values$problem_idx <- premessa:::get_problem_idx(hot_to_r(input$paneleditorui_panel_table), common.names)
    })


    observe({
        if(!is.null(input$paneleditorui_process_files) &&
            input$paneleditorui_process_files != 0) {

            isolate({
                df <- rhandsontable::hot_to_r(input$paneleditorui_panel_table)
                print("FIXMEE")
                df$remove <- NULL
                panel.table$remove <- NULL
                premessa:::process_files(working.directory, "renamed", 
                    panel.table, df)

            })
        }
    })

    output$paneleditorui_panel_table <- rhandsontable::renderRHandsontable({

        
        
        rhandsontable::rhandsontable(panel.table, common_names = common.names) %>%


        hot_cols(renderer = "
            function(instance, td, row, col, prop, value, cellProperties) {
                Handsontable.TextCell.renderer.apply(this, arguments)
                if(instance.params != null && value != instance.params.common_names[row]) {
                    td.style.background = 'lightpink'
                }
                return(td)
            }"
        )
    })
})