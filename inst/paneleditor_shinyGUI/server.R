


render_paneleditor_ui <- function(working.directory, ...) {renderUI({
    fluidPage(
        fluidRow(
            column(6,
                textInput("paneleditorui_output_folder", label = "Output folder name", value = "renamed")
            ),
            column(3,
                actionButton("paneleditorui_process_files", "Process files")
            )
        ),
        fluidRow(
            column(12,
                    rhandsontable::rHandsontableOutput("paneleditorui_panel_table")
            )
        )
    )
})}


get_panel_table <- function(files.list) {
    print("Reading FCS parameters...")
    panel.table <- premessa:::read_parameters(files.list)
    print("Done")
    common.names <- premessa:::get_common_names(panel.table)
    problem.idx <- premessa:::get_problem_idx(panel.table, common.names)
    panel.table <- panel.table[, order(colSums(problem.idx), decreasing = T)]


    panel.table <- data.frame(common.names, panel.table, check.names = F, stringsAsFactors = F)
    names(panel.table)[1] <- "Most common"
    return(panel.table)
}



shinyServer(function(input, output, session) {
    working.directory <- dirname(file.choose())

    output$paneleditorUI <- render_paneleditor_ui(working.directory)

    files.list <- list.files(working.directory, pattern = "*.fcs", ignore.case = T)
    files.list <- file.path(working.directory, files.list)

    panel.table <- get_panel_table(files.list)



    observe({
        if(!is.null(input$paneleditorui_process_files) &&
            input$paneleditorui_process_files != 0) {

            isolate({
                df <- rhandsontable::hot_to_r(input$paneleditorui_panel_table)
                to.remove <- NULL
                if(any(df$Remove))
                    to.remove <- row.names(df)[df$Remove]
                df$Remove <- NULL
                panel.table$"Most common" <- df$"Most common" <- NULL
                premessa:::process_files(working.directory, input$paneleditorui_output_folder, 
                    panel.table, df, to.remove)

            })
        }
    })

    output$paneleditorui_panel_table <- rhandsontable::renderRHandsontable({
        df <- data.frame(Remove = FALSE, panel.table, check.names = F, stringsAsFactors = F)

        hot <- rhandsontable::rhandsontable(df, rowHeaderWidth = 100)
        hot <- rhandsontable::hot_cols(hot, fixedColumnsLeft = 2, renderer = "
            function(instance, td, row, col, prop, value, cellProperties) {
                if(col == 0)
                    Handsontable.CheckboxCell.renderer.apply(this, arguments)
                else {
                    Handsontable.TextCell.renderer.apply(this, arguments)

                    if(instance.params != null) { 
                        if(instance.params.data[row][0])
                            td.style.background = 'lightgrey'
                        else if(value != instance.params.data[row][1] && col != 0)
                            td.style.background = 'lightpink'
                    }
                }
                return(td)
            }"
        )
        hot
    })
})


