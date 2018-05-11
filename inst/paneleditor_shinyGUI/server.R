


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
    panel.table <- panel.table[, order(colSums(problem.idx), decreasing = T), drop = F]

    panel.table <- data.frame(Parameter = row.names(panel.table), common.names, panel.table, check.names = F, stringsAsFactors = F)
    names(panel.table)[2] <- "Most common"
    return(panel.table)
}



shinyServer(function(input, output, session) {
    working.directory <- dirname(file.choose())

    output$paneleditorUI <- render_paneleditor_ui(working.directory)

    files.list <- list.files(working.directory, pattern = "*.fcs$", ignore.case = T)
    files.list <- file.path(working.directory, files.list)

    panel.table <- get_panel_table(files.list)



    observe({
        if(!is.null(input$paneleditorui_process_files) &&
            input$paneleditorui_process_files != 0) {

            isolate({
                df <- rhandsontable::hot_to_r(input$paneleditorui_panel_table)
                for(i in 1:ncol(df))
                    df[, i] <- gsub("absent", NA, df[, i])
                df$Remove <- as.logical(df$Remove)


                panel.table$"Most common" <- df$"Most common" <- NULL

                showModal(modalDialog(
                    title = "Panel editor report",
                    "File processing started, please wait..."
                ))

                premessa:::rename_parameters_in_files(working.directory, input$paneleditorui_output_folder, df)

                showModal(modalDialog(
                    title = "Panel editor report",
                    sprintf("File processing completed. The output files are located in: %s", file.path(working.directory, input$paneleditorui_output_folder))
                ))


            })
        }
    })

    output$paneleditorui_panel_table <- rhandsontable::renderRHandsontable({
        temp <- panel.table

        for(i in 1:ncol(temp))
            temp[, i][is.na(temp[, i])] <- "absent"

        df <- data.frame(Remove = FALSE, temp, check.names = F, stringsAsFactors = F)


        hot <- rhandsontable::rhandsontable(df, rowHeaderWidth = 100)
        hot <- rhandsontable::hot_cols(hot, fixedColumnsLeft = 3, renderer = "
            function(instance, td, row, col, prop, value, cellProperties) {
                if(col == 0)
                    Handsontable.renderers.CheckboxRenderer.apply(this, arguments)
                else {
                    Handsontable.renderers.TextRenderer.apply(this, arguments)

                    if(instance.params != null) {
                        if(instance.params.data[row][0])
                            td.style.background = 'lightgrey'
                        else {
                            if(value == 'absent')
                                td.style.background = 'orange'
                            else if(value != instance.params.data[row][2] && col > 2)
                                td.style.background = 'lightpink'
                        }
                    }
                }
                return(td)
            }"
        )
        hot
    })
})


