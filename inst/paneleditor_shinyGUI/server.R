


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
    working.directory <- "C:/Users/fgherardini/temp/irina/fcs"

    output$paneleditorUI <- render_paneleditor_ui(working.directory)


    files.list <- list.files(working.directory, pattern = "*.fcs", ignore.case = T)
    files.list <- files.list[1:2]
    files.list <- file.path(working.directory, files.list)


    panel.table <- premessa:::read_parameters(files.list)
    common.names <- premessa:::get_most_common_names(panel.table)
    panel.table <- cbind(remove = FALSE, panel.table)

    print(panel.table[1:5,])
    print(common.names)
   

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
        rhandsontable::rhandsontable(panel.table, stretchH = "all") #%>%


        #hot_cols(renderer = "
        #    function(instance, td, row, col, prop, value, cellProperties) {
        #        Handsontable.TextCell.renderer.apply(this, arguments)
        #        console.log(instance)
        #        console.log(instance.params)
   # 
   #             Handsontable.TextCell.renderer.apply(this, arguments);
   #             console.log(HTMLWidgets.widgets)
   #             var tbl = HTMLWidgets.widgets.filter(function(widget) {
#
#                    return widget.name === 'rhandsontable'
#                })[0]
#                console.log('TBL')
#                console.log(tbl)
#   
#                //console.log(tbl.params.common_names)
#                //if(!(tbl.params.common_names.includes(value)))
#                 //   td.style.background = 'lightgreen'
#                return(td)
#            }"
#        )
    })
})