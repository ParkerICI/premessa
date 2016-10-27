gatePlot <- function (outputId) {
    #HTML(paste("<div id=\"", outputId, "\" class=\"shiny-gateplot\"><svg /></div>", sep=""))
    HTML(paste("<div id=\"", outputId, "\" class=\"shiny-gateplot\"><canvas id=\"gatePlotCanvas\"></canvas></div>", sep=""))
}




render_normalizer_ui <- function(working.directory, ...){renderUI({
    fluidPage(
        fluidRow(
            tags$head(tags$script(src = "gate-plot.js")),
            tags$head(tags$script(src = "d3.min.js")),
            column(12,
                p("Test"),
                gatePlot("plot")

            )
        )
    )
})}


shinyServer(function(input, output, session) {
    working.directory <- "C:\\Users\\fgherardini\\temp\\bead-normalization\\sample_data"
    output$normalizerUI <- render_normalizer_ui(working.directory, input, output, session)
    library(plotly)
    library(flowCore)

   #fcs <- read.FCS(file.path(working.directory, "20120222_cells_found.fcs"))
#    m <- exprs(fcs)
#    m <- asinh(m / 5)
#    m <- m[1:100000,]
#    m <- data.frame(m)
#    print(m[1:5,])

    output$plot <- reactive({
        ret <- list(x = rnorm(100, mean = 100, sd = 10), y = rnorm(100, mean = 100, sd = 10))
        return(ret)
    })
})
