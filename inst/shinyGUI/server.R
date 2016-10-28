gatePlot <- function (outputId) {
    #HTML(paste("<div id=\"", outputId, "\" class=\"shiny-gateplot\"><svg /></div>", sep=""))
    HTML(paste("<div id=\"", outputId, "\" class=\"shiny-gateplot\"><canvas id=\"gatePlotCanvas\"></canvas></div>", sep=""))
}




render_normalizer_ui <- function(working.directory, ...){renderUI({
    fluidPage(
        fluidRow(
            tags$head(tags$script(src = "gate-plot.js")),
            tags$head(tags$script(src = "d3.min.js")),
            singleton(tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'gate-plot.css'))),
            column(3,
                gatePlot("plot1")
            ),
            column(3,
                gatePlot("plot2")
            ),
            column(3,
                gatePlot("plot3")
            ),
            column(3,
                gatePlot("plot4")
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
    #m <- exprs(fcs)
    #m <- asinh(m / 5)
    #m <- m[1:100000,]



    output$plot1 <- reactive({
        #ret <- list(x = m[, 3], y = m[,22])
        ret <- list(x = 1:10, y = 1:10)
        return(ret)
    })

    output$plot2 <- reactive({
        ret <- list(x = 1:10, y = 1:10)
        return(ret)
    })

    output$plot3 <- reactive({
        ret <- list(x = 1:10, y = 1:10)
        return(ret)
    })

    output$plot4 <- reactive({
        ret <- list(x = 1:10, y = 1:10)
        return(ret)
    })
})
