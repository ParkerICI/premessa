shinyUI(
    navbarPage("premessa",
        tabPanel("Normalize data",
            tags$head(tags$script(src = "gate-plot.js")),
            tags$head(tags$script(src = "d3.min.js")),
            singleton(tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'gate-plot.css'))),
            fluidPage(
                fluidRow(
                    uiOutput("normalizerUI")
                ),
                fluidRow(
                    uiOutput("normalizerUI_plot_outputs")

                )
            )
        ),
        tabPanel("Remove beads",
            fluidPage(
                fluidRow(
                    uiOutput("beadremovalUI")
                ),
                fluidRow(
                    uiOutput("beadremovalUI_plot_outputs")
                )
            )
        )
    )
)



