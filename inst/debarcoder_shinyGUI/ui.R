shinyUI(
    navbarPage("premessa",
        header = list(
            tags$head(tags$script(src = "shinyoutputbindings.js")),
            singleton(tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'spinner.css')))
        ),
        tabPanel("Debarcode data",
            uiOutput("debarcoderUI")
        )
    )
)



