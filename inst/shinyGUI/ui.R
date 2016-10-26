shinyUI(
    navbarPage("cytofNormalizeR",
        tabPanel("Normalize data", uiOutput("normalizerUI"))
    )
)
