shinyUI(
    navbarPage("premessa",
        tabPanel("Panel editor",
            fluidPage(
                fluidRow(
                    uiOutput("paneleditorUI")
                )
            )
        )
    )
)



