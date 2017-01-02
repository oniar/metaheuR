Algoritmoa <- tabPanel("Algoritmoa",
                       column(6, sidebarLayout(
                         fluidRow(sidebarPanel(algoritmoa)),
                         fluidRow(mainPanel(
                           tags$h3("Ezaugarriak: "), tags$h4(textOutput("algoritmoEzaugarriak"))
                         ))
                       )),
                       column(6, wellPanel(
                         uiOutput("uiAlg")
                       ))
                      
                      )
