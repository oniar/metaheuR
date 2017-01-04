Algoritmoa <- tabPanel("Algoritmoa",
                       column(6, sidebarLayout(fluid = TRUE,
                         fluidRow(sidebarPanel(width = 12,
                           algoritmoa
                           )),
                         fluidRow(mainPanel(width = 12,
                           tags$h3("Ezaugarriak: "), tags$h4(textOutput("algoritmoEzaugarriak"))
                         ))
                       )),
                       
                       
                       
                       
                       
                       
                       column(4, fluidRow(mainPanel(width = 12,
                         uiOutput("uiAlg")
                       ))),
                       column(2, mainPanel(width = 12,
                         uiOutput("ui.ranking")
                       ))
                      
                      )
