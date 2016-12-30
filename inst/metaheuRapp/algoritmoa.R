Algoritmoa <- tabPanel("Algoritmoa",
                       column(4, sidebarLayout(
                         fluidRow(sidebarPanel(algoritmoa)),
                         fluidRow(mainPanel(
                           tags$h3("Ezaugarriak: "), tags$h4(textOutput("algoritmoEzaugarriak"))
                         ))
                       )),
                       column(4, wellPanel(
                         uiOutput("ui")
                       ))
                      #  ,
                      #    column(4, sidebarLayout(
                      #    fluidRow(sidebarPanel(hasiera.soluzioa)),
                      #    fluidRow(sidebarPanel(ingurunea))
                      #    
                      # ))
                      )
