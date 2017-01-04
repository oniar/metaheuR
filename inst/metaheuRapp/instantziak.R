Instantziak<-tabPanel(
  "Instantziak",
  column(4, sidebarLayout(fluid = TRUE,
    fluidRow(
      sidebarPanel(width = 12,
        problema
      )
    ),
    fluidRow(mainPanel(width = 12,
      tags$h3("Ezaugarriak: "), 
      tags$h4(textOutput("problemaEzaugarriak"))
      
    ))
  )),
  
  column(4, mainPanel(width = 12,
    uiOutput("uiInst"),
    tags$h4(textOutput("alfabeto")),
    tableOutput("matrize")
    
  )),
  
  column(4, mainPanel(width = 12,
    tags$h3("Code: "), verbatimTextOutput("problemaKodea")
  ))
  
)