Instantziak<-tabPanel(
  "Instantziak",
  column(4, sidebarLayout(
    fluidRow(sidebarPanel(
      problema
    )),
    fluidRow(mainPanel(
      tags$h3("Ezaugarriak: "), tags$h4(textOutput("problemaEzaugarriak"))
      ))
  )),
  
  column(4, mainPanel(
    uiOutput("uiInst"),
    tags$h4(tableOutput("matrize"))
 
  )),
  
  column(4, mainPanel(
    tags$h3("Code: "), verbatimTextOutput("problemaKodea")
  ))
  
)