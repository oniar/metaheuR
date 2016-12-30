Instantziak<-tabPanel(
  "Instantziak",
  column(6, sidebarLayout(
    fluidRow(sidebarPanel(
      problema
    )),
    fluidRow(mainPanel(
      tags$h3("Ezaugarriak: "), tags$h4(textOutput("problemaEzaugarriak"))
      ))
  )),
  column(6, mainPanel(
    tags$h3("Code: "), verbatimTextOutput("problemaKodea")
  ))
  
)