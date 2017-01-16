Instantziak<-tabPanel(
  "Instantziak",
  column(4, sidebarLayout(fluid = TRUE,
                          fluidRow(
                            sidebarPanel(width = 12,
                                         selectInput(
                                           inputId = "Problema",
                                           label = "Hautatu problema: ",
                                           choices = c(
                                             "Travelling salesman problem",
                                             "Closest String Problem",
                                             "Farthest String Problem"
                                           ), 
                                           width = '100%',
                                           selected = TRUE
                                         )
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