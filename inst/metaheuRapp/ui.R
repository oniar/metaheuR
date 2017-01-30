library(shiny)
library(metaheuR)

launchApp<-function (...){
  shiny::runApp(system.file("metaheuRapp",package="metaheuR"),...)
}

shinyUI(
  navbarPage(
    "Optimizazio heuristikorako R pakete baten hedapena",
    
    #-----------INSTANTZIAK-----------#
    tabPanel(
      "Instantziak",
      column(4, sidebarLayout(
        fluid = TRUE,
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
        fluidRow(
          mainPanel(width = 12,
                    tags$h3("Ezaugarriak: "), 
                    tags$h4(textOutput("problemaEzaugarriak"))
                    
          ))
      )),
      
      column(4, 
             mainPanel(width = 12,
                       uiOutput("uiInst"),
                       tags$h4(textOutput("alfabeto")),
                       tableOutput("matrize")
                       
             )),
      
      column(4, 
             mainPanel(width = 12,
                       tags$h3("Code: "), verbatimTextOutput("problemaKodea")
             ))
      
    ),
    
    
    #-----------ALGORITMOA-----------#
    tabPanel("Algoritmoa",
             column(6, 
                    sidebarLayout(
                      fluid = TRUE,
                      fluidRow(
                        sidebarPanel(width = 12,
                                     selectInput(
                                       inputId = "Algoritmoa",
                                       label = "Hautatu algoritmoa: ",
                                       choices = c(
                                         "Bilaketa lokala",
                                         "Algoritmo genetikoa"
                                       )
                                     )
                        )),
                      fluidRow(
                        mainPanel(width = 12,
                                  tags$h3("Ezaugarriak: "), tags$h4(textOutput("algoritmoEzaugarriak"))
                        ))
                    )),
             
             
             
             
             
             
             column(4, 
                    fluidRow(
                      mainPanel(width = 12,
                                uiOutput("uiAlg")
                      ))),
             column(2, 
                    mainPanel(width = 12,
                              uiOutput("ui.ranking")
                    ))
             
    ),
    
    
    #-----------EXEKUZIOA-----------#
    tabPanel("Exekuzioa",
             column(8, 
                    sidebarLayout(
                      fluidRow(sidebarPanel(
                        tags$h3("Gelditzeko irizpideak: "),
                        fluidRow(column(width = 4,
                                        numericInput(
                                          "denbora",
                                          label = "Denbora: ",
                                          value = 0
                                        )
                                        
                        ),
                        column(width = 4, 
                               numericInput("it.kopurua",
                                            label = "Iterazioak: ",
                                            value = 0)
                               
                        ),
                        column(width = 4, 
                               numericInput("eb.kopurua",
                                            label = "Ebaluazioak: ",
                                            value = 0)
                        )),
                        fluidRow(column(width = 4,offset = 4,
                                        actionButton("run",
                                                     label = "Run")
                                        
                        ))
                      )),
                      fluidRow(plotOutput("plotProgresioa"))
                    )),
             column(4, 
                    mainPanel(
                      tags$h3("Code: "), verbatimTextOutput("algoritmoKodea"), uiOutput("emaitzak"),textOutput("onin")
                    )))
    
  )
)
