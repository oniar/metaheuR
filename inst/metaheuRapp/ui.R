#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(
  navbarPage(
    "Optimizazio heuristikorako R pakete baten hedapena",
    
    #-----------INSTANTZIAK-----------#
    tabPanel(
      "Instantziak",
      column(6, sidebarLayout(
        fluidRow(sidebarPanel(
          selectInput(
            inputId = "Problema",
            label = "Hautatu problema: ",
            choices = c(
              "Grafoa koloreztatzearen problema",
              "Motxilaren problema",
              "Garraio problema"
            )
          )
        )),
        fluidRow(mainPanel(
          tags$h3("Ezaugarriak: "), tags$h4(textOutput("problemaEzaugarriak"))
        ))
      )),
      column(6, mainPanel(
        tags$h3("Code: "), verbatimTextOutput("problemaKodea")
      ))
      
    ),
    
    #-----------ALGORITMOA-----------#
    tabPanel("Algoritmoa",
             column(12,sidebarLayout(
               fluidRow(sidebarPanel(
                 selectInput(
                   inputId = "Algoritmoa",
                   label = "Hautatu algoritmoa: ",
                   choices = c(
                     "Grafoa koloreztatzearen problema",
                     "Motxilaren problema",
                     "Garraio problema"
                   )
                 )
               )),
               fluidRow(mainPanel(
                 tags$h3("Ezaugarriak: "), tags$h4(textOutput("algoritmoEzaugarriak"))
               ))
             ))),
    
    
    #-----------EXEKUZIOA-----------#
    tabPanel("Exekuzioa",
             column(8, sidebarLayout(
               fluidRow(sidebarPanel(
                 tags$h3("Gelditzeko irizpideak: "),
                 fluidRow(column(6, numericInput(
                   "t",
                   label = "t: ",
                   value = 0
                 )),
                 
                 column(
                   6, numericInput("Eb",
                                   label = "Eb: ",
                                   value = 0)
                 )),
                 
                 
                 actionButton("run", label = "Run")
               )),
               fluidRow(plotOutput("plotProgresioa"))
             )),
             column(4, mainPanel(
               tags$h3("Code: "), verbatimTextOutput("algoritmoKodea")
             )))
    
  )
)
