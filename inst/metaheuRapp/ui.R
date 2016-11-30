#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(navbarPage("Optimizazio heuristikorako R pakete baten hedapena",
  tabPanel(
  # Application title
  "Instantziak",
  
  # Sidebar with a slider input for number of bins
  column(6,sidebarLayout(# Show a plot of the generated distribution
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
    fluidRow(mainPanel(tags$h3("Ezaugarriak: "),tags$h4(textOutput("distPlot"))))
)
),column(6,mainPanel(tags$h3("Code: "),verbatimTextOutput("distPlot2")))
  
),

tabPanel(
  # Application title
  "Algoritmoa",
  
  # Sidebar with a slider input for number of bins
  column(6,sidebarLayout(# Show a plot of the generated distribution
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
    fluidRow(mainPanel(tags$h3("Ezaugarriak: "),tags$h4(textOutput("distPlot3"))))
  )
  ),column(6,mainPanel(tags$h3("Code: "),verbatimTextOutput("distPlot4")))
  
),

tabPanel(
  # Application title
  "Exekuzioa",
  
  # Sidebar with a slider input for number of bins
  column(6,sidebarLayout(# Show a plot of the generated distribution
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
    fluidRow(mainPanel(tags$h4(textOutput("distPlot5"))))
  )
  ),column(6,mainPanel(verbatimTextOutput("distPlot6")))
  
)

))
