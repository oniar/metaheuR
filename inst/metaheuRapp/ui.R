#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(metaheuR)
source('lag.R')
source('instantziak.R')
source('algoritmoa.R')
source('exekuzioa.R')

shinyUI(
  navbarPage(
    "Optimizazio heuristikorako R pakete baten hedapena",
    
    #-----------INSTANTZIAK-----------#
      Instantziak,
    
    
    #-----------ALGORITMOA-----------#
      Algoritmoa,
    
    
    #-----------EXEKUZIOA-----------#
      Exekuzioa
    
  )
)
