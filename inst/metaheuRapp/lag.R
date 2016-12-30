library(shiny)
library(metaheuR)

  problema<-selectInput(
    inputId = "Problema",
    label = "Hautatu problema: ",
    choices = c(
      "Travelling salesman problem",
      "Closest String Problem",
      "Farthest String Problem"
    )
  )
  
  algoritmoa<-selectInput(
    inputId = "Algoritmoa",
    label = "Hautatu algoritmoa: ",
    choices = c(
      "Bilaketa lokala",
      "Algoritmo genetikoa"
    )
  )
  
  hasiera.soluzioa<-selectInput(
    inputId = "Has.sol",
    label = "Hautatu hasierako soluzioa: ",
    choices = c("Hausazkoa", "")
  )
  
  ingurunea<-selectInput(
    inputId = "Ingurunea",
    label = "Hautatu ingurunea: ",
    choices = c("esdfa", "fasdfa","sdafasdfa")
  )
  
  selector<-selectInput(
    inputId = "Selector",
    label = "Hautatu selector: ",
    choices = c("greedy","first improvement")
  )
  
  restart.estrategia<-selectInput(
    inputId = "Restart",
    label = "Hautatu restart estrategia: ",
    choices = c("ausazkoa", "perturbazioa")
  )
  
  ratio<-sliderInput(
    inputId = "Ratio",
    label = "Hautatu ratioa",
    min = 0,
    max = 1,
    value = 0.5
  )
  