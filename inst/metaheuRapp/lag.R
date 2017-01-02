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
  
  eskuz.matrizea.tsp<-textInput(
    inputId = "mat.tsp",
    label = "Sartu zure datuak banan banan matrizea eraikitzeko, n karratua izan behar du.",
    placeholder = "1 2 3 4 5 6 7 8 9 ..."
  )
  
  eskuz.matrizea.str<-textInput(
    inputId = "mat.str",
    label = "Sartu zure sententziak ',' batez banandurik.",
    placeholder = "a b c b, b a c a ..."
  )
  
  sortu.matrizea<-actionButton(
    inputId = "sor.mat",
    label = "Sortu"
  )
  
  tamaina.matrizea<-numericInput(
    inputId = "tam.mat",
    label = "n matrize zabalera sartuz sortu ausazko matrizea",
    value = 2, 
    min = 2
  )
  
  ausazko.matrizea<-actionButton(
    inputId = "aus.mat",
    label = "Sortu"
  )
  
  alfabetoa<-textInput(
    inputId = "alfabeto",
    label = "Sartu alfabetoa",
    placeholder = "a b c ..."
  )
  
  
  