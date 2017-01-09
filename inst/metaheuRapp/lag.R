library(shiny)
library(metaheuR)

source('server.R')

problema<-selectInput(
  inputId = "Problema",
  label = "Hautatu problema: ",
  choices = c(
    #"Travelling salesman problem",
    "Closest String Problem",
    "Farthest String Problem"
  ), 
  width = '100%',
  selected = TRUE
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

ingurunea.tsp<-selectInput(
  inputId = "ingurunea.tsp",
  label = "Hautatu ingurunea: ",
  choices = c("swapNeighborhood","exchangeNeighborhood", "insertNeighborhood")
)

ingurunea.str<-selectInput(
  inputId = "ingurunea.str",
  label = "Hautatu ingurunea: ",
  choices = c("hammingNeighborhood")
)

selector<-selectInput(
  inputId = "selector",
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

file.input.csp<-fileInput(
  inputId = "ireki.fitx.csp",
  label = "Ireki zure fitxategia"
)

file.input.fsp<-fileInput(
  inputId = "ireki.fitx.fsp",
  label = "Ireki zure fitxategia"
)

sortu.matrizea.tsp<-actionButton(
  inputId = "sor.mat.tsp",
  label = "Sortu"
)

sortu.matrizea.str<-actionButton(
  inputId = "sor.mat.str",
  label = "Sortu"
)

tamaina.matrizea<-numericInput(
  inputId = "tam.mat",
  label = "n matrize zabalera sartuz sortu ausazko matrizea",
  value = 4, 
  min = 4
)

ausazko.matrizea<-actionButton(
  inputId = "aus.mat.tsp",
  label = "Sortu"
)

alfabeto.csp<-textInput(
  inputId = "alfabeto.csp",
  label = "Sartu alfabetoa",
  placeholder = "a b c ..."
)

alfabeto.fsp<-textInput(
  inputId = "alfabeto.fsp",
  label = "Sartu alfabetoa",
  placeholder = "a b c ..."
)

populazio.tamaina<-numericInput(
  inputId = "populazio.tamaina",
  label = "Hasierako populazioa hausaz sortzen den arren, populazioaren tamaina sartu:",
  value = 100
)

subpopulazioa<-selectInput(
  inputId = "subpopulazioa",
  label = "Hautatu subpopulazioaren hautaketa: ",
  choices = c("elitistSelection", "tournamentSelection", "rouletteSelection")
)


use.ranking<-selectInput(
  inputId = "use.ranking",
  label = "Rangkinak erabili nahi dituzu?",
  choices = c("Bai","Ez")
)

selection.ratio<-sliderInput(
  inputId = "selection.ratio",
  label = "Selection ratio",
  min = 0,
  max = 1,
  value = 0.5
)

gurutzaketa.aukeratu<-selectInput(
  inputId = "gurutzaketa",
  label = "Hautatu gurutzaketaren hautaketa: ",
  choices = c("elitistSelection", "tournamentSelection", "rouletteSelection")
)

gurutzaketa.tsp<-selectInput(
  inputId = "gurutzaketa.tsp",
  label = "Hautatu gurutzaketa: ",
  choices = c("orderCrossover")
)

gurutzaketa.str<-selectInput(
  inputId = "gurutzaketa.str",
  label = "Hautatu gurutzaketa: ",
  choices = c("kPointCrossover")
)

cross.k<-numericInput(
  inputId = "cross.k",
  label = "K",
  value = 2,
  min = 1
)

mutazioa.tsp<-selectInput(
  inputId = "mutazioa.tsp",
  label = "Mutazioa",
  choices = c("swapMutation")
)

mutazioa.str<-selectInput(
  inputId = "mutazioa.str",
  label = "Mutazioa",
  choices = c("factorMutation")
)

ratio<-sliderInput(
  inputId = "ratio",
  label = "Ratio",
  min = 0,
  max = 1,
  value = 0.5
)

mutation.rate<-sliderInput(
  inputId = "mutation.rate",
  label = "Mutation Rate",
  min = 0,
  max = 1,
  value = 0.5
)


denbora<-numericInput(
  "denbora",
  label = "Denbora: ",
  value = 0
)

eb.kopurua<-numericInput("eb.kopurua",
                         label = "Ebaluazioak: ",
                         value = 0)

it.kopurua<-numericInput("it.kopurua",
                         label = "Iterazioak: ",
                         value = 0)

run<-actionButton("run",
                  label = "Run")

