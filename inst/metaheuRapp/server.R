library(shiny)
library(metaheuR)



shinyServer(function(input, output) {
  
  rvalues                     <-reactiveValues()
  
  rvalues$problema            <- NULL
  rvalues$ingurunea           <- NULL
  rvalues$alfabeto            <- NULL
  rvalues$initial.solution    <- NULL
  rvalues$matrix              <- NULL
  rvalues$initial.population  <- NULL
  rvalues$selectSubpopulation <- NULL
  rvalues$selectCross         <- NULL
  rvalues$cross               <- NULL
  rvalues$mutate              <- NULL
  rvalues$emaitza             <- NULL
  
  
  observeEvent(input$sor.inst,{
    if(input$Problema == "Travelling salesman problem"){
      if(!is.null(input$tam.mat)){
        rvalues$matrix<-matrix(runif(input$tam.mat ^ 2), ncol = input$tam.mat)
        rvalues$problema <- tspProblem(rvalues$matrix)
        showNotification("Problema ondo sortu da!")
      }else{
        showNotification("Matrizearen tamaina falta da")
      }
      
    }else{
      if(!(input$alfabeto=="")&&!is.null(input$ireki.fitx)){
        a<-input$alfabeto
        aa<-strsplit(a," ")
        alpha<-aa[[1]]
        rvalues$alfabeto<-alpha
        fi<-input$ireki.fitx
        d <- read.table(fi$datapath, header = TRUE, sep = " ")
        rvalues$matrix<-as.matrix(d)
        if(input$Problema == "Closest String Problem"){
          rvalues$problema <- closestStringProblem(rvalues$matrix,alpha)
        }else{
          rvalues$problema <- farthestStringProblem(rvalues$matrix,alpha)
        }
        showNotification("Problema ondo sortu da!")
      }else{
         showNotification("Errorea gertatu da!")
      }
      
    }
  }
  )
  
  observeEvent(input$sor.alg,{
    if(input$Algoritmoa == "Bilaketa lokala"){
      if(input$Problema == "Travelling salesman problem"){
        if(!is.null(input$tam.mat)){
          rvalues$initial.solution<-randomPermutation(input$tam.mat)
        }else{
          showNotification("Instantziak atala ez duzu ongi bete!")
        }
      }else{
        if(!is.null(rvalues$matrix))
        rvalues$initial.solution<-factor(replicate(ncol(rvalues$matrix), paste(sample(rvalues$alfabeto, 1, replace = TRUE), collapse = "")))
      }
      if(!is.null(rvalues$initial.solution)){
      switch (input$ingurunea,
              "swapNeighborhood" = h.ngh <- swapNeighborhood(base = rvalues$initial.solution),
              "exchangeNeighborhood" = h.ngh <- exchangeNeighborhood(base = rvalues$initial.solution),
              "insertNeighborhood" = h.ngh <- insertNeighborhood(base = rvalues$initial.solution),
              "hammingNeighborhood" = h.ngh <-hammingNeighborhood(base = rvalues$initial.solution)
      )
      rvalues$ingurunea <- h.ngh
      }else{
        showNotification("Instantziak atala ez duzu ongi bete!")
      }
    }else{##Algoritmo genetikoa
      if(input$Problema == "Travelling salesman problem"){
        n.pop.small <- input$tam.mat
        createRndSolution <- function(x) {
          sol <- randomPermutation(input$tam.mat)
        }
        pop.small <- lapply(1:n.pop.small, FUN=createRndSolution)
        rvalues$initial.population<-pop.small
        
      }else{
        if(!is.null(rvalues$matrix)&&!is.null(rvalues$alfabeto)){
        n.pop.small <- ncol(rvalues$matrix)
        levels <- rvalues$alfabeto
        createRndSolution <- function(x) {
          sol <- factor(replicate(ncol(rvalues$matrix), paste(sample(rvalues$alfabeto, 1, replace = TRUE), collapse = "")))
        }
        pop.small <- lapply(1:n.pop.small, FUN=createRndSolution)
        rvalues$initial.population<-pop.small
        }
      }
      switch (input$selectSubpopulation,
              "elitistSelection" = rvalues$selectSubpopulation <- elitistSelection,
              "tournamentSelection" = rvalues$selectSubpopulation <- tournamentSelection, 
              "rouletteSelection" = rvalues$selectSubpopulation <- rouletteSelection
      ) 
      
      switch (input$selectCross,
              "elitistSelection" = rvalues$selectCross <- elitistSelection,
              "tournamentSelection" = rvalues$selectCross <- tournamentSelection, 
              "rouletteSelection" = rvalues$selectCross <- rouletteSelection
      )
      
      switch (input$cross,
              "orderCrossover" = rvalues$cross <- orderCrossover,
              "kPointCrossover" = rvalues$cross <- kPointCrossover
              
      )
      
      switch (input$mutate,
              "swapMutation" = rvalues$mutate <- swapMutation,
              "factorMutation" = rvalues$mutate <- factorMutation
      )
      showNotification("Ongi bete duzu, zoaz exekuzioa leihatilara!")
    }
  }
  )
  
  observeEvent(input$run,{
    if(!is.null(rvalues$problema)){
    args<-list()
    args$resources <- cResource(time = input$denbora)
    args$evaluate         <- rvalues$problema$evaluate
    if(input$Algoritmoa == "Bilaketa lokala"){
      if(!is.null(rvalues$initial.solution)&&!is.null(rvalues$ingurunea)){
      
      args$initial.solution <- rvalues$initial.solution
      args$neighborhood     <- rvalues$ingurunea
      if(input$selector =="greedy"){
        args$selector         <- greedySelector
      }else{
        args$selector         <- firstImprovementSelector
      }
      rvalues$emaitza <- do.call(basicLocalSearch, args)
      
      print("amaituta")
      }
    }else{
      if(!is.null(rvalues$initial.population)&&!is.null(rvalues$selectSubpopulation)&&
         !is.null(input$selection.ratio)&&!is.null(rvalues$selectCross)&&!is.null(rvalues$mutate)&&
         !is.null(input$ratio)&&!is.null(input$mutation.rate)&&!is.null(rvalues$cross)){
      args$initial.population <- rvalues$initial.population
      args$selectSubpopulation <- rvalues$selectSubpopulation
      if(!is.null(input$use.ranking)){
        if(input$use.ranking=="Bai"){
          args$use.rankings<- TRUE
        }else{
          args$use.rankings<- FALSE
        }
      }
      
      args$selection.ratio<-input$selection.ratio
      args$selectCross <- rvalues$selectCross
      args$mutate <- rvalues$mutate
      args$ratio       <- input$ratio
      args$mutation.rate        <- input$mutation.rate
      args$cross                <- rvalues$cross
      if(input$Problema!="Travelling salesman problem"){
        is(!is.null(input$cross.k))
        args$k<-input$cross.k
      }
        rvalues$emaitza<-do.call(basicGeneticAlgorithm,args)
        print("amaituta")
        
      
    }
    }
    #}
    # else{
    #   #print(rvalues$problema)
    #   #print(rvalues$initial.solution)
    #   #print(rvalues$ingurunea)
    #   # 
    # }
    
    print(getSolution(rvalues$emaitza))
    print(rvalues$emaitza@resources)
  }})
  
  
  
  
  
  
  
  output$oni<-renderText({
    gettext(
      #ingurunea()
    )
    
  })
  
  
  output$problemaEzaugarriak <- renderText({
    if (is.null(input$Problema))
      return()

    switch (
      input$Problema,
      "Travelling salesman problem" = paste(
        "This function generates an evaluation function associated with a TSP problem"
      ),
      "Closest String Problem"      = paste(
        "Hainbat sekuentzia emanik, gertueneko beste sekuentzia bat itzultzen du."
      ),
      "Farthest String Problem"     = paste(
        "Hainbat sekuentzia emanik, hurruneneko beste sekuentzia bat itzultzen du."
      )
    )
  })
  
  
  ###Problema kodea#

  output$problemaKodea <- renderText({
    if (input$Problema == "Travelling salesman problem") {
      cmatrix <- matrix(runif(100), ncol = 10)
      tsp <- tspProblem(cmatrix)
      paste(tsp[1])
    }

    else if (input$Problema == "Closest String Problem") {
      cmatrix <-
        matrix(
          data = c('a', 'a', 'a', 'a', 'a', 'a'),
          ncol = 2,
          byrow = TRUE
        )
      csp <- closestStringProblem(cmatrix, c('a'))
      paste(csp[1])
    }

    else if (input$Problema == "Farthest String Problem") {
      cmatrix <-
        matrix(
          data = c('a', 'a', 'a', 'a', 'a', 'a'),
          ncol = 2,
          byrow = TRUE
        )
      fsp <- farthestStringProblem(cmatrix, c('a'))
      paste(fsp[1])
    }

  })
  
  
  ###Problema Instantziak###
  
  output$uiInst <- renderUI({
    if (is.null(input$Problema))
      return()
    
    switch (
      input$Problema,
      "Travelling salesman problem" = tags$div(
        numericInput(
          inputId = "tam.mat",
          label = "n matrize zabalera sartuz sortu ausazko matrizea",
          value = 4, 
          min = 4
        ),
        actionButton(
          inputId = "sor.inst",
          label = "Baieztatu"
        )
        
      ),
      "Closest String Problem" = tags$div(
        textInput(
          inputId = "alfabeto",
          label = "Sartu alfabetoa",
          placeholder = "a b c ..."
        ),
        fileInput(
          inputId = "ireki.fitx",
          label = "Ireki zure fitxategia"
        ),
        actionButton(
          inputId = "sor.inst",
          label = "Baieztatu"
        )
      ),
      "Farthest String Problem" = tags$div(
        textInput(
          inputId = "alfabeto",
          label = "Sartu alfabetoa",
          placeholder = "a b c ..."
        ),
        fileInput(
          inputId = "ireki.fitx",
          label = "Ireki zure fitxategia"
        ),
        actionButton(
          inputId = "sor.inst",
          label = "Baieztatu"
        )
      )
    )
    
  })
  
  output$matrize <- renderTable({
    if (input$Problema == "Travelling salesman problem") {
      rvalues$matrix
    }
  })
  
  
  
  
  
  
  
  
  
  
  
  ###Algoritmo ezaugarriak###
  
  output$algoritmoEzaugarriak <- renderText({
    if (input$Algoritmoa == "Bilaketa lokala") {
      paste(
        "Bilaketa lokalean soluzio bakar bat mantentzen dugu, eta soluzio horretatik abiatuta 
        beste batera mugitzen saiatuko gara; mugimenduak nola egiten diren desberdintzen du algoritmoak.
        Metodo honek arazo larri bat du: optimo lokaletantrabaturik gelditzen da."
      )
    }
    else if (input$Algoritmoa == "Algoritmo genetikoa") {
      paste(
"Algoritmo hauetan,soluzio bakar bat izan beharrean soluzio-<<populazio>> bat multzo bat,alegia
mantentzen dugu; iterazioz iterazio populazioari aldaketak egingo
zaizkio, eta hala horren <<eboluzioa>> ahalbidetzen, eta geroz eta soluzio
hobeagoak lortzen da. Atal honetan dauden algoritmo askok naturan bilatzen
dute inspirazioa"
              )
    }

  })

  
  
  
  output$uiAlg <- renderUI({
    if (is.null(input$Algoritmoa))
      return()
    
    switch (input$Algoritmoa,
            "Bilaketa lokala" = {
              if (is.null(input$Algoritmoa))
                return()
              
              switch (input$Problema,
                      "Travelling salesman problem" = tags$div(
                        selectInput(
                          inputId = "ingurunea",
                          label = "Hautatu ingurunea: ",
                          choices = c("swapNeighborhood","exchangeNeighborhood", "insertNeighborhood")
                        ),
                        selectInput(
                          inputId = "selector",
                          label = "Hautatu selector: ",
                          choices = c("greedy","first improvement")
                        ),
                        actionButton(
                          inputId = "sor.alg",
                          label = "Baieztatu"
                        )
                      ),
                      "Closest String Problem" = tags$div(
                        selectInput(
                          inputId = "ingurunea",
                          label = "Hautatu ingurunea: ",
                          choices = c("hammingNeighborhood")
                        ),
                        selectInput(
                          inputId = "selector",
                          label = "Hautatu selector: ",
                          choices = c("greedy","first improvement")
                        ),
                        actionButton(
                          inputId = "sor.alg",
                          label = "Baieztatu"
                        )
                        
                      ),
                      
                      "Farthest String Problem" = tags$div( 
                        selectInput(
                          inputId = "ingurunea",
                          label = "Hautatu ingurunea: ",
                          choices = c("hammingNeighborhood")
                        ),
                        selectInput(
                          inputId = "selector",
                          label = "Hautatu selector: ",
                          choices = c("greedy","first improvement")
                        ),
                        actionButton(
                          inputId = "sor.alg",
                          label = "Baieztatu"
                        )
                      )
                      
              )
            },
            "Algoritmo genetikoa" = {
              if (is.null(input$Algoritmoa))
                return()
              
              if (input$Problema == "Travelling salesman problem"){ tags$div(
                numericInput(
                  inputId = "populazio.tamaina",
                  label = "Hasierako populazioa hausaz sortzen den arren, populazioaren tamaina sartu:",
                  value = 100
                ),
                selectInput(
                  inputId = "selectSubpopulation",
                  label = "Hautatu subpopulazioaren hautaketa: ",
                  choices = c("elitistSelection", "tournamentSelection", "rouletteSelection")
                ),
                sliderInput(
                  inputId = "selection.ratio",
                  label = "Selection ratio",
                  min = 0,
                  max = 1,
                  value = 0.5
                ),
                selectInput(
                  inputId = "selectCross",
                  label = "Hautatu gurutzaketaren hautaketa: ",
                  choices = c("elitistSelection", "tournamentSelection", "rouletteSelection")
                ),
                selectInput(
                  inputId = "cross",
                  label = "Hautatu gurutzaketa: ",
                  choices = c("orderCrossover")
                ),
                selectInput(
                  inputId = "mutate",
                  label = "Mutazioa",
                  choices = c("swapMutation")
                ),
                sliderInput(
                  inputId = "ratio",
                  label = "Ratio",
                  min = 0,
                  max = 1,
                  value = 0.5
                ),
                sliderInput(
                  inputId = "mutation.rate",
                  label = "Mutation Rate",
                  min = 0,
                  max = 1,
                  value = 0.5
                ),
                actionButton(
                  inputId = "sor.alg",
                  label = "Baieztatu"
                )
              )}else{tags$div(
                numericInput(
                  inputId = "populazio.tamaina",
                  label = "Hasierako populazioa hausaz sortzen den arren, populazioaren tamaina sartu:",
                  value = 100
                ),
                selectInput(
                  inputId = "selectSubpopulation",
                  label = "Hautatu subpopulazioaren hautaketa: ",
                  choices = c("elitistSelection", "tournamentSelection", "rouletteSelection")
                ),
                sliderInput(
                  inputId = "selection.ratio",
                  label = "Selection ratio",
                  min = 0,
                  max = 1,
                  value = 0.5
                ),
                selectInput(
                  inputId = "selectCross",
                  label = "Hautatu gurutzaketaren hautaketa: ",
                  choices = c("elitistSelection", "tournamentSelection", "rouletteSelection")
                ),
                selectInput(
                  inputId = "cross",
                  label = "Hautatu gurutzaketa: ",
                  choices = c("kPointCrossover")
                ),
                numericInput(
                  inputId = "cross.k",
                  label = "K:",
                  value = 2,
                  min = 1
                ),
                selectInput(
                  inputId = "mutate",
                  label = "Mutazioa",
                  choices = c("factorMutation")
                ),
                sliderInput(
                  inputId = "ratio",
                  label = "Ratio",
                  min = 0,
                  max = 1,
                  value = 0.5
                ),
                sliderInput(
                  inputId = "mutation.rate",
                  label = "Mutation Rate",
                  min = 0,
                  max = 1,
                  value = 0.5
                ),
                actionButton(
                  inputId = "sor.alg",
                  label = "Baieztatu"
                )
              )
              }
              
            }
    )
    
  })
  
  output$ui.ranking<-renderUI({
    if (is.null(input$Algoritmoa))
      return()
    
    if (is.null(input$selectSubpopulation))
      return()
    
    if(input$Algoritmoa=="Algoritmo genetikoa" && input$selectSubpopulation=="rouletteSelection"){
      selectInput(
        inputId = "use.ranking",
        label = "Rangkinak erabili nahi dituzu?",
        choices = c("Bai","Ez")
      )
    }
    
  })
  
  # output$algoritmoKodea <- renderText({
  #   if (input$Problema == "Grafoa koloreztatzearen problema") {
  #     library("igraph")
  #     library("metaheuR")
  #     n <- 10
  #     rnd.graph <- random.graph.game(n, p.or.m = 0.5)
  #     gcol.problem <- graphColoringProblem(rnd.graph)
  #     paste(gcol.problem[1])
  #   }
  #   else if (input$Problema == "Motxilaren problema") {
  #     n <- 100
  #     w <- runif(n)
  #     v <- runif(n)
  #     l <- sum(w[runif(n) > 0.5])
  #     knp <- knapsackProblem(w, v, l)
  #     paste(knp[1])
  #   }
  #   
  #   else if (input$Problema == "Garraio problema") {
  #     cmatrix <- matrix(runif(100), ncol = 10)
  #     tsp <- tspProblem(cmatrix)
  #     paste(tsp[1])
  #   }
  # })
  
  output$plotProgresioa <- renderPlot({
    if(!is.null(rvalues$emaitza))
      plotProgress(rvalues$emaitza, size=1.1) + labs(y="Evaluation")
    
  })
  # })
})