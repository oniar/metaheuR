
library(shiny)
library(metaheuR)


shinyServer(function(input, output) {
  
  

  
  output$problemaEzaugarriak <- renderText({
    
    if (input$Problema =="Travelling salesman problem") {
      paste("This function generates an evaluation function associated with a TSP problem")
    }
    
    else if (input$Problema =="Closest String Problem") {
      paste("Hainbat sekuentzia emanik, gertueneko beste sekuentzia bat itzultzen du.")
    }
    
    else if (input$Problema =="Farthest String Problem") {
      paste("Hainbat sekuentzia emanik, hurruneneko beste sekuentzia bat itzultzen du.")
    }
    
  })
  
  output$problemaKodea <- renderText({
    
    if (input$Problema =="Travelling salesman problem") {
      cmatrix <- matrix(runif(100), ncol=10)
      tsp <- tspProblem(cmatrix)
      paste(tsp[1])
    }
    
    else if (input$Problema =="Closest String Problem") {
      cmatrix <- matrix(data=c('a','a','a','a','a','a'),ncol = 2, byrow = TRUE)
      csp <- closestStringProblem(cmatrix,c('a'))
      paste(csp[1])
    }
    
    else if (input$Problema =="Farthest String Problem") {
      cmatrix <- matrix(data=c('a','a','a','a','a','a'),ncol = 2, byrow = TRUE)
      fsp <- farthestStringProblem(cmatrix,c('a'))
      paste(fsp[1])
    }
    
  })
  
  output$algoritmoEzaugarriak <- renderText({
    
    if (input$Algoritmoa =="Bilaketa lokala") {
      paste("This function generates an evaluation, validity and correction functions associated with a classical graph coloring problem")
    }
    else if (input$Algoritmoa =="Algoritmo genetikoa") {
      paste("This function generates an evaluation, validity and correction functions associated with a classical knapsack problem")
    }
    
  })
  
  
  output$uiAlg <- renderUI({
    
    if (is.null(input$Algoritmoa))
      return()
    
    switch (input$Algoritmoa,
            "Bilaketa lokala" = tags$div(
              hasiera.soluzioa,
              ingurunea,
              selector,
              restart.estrategia
            ),
            "Algoritmo genetikoa" = ""
    )
    
  })
  
 
  
  output$uiInst <- renderUI({

    if (is.null(input$Problema))
       return()
     
     switch (input$Problema,
             "Travelling salesman problem" = tags$div(
               eskuz.matrizea.tsp,
               sortu.matrizea,
               tags$h3("edo"),
               tamaina.matrizea,
               ausazko.matrizea,
               div(
                   
               )
             ),
             "Closest String Problem" = tags$div(
               eskuz.matrizea.str,
               alfabetoa,
               sortu.matrizea,
               tags$h3("edo"),
               tamaina.matrizea,
               ausazko.matrizea,
               div(
                 
               )
               
             ),
             "Farthest String Problem" = tags$div(
               eskuz.matrizea.str,
               alfabetoa,
               sortu.matrizea,
               tags$h3("edo"),
               tamaina.matrizea,
               ausazko.matrizea,
               div(
                 
               )
             )
     )
    
  })
  

  
  observeEvent(input$aus.mat, {
    output$matrize<-renderTable({
      n<-input$tam.mat
      cmatrix<-matrix(runif(n^2), ncol = n)
      cmatrix
    })
  })
  
  observeEvent(input$sor.mat, {
    output$matrize<-renderTable({
      data<-input$mat.tsp
      data<-strsplit(data,split = " ")
      data<-as.integer(unlist(data))
      cmatrix<-matrix(data=data,nrow = sqrt(length(data)), byrow = TRUE)
      cmatrix
    })
  })
  

  output$algoritmoKodea <- renderText({
    
    if (input$Problema =="Grafoa koloreztatzearen problema") {
      
      library("igraph")
      library("metaheuR")
      n <- 10
      rnd.graph <- random.graph.game(n, p.or.m=0.5)
      gcol.problem <- graphColoringProblem(rnd.graph)
      paste(gcol.problem[1])
    }
    else if (input$Problema =="Motxilaren problema") {
      n <- 100
      w <- runif(n)
      v <- runif(n)
      l <- sum(w[runif(n) > 0.5])
      knp <- knapsackProblem(w, v, l)
      paste(knp[1])
    }
    
    else if (input$Problema =="Garraio problema") {
      cmatrix <- matrix(runif(100), ncol=10)
      tsp <- tspProblem(cmatrix)
      paste(tsp[1])
    }
    
    
    
  })
  
  output$plotProgresioa <- renderPlot({
    
    plot(x = 1,y = 1)
    
  })
  
  
})
