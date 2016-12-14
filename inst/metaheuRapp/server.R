
library(shiny)

shinyServer(function(input, output) {
   
  output$problemaEzaugarriak <- renderText({
    
    if (input$Problema =="Grafoa koloreztatzearen problema") {
      paste("This function generates an evaluation, validity and correction functions associated with a classical graph coloring problem")
    }
    else if (input$Problema =="Motxilaren problema") {
      paste("This function generates an evaluation, validity and correction functions associated with a classical knapsack problem")
    }
    
    else if (input$Problema =="Garraio problema") {
      paste("This function generates an evaluation function associated with a TSP problem")
    }
    
  })
  
  output$problemaKodea <- renderText({
    
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
  
  output$algoritmoEzaugarriak <- renderText({
    
    if (input$Algoritmoa =="Grafoa koloreztatzearen problema") {
      paste("This function generates an evaluation, validity and correction functions associated with a classical graph coloring problem")
    }
    else if (input$Algoritmoa =="Motxilaren problema") {
      paste("This function generates an evaluation, validity and correction functions associated with a classical knapsack problem")
    }
    
    else if (input$Algoritmoa =="Garraio problema") {
      paste("This function generates an evaluation function associated with a TSP problem")
    }
    
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
