Exekuzioa<-    tabPanel("Exekuzioa",
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