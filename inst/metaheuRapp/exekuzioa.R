Exekuzioa <- tabPanel("Exekuzioa",
                      column(8, sidebarLayout(
                        fluidRow(sidebarPanel(
                          tags$h3("Gelditzeko irizpideak: "),
                          fluidRow(column(width = 4,
                                          numericInput(
                                            "denbora",
                                            label = "Denbora: ",
                                            value = 0
                                          )
                                          
                          ),
                          column(width = 4, 
                                 numericInput("it.kopurua",
                                              label = "Iterazioak: ",
                                              value = 0)
                                 
                          ),
                          column(width = 4, 
                                 numericInput("eb.kopurua",
                                              label = "Ebaluazioak: ",
                                              value = 0)
                          )),
                          fluidRow(column(width = 4,offset = 4,
                                          actionButton("run",
                                                       label = "Run")
                                          
                          ))
                        )),
                        fluidRow(plotOutput("plotProgresioa"))
                      )),
                      column(4, mainPanel(
                        tags$h3("Code: "), verbatimTextOutput("algoritmoKodea"), textOutput("oni"),textOutput("onin")
                      )))