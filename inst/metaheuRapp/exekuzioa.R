Exekuzioa <- tabPanel("Exekuzioa",
                      column(8, sidebarLayout(
                        fluidRow(sidebarPanel(
                          tags$h3("Gelditzeko irizpideak: "),
                          fluidRow(column(width = 4,
                            denbora
                          ),
                          column(width = 4, 
                            it.kopurua
                          ),
                          column(width = 4, 
                            eb.kopurua
                          )),
                          fluidRow(column(width = 4,offset = 4,
                            run
                          ))
                        )),
                        fluidRow(plotOutput("plotProgresioa"))
                      )),
                      column(4, mainPanel(
                        tags$h3("Code: "), verbatimTextOutput("algoritmoKodea"), textOutput("oni")
                      )))