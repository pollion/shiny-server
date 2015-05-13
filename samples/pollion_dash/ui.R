shinyUI(fluidPage(theme = "bootstrap3.css",
                  fluidRow(
                          column(12,
                                 "Heading",
                                 fluidRow(
                                         column(3, 
                                                names(data)[1]),
                                         column(3,
                                                plotOutput("single_barplot_var1"),
                                                checkboxInput("Anzahl_Beobachtungen", "Anzahl Beobachtungen", FALSE),
                                                checkboxInput("labelxvertical", "Vertikale Beschriftung", FALSE)
                                                ),
                                         column(3,
                                                htmlOutput("summary1")),
                                         column(3,
                                                "detaillierte Analysen")
                                 ),
                                 
                                 fluidRow(
                                         column(3, 
                                                names(data)[2]),
                                         column(3,
                                                plotOutput("single_hist_var2")),
                                         column(3,
                                                htmlOutput("summary2")),
                                         column(3,
                                                "detaillierte Analysen")
                                 ),
                                 fluidRow(
                                         column(3, 
                                                names(data)[3]),
                                         column(3,
                                                plotOutput("single_barplot_var3"),
                                                checkboxInput("Anzahl_Beobachtungen3", "Anzahl Beobachtungen", FALSE),
                                                checkboxInput("labelxvertical3", "Vertikale Beschriftung", FALSE)
                                                ),
                                         column(3,
                                                htmlOutput("summary3")),
                                         column(3,
                                                "detaillierte Analysen")
                                 ),
                                 
                                 fluidRow(
                                         column(3, 
                                                names(data)[4]),
                                         column(3,
                                                plotOutput("single_barplot_var4"),
                                                checkboxInput("Anzahl_Beobachtungen4", "Anzahl Beobachtungen", FALSE),
                                                checkboxInput("labelxvertical4", "Vertikale Beschriftung", FALSE)
                                                ),
                                         column(3,
                                                htmlOutput("summary4")),
                                         column(3,
                                                "detaillierte Analysen")
                                 ),
                                 
                                 fluidRow(
                                         column(3, 
                                                names(data)[5]),
                                         column(3,
                                                plotOutput("single_barplot_var5"),
                                                checkboxInput("Anzahl_Beobachtungen5", "Anzahl Beobachtungen", FALSE),
                                                checkboxInput("labelxvertical5", "Vertikale Beschriftung", FALSE)
                                                ),
                                         column(3,
                                                htmlOutput("summary5")),
                                         column(3,
                                                "detaillierte Analysen")
                                 ),
                                 
                                 fluidRow(
                                         column(3, 
                                                names(data)[6]),
                                         column(3,
                                                plotOutput("single_barplot_var6"),
                                                checkboxInput("Anzahl_Beobachtungen6", "Anzahl Beobachtungen", FALSE),
                                                checkboxInput("labelxvertical6", "Vertikale Beschriftung", FALSE)
                                                ),
                                         column(3,
                                                htmlOutput("summary6")),
                                         column(3,
                                                "detaillierte Analysen")
                                 ),
                                 
                                 fluidRow(
                                         column(3, 
                                                names(data)[7]),
                                         column(3,
                                                plotOutput("single_barplot_var7"),
                                                checkboxInput("Anzahl_Beobachtungen7", "Anzahl Beobachtungen", FALSE),
                                                checkboxInput("labelxvertical7", "Vertikale Beschriftung", FALSE)
                                                ),
                                         column(3,
                                                htmlOutput("summary7")),
                                         column(3,
                                                "detaillierte Analysen")
                                 ),
                                 
                                 fluidRow(
                                         column(3, 
                                                names(data)[8]),
                                         column(3,
                                                plotOutput("single_barplot_var8"),
                                                checkboxInput("Anzahl_Beobachtungen8", "Anzahl Beobachtungen", FALSE),
                                                checkboxInput("labelxvertical8", "Vertikale Beschriftung", FALSE)
                                                ),
                                         column(3,
                                                htmlOutput("summary8")),
                                         column(3,
                                                "detaillierte Analysen")
                                 ),
                                 
                                 fluidRow(
                                         column(3, 
                                                names(data)[9]),
                                         column(3,
                                                plotOutput("single_barplot_var9"),
                                                checkboxInput("Anzahl_Beobachtungen9", "Anzahl Beobachtungen", FALSE),
                                                checkboxInput("labelxvertical9", "Vertikale Beschriftung", FALSE)
                                                ),
                                         column(3,
                                                htmlOutput("summary9")),
                                         column(3,
                                                "detaillierte Analysen") 
                                 ),
                                 
                                 fluidRow(
                                         column(3, 
                                                names(data)[10]),
                                         column(3,
                                                plotOutput("single_barplot_var10"),
                                                checkboxInput("Anzahl_Beobachtungen10", "Anzahl Beobachtungen", FALSE),
                                                checkboxInput("labelxvertical10", "Vertikale Beschriftung", FALSE)
                                                ),
                                         column(3,
                                                htmlOutput("summary10")),
                                         column(3,
                                                "detaillierte Analysen")
                                 ),
                                
                                 fluidRow(
                                         column(3, 
                                                names(data)[11]),
                                         column(3,
                                                plotOutput("wordcloud11")
                                         ),
                                         column(3,
                                                htmlOutput("summary11")),
                                         column(3,
                                                "detaillierte Analysen")
                                 ),
                                 
                                 fluidRow(
                                         column(3, 
                                                names(data)[12]),
                                         column(3,
                                                plotOutput("wordcloud12")
                                         ),
                                         column(3,
                                                htmlOutput("summary12")),
                                         column(3,
                                                "detaillierte Analysen")
                                 )
                                 
                          )
                  )
)
)
