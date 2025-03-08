coanalysis <- navbarMenu(list(icon("bars"), "Co-analysis"),
                                tabPanel(title = list(icon("screwdriver-wrench"), "Guide of co-analysis"), value = "coanalysis0",
                                        br(), br(),style = "font-size:20px;",
                                        uiOutput("ui_coanalysis0")
                                ),
                                tabPanel(title = list(icon("screwdriver-wrench"), "Peaks annotation(ChIPseeker)"), value = "coanalysis1",
                                        br(), br(),style = "font-size:20px;",
                                        uiOutput("ui_coanalysis1")
                                ),
                                tabPanel(title = list(icon("screwdriver-wrench"), "Candidate explore"), value = "coanalysis3",
                                        br(), br(),
                                        uiOutput("ui_coanalysis3")
                                )
                        )

