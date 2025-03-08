timeseriesanalysis <- tabPanel(title = list(icon("chart-line"),"Time Series Analysis"), value = "timeseriesanalysis",
                        fluidPage(
                            column(width = 12, uiOutput("ui_timeseriesanalysis")),
                            column(width = 12, tags$h4(p(em("Time serise analysis is base on the Mfuzz R package"),style="color:black;text-align:center;font-size:20px;")))
                        )
                      )