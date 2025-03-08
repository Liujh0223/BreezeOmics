wgcna <- tabPanel(title = list(icon("network-wired"), "WGCNA"), value = "wgcna",
                        if(Sys.info()["sysname"] == "Linux"){
                          fluidPage(style = "height:700px;",
                              br(), br(), br(), br(),br(), br(),
                              column(width = 12, align="center", 
                                          HTML("<p style='font-size:30px;color:black' class='title'>Note: <br/> This feature is only available in the local version. <br/>"),
                                          HTML("<p style='font-size:20px;' class='title'> Due to the large amount of computing resources required for WGCNA analysis to prevent excessive load on the server.<br/> 
                                          Thank you for your encouragement and support!!</p>")),
                              br(), br(), br(), br(),br(), br(),
                              column(width = 12, tags$h4(p(em("Weighted correlation network analysis(WGCNA) is base on the WGCNA R package"),style="color:black;text-align:center;font-size:20px;")))
                          )
                        }else {
                          fluidPage(
                              column(width = 12, uiOutput("ui_wgcna")),
                              br(),
                              HTML('<hr style="width: 100%;border-top: 1px solid #ddd;margin-top: 30px;" />'),
                              column(width = 12, tags$h4(p(em("Weighted Gene Co-expression Network Analysis (WGCNA) is base on the WGCNA R package"),style="color:black;text-align:center;font-size:20px;margin-top: -10px;"))),
                          )
                        }

                      )