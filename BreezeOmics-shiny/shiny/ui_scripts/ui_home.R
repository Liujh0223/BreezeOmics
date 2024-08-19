 library(shiny)
 home <- tabPanel(title = HTML('<i class="fas fa-house"></i>',"HOME"), 
                 value = "home",
                 fluidRow(style = "width:initial",
                  #  column(width = 12, align="center", 
                  #           HTML("<p class='title'>温馨提示: 由于该网页目前处于开发测试阶段，仅在华农校内局域网开放仍未正式上线互联网，故只能使用华农校园网访问供大家学习使用。<br/> 
                  #           如使用过程中出现任何问题（BUG，疑问，建议，甚至希望开发其他功能等），可以直接联系开发者或邮件咨询364832885@qq.com。<br/> 
                  #           感谢您的鼓励与支持！！！</p>")),
                   
                  #  br(), br(), br(), br(),br(), br(),
                  #  HTML("<h1><center>WELCOME TO <b>RNA-seq Analysis Visualization Platform</b></center></h1>"),
                  #  br(), br(), br(), br(),
                   fluidRow(style = "width:80%;margin-left:10%;margin-top: 2%;",
                     column(width = 12, align = "center",
                            myactionButton(id = "ui_guide", width = "94%", bcol = "#3fa9b7", hbcol = "#ffffff",bordercol = "#000000",borderradius = "10px",
                                          label = HTML(paste0("<div style='color:white;display:inline-block;font-size: 30px;font-weight: 700;'> First time using &nbsp</div>",
                                                        "<div style='color:#1c00a2;display:inline-block;font-size: 30px;font-weight: 700;'> BreezeOmics-Vis &nbsp</div>",
                                                        "<div style='color:white;display:inline-block;font-size: 30px;font-weight: 700;'> and don't know &nbsp</div>",
                                                        "<div style='color:#1c00a2;display:inline-block;font-size: 30px;font-weight: 700;'> how to use it ? &nbsp</div>",
                                                        "<div style='color:white;display:inline-block;font-size: 30px;font-weight: 700;'>  click to know everything ! </div>"
                                                        )))
                     )
                   ),
                   br(), br(),
                   fluidRow(
                     column(width = 4, style = "left: 180px;", 
                      column(width = 12, align = "center", style = "margin-bottom: 8px;",
                              tab_voronoys(texto = "Quality control", 
                                            texto2 = "Including Correlation analysis and PCA.",
                                            texto3 = "Based on Pearson correlation and ANOVA.",
                                            cor = "#b1d4fe", icon = icon("right-to-bracket"), id = "ui_datacheck")
                      ),
                      column(width = 12, align = "center", style = "margin-bottom: 8px;",
                              tab_voronoys(texto = "DEGs", 
                                            texto2 = "Differentiallly expressed genes analysis.",
                                            texto3 = "Based on R packages of DEseq2, edgeR.",
                                            cor = "#b1d4fe", icon = icon("right-to-bracket"), id = "ui_differenceanalysis")
                      ),
                      column(width = 12, align = "center", style = "margin-bottom: 8px;",
                              tab_voronoys(texto = "Time series analysis", 
                                            texto2 = "Based on Mfuzz R packages. ",
                                            texto3 = "The principle is based on the fuzzy C-means (FCM) clustering algorithm.",
                                            cor = "#b1d4fe", icon = icon("right-to-bracket"), id = "ui_timeseriesanalysis")
                      ),
                      column(width = 12, align = "center", style = "margin-bottom: 8px;",
                              tab_voronoys(texto = "Enrichment analysis", 
                                            texto2 = "Funtion and pathway enrichment analysis.",
                                            texto3 = "Based on KEGG and GO databases, Hypergeometric Distribution menthod.",
                                            cor = "#b1d4fe", icon = icon("right-to-bracket"), id = "ui_enrichmentanallysis")
                      ),
                      column(width = 12, align = "center", style = "margin-bottom: 8px;",
                              tab_voronoys(texto = "WGCNA", 
                                            texto2 = "Weighted correlation network analysis.",
                                            texto3 = "Based on R packages of WGCNA.",
                                            cor = "#b1d4fe", icon = icon("right-to-bracket"), id = "ui_wgcna")
                      ),
                      column(width = 12, align = "center", style = "margin-bottom: 8px;",
                              tab_voronoys(texto = "Co-analysis", 
                                            texto2 = "Peaks annotation & Interacts with RNA-seq",
                                            texto3 = "Based on ChIPseeker as well as other visualization R packages.",
                                            cor = "#b1d4fe", icon = icon("right-to-bracket"), id = "ui_co_analysis")
                      ),
                      column(width = 12, align = "center", style = "margin-bottom: 8px;",
                              tab_voronoys(texto = "TOOLS", 
                                            texto2 = "A collection of utilities.",
                                            texto3 = "",
                                            cor = "#b1d4fe", icon = icon("right-to-bracket"), id = "ui_tools")
                      )
                     ),
                     column(width = 8, style = "right: 80px;", align = "center", tags$img(src = "shinyrnaseq_tree.jpg", width = "900px", height = "1020px")),
                   ),

                   fluidRow(style = "width:80%;margin-left:10%",
                   ),
                   fluidRow(style = "width:80%;margin-left:10%;margin-top: 2%;",
                   )
                 )
        )