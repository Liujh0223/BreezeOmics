output$ui_coanalysis0 <- renderUI({
    fluidRow(
        br(),br(),br(),br(),br(),
        column(width = 12, align = "center", style = "margin-bottom: 100px;",
            column(width = 4, align = "center", offset = 4, style = "margin-bottom: 20px;",
                    tab_voronoys(texto = "Peaks annotation",
                                texto2 = "Based on R packages of ChIPseeker.",
                                texto3 = "Upload BED6 or BED6+4 file by ChIP-seq.",
                                cor = "#b1d4fe", icon = icon("right-to-bracket"), id = "ui_coanalysis1")
            ),
            column(width = 4, align = "center", offset = 4, style = "margin-bottom: 80px",
                    tab_voronoys(texto = "Candidate explore", 
                                texto2 = ". ",
                                texto3 = "Upload expression matrix and peak annotation file.",
                                cor = "#b1d4fe", icon = icon("right-to-bracket"), id = "ui_coanalysis3")
            )
        ),
        br(),br(),br(),br()
    )
})

observeEvent(input$ui_coanalysis1,{
  updateTabsetPanel(session = session, inputId = "navbar", selected = "coanalysis1")
})
observeEvent(input$ui_coanalysis2,{
  updateTabsetPanel(session = session, inputId = "navbar", selected = "coanalysis2")
})
observeEvent(input$ui_coanalysis3,{
  updateTabsetPanel(session = session, inputId = "navbar", selected = "coanalysis3")
})