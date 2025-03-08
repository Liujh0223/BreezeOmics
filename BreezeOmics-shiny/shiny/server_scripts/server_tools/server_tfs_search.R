library(shiny)
# sidebar panels
output$tfs_search_option <- renderUI({
    fluidPage(
        column(width = 12,
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
            column(width = 12, style = "font-size:12px",selectInput("tfs_search_species", "Genomic selection", choices = unlist(list(list.files("planttfdb"))))),
            column(width = 12, selectInput("tf_selection", "TF family selection", choices = NULL), style = "font-size:12px")
        )
    )
})

# main panels
output$tfs_search_main_panel <- renderUI({
           fluidPage(
                column(width = 10, align = "left", style="background-color: #0000001f;height: 50px;font-weight: 900;", 
                        HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right")," TF family: ", input$tf_selection, "</p>"))),
                column(width = 2, align = "right", downloadButton("download_tfs_search", label = "Download", style = "text-align: center;margin-top: 6px;font-size:13px"),style="background-color: #0000001f;height: 50px;"),
                column(width = 12, DT::dataTableOutput(outputId = "tfs_search", width = "100%"), style="background-color: #AEEEEE;margin-bottom: 20px;"),

                column(width = 12, style = "margin-bottom: 0px;",
                        tags$h4(p(em("Data source: PlantTFDB"),style="color:black;text-align:center;font-size:20px;"))),
                column(width = 12, align = "center", HTML(paste0("<a style='color: black;font-size: 15px;' href='https://planttfdb.gao-lab.org/index.php' target='_blank'>", "Plant Transcription Factor Database
 - https://planttfdb.gao-lab.org/index.php", "</a>")))
           )
})

# selection
observe({
        req(input$tfs_search_species)
        # 读取选择的文件
        data <- read.csv(paste0("planttfdb/", input$tfs_search_species), sep = "\t")
        # 填充选择框选项
        updateSelectInput(session, "tf_selection", choices = unique(data$Family))
  })
filtered_data_tf <- reactive({
    req(input$tfs_search_species, input$tf_selection)

    # 读取选择的文件
    data <- read.csv(paste0("planttfdb/", input$tfs_search_species), sep = "\t")
    subset(data, 
            Family == input$tf_selection)
})
output$tfs_search <- DT::renderDataTable(filtered_data_tf(), rownames = FALSE, options = list(dom = 'fltipr', pageLength = 10, scrollX=TRUE, orderClasses = T,
                                                                        rowCallback = DT::JS(
                                                                                                "function(row, data) {",
                                                                                                "for (i = 1; i < data.length; i++) {",
                                                                                                "if (data[i] != null){",
                                                                                                "if (data[i]>1 | data[i]<1){",
                                                                                                        "$('td:eq('+i+')', row).html(data[i].toFixed(5));", # vlaue.toExponential(3)科学计数法
                                                                                                "}",
                                                                                                "}else{$('td:eq('+i+')', row).html('');}",
                                                                                                "}",
                                                                                                "}"),
                                                                        columnDefs = list(
                                                                                        list(className = 'dt-center', 
                                                                                                targets = '_all')
                                                                                        )
                                                                        ), selection = "none")
output$download_tfs_search <- downloadHandler(
    filename = function() {paste0(input$tfs_search_species, "_", input$tf_selection,".txt")},
    content = function(file) {
      write.table(filtered_data_tf(), file, row.names = FALSE, sep="\t", quote = F)
    }
  )
