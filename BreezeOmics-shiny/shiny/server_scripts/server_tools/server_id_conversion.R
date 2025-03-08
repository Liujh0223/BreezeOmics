library(shiny)

# sidebar panels
output$locus_conv_option <- renderUI({
    fluidPage(
        column(width = 12,
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
            column(width = 12, style = "font-size:12px",selectInput("id_pairs", "ID pairs selection", choices = unlist(list(list.files("id_conversion"))))),
        )
    )
})

output$locus_conv_main_panel <- renderUI({
           fluidPage(
                column(width = 10, align = "left", style="background-color: #0000001f;height: 50px;font-weight: 900;", 
                        HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right")," ID pairs: ", input$id_pairs, "</p>"))),
                column(width = 2, align = "right", downloadButton("download_id_pairs", label = "Download", style = "text-align: center;margin-top: 6px;font-size:13px"),style="background-color: #0000001f;height: 50px;"),
                column(width = 12, DT::dataTableOutput(outputId = "id_pairs_table", width = "100%"), style="background-color: #AEEEEE;margin-bottom: 20px;")
           )
})

#
filtered_data_id_pairs <- reactive({
    req(input$id_pairs)
    # 读取选择的文件
    data <- read.csv(paste0("id_conversion/", input$id_pairs), sep = "\t")
})
output$id_pairs_table <- DT::renderDataTable(filtered_data_id_pairs(), rownames = FALSE, options = list(dom = 'fltipr', pageLength = 10, scrollX=TRUE, orderClasses = T,
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
output$download_id_pairs <- downloadHandler(
    filename = function() {paste0(input$id_pairs,".txt")},
    content = function(file) {
      write.table(filtered_data_id_pairs(), file, row.names = FALSE, sep="\t", quote = F)
    }
  )
