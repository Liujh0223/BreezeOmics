library(shiny)

# sidebar panels
output$col_stat_option <- renderUI({
    fluidPage(
        column(width = 12,
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
            column(width = 12, fileInput("col_stat_upload", "Upload file", accept = list(".csv", ".txt", ".xlsx")), style = "font-size:12px"),
            column(width = 12, selectInput("col_stat_column", "Column selection", choices = NULL), style = "font-size:12px"),
        )
    )
})

# main panels
output$col_stat_main_panel <- renderUI({
           fluidPage(
                column(width = 12, align = "left", style="background-color: #0000001f;height: 50px;font-weight: 900;", 
                                HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right"),ifelse(!is.null(input$col_stat_upload), input$col_stat_upload, "Please upload your file in the sidebar panel"), "</p>"))),
                column(width = 12, DT::dataTableOutput(outputId = "col_stat_file", width = "100%"), style="background-color: #AEEEEE;margin-bottom: 20px;"),

                column(width = 10, align = "left", style="background-color: #0000001f;height: 50px;font-weight: 900;", 
                                HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right"),ifelse(!is.null(input$col_stat_upload), paste0("Frequency of ", input$col_stat_column), "Please upload your file in the sidebar panel"), "</p>"))),
                column(width = 2, align = "right", downloadButton("download_col_stat_result", label = "Download", style = "text-align: center;margin-top: 6px;font-size:13px"),style="background-color: #0000001f;height: 50px;"),
                column(width = 12, DT::dataTableOutput(outputId = "col_stat_result", width = "100%"), style="background-color: #AEEEEE;margin-bottom: 20px;")
                
           )
})

col_stat_file <- reactive({
    req(input$col_stat_upload)
    # 读取选择的文件
    fileformat <- strsplit(input$col_stat_upload$datapath, "\\.")[[1]][-1]
    # print(fileformat)
    if(fileformat == "csv"){
        data <- read.csv(input$col_stat_upload$datapath, sep = ",")
    }else if(fileformat == "txt"){
        data <- read.csv(input$col_stat_upload$datapath, sep = "\t")
    }else if(fileformat == "xlsx"){
        data <- openxlsx::read.xlsx(input$col_stat_upload$datapath, colNames = TRUE, rowNames = F)
    }else {
        shinyjs::runjs('alert("Currently, only txt, csv, and xlsx files are supported")')
        return(NULL)
    }
})

observe({
        req(col_stat_file())
        # 读取选择的文件
        data <- col_stat_file()
        # 填充选择框选项
        updateSelectInput(session, "col_stat_column", choices = unique(colnames(data)))
  })
#
output$col_stat_file <- DT::renderDataTable(col_stat_file(), rownames = FALSE, options = list(dom = 'fltipr', pageLength = 5, scrollX=TRUE, orderClasses = T,
                                                                        columnDefs = list(
                                                                                        list(className = 'dt-center', 
                                                                                                targets = '_all')
                                                                                        )
                                                                        ), selection = "none")
#
col_stat_result <- reactive({
    req(col_stat_file(), input$col_stat_column, input$col_stat_upload)
    file <- col_stat_file()
    frequency_table <- data.frame(table(file[, input$col_stat_column]))
    colnames(frequency_table) <- c(input$col_stat_column, "Frequency")
    return(frequency_table)
})
output$col_stat_result <- DT::renderDataTable(col_stat_result(), rownames = FALSE, options = list(dom = 'fltipr', pageLength = 5, scrollX=TRUE, orderClasses = T,
                                                                        columnDefs = list(
                                                                                        list(className = 'dt-center', 
                                                                                                targets = '_all')
                                                                                        )
                                                                        ), selection = "none")
#
output$download_col_stat_result <- downloadHandler(
    filename = function() {paste0("Frequency of ", input$col_stat_column, ".txt")},
    content = function(file) {
      write.table(col_stat_result(), file, row.names = FALSE, sep="\t", quote = F)
    }
  )