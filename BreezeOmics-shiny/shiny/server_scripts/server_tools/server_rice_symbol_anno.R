library(shiny)
# sidebar panels
output$symbol_anno_option <- renderUI({
    fluidPage(
        column(width = 12,
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
            column(width = 12, style = "font-size:12px",
                selectInput("symbol_anno_species", "Genomic selection", choices = unlist(list(list.files("symbol/Oryza sativa(IRGSP)"))))),
            column(width = 12, fileInput("symbol_anno_file_upload", "Upload file which you need annotation", accept = list(".csv", ".txt", ".xlsx")), style = "font-size:12px")
        )
    )
})

# main panels
output$symbol_anno_main_panel <- renderUI({
           fluidPage(
                uiOutput("ui_symbol_anno")
           )
})


# symbol_anno_process
symbol_anno_process <- reactiveValues(process="start")

# ui_symbol_anno
output$ui_symbol_anno <- renderUI({
    if(symbol_anno_process$process == "start"){
        fluidRow(
            column(width = 12, align = "left", style="background-color: #0000001f;height: 50px;font-weight: 900;", 
                            HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right"),"Symbol file ", "</p>"))),
            column(width = 12, DT::dataTableOutput(outputId = "symbol_file", width = "100%"), style="background-color: #AEEEEE;margin-bottom: 20px;"),
            column(width = 12, align = "left", style="background-color: #0000001f;height: 50px;font-weight: 900;", 
                            HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right"),ifelse(!is.null(input$symbol_anno_file_upload),"File you upload","Please upload your file in the sidebar panel"), "</p>"))),
            column(width = 12, DT::dataTableOutput(outputId = "symbol_input_file", width = "100%"), style="background-color: #AEEEEE;margin-bottom: 20px;"),
            column(width = 3, selectInput("symbol_anno_column_selection", "The column that contains the gene id", choices = NULL, width = "100%"), style="margin-bottom: 30px"),
            column(width = 3, actionButton("symbol_anno_start", "Start annotation"), style="top: 23px;")
        )
    }else {
       fluidRow(
            column(width = 10, align = "left", style="background-color: #0000001f;height: 50px;font-weight: 900;", 
                            HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right"),"The annotation is complete!", "</p>"))),
            column(width = 2, align = "right", downloadButton("download_symbol_anno_result", label = "Download", style = "text-align: center;margin-top: 6px;font-size:13px"),style="background-color: #0000001f;height: 50px;"),
            column(width = 12, DT::dataTableOutput(outputId = "result_file", width = "100%"), style="background-color: #AEEEEE;margin-bottom: 20px;"),
            column(width = 12, align = "center", actionButton("symbol_anno_back", "Back", width = "10%"))
       )
    }

})

#
observe({
        req(input$symbol_anno_file_upload)

        # 读取选择的文件
        fileformat <- strsplit(input$symbol_anno_file_upload$datapath, "\\.")[[1]][-1]
        # print(fileformat)
        if(fileformat == "csv"){
            data <- read.csv(input$symbol_anno_file_upload$datapath, sep = ",")
        }else if(fileformat == "txt"){
            data <- read.csv(input$symbol_anno_file_upload$datapath, sep = "\t")
        }else if(fileformat == "xlsx"){
            data <- openxlsx::read.xlsx(input$symbol_anno_file_upload$datapath, colNames = TRUE, rowNames = F)
        }else {
           shinyjs::runjs('alert("Currently, only txt, csv, and xlsx files are supported")')
           return(NULL)
        }
        
        # 填充选择框选项
        updateSelectInput(session, "symbol_anno_column_selection", choices = unique(colnames(data)))
  })



symbol_file <- reactive({
    req(input$symbol_anno_species)
    file <- read.csv(paste0("symbol/Oryza sativa(IRGSP)/", input$symbol_anno_species), sep = "\t")
})
output$symbol_file <- DT::renderDataTable(symbol_file(), rownames = FALSE, options = list(dom = 'fltipr', pageLength = 5, scrollX=TRUE, orderClasses = T,
                                                                        columnDefs = list(
                                                                                        list(className = 'dt-center', 
                                                                                                targets = '_all')
                                                                                        )
                                                                        ), selection = "none")

#
symbol_input_file <- reactive({
    req(input$symbol_anno_file_upload)
    # 读取选择的文件
    fileformat <- strsplit(input$symbol_anno_file_upload$datapath, "\\.")[[1]][-1]
    # print(fileformat)
    if(fileformat == "csv"){
        data <- read.csv(input$symbol_anno_file_upload$datapath, sep = ",")
    }else if(fileformat == "txt"){
        data <- read.csv(input$symbol_anno_file_upload$datapath, sep = "\t")
    }else if(fileformat == "xlsx"){
        data <- openxlsx::read.xlsx(input$symbol_anno_file_upload$datapath, colNames = TRUE, rowNames = F)
    }else {
        shinyjs::runjs('alert("Currently, only txt, csv, and xlsx files are supported")')
        return(NULL)
    }
})
output$symbol_input_file <- DT::renderDataTable(symbol_input_file(), rownames = FALSE, options = list(dom = 'fltipr', pageLength = 5, scrollX=TRUE, orderClasses = T,
                                                                        columnDefs = list(
                                                                                        list(className = 'dt-center', 
                                                                                                targets = '_all')
                                                                                        )
                                                                        ), selection = "none")

#
observeEvent(input$symbol_anno_start, {
    if(is.null(input$symbol_anno_file_upload)){
        shinyjs::runjs('alert("Please upload your file in the sidebar panel first!")')
        return(NULL)
    }else {
       symbol_file <- symbol_file()$gene_id
       symbol_input_file <- symbol_input_file()[,input$symbol_anno_column_selection]
       text <- intersect(symbol_file, symbol_input_file)
       if(length(text) < 10){
            shinyjs::runjs('alert("If the number of intersections is less than 10, check whether the ID column is correct")')
            return(NULL)
       }else {
          symbol_anno_process$process <- "running"
       }
    }
})
result_file <- reactive({
    if(symbol_anno_process$process == "running"){
        symbol_file <- symbol_file()
        names(symbol_file)[1] <- input$symbol_anno_column_selection
        symbol_input_file <- symbol_input_file()

        print(colnames(symbol_file))
        print(colnames(symbol_input_file))

        result_file <- dplyr::inner_join(symbol_file, symbol_input_file, by = input$symbol_anno_column_selection)
        return(result_file)
    }else {
       return(NULL)
    }
})
output$download_symbol_anno_result <- downloadHandler(
    filename = function() {"symbol annotation.txt"},
    content = function(file) {
      write.table(result_file(), file, row.names = FALSE, sep="\t", quote = F)
    }
  )
output$result_file <- DT::renderDataTable(result_file(), rownames = FALSE, options = list(dom = 'fltipr', pageLength = 10, scrollX=TRUE, orderClasses = T,
                                                                        rowCallback = DT::JS(
                                                                                        "function(row, data, index) {",
                                                                                        "  var maxLength = 20;", # 设置最大字符数
                                                                                        "  $('td', row).each(function(i, cell) {",
                                                                                        "    var cellData = cell.textContent.trim();",
                                                                                        "    if (cellData.length > maxLength) {",
                                                                                        "      cell.textContent = cellData.substring(0, maxLength) + '...';", # 截断并添加省略号
                                                                                        "    }",
                                                                                        "  });",
                                                                                        "}"),
                                                                        columnDefs = list(
                                                                                        list(className = 'dt-center', 
                                                                                                targets = '_all')
                                                                                        )
                                                                        ), selection = "none")
#
observeEvent(input$symbol_anno_back, {
    symbol_anno_process$process <- "start"
})
