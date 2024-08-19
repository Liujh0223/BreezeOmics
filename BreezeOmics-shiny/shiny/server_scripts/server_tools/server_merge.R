library(shiny)

# sidebar panels
output$merge_option <- renderUI({
    fluidPage(
        column(width = 12,
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
            column(width = 12, fileInput("merge_file1_upload", "Upload left file", accept = list(".csv", ".txt", ".xlsx")), style = "font-size:12px"),
            column(width = 12, fileInput("merge_file2_upload", "Upload right file", accept = list(".csv", ".txt", ".xlsx")), style = "font-size:12px")
            
        )
    )
})

# main panels
output$merge_main_panel <- renderUI({
           fluidPage(
                column(width = 12, align = "left", style="background-color: #0000001f;height: 50px;font-weight: 900;", 
                                HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right"),"Left file: ", ifelse(!is.null(input$merge_file1_upload),input$merge_file1_upload$name, "Please upload left file in the sidebar panel"), "</p>"))),
                column(width = 12, DT::dataTableOutput(outputId = "merge_file1", width = "100%"), style="background-color: #AEEEEE;margin-bottom: 20px;"),
                column(width = 12, align = "left", style="background-color: #0000001f;height: 50px;font-weight: 900;", 
                                HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right"),"Right file: ", ifelse(!is.null(input$merge_file2_upload),input$merge_file2_upload$name, "Please upload right file in the sidebar panel"), "</p>"))),
                column(width = 12, DT::dataTableOutput(outputId = "merge_file2", width = "100%"), style="background-color: #AEEEEE;margin-bottom: 20px;"),

                column(width = 2, selectInput("merge_column", "Merge by which column", width = "100%", choices = NULL), style = "font-size:12px"),
                column(width = 8, style = "font-size:12px",
                        selectInput("merge_mode", "Merge mode",  width = "100%", choices = list(
                            "inner_join(Keep rows that are common to both datasets, i.e., intersections)",
                            "left_join(All rows in the left dataset are retained, and matching rows in the right dataset are merged with the left dataset)",
                            "right_join(All rows in the dataset on the right are retained, and matching rows in the dataset on the left are merged with the dataset on the right)",
                            "full_join(Keep all rows in both datasets and fill in the missing values with NA)",
                            "semi_join(Returns rows with matches in the dataset on the left, not rows in the dataset on the right)",
                            "anti_join(Returns rows in the dataset on the left that have no matches, and does not return rows in the dataset on the right)"))),
                column(width = 2, actionButton("merge_start", "Start merge", style="font-size:15px; width:100%"), style="top: 23px;"),
                column(width = 12, uiOutput("merge_result_ui"))
           )
})

# file1
merge_file1 <- reactive({
    req(input$merge_file1_upload)
    # 读取选择的文件
    fileformat <- strsplit(input$merge_file1_upload$datapath, "\\.")[[1]][-1]
    # print(fileformat)
    if(fileformat == "csv"){
        data <- read.csv(input$merge_file1_upload$datapath, sep = ",")
    }else if(fileformat == "txt"){
        data <- read.csv(input$merge_file1_upload$datapath, sep = "\t")
    }else if(fileformat == "xlsx"){
        data <- openxlsx::read.xlsx(input$merge_file1_upload$datapath, colNames = TRUE, rowNames = F)
    }else {
        shinyjs::runjs('alert("Currently, only txt, csv, and xlsx files are supported")')
        return(NULL)
    }
})

merge_file2 <- reactive({
    req(input$merge_file2_upload)
    # 读取选择的文件
    fileformat <- strsplit(input$merge_file2_upload$datapath, "\\.")[[1]][-1]
    # print(fileformat)
    if(fileformat == "csv"){
        data <- read.csv(input$merge_file2_upload$datapath, sep = ",")
    }else if(fileformat == "txt"){
        data <- read.csv(input$merge_file2_upload$datapath, sep = "\t")
    }else if(fileformat == "xlsx"){
        data <- openxlsx::read.xlsx(input$merge_file2_upload$datapath, colNames = TRUE, rowNames = F)
    }else {
        shinyjs::runjs('alert("Currently, only txt, csv, and xlsx files are supported")')
        return(NULL)
    }
})

output$merge_file1 <- DT::renderDataTable(merge_file1(), rownames = FALSE, options = list(dom = 'fltipr', pageLength = 3, scrollX=TRUE, orderClasses = T,
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
output$merge_file2 <- DT::renderDataTable(merge_file2(), rownames = FALSE, options = list(dom = 'fltipr', pageLength = 3, scrollX=TRUE, orderClasses = T,
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

observe({
        req(merge_file1(), merge_file2())
        merge_stat$process <- "stop"
        # 读取选择的文件
        file1_colname <- colnames(merge_file1())
        file2_colname <- colnames(merge_file2())
        if(length(intersect(file1_colname, file2_colname)) == 0){
            shinyjs::runjs('alert("ERROR: There is no overlap between the column names of file1 and file2")')
            merge_colunm <- NULL
        }else {
           merge_colunm <- unique(intersect(file1_colname, file2_colname))
        }
        # 填充选择框选项
        updateSelectInput(session, "merge_column", choices = merge_colunm)
  })

merge_stat <- reactiveValues(process="stop")

observeEvent(input$merge_start, {
    req(input$merge_column)
    merge_stat$process <- "start"
})

merge_result <- reactive({
    req(input$merge_column, merge_file1(), merge_file2(), input$merge_mode)
    file1 <- merge_file1()
    file2 <- merge_file2()
    merge_mode <- input$merge_mode

    frequency_file1 <- table(file1[,input$merge_column])
    frequency_file2 <- table(file2[,input$merge_column])
    print(length(frequency_file1[1]))
    print(length(frequency_file2[1]))

    if (frequency_file1[1] > 2000 | frequency_file2[1] > 2000) {
        merge_stat$process <- "stop"
        shinyjs::runjs(paste0('alert("你所选合并列的',names(frequency_file1[1]),'或',names(frequency_file1[1]),'重复大于2000，终止合并，请检查合并列是否正确")'))

        return(NULL)
    } else {
        if(input$merge_mode == "inner_join(Keep rows that are common to both datasets, i.e., intersections)") {# 保留两个数据集中共有的行，即交集
            result <- dplyr::inner_join(file1, file2, by = input$merge_column)
        }else if (input$merge_mode == "left_join(All rows in the left dataset are retained, and matching rows in the right dataset are merged with the left dataset)") {# 保留左侧数据集中的所有行，右侧数据集中匹配的行与左侧数据集进行合并
        result <- dplyr::left_join(file1, file2, by = input$merge_column)
        }else if (input$merge_mode == "right_join(All rows in the dataset on the right are retained, and matching rows in the dataset on the left are merged with the dataset on the right)") {# 保留右侧数据集中的所有行，左侧数据集中匹配的行与右侧数据集进行合并
        result <- dplyr::right_join(file1, file2, by = input$merge_column)
        }else if (input$merge_mode == "full_join(Keep all rows in both datasets and fill in the missing values with NA)") {# 保留两个数据集中所有的行，用NA填充缺失的值
        result <- dplyr::full_join(file1, file2, by = input$merge_column)
        }else if (input$merge_mode == "semi_join(Returns rows with matches in the dataset on the left, not rows in the dataset on the right)") {# 返回左侧数据集中有匹配项的行，不返回右侧数据集中的行
        result <- dplyr::semi_join(file1, file2, by = input$merge_column)
        }else if (input$merge_mode == "anti_join(Returns rows in the dataset on the left that have no matches, and does not return rows in the dataset on the right)") {# 返回左侧数据集中没有匹配项的行，不返回右侧数据集中的行
        result <- dplyr::anti_join(file1, file2, by = input$merge_column)
        }
        return(result)
    }


})

output$merge_result_ui <- renderUI({
    req(merge_result)
    if(merge_stat$process == "start"){
        fluidRow(
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
            column(width = 10, align = "left", style="background-color: #0000001f;height: 50px;font-weight: 900;", 
                            HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right"),"The merge is complete! ", "</p>"))),
            column(width = 2, align = "right", radioGroupButtons(inputId = "merge_format",label = NULL, choices = c(".csv", ".txt"), justified = TRUE, width = "100%", checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
            column(width = 2, align = "right", downloadButton("download_merge_result", label = "Download", style = "text-align: center;margin-top: 6px;font-size:13px"),style="background-color: #0000001f;height: 50px;"),
            column(width = 12, DT::dataTableOutput(outputId = "merge_result", width = "100%"), style="background-color: #AEEEEE;margin-bottom: 20px;")
        )
    }else {
       return(NULL)
    }
})
output$download_merge_result <- downloadHandler(
    filename = function() {paste0("merge_result", input$merge_format)},
    content = function(file) {
        if(input$merge_format==".txt"){
            write.table(merge_result(), file, row.names = FALSE, sep="\t", quote = F)
        }else {
            write.table(merge_result(), file, row.names = FALSE, sep=",", quote = F)
        }
      
    }
  )
output$merge_result <- DT::renderDataTable(merge_result(), rownames = FALSE, options = list(dom = 'fltipr', pageLength = 10, scrollX=TRUE, orderClasses = T,
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