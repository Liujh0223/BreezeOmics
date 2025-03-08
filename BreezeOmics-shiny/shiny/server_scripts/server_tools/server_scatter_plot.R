# sidebar panels
output$Scatter_plot_option <- renderUI({
    fluidPage(
        column(width = 12,
            column(width = 12, actionButton(inputId = "reset_scatter_plot", label = "Reset", icon = icon("arrows-rotate"),style = "font-size: 17px;margin-bottom: -10px;WIDTH: 300px;margin-bottom: 10px;")),
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
            column(width = 12, fileInput(inputId = "upload_scatter_plot_matrix", label = "Upload data matrix", accept = list(".csv", ".xlsx", ".txt")), style = "font-size:18px"),
            br()
        )
    )
})

#main panels
output$Scatter_plot_panel <- renderUI({uiOutput("scatter_plot_plot")})
output$scatter_plot_plot <- renderUI({
    if(values_scatter_plot$upload_matrix == "uploaded"){
        fluidRow(
            column(width = 12,
                column(width = 12, align = "left", style="background-color: #dddddd6b;height: 50px;width: 100%;font-weight: 900;line-height: 2.5em;font-size: 20px;", HTML(input$upload_scatter_plot_matrix$name)),
                column(width = 12, align = "center", DT::dataTableOutput(outputId = "output_scatter_plot_matrix"), style="background-color: #dddddd6b;margin-bottom: 20px;")
            ),
            br(),
            HTML('<hr style="width: 98%;border-top: 1px solid #ddd;" />'),
            column(width = 2, offset = 3, style = "font-size:12px",selectInput("scatter_plot_x", "Select X axis", choices = NULL)),
            column(width = 2, style = "font-size:12px",selectInput("scatter_plot_y", "Select Y axis", choices = NULL)),
            column(width = 2, style = "font-size:12px",selectInput("scatter_plot_col", "Select colours", choices = NULL)),
            HTML('<hr style="width: 98%;border-top: 1px solid #ddd;" />'),
            column(width = 8, align = "center", plotOutput("scatter_plot", width = "500px", height = "500px") %>% withSpinner, style = "top: 130px;"),
            column(width = 4,
                column(width = 6, textInput("scatter_plot_x_title", value = "X title", label = "X axis title")),
                column(width = 6, textInput("scatter_plot_y_title", value = "Y title", label = "Y axis title")),
                column(width = 12, textInput("scatter_plot_x_hbline", value = NULL, label = "X auxiliary line(intercept,linetype,size,colour;1,1,1,black)", width = "100%")),
                column(width = 12, textInput("scatter_plot_y_vbline", value = NULL, label = "Y auxiliary line(intercept,linetype,size,colour;1,1,1,black)", width = "100%")),
                column(width = 12, textInput("scatter_plot_abline", value = NULL, label = "Slash (intercept,slope,linetype,size,colour;1,1,1,1,black)", width = "100%")),
                column(width = 4, colourpicker::colourInput("scatter_plot_point_color", label = "Point colour", "royalblue")),
                column(width = 4, numericInput("scatter_plot_point_size", value = 2.5, label = "Point size")),
                column(width = 4, numericInput("scatter_plot_point_shape", value = 16, max = 25, min = 1,label = "Point shape")),
                column(width = 6, numericInput("scatter_plot_title_size", value = 15, label = "Title size")),
                column(width = 6, numericInput("scatter_plot_text_size", value = 15, label = "Text size")),
                column(width = 4, checkboxInput("scatter_plot_show_pcc", value = F, label = "Show PCC"), style = "margin-top: 15px;"),
                column(width = 4, numericInput("scatter_plot_pcc_x", value = 5, label = "PCC x pos")),
                column(width = 4, numericInput("scatter_plot_pcc_y", value = -4, label = "PCC y pos")),
                column(width = 4, checkboxInput("scatter_plot_show_legend", value = F, label = "Show legend"), style = "margin-top: 15px;"),
                column(width = 4, numericInput("scatter_plot_legend_x", value = 0.9, label = "Legend x pos")),
                column(width = 4, numericInput("scatter_plot_legend_y", value = 0.95, label = "Legend y pos")),
                column(width = 3, numericInput("scatter_plot_x_min", value = -5, label = "MIN X")),
                column(width = 3, numericInput("scatter_plot_x_max", value = 5, label = "MAX X")),
                column(width = 3, numericInput("scatter_plot_y_min", value = -5, label = "MIN Y")),
                column(width = 3, numericInput("scatter_plot_y_max", value = 5, label = "MAX Y")),
                column(width = 4, numericInput("scatter_plot_width", value = 8, label = "Width")),
                column(width = 4, numericInput("scatter_plot_height", value = 8, label = "Height")),
                column(width = 4, selectInput(inputId = "scatter_plot_format",label = "Format", choices = c("PNG", "PDF"), selected = "PNG", width = "100%")),
                column(width = 12, downloadButton("download_scatter_plot", label = NULL, style = "width:100%;"))
            )
        )
    }else {
       fluidPage(
            column(width = 12,style = "margin-bottom:300px; margin-top:200px",
                HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
                column(width = 12, align="center", 
                            HTML("<p class='title'>This section is a highly customized scatter plot, which supports calculating XY axis correlations, drawing volcano maps, etc., and the R package depends on \"ggplot2\"。<br/> 
                            At present, this section only needs to input numeric matrices, which supports CSV and XLSX formats.</p>")),
                HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />')

            )
       )
    }
})

# resets
observeEvent(input$reset_scatter_plot, {
  shinyjs::reset("upload_scatter_plot_matrix")
  values_scatter_plot$upload_matrix <- 'NULL'
})

# 
values_scatter_plot <- reactiveValues(upload_matrix = "NULL")
#check matrix
observeEvent(input$upload_scatter_plot_matrix, {
    countdata <- input$upload_scatter_plot_matrix
    #读取matrix
    fileformat <- strsplit(countdata$datapath, "\\.")[[1]][-1]
    if (fileformat == "csv") {
        countdata <- read.csv(countdata$datapath, header = TRUE)
    }else if (fileformat == "xlsx") {
        countdata <- openxlsx::read.xlsx(countdata$datapath, colNames = TRUE, rowNames = F)
    }else if (fileformat == "txt"){
        countdata <- read.csv(countdata$datapath, sep = "\t", header = TRUE)
    }

    #判断matrix是否数值
    # data <- unlist(matrix(countdata))
    # if (typeof(data) == "character") {
    #         shinyjs::reset("upload_complexheatmap_matrix")
    #         values_complexheatmap$upload_matrix <- 'NULL'
    #         shinyjs::reset("upload_complexheatmap_col_anno")
    #         values_complexheatmap$upload_row_anno <- 'NULL'
    #         shinyjs::reset("upload_complexheatmap_row_anno")
    #         values_complexheatmap$upload_col_anno <- 'NULL'
    #         shinyjs::runjs('alert("ERROR: The matrix must be numeric.")')
    #         return(NULL)
    # }

    values_scatter_plot$upload_matrix <- 'uploaded'
})
#读取矩阵, 输出矩阵
scatter_plot_matrix <- reactive({
    if(values_scatter_plot$upload_matrix == "uploaded"){
        countdata <- input$upload_scatter_plot_matrix
        #读取matrix
        fileformat <- strsplit(countdata$datapath, "\\.")[[1]][-1]
        if (fileformat == "csv") {
            countdata <- read.csv(countdata$datapath, header = TRUE)
        }else if (fileformat == "xlsx") {
            countdata <- openxlsx::read.xlsx(countdata$datapath, colNames = TRUE, rowNames = F)
        }else if (fileformat == "txt"){
            countdata <- read.csv(countdata$datapath, sep = "\t", header = TRUE)
        }
        return(countdata)
    }else {
       return(NULL)
    }
})
output$output_scatter_plot_matrix <- DT::renderDataTable(scatter_plot_matrix(), options = list(dom = 'fltpr', pageLength = 5, scrollX=TRUE, orderClasses = T), selection = "none")

observe({
        req(scatter_plot_matrix())
        # 读取选择的文件
        file1_colname <- colnames(scatter_plot_matrix())
        # 填充选择框选项
        updateSelectInput(session, "scatter_plot_x", choices = file1_colname)
        updateSelectInput(session, "scatter_plot_y", choices = file1_colname)
        updateSelectInput(session, "scatter_plot_col", choices = c("NULL", file1_colname))
  })


# scatter plot
scatter_plot <- reactive({
    req(scatter_plot_matrix(), input$scatter_plot_x, input$scatter_plot_y, input$scatter_plot_col)
    data <- scatter_plot_matrix()
    if(input$scatter_plot_col == "NULL"){
        data <- data.frame(x = data[,input$scatter_plot_x], y = data[,input$scatter_plot_y], colour = input$scatter_plot_point_color)
    }else {
       data <- data.frame(x = data[,input$scatter_plot_x], y = data[,input$scatter_plot_y], colour = data[,input$scatter_plot_col])
    }

    a <- ggplot(data, aes(x = x, y = y, colour = data$colour)) +
            geom_point(size = input$scatter_plot_point_size, shape = input$scatter_plot_point_shape) + theme_bw() +
            scale_color_manual(values = data$colour)+
            xlab(input$scatter_plot_x_title) + ylab(input$scatter_plot_y_title) + 
            theme(axis.title = element_text(size = input$scatter_plot_title_size, colour = "black"), axis.text = element_text(size = input$scatter_plot_text_size, colour = "black")) + 
            xlim(input$scatter_plot_x_min, input$scatter_plot_x_max) + ylim(input$scatter_plot_y_min, input$scatter_plot_y_max) 
            

    if(input$scatter_plot_x_hbline != ""){
        input_string_x <- input$scatter_plot_x_hbline
        split_by_semicolon_x <- strsplit(input_string_x, ";")[[1]]
        # 检查所有元素的格式
        result_x <- all(sapply(split_by_semicolon_x, check_format_1))
        if(result_x == T){
            result_matrix_x <- format_to_datafarme(input$scatter_plot_x_hbline)
            a <- a + lapply(1:nrow(result_matrix_x), function(i) {geom_hline(yintercept = as.numeric(result_matrix_x$V1[i]), linetype = as.numeric(result_matrix_x$V2[i]), size = as.numeric(result_matrix_x$V3[i]), color = result_matrix_x$V4[i])}) # nolint
        }
    }
    if(input$scatter_plot_y_vbline != ""){
        input_string_y <- input$scatter_plot_y_vbline
        split_by_semicolon_y <- strsplit(input_string_y, ";")[[1]]
        # 检查所有元素的格式
        result_y <- all(sapply(split_by_semicolon_y, check_format_1))
        if(result_y == T){
            result_matrix_y <- format_to_datafarme(input$scatter_plot_y_vbline)
            a <- a + lapply(1:nrow(result_matrix_y), function(i) {geom_vline(xintercept = as.numeric(result_matrix_y$V1[i]), linetype = as.numeric(result_matrix_y$V2[i]), size = as.numeric(result_matrix_y$V3[i]), color = result_matrix_y$V4[i])}) # nolint
        }
    }
    if(input$scatter_plot_abline != ""){
        input_string <- input$scatter_plot_abline
        split_by_semicolon <- strsplit(input_string, ";")[[1]]
        # 检查所有元素的格式
        result <- all(sapply(split_by_semicolon, check_format_2))
        print(result)
        if(result == T){
            result_matrix <- format_to_datafarme(input$scatter_plot_abline)
            print(result_matrix)
            a <- a + lapply(1:nrow(result_matrix), function(i) {geom_abline(intercept = as.numeric(result_matrix$V1[i]), slope = as.numeric(result_matrix$V2[i]), linetype = as.numeric(result_matrix$V3[i]), size = as.numeric(result_matrix$V4[i]), color = result_matrix$V5[i])}) # nolint
        }
    }
    if(input$scatter_plot_show_pcc == T){
        correlation <- cor(data$x, data$y)
        a <- a + annotate("text", x = input$scatter_plot_pcc_x, y = input$scatter_plot_pcc_y, label = paste("R =", round(correlation, 2)), hjust = 1, vjust = 1, size = 11)
    }
    if(input$scatter_plot_show_legend == T){
        a <- a + theme(legend.position = c(input$scatter_plot_legend_x, input$scatter_plot_legend_y), legend.title = element_blank())
    }else {
       a <- a + theme(legend.position = "none", legend.title = element_blank())
    }

    return(a)
})
output$scatter_plot <- renderPlot(scatter_plot())
output$download_scatter_plot <- downloadHandler(#下载
    filename = function() { paste("scatter_plot", input$scatter_plot_format, sep = ".")},
    content = function(file) {
    p <- scatter_plot()
    ggsave(file, p, width = input$scatter_plot_width, height = input$scatter_plot_height)
})