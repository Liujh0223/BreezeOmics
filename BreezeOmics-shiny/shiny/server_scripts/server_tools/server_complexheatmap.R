library(ComplexHeatmap)

#sidebar panels
output$complexheatmap_option <- renderUI({
    fluidPage(
        column(width = 12,
            column(width = 12, actionButton(inputId = "reset_complexheatmap_plot", label = "Reset", icon = icon("arrows-rotate"),style = "font-size: 17px;margin-bottom: -10px;WIDTH: 300px;margin-bottom: 10px;")),
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
            column(width = 12, fileInput(inputId = "upload_complexheatmap_matrix", label = "Upload genes expression matrix", accept = list(".csv", ".xlsx", ".txt")), style = "font-size:18px"),
            column(width = 12, align="left", offset = 0, style = "font-size: 18px;margin-top: -20px;",
                    shinyWidgets::radioGroupButtons(inputId = "complexheatmap_cpm", label = "CPM Homogenization or not", choices = c("YES", "NO"), selected = "NO", individual = TRUE, width = "100%",
                    checkIcon = list(yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"), no = tags$i(class = "fa fa-square-o",style = "color: steelblue")))),
            column(width = 12, align="left", offset = 0, style = "font-size: 18px;margin-top: -0px;",
                    shinyWidgets::radioGroupButtons(inputId = "complexheatmap_scale", label = "Scale or not", choices = c("YES", "NO"), selected = "YES", individual = TRUE, width = "100%",
                    checkIcon = list(yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"), no = tags$i(class = "fa fa-square-o",style = "color: steelblue")))),
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
            column(width = 12, fileInput(inputId = "upload_complexheatmap_col_anno", label = "Upload column annotation file", accept = list(".csv", ".xlsx", ".txt")), style = "font-size:18px"),
            column(width = 12, fileInput(inputId = "upload_complexheatmap_row_anno", label = "Upload row annotation file", accept = list(".csv", ".xlsx", ".txt")), style = "font-size:18px")
        )
    )
})

#main panels
output$complexheatmap_main_panel <- renderUI({uiOutput("complexheatmap_plot")})

#main panels ui
output$complexheatmap_plot <- renderUI({
    if(values_complexheatmap$upload_matrix == "uploaded"){
        fluidRow(
            column(width = 12,
                column(width = 4, align = "left", style="background-color: #dddddd6b;height: 50px;width: 250px;font-weight: 900;line-height: 2.5em;font-size: 20px;", HTML("Heatmap matrix")),
                column(width = 2, downloadButton(outputId = "download_complexheatmap_matrix", label = "Download", style = "height:30px;line-height: 0em;"), style="background-color: #dddddd6b;height: 50px;line-height: 3.5em;width: 150px"),
                column(width = 12, align = "center", DT::dataTableOutput(outputId = "output_complexheatmap_matrix"), style="background-color: #dddddd6b;margin-bottom: 20px;")
            ),
            br(),
            HTML('<hr style="width: 98%;border-top: 1px solid #ddd;" />'),
            column(width = 12, uiOutput(outputId = "col_anno_ui")),
            column(width = 12, uiOutput(outputId = "row_anno_ui")),
            
            fluidRow(style = "margin-bottom: -0px;",
                column(width = 12, uiOutput("col_anno_setting_ui"), style = "border-right:1px solid #ddd;"),
                column(width = 12, uiOutput("row_anno_setting_ui"))
                
            ),

            mainPanel(width = 8,
                # sidebarPanel(width = 12),
                column(width = 12, align = "center", plotOutput("plot_complexheatmap", width = "100%", height = "800px") %>% withSpinner)
            ),
            sidebarPanel(width = 4,

                fluidRow(style = "margin-bottom: -20px;",#Graph colour
                    column(width = 12, align="left", offset = 0, style = "margin-top: -15px;margin-bottom: -5px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Graph colour</div>")),
                    column(width = 4, colourpicker::colourInput("complexheatmap_col1", label = NULL, "blue")),
                    column(width = 4, colourpicker::colourInput("complexheatmap_col2", label = NULL, "white")),
                    column(width = 4, colourpicker::colourInput("complexheatmap_col3", label = NULL, "red"))
                ),# close fluidRow
                HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
                fluidRow(style = "margin-bottom: -20px;",#X title
                    column(width = 3, align="left", offset = 0, style = "margin-top: -15px;margin-bottom: -5px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>X title</div>")),
                    column(width = 3, align="left", offset = 0, style = "margin-top: -15px;margin-bottom: -5px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Size</div>")),
                    column(width = 3, align="left", offset = 0, style = "margin-top: -15px;margin-bottom: -5px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Colour</div>")),
                    column(width = 3, align="left", offset = 0, style = "margin-top: -15px;margin-bottom: -5px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Side</div>")),
                    column(width = 3, textInput("complexheatmap_x_title", value = "", label = NULL)),
                    column(width = 3, numericInput("complexheatmap_x_title_size", label = NULL, value = 8)),
                    column(width = 3, colourpicker::colourInput("complexheatmap_x_title_col", label = NULL, "black")),
                    column(width = 3, shinyWidgets::pickerInput(inputId = "complexheatmap_x_side",label = NULL, choices = c("right", "left"), selected = "right", width = "100%")),
                ),# close fluidRow
                HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
                fluidRow(style = "margin-bottom: -20px;",#Y title
                    column(width = 3, align="left", offset = 0, style = "margin-top: -15px;margin-bottom: -5px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Y title</div>")),
                    column(width = 3, align="left", offset = 0, style = "margin-top: -15px;margin-bottom: -5px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Size</div>")),
                    column(width = 3, align="left", offset = 0, style = "margin-top: -15px;margin-bottom: -5px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Colour</div>")),
                    column(width = 3, align="left", offset = 0, style = "margin-top: -15px;margin-bottom: -5px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Side</div>")),
                    column(width = 3, textInput("complexheatmap_y_title", value = "", label = NULL)),
                    column(width = 3, numericInput("complexheatmap_y_title_size", label = NULL, value = 8)),
                    column(width = 3, colourpicker::colourInput("complexheatmap_y_title_col", label = NULL, "black")),
                    column(width = 3, shinyWidgets::pickerInput(inputId = "complexheatmap_y_side",label = NULL, choices = c("top", "bottom"), selected = "top", width = "100%")),
                ),# close fluidRow
                HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
                fluidRow(style = "margin-bottom: -20px;",#X label
                    column(width = 12, align="center", offset = 0, style = "margin-top: -15px;margin-bottom: -5px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>X name</div>")),
                    column(width = 4, align = "left", checkboxInput("complexheatmap_show_x_name", label = "Label", value = TRUE)),
                    column(width = 4, align = "left", checkboxInput("complexheatmap_x_cluster", label = "Cluster", value = TRUE)),
                    column(width = 4, align = "left", checkboxInput("complexheatmap_show_x_dend", label = "Dend", value = TRUE)),
                    column(width = 4, align="left", offset = 0, style = "margin-top: -15px;margin-bottom: -5px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Rotation</div>")),
                    column(width = 4, align="left", offset = 0, style = "margin-top: -15px;margin-bottom: -5px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Size</div>")),
                    column(width = 4, align="left", offset = 0, style = "margin-top: -15px;margin-bottom: -5px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Colour</div>")),
                    column(width = 4, numericInput("complexheatmap_x_rot", label = NULL, value = 0)),
                    column(width = 4, numericInput("complexheatmap_x_size", label = NULL, value = 10)),
                    column(width = 4, colourpicker::colourInput("complexheatmap_x_col", label = NULL, "black")),
                ),# close fluidRow
                HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
                fluidRow(style = "margin-bottom: -20px;",#Y label
                    column(width = 12, align="center", offset = 0, style = "margin-top: -15px;margin-bottom: -5px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Y name</div>")),
                    column(width = 4, align = "left", checkboxInput("complexheatmap_show_y_name", label = "Label", value = TRUE)),
                    column(width = 4, align = "left", checkboxInput("complexheatmap_y_cluster", label = "Cluster", value = TRUE)),
                    column(width = 4, align = "left", checkboxInput("complexheatmap_show_y_dend", label = "Dend", value = TRUE)),
                    column(width = 4, align="left", offset = 0, style = "margin-top: -15px;margin-bottom: -5px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Rotation</div>")),
                    column(width = 4, align="left", offset = 0, style = "margin-top: -15px;margin-bottom: -5px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Size</div>")),
                    column(width = 4, align="left", offset = 0, style = "margin-top: -15px;margin-bottom: -5px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Colour</div>")),
                    column(width = 4, numericInput("complexheatmap_y_rot", label = NULL, value = 45)),
                    column(width = 4, numericInput("complexheatmap_y_size", label = NULL, value = 10)),
                    column(width = 4, colourpicker::colourInput("complexheatmap_y_col", label = NULL, "black"))
                ),# close fluidRow
                HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
                fluidRow(style = "margin-bottom: -20px;",#Legend
                    column(width = 3, align="left", offset = 0, style = "margin-top: -15px;margin-bottom: -5px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Legend</div>")),
                    column(width = 3, align="left", offset = 0, style = "margin-top: -15px;margin-bottom: -5px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Side</div>")),
                    column(width = 3, align="left", offset = 0, style = "margin-top: -15px;margin-bottom: -5px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Height</div>")),
                    column(width = 3, align="left", offset = 0, style = "margin-top: -15px;margin-bottom: -5px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Direction</div>")),
                    column(width = 3, textInput("complexheatmap_legend_title", value = "", label = NULL)),
                    column(width = 3, shinyWidgets::pickerInput(inputId = "complexheatmap_legend_side",label = NULL, choices = c("topleft", "topcenter"), selected = "topleft", width = "100%")),
                    column(width = 3, numericInput("complexheatmap_legend_title_height", label = NULL, value = 2.5)),
                    column(width = 3, shinyWidgets::pickerInput(inputId = "complexheatmap_legend_direction",label = NULL, choices = c("v", "h"), selected = "v", width = "100%"))
                ),# close fluidRow
                HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
                fluidRow(style = "margin-bottom: -20px;",#width height format
                    column(width = 6, align="left", offset = 0, style = "margin-top: -15px;margin-bottom: -5px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Width</div>")),
                    column(width = 6, align="left", offset = 0, style = "margin-top: -15px;margin-bottom: -5px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Height</div>")),
                    column(width = 6, numericInput("complexheatmap_width", label = NULL, value = 6)),
                    column(width = 6, numericInput("complexheatmap_height", label = NULL, value = 10)),
                    column(width = 6, shinyWidgets::pickerInput(inputId = "complexheatmap_format",label = NULL, choices = c("PNG", "PDF"), selected = "PNG", width = "100%")),
                    column(width = 12, downloadButton("download_complexheatmap", label = "Download", width = "100%"))
                ),# close fluidRow
            )
        )
    }else {
       fluidPage(
            column(width = 12,style = "margin-bottom:300px; margin-top:200px",
                HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
                column(width = 12, align="center", 
                            HTML("<p class='title'>This section is the heat map section, and the R package dependency package is\"ComplexHeatmap\"。<br/> 
                            This section currently requires the input of a numerical gene expression matrix.<br/> 
                            Users can choose whether to need normalization according to their own situation, and this program provides CPM normalization mode.</p>")),
                HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />')

            )
       )
    }
})

values_complexheatmap <- reactiveValues(upload_matrix = "NULL", upload_row_anno = "NULL", upload_col_anno = "NULL")

#check matrix
observeEvent(input$upload_complexheatmap_matrix, {
    countdata <- input$upload_complexheatmap_matrix

    #读取matrix
    fileformat <- strsplit(countdata$datapath, "\\.")[[1]][-1]
    if (fileformat == "csv") {
        countdata <- read.csv(countdata$datapath, header = TRUE, row.names = 1)
    }else if (fileformat == "xlsx") {
        countdata <- openxlsx::read.xlsx(countdata$datapath, colNames = TRUE, rowNames = T)
    }else if (fileformat == "txt") {
       countdata <- read.table(countdata$datapath, col.names = 1, row.names = 1, sep = "\t")
    }

    # sample name detect
    if (!check_column_names(input$upload_complexheatmap_matrix$datapath)) {
        shinyjs::runjs('alert("ERROR: The sample name can only be composed of <a~z A~Z 0~9 _ .>, and only the beginning of the letter is supported. Please check that your sample name is named correctly")')
        shinyjs::reset("upload_complexheatmap_matrix")
        values_complexheatmap$upload_matrix <- 'NULL'
        shinyjs::reset("upload_complexheatmap_col_anno")
        values_complexheatmap$upload_row_anno <- 'NULL'
        shinyjs::reset("upload_complexheatmap_row_anno")
        values_complexheatmap$upload_col_anno <- 'NULL'
        return(NULL)
    }

    #判断是否超过10000行或10000列
    if(ncol(countdata) > 10000 | nrow(countdata) > 10000){
        shinyjs::reset("upload_complexheatmap_matrix")
        values_complexheatmap$upload_matrix <- 'NULL'
        shinyjs::reset("upload_complexheatmap_col_anno")
        values_complexheatmap$upload_row_anno <- 'NULL'
        shinyjs::reset("upload_complexheatmap_row_anno")
        values_complexheatmap$upload_col_anno <- 'NULL'
        shinyjs::runjs('alert("ERROR: Due to limited computing resources, the number of rows or columns in a matrix cannot exceed 10,000")')
        return(NULL)
    }

    #判断matrix是否数值
    data <- unlist(matrix(countdata))
    if (typeof(data) == "character") {
            shinyjs::reset("upload_complexheatmap_matrix")
            values_complexheatmap$upload_matrix <- 'NULL'
            shinyjs::reset("upload_complexheatmap_col_anno")
            values_complexheatmap$upload_row_anno <- 'NULL'
            shinyjs::reset("upload_complexheatmap_row_anno")
            values_complexheatmap$upload_col_anno <- 'NULL'
            shinyjs::runjs('alert("ERROR: The matrix must be numeric.")')
            return(NULL)
    }

    values_complexheatmap$upload_matrix <- 'uploaded'
})

#resets
observeEvent(input$reset_complexheatmap_plot, {
  shinyjs::reset("upload_complexheatmap_matrix")
  values_complexheatmap$upload_matrix <- 'NULL'
  shinyjs::reset("upload_complexheatmap_col_anno")
  values_complexheatmap$upload_row_anno <- 'NULL'
  shinyjs::reset("upload_complexheatmap_row_anno")
  values_complexheatmap$upload_col_anno <- 'NULL'
})

#读取矩阵，均一化，取均值，输出矩阵
complexheatmap_matrix <- reactive({
    if(values_complexheatmap$upload_matrix == "uploaded"){
        countdata <- input$upload_complexheatmap_matrix
        
        #读取matrix
        fileformat <- strsplit(countdata$datapath, "\\.")[[1]][-1]
        if (fileformat == "csv") {
            countdata <- read.csv(countdata$datapath, header = TRUE, row.names = 1)
        }else if (fileformat == "xlsx") {
            countdata <- openxlsx::read.xlsx(countdata$datapath, colNames = TRUE, rowNames = T)
        }else if (fileformat == "txt") {
            countdata <- read.table(countdata$datapath, col.names = 1, row.names = 1, sep = "\t")
        }
        #均一化（自定义步骤）
        if(input$complexheatmap_cpm == "YES"){
            countdata <- t(t(countdata) / colSums(countdata) * 1000000)
            countdata <- data.frame(countdata)
        }
        #scale（自定义步骤）
        if(input$complexheatmap_scale == "YES"){
            countdata <- t(scale(t(countdata)))
            countdata <- na.omit(countdata)
        }
        return(countdata)
    }else {
       return(NULL)
    }
})
output$output_complexheatmap_matrix <- DT::renderDataTable(complexheatmap_matrix(), options = list(dom = 'fltpr', pageLength = 5, scrollX=TRUE, orderClasses = T), selection = "none")
output$download_complexheatmap_matrix <- downloadHandler(#下载enrich_geneset模板
    filename = function() {"heatmap_matrix.csv"},
    content = function(file) {
      data <- complexheatmap_matrix()
      write.csv(data, file, fileEncoding="GBK", quote = FALSE)
    }
)

#annotation file check
observeEvent(input$upload_complexheatmap_col_anno, {
    #先判断是否上传matrix
    if(values_complexheatmap$upload_matrix != 'uploaded'){
        shinyjs::reset("upload_complexheatmap_col_anno")
        values_complexheatmap$upload_row_anno <- 'NULL'
        shinyjs::runjs('alert("ERROR: upload matrix file first.")')
    }else {

        col_file <- input$upload_complexheatmap_col_anno
        data <- complexheatmap_matrix()
        data_col_name <- colnames(data)

        #读取file colname
        fileformat <- strsplit(col_file$datapath, "\\.")[[1]][-1]
        if (fileformat == "csv") {
            col_file <- read.csv(col_file$datapath, header = TRUE, row.names = 1)
            anno_col_name <- colnames(col_file)
        }else if (fileformat == "xlsx") {
            col_file <- openxlsx::read.xlsx(col_file$datapath, colNames = TRUE, rowNames = T)
            anno_col_name <- colnames(col_file)
        }else if (fileformat == "txt") {
            col_file <- read.table(col_file$datapath, col.names = 1, row.names = 1, sep = "\t")
            anno_col_name <- colnames(col_file)
        }

        #判断列名是否相同
        if(identical(intersect(anno_col_name, data_col_name), data_col_name)) {
            values_complexheatmap$upload_col_anno <- 'uploaded'
        }else {
            shinyjs::reset("upload_complexheatmap_col_anno")
            values_complexheatmap$upload_row_anno <- 'NULL'
            shinyjs::runjs('alert("ERROR: The column names in the matrix are not the same as the column names in your annotation file.")')
            return(NULL)
        }
    }
})
observeEvent(input$upload_complexheatmap_row_anno, {
    #先判断是否上传matrix
    if(values_complexheatmap$upload_matrix != 'uploaded'){
        shinyjs::reset("upload_complexheatmap_row_anno")
        values_complexheatmap$upload_row_anno <- 'NULL'
        shinyjs::runjs('alert("ERROR: upload matrix file first.")')
    }else {

        row_file <- input$upload_complexheatmap_row_anno
        data <- complexheatmap_matrix()
        data_row_name <- rownames(data)

        #读取file rowname
        fileformat <- strsplit(row_file$datapath, "\\.")[[1]][-1]
        if (fileformat == "csv") {
            row_file <- read.csv(row_file$datapath, header = TRUE, row.names = 1)
            anno_row_name <- rownames(row_file)
        }else if (fileformat == "xlsx") {
            row_file <- openxlsx::read.xlsx(row_file$datapath, rowNames = TRUE, rowNames = T)
            anno_row_name <- rownames(row_file)
        }else if (fileformat == "txt") {
            row_file <- read.table(row_file$datapath, col.names = 1, row.names = 1, sep = "\t")
            anno_row_name <- colnames(row_file)
        }

        #判断列名是否相同
        if(identical(intersect(anno_row_name, data_row_name), data_row_name)) {
            values_complexheatmap$upload_row_anno <- 'uploaded'
        }else {
            shinyjs::reset("upload_complexheatmap_row_anno")
            values_complexheatmap$upload_row_anno <- 'NULL'
            shinyjs::runjs('alert("ERROR: The rowumn names in the matrix are not the same as the rowumn names in your annotation file.")')
            return(NULL)
        }
    }
})

#annotation file ui
output$col_anno_ui <- renderUI({
    if(values_complexheatmap$upload_col_anno == 'uploaded'){
        fluidRow(
            column(width = 12,
                column(width = 4, align = "left", style="background-color: #dddddd6b;height: 50px;width: 250px;font-weight: 900;line-height: 2.5em;font-size: 20px;", HTML("Column annotation")),
                column(width = 12, align = "center", DT::dataTableOutput(outputId = "output_complexheatmap_col_anno"), style="background-color: #dddddd6b;margin-bottom: 20px;")
            ),
            br(),
            HTML('<hr style="width: 98%;border-top: 1px solid #ddd;" />')
        )
    }
})
col_anno_file <- reactive({
    if(values_complexheatmap$upload_col_anno == 'uploaded'){
        #读取file colname
        col_file <- input$upload_complexheatmap_col_anno
        fileformat <- strsplit(col_file$datapath, "\\.")[[1]][-1]
        if (fileformat == "csv") {
            col_file <- read.csv(col_file$datapath, header = TRUE, row.names = 1)
        }else if (fileformat == "xlsx") {
            col_file <- openxlsx::read.xlsx(col_file$datapath, colNames = TRUE, rowNames = T)
        }else if (fileformat == "txt") {
            col_file <- read.table(col_file$datapath, col.names = 1, row.names = 1, sep = "\t")
        }

        return(col_file)
    }else {
       return(NULL)
    }
})
output$output_complexheatmap_col_anno <- DT::renderDataTable(col_anno_file(), options = list(dom = 'fltpr', pageLength = 5, scrollX=TRUE, orderClasses = T), selection = "none")

output$row_anno_ui <- renderUI({
    if(values_complexheatmap$upload_row_anno == 'uploaded'){
        fluidRow(
            column(width = 12,
                column(width = 4, align = "left", style="background-color: #dddddd6b;height: 50px;width: 250px;font-weight: 900;line-height: 2.5em;font-size: 20px;", HTML("Row annotation")),
                column(width = 12, align = "center", DT::dataTableOutput(outputId = "output_complexheatmap_row_anno"), style="background-color: #dddddd6b;margin-bottom: 20px;")
            ),
            br(),
            HTML('<hr style="width: 98%;border-top: 1px solid #ddd;" />')
        )
    }
})
row_anno_file <- reactive({
    if(values_complexheatmap$upload_row_anno == 'uploaded'){
        #读取file rowname
        row_file <- input$upload_complexheatmap_row_anno
        fileformat <- strsplit(row_file$datapath, "\\.")[[1]][-1]
        if (fileformat == "csv") {
            row_file <- read.csv(row_file$datapath, header = TRUE, row.names = 1)
        }else if (fileformat == "xlsx") {
            row_file <- openxlsx::read.xlsx(row_file$datapath, colNames = TRUE, rowNames = T)
        }else if (fileformat == "txt") {
            row_file <- read.table(row_file$datapath, col.names = 1, row.names = 1, sep = "\t")
        }

        return(row_file)
    }else {
       return(NULL)
    }
})
output$output_complexheatmap_row_anno <- DT::renderDataTable(row_anno_file(), options = list(dom = 'fltpr', pageLength = 5, scrollX=TRUE, orderClasses = T), selection = "none")

#annototation file merge
merge_col_anno <- reactive({
    if(!is.null(col_anno_file())){
        col_anno_file <- col_anno_file()
        heatmap_matrix <- complexheatmap_matrix()

        col_anno_file <- t(col_anno_file)
        col_anno_file <- data.frame(col_anno_file)
        col_anno_file$sample <- rownames(col_anno_file)

        heatmap_matrix <- data.frame(sample = colnames(heatmap_matrix))

        merge_anno <- dplyr::inner_join(heatmap_matrix, col_anno_file, by = "sample")
        merge_anno <- merge_anno[,-1]
        return(merge_anno)
    }
})
merge_row_anno <- reactive({
    if(!is.null(row_anno_file())){
        row_anno_file <- row_anno_file()
        heatmap_matrix <- complexheatmap_matrix()

        row_anno_file$sample <- rownames(row_anno_file)
        heatmap_matrix <- data.frame(sample = rownames(heatmap_matrix))

        merge_anno <- dplyr::inner_join(heatmap_matrix, row_anno_file, by = "sample")
        rownames(merge_anno) <- merge_anno$sample
        print(colnames(merge_anno))

        return(merge_anno)
    }
})

#anno ui
output$col_anno_setting_ui <- renderUI({
    if(!is.null(col_anno_file())){
        fluidRow(
            column(width = 12,
                column(width = 2, align = "left", selectInput(inputId = "col_spilt", label = "Column split", choices = c("NULL", colnames(merge_col_anno()))), style = "border-right: 1px solid #ddd;"),
                column(width = 2, align = "left", selectInput(inputId = "col_anno1", label = "Column annotation", choices = c("NULL", colnames(merge_col_anno())))),
                HTML('<hr style="width: 98%;border-top: 1px solid #ddd;" />')
            )
        )
    }
})
output$row_anno_setting_ui <- renderUI({
    if(!is.null(row_anno_file())){
        fluidRow(
            column(width = 12,
                column(width = 2, align = "left", selectInput(inputId = "row_spilt", label = "Row split", choices = c("NULL", colnames(merge_row_anno()))), style = "border-right: 1px solid #ddd;"),
                column(width = 1, align = "left", selectInput(inputId = "row_label", label = "Row label", choices = c("NULL", colnames(merge_row_anno())))),
                column(width = 1, align = "left", numericInput("row_label_size", label = "Label size", value = 8)),
                column(width = 2, align = "left", selectInput(inputId = "row_anno1", label = "Row annotation", choices = c("NULL", colnames(merge_row_anno()))), style = "border-left: 1px solid #ddd;"),
                HTML('<hr style="width: 98%;border-top: 1px solid #ddd;" />')
            )
        )
    }
})

# complexheatmap plot
plot_complexheatmap <- reactive({
    if(values_complexheatmap$upload_matrix == "uploaded"){
        data <- complexheatmap_matrix()


        if(!is.null(merge_col_anno()) & !is.null(input$col_spilt)){
            if(input$col_spilt != "NULL"){
                col_anno <- merge_col_anno()
                col_split <- match(input$col_spilt, colnames(col_anno))
                col_split <- col_anno[,col_split]
            }else {
               col_split <- NULL
            }
        }else {
           col_split <- NULL
        }

        if(!is.null(merge_row_anno()) & !is.null(input$row_spilt)){
            if(input$row_spilt != "NULL"){
                row_anno <- merge_row_anno()
                row_split <- match(input$row_spilt, colnames(row_anno))
                row_split <- row_anno[,row_split]
            }else {
               row_split <- NULL
            }

        } else {
           row_split <- NULL
        }

        p <- ComplexHeatmap::Heatmap(data, border = T,
                col = c(input$complexheatmap_col1, input$complexheatmap_col2, input$complexheatmap_col3), na_col = 'white',
                row_title = input$complexheatmap_x_title, row_title_side = input$complexheatmap_x_side, row_title_gp = gpar(fontsize=input$complexheatmap_x_title_size, col = input$complexheatmap_x_title_col),
                column_title = input$complexheatmap_y_title, column_title_side = input$complexheatmap_y_side, column_title_gp = gpar(fontsize=input$complexheatmap_y_title_size, col = input$complexheatmap_y_title_col),
                show_row_names = input$complexheatmap_show_x_name, cluster_rows = input$complexheatmap_x_cluster, show_row_dend = input$complexheatmap_show_x_dend, row_names_rot = input$complexheatmap_x_rot, row_names_gp = gpar(fontsize=input$complexheatmap_x_size, col = input$complexheatmap_x_col),
                show_column_names = input$complexheatmap_show_y_name, cluster_columns = input$complexheatmap_y_cluster, show_column_dend = input$complexheatmap_show_y_dend, column_names_rot = input$complexheatmap_y_rot, column_names_gp = gpar(fontsize=input$complexheatmap_y_size, col = input$complexheatmap_y_col),
                heatmap_legend_param = list(title= input$complexheatmap_legend_title, title_position = input$complexheatmap_legend_side, legend_height=unit(input$complexheatmap_legend_title_height,"in"), legend_direction=input$complexheatmap_legend_direction),
                heatmap_width = unit(input$complexheatmap_width, "in"), heatmap_height = unit(input$complexheatmap_height, "in"), column_split = col_split, row_split = row_split)

        if(!is.null(merge_row_anno()) & !is.null(input$row_spilt)){
            if(input$row_label != "NULL"){
                row_anno <- merge_row_anno()
                row_anno <- data.frame(row_anno)
                row_label <- match(input$row_label, colnames(row_anno))
                row_label <- data.frame(sample = rownames(row_anno), label = row_anno[,row_label])
                row_label[row_label == ""] <- NA
                row_label <- na.omit(row_label)
                row_label <- data.frame(row_label)

                a=rowAnnotation(link = anno_mark(at = which(row.names(row_anno) %in% row_label$sample), labels = row_label$label, labels_gp = gpar(fontsize=input$row_label_size,col="black")))

                p <- p+a
            }
        }


        return(p)
    }else {
       return(NULL)
    }
})

output$plot_complexheatmap <- renderPlot(plot_complexheatmap())
output$download_complexheatmap <- downloadHandler(#下载enrich_geneset模板
    filename = function() {ifelse(input$complexheatmap_format=="PNG", "complexheatmap.png", "complexheatmap.pdf")},
    content = function(file) {
        width <- input$complexheatmap_width+1
        height <- input$complexheatmap_height+1
      if(input$complexheatmap_format=="PNG"){
        png(file, width = width, height = height, units = "in", res = 200)
            draw(plot_complexheatmap())
        dev.off()
      }else if (input$complexheatmap_format == "PDF") {
        pdf(file, width = width, height = height )
            draw(plot_complexheatmap())
        dev.off()
      }
    }
)