datacheck_process <- reactiveValues()
datacheck_process$value <- "start"

# render UI
observeEvent(datacheck_process$value, {
  if (datacheck_process$value == "start") {
    output$ui_datacheck <- renderUI({
      fluidRow(style = "height:700px;",
        br(),br(),br(),br(),br(),
        column(width = 12, style = "left:00px;",
               fluidRow(
                 column(width = 7, align="left", offset = 5,
                        HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step1: Upload you Matrix</div>")),
                column(width = 1, align = "right", offset = 4,
                    shinyWidgets::dropdownButton(
                      tags$h4(p(icon("check",lib = "font-awesome"),em("Upload gene expression matrix data (the value must be an integer read value), and the file format is only supported by .csv .xlsx"),style="color:#337ab7;text-align:center;font-size:15px;")),
                      column(width = 12, align = "center", downloadButton("download_deg_matrix_template2", label = "Template matrix", style = "border-color: #ccc;font-size:12px;text-align: center;")),
                      circle = TRUE, status = "default",
                      icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                    )
                  ),
                 column(width = 7, align="left", offset = 0,
                        fileInput(inputId = "datacheck_upload_matrix", label = NULL, accept = list(".csv", ".xlsx"), width = "340px"))
               ),# close fluidRow
               br(),
               fluidRow(
                 column(width = 7, align="left", offset = 5,
                        HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step2: Download you Group info</div>")),
                column(width = 1, align = "right", offset = 4,
                    shinyWidgets::dropdownButton(
                      tags$h4(p(icon("check",lib = "font-awesome"),em("Download your group information table, modify the group column (the sample column does not need to be modified), and upload it in Step 3."),style="color:#337ab7;text-align:center;font-size:15px;")),
                      circle = TRUE, status = "default",
                      icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                    )
                  ),
                 column(width = 7, align="left", offset = 0,
                        downloadButton(outputId = "datacheck_download_groupinfo", label = "Download", icon = icon("download"), style = "width: 340px;margin-bottom: -10px;"))
               ),# close fluidRow
               br(),br(),br(),
               fluidRow(
                column(width = 7, align="left", offset = 5,
                        HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step3: Upload you Group info</div>")),
                column(width = 1, align = "right", offset = 4,
                    shinyWidgets::dropdownButton(
                      tags$h4(p(icon("check",lib = "font-awesome"),em("Upload your sample group file (download the step2 file and modify the group column, and set the same group name for the same group of samples)."),style="color:#337ab7;text-align:center;font-size:15px;")),
                      column(width = 12, align = "center", downloadButton("download_deg_group_template2", label = "Template groupinfo", style = "border-color: #ccc;font-size:12px;text-align: center;")),
                      circle = TRUE, status = "default",
                      icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                    )
                  ),
                 column(width = 7, align="left", offset = 0,
                        fileInput(inputId = "datacheck_upload_groupinfo", label = NULL, accept = ".csv", width = "340px"))
               ),# close fluidRow
               br(),br(),br(),
               fluidRow(
                 column(width = 2, align="center", offset = 4,
                        actionButton(inputId = "reset_datacheck", label = "RESET", icon = icon("rotate-left"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;")),
                 column(width = 3, align="center", offset = 0,
                        actionButton(inputId = "enter_datacheck", label = "ENTER", icon = icon("check"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;"))
               )# close fluidRow
        )#, #close cloumn
        #column(width = 6, slickROutput("slickr"), style = "right:200px;"
        #) #close cloumn
      )# close fluidRow
    })
  } else if (datacheck_process$value == "processing") {
    output$ui_datacheck <- renderUI({
      fluidRow(
        br(),br(),
        tags$head(tags$style(HTML('.form-control {height: 37.5px;border: 1px solid #000;}'))),
        tags$head(tags$style(HTML('.selectize-input {border-color: black;line-height:23px; font-size:14px;height:38px}'))),
        HTML('<hr style="width: 90%;border-top: 2px solid #000;" />'),
        column(width = 12, align = "left", style = "width:80%;margin-left:10%;",
               column(width = 4, align = "left", h3("Expression heatmap of Var top 500 genes ")),
               column(width = 1, align = "left", offset = 0, style = "bottom: -15px;left: -30px;",
                  shinyWidgets::dropdownButton(
                    tags$h4(p(icon("check",lib = "font-awesome"),em("Genes of variance top 500: Expression heat map of the top 500 genes with the largest variance across all samples."),style="color:#337ab7;text-align:center;font-size:15px;")),
                    circle = TRUE, status = "default",
                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T)
                 ),
               HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'), br(),
               column(width = 10, align = "center", plotOutput(outputId = "plottop500", height = "470px", width = "500px")%>% withSpinner(), style = "margin-bottom: 50px;"),
               column(width = 2, align = "left", style = "left:-30px;margin-top:100px;",
                      column(width = 12, colourpicker::colourInput("color_up_top500", label = NULL, "#ff0000"),style = "margin-bottom:-13px;"),
                      column(width = 12, colourpicker::colourInput("color_mid_top500", label = NULL, "#ffffff"),style = "margin-bottom:-13px;"),
                      column(width = 12, colourpicker::colourInput("color_down_top500", label = NULL, "#0000ff")),
                      column(width = 12, numericInput("top500groupcolor", label = "Group colour", value = 1234, width = "100%")),
                      column(width = 5, numericInput("top500width", label = "Width", value = 8, width = "90%")),
                      column(width = 5, numericInput("top500height", label = "Height", value = 8, width = "90%"), style = "margin-left:-35px"),
                      column(width = 6, selectInput(inputId = "top500format", label = "Format", choices = c("PNG" = "png", "PDF" = "pdf"),width = "80%"),style = "position: absolute;top: 209px;right: -3px;"),
                      column(width = 12, align = "center", downloadButton("downloadtoop500", label = "Download Variance top 500",style = "width: 188px;border-color: #ccc;font-size:10px;text-align: center;")))
          ),
        HTML('<hr style="width: 90%;border-top: 2px solid #000;" />'),
        column(width = 12, align = "left", style = "width:80%;margin-left:10%;",
               column(width = 3, align = "left", h3("Sample hierarchical clustering")),
               column(width = 1, align = "left", offset = 0, style = "bottom: -15px;left: -40px;",
                  shinyWidgets::dropdownButton(
                    tags$h4(p(icon("check",lib = "font-awesome"),em("Sampul Hillach Carklesterling: Construction of a Cluster Tree Based on Euclidean Distance"),style="color:#337ab7;text-align:center;font-size:15px;")),
                    circle = TRUE, status = "default",
                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T)
                 ),
               HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'), br(),
               column(width = 10, align = "center", plotOutput(outputId = "datacheck_hcluster", height = "470px", width = "600px")%>% withSpinner(), style = "margin-bottom: 50px;"),      
               column(width = 2, align = "left", style = "left:-30px;margin-top:00px;",
                      column(width = 2, align = "left", h6("Top")),
                      column(width = 10, offset = 0, shinyWidgets::sliderTextInput(inputId = "datacheck_hc_top",label = NULL, choices = c(1:5),grid = TRUE, width = "100%", selected = 5), style = "margin-bottom: -10px;"),
                      column(width = 2, align = "left", h6("Bottom")),
                      column(width = 10, offset = 0, shinyWidgets::sliderTextInput(inputId = "datacheck_hc_bottom",label = NULL, choices = c(1:5),grid = TRUE, width = "100%", selected = 2), style = "margin-bottom: -10px;"),
                      column(width = 2, align = "left", h6("Left")),
                      column(width = 10, offset = 0, shinyWidgets::sliderTextInput(inputId = "datacheck_hc_left",label = NULL, choices = c(1:5),grid = TRUE, width = "100%", selected = 4), style = "margin-bottom: -10px;"),
                      column(width = 2, align = "left", h6("Right")),
                      column(width = 10, offset = 0, shinyWidgets::sliderTextInput(inputId = "datacheck_hc_right",label = NULL, choices = c(1:5),grid = TRUE, width = "100%", selected = 1), style = "margin-bottom: -10px;"),
                      column(width = 6, numericInput("datacheck_hc_cex", label = "Font-size", value = 1, width = "100%")),
                      column(width = 6, numericInput("datacheck_hc_seed", label = "Colours", value = 1234, width = "100%")),
                      column(width = 5, numericInput("datacheck_hc_width", label = "Width", value = 8, width = "90%")),
                      column(width = 5, numericInput("datacheck_hc_height", label = "Height", value = 8, width = "90%"), style = "margin-left:-35px"),
                      column(width = 6, selectInput(inputId = "datacheck_hc_format", label = "Format", choices = c("PNG" = "png", "PDF" = "pdf"),width = "80%"),style = "position: absolute;top: 337px;right: -3px;"),
                      column(width = 12, align = "center", downloadButton("downloaddatacheck_hc", label = "Sample hierarchical clustering",style = "width: 188px;border-color: #ccc;font-size:10px;text-align: center;margin-top: 10px;")))

        ),
        HTML('<hr style="width: 90%;border-top: 2px solid #000;" />'),
        column(width = 12, align = "left", style = "width:80%;margin-left:10%;",
               column(width = 3, align = "left", h3("Sample correlation heatmap")),
               column(width = 1, align = "left", offset = 0, style = "bottom: -15px;left: -40px;",
                  shinyWidgets::dropdownButton(
                    tags$h4(p(icon("check",lib = "font-awesome"),em("Correlation heatmap of each sample: The correlation between samples, the larger the correlation coefficient, the more correlation, the maximum is 1."),style="color:#337ab7;text-align:center;font-size:15px;")),
                    circle = TRUE, status = "default",
                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T)
                 ),
               HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'), br(),
               column(width = 10, align = "center", plotOutput(outputId = "corheatmap", width = "600px", height = "500px")%>% withSpinner(), style = "margin-bottom: 50px;"),# corheatmap
        
               column(width = 2, align = "left", style = "left:-30px;margin-top:50px;",
                      column(width = 12, colourpicker::colourInput("color_up_corheatmap", label = NULL, "#DD3E2C"),style = "margin-bottom:-13px;"),
                      column(width = 12, colourpicker::colourInput("color_mid_corheatmap", label = NULL, "#FEFEC5"),style = "margin-bottom:-13px;"),
                      column(width = 12, colourpicker::colourInput("color_down_corheatmap", label = NULL, "#4878B6")),
                      column(width = 6, numericInput("corheatmapangel", label = "Angel", value = -55, width = "100%")),
                      column(width = 6, numericInput("corheatmapmargin", label = "Margin", value = 0.5, width = "100%")),
                      column(width = 6, checkboxInput("corheatcor", label = "Display PCC", value = TRUE)),
                      column(width = 6, numericInput("corheatpcccex", label = "PCC size", value = 4, width = "100%")),
                      column(width = 5, numericInput("corheatmapwidth", label = "Width", value = 10, width = "90%")),
                      column(width = 5, numericInput("corheatmapheight", label = "Height", value = 9, width = "90%"), style = "margin-left:-35px"),
                      column(width = 6, selectInput(inputId = "corheatmapformat", label = "Format", choices = c("PNG" = "png", "PDF" = "pdf"),width = "80%"),style = "position: absolute;top: 286px;right: -3px;"),
                      column(width = 12, align = "center", downloadButton("downloadcorheatmap", label = "Download Cor heatmap",style = "width: 188px;border-color: #ccc;font-size:10px;text-align: center;margin-top: 10px;")))
        ),
        HTML('<hr style="width: 90%;border-top: 2px solid #000;" />'),
        column(width = 12, align = "left", style = "width:80%;margin-left:10%;margin-bottom: 70px;",
               column(width = 4, align = "left", h3("Principal component analysis")),
               column(width = 1, align = "left", offset = 0, style = "bottom: -15px;left: -170px;",
                  shinyWidgets::dropdownButton(
                    tags$h4(p(icon("check",lib = "font-awesome"),em("Principal component analysis (PCA) is a commonly used data dimensionality reduction and feature extraction method for transforming high-dimensional data sets into low-dimensional spaces, and the goal of PCA is to find the most important direction (principal component) in the original data."),style="color:#337ab7;text-align:center;font-size:15px;")),
                    tags$h4(p(icon("check",lib = "font-awesome"),em("The higher the cumulative variance of the two principal components, the better."),style="color:#337ab7;text-align:center;font-size:15px;")),
                    tags$h4(p(icon("check",lib = "font-awesome"),em("When the clustering effect is not good, you can try to check the Sscale method, which may be pleasantly surprised."),style="color:#337ab7;text-align:center;font-size:15px;")),
                    circle = TRUE, status = "default",
                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T)
                 ),
               HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'), br(),br(),
               column(width = 5, column(width = 12, plotOutput(outputId = "screeplot", height = "475px",width = "500px")%>% withSpinner())),
               column(width = 5, align = "left", plotOutput(outputId = "pcaplot", width = "100%", height = "475px")%>% withSpinner(), style = "left:-70px;"),# corheatmap
               column(width = 2, align = "left", style = "left:-30px;margin-top:-10px;",
                      column(width = 5, numericInput("pcapx", label = "Axis 1", value = 1, min = "1", max = "5", width = "90%")),
                      column(width = 5, numericInput("pcapy", label = "Axis 2", value = 2, min = "1", max = "5", width = "90%"), style = "margin-left:-34px"),
                      column(width = 5, numericInput("pcapaxis", label = "Zoom", value = 0,min = "0", width = "100%"),style = "position: absolute;right: 16px;top: 0px;"),
                      column(width = 6, checkboxInput("pcaellipse", label = "Ellipse", value = FALSE)),
                      column(width = 6, checkboxInput("pcaellipselable", label = "Tags", value = FALSE)),
                      column(width = 6, checkboxInput("pcasamplename", label = "Sample", value = TRUE)),
                      column(width = 6, checkboxInput("pcagrid", label = "Grid", value = TRUE)),
                      column(width = 6, checkboxInput("pcaguidelines", label = "Guidelines", value = TRUE), style = "margin-bottom:0px"),
                      column(width = 6, checkboxInput("pcaborders", label = "Borders", value = TRUE), style = "margin-bottom:0px"),
                      column(width = 12, align = "left", checkboxInput("pcascale", label = "Scale", value = FALSE), style = "margin-bottom:60px"),
                      column(width = 5, numericInput("pcawidth", label = "Width", value = 8, width = "90%")),
                      column(width = 5, numericInput("pcaheight", label = "Height", value = 6, width = "90%"), style = "margin-left:-34px"),
                      column(width = 5, selectInput(inputId = "pcaformat", label = "Format", choices = c("PNG" = "png", "PDF" = "pdf"),width = "100%"),style = "position: absolute;top: 317px;right: 16px;"),
                      column(width = 12, align = "center", downloadButton("downloadscreeplot", label = "Download Scree plot",style = "margin-bottom: 10px;width: 188px;border-color: #ccc;font-size:10px;text-align: center;")),
                      column(width = 12, align = "center", downloadButton("downloadpca", label = "Download PCA plot",style = "width: 188px;border-color: #ccc;font-size:10px;text-align: center;"))
                      )
                      
          ),
        br(),
        HTML('<hr style="width: 90%;border-top: 2px solid #000;" />'),
        column(width = 12, align = "center",
          actionButton(inputId = "reste_datacheck_2", label = "BACK", icon = icon("angle-left"),
                       style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;margin-bottom: 10px")
        )#close cloumn
      )# close fluidRow
    })# close output$ui_datacheck
  }
})

output$download_deg_matrix_template2 <- downloadHandler(#下载deg matrix 模板
    filename = function() {"gene_count_matrix_template.csv"},
    content = function(file) {
      data <- read.csv("template/gene_count_matrix_template.csv")
      write.csv(data, file, row.names = F, fileEncoding="GBK", quote = FALSE)
    }
)
output$download_deg_group_template2 <- downloadHandler(#下载deg matrix 模板
    filename = function() {"groupinfo_template.csv"},
    content = function(file) {
      data <- read.csv("template/groupinfo_template.csv")
      colnames(data) <- c("sample", "group")
      write.csv(data, file, row.names = F, fileEncoding="GBK", quote = FALSE)
    }
)

#file upload stat
values_datacheck <- reactiveValues(upload_datacheck_matrix = "NULL", upload_datacheck_groupinfo = "NULL")
observeEvent(input$datacheck_upload_matrix, {values_datacheck$upload_datacheck_matrix <- 'uploaded'})
observeEvent(input$datacheck_upload_groupinfo, {values_datacheck$upload_datacheck_groupinfo <- 'uploaded'})

# ENTER
observeEvent(input$enter_datacheck,{
  if(values_datacheck$upload_datacheck_matrix == "uploaded" & values_datacheck$upload_datacheck_groupinfo == "uploaded"){
    datacheck_process$value <- "processing"
  } else {
     shinyjs::runjs('alert("ERROR: Please upload matrix and group info.")')
     datacheck_process$value <- "start"
  }
})

# RESET
observeEvent(input$reset_datacheck,{
  shinyjs::reset("datacheck_upload_matrix")
  shinyjs::reset("datacheck_upload_groupinfo")
  values_datacheck$upload_datacheck_matrix <- 'NULL'
  values_datacheck$upload_datacheck_groupinfo <- 'NULL'
})
observeEvent(input$reste_datacheck_2,{
  datacheck_process$value <- "start"
  values_datacheck$upload_datacheck_matrix <- 'NULL'
  values_datacheck$upload_datacheck_groupinfo <- 'NULL'
})

# read matrix
countdata <- reactive({#读取count矩阵，返回datafarme，header=T， rowname=F
  countdata <- input$datacheck_upload_matrix
  if (is.null(countdata)) {
    return(NULL)
  }else {
    shinyjs::reset("datacheck_upload_groupinfo")
    fileformat <- strsplit(countdata$datapath, "\\.")[[1]][-1]
    if (fileformat == "csv") {
      countdata <- read.csv(countdata$datapath, header = TRUE)
    }else if (fileformat == "xlsx") {
        countdata <- openxlsx::read.xlsx(countdata$datapath, colNames = TRUE, rowNames = F)
    }else {
          shinyjs::reset("datacheck_upload_matrix")
          shinyjs::reset("datacheck_upload_groupinfo")
          shinyjs::runjs('alert("ERROR: Format of matrix file must be <.csv> or <.xlsx>.")')
          return(NULL)
    }
  }
  # sample name formay detect
  if (!check_column_names(input$datacheck_upload_matrix$datapath)) {
    shinyjs::reset("datacheck_upload_matrix")
    shinyjs::reset("datacheck_upload_groupinfo")
    shinyjs::runjs('alert("ERROR: The sample name can only be composed of <a~z A~Z 0~9 _ .>, and only the beginning of the letter is supported. Please check that your sample name is named correctly")')
    datacheck_process$value <- "start"
    return(NULL)
  }
  # 
  data <- unlist(matrix(countdata[, -1]))
  if (typeof(data) == "character") {
        shinyjs::reset("datacheck_upload_matrix")
        shinyjs::reset("datacheck_upload_groupinfo")
        shinyjs::runjs('alert("ERROR: The matrix must be numeric.")')
        datacheck_process$value <- "start"
        return(NULL)
  }
  # if (all(data == floor(data)) == FALSE) {#判断整数
  #       sendSweetAlert(
  #       session = session,
  #       title = "ERROR",
  #       text = "基因Count值必须为整数",
  #       type = "error")
  #       shinyjs::reset("uploadcountdata")
  #       shinyjs::reset("uploadgroupinfo")
  #       return(NULL)
  # }
  countdata
})

# CPM
cpm <- reactive({#将conutdata标准化为CPM，返回datafarme， header=T， rowname=1
  countdata <- countdata()
  if (is.null(countdata)) {
    return(NULL)
  }else {
    rownames(countdata) <- countdata[, 1]
    countdata <- countdata[, -1]
    cpm <- t(t(countdata) / colSums(countdata) * 1000000)
    cpm <- data.frame(cpm)

  }
  cpm
})

# read group_info
groupinfotemplate <- reactive({#deggroupinfo模板(生物学重复分组), 返回datafarme header=T，rowname=1
  if (is.null(countdata())) {
    return(NULL)
  }else {
      readscountdata <- countdata()
      readscountdata <- readscountdata[, -1]
      group <- colnames(readscountdata)
      group <- data.frame(group)
      rownames(group) <- group$group
      group
  }
})

# download group_info
output$datacheck_download_groupinfo <- downloadHandler(#下载groupinfo模板
  filename = function() {"groupinfo.csv"},
  content = function(file) {
    if(is.null(groupinfotemplate())) {return(NULL)}else {
        write.csv(groupinfotemplate(), file, row.names = TRUE, fileEncoding="GBK", quote = FALSE)
    }
  }
)

# completion group info
groupinfo <- reactive({#pcadeggroupinfo,返回datafarme,headerT, rownames=F
  groupinfo <- input$datacheck_upload_groupinfo
  countdata <- countdata()

  if (is.null(groupinfo)) {
    return(NULL)
  } else {
      if (is.null(countdata)) {
        shinyjs::runjs('alert("ERROR: Please upload matrix before upload group info.")')
        shinyjs::reset("datacheck_upload_groupinfo")
        return(NULL)
      }
  }

  fileformat <- strsplit(groupinfo$datapath, "\\.")[[1]][-1]
  if (fileformat == "csv") {
    groupinfo <- read.csv(groupinfo$datapath, header = TRUE)
  }else if (fileformat == "xlsx") {
    groupinfo <- openxlsx::read.xlsx(groupinfo$datapath, colNames = TRUE, rowNames = F)
  }else {
        shinyjs::runjs('alert("ERROR: Format of group info file must be <.csv> or <.xlsx>.")')
        shinyjs::reset("datacheck_upload_groupinfo")
        return(NULL)
  }
  allsample <- groupinfo[, 1]
  cpmsamplename <- colnames(countdata())
  for (i in allsample) {
    if (i %in% cpmsamplename == FALSE) {
      shinyjs::runjs('alert("ERROR: The sample name in group info does not correspond to the sample name in matrix.")')
      shinyjs::reset("datacheck_upload_groupinfo")
      return(NULL)
    }
  }
  colnames(groupinfo) <- c("sample", "group")
  groupinfo
})


# corheatmap
output$corheatmap <- renderPlot(return(plotcorheat()))

#PCA
output$screeplot <- renderPlot(screeplot())
output$pcaproportiontable <- renderTable(rownames = TRUE, pcaproportiontable(), width = "90%")
output$pcaplot <- renderPlot(pcaplot())

# plottop500
output$plottop500 <- renderPlot({plottop500()})

# Hierarchical clustering
output$datacheck_hcluster <- renderPlot(datacheck_hcluster())