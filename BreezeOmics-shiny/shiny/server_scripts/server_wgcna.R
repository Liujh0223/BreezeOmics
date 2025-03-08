wgcna_process <- reactiveValues(value = "start", step2 ="stop", step3 ="stop", step4 ="stop", step5 ="stop")
# library(WGCNA)

# render UI
observeEvent(wgcna_process$value, {
    if (wgcna_process$value == "start") {
        output$ui_wgcna <- renderUI({
            fluidRow(style = "",
                    column(width = 12, align = "center",
                    HTML("<p style='font-size:20px;color:black' class='title'>WARNNING:  <br/>"),
                    HTML("<p style='font-size:20px;color:black' class='title'>WGCNA is an advanced analytics methods of RNAseq, we only provides an implementation method to make this happen, the content of the analysis is not explained <br/>"),
                    HTML("<p style='font-size:20px;color:black' class='title'>So users must have a full understanding of WGCNA and a complete experimental design before using this function. <br/>"),
                    ),
                    br(), br(), br(), br(), br(),
                    column(width = 6, align = "center", style = "left: 450px;margin-top: 30px;",
                        fluidRow(
                            column(width = 8, align="left", offset = 4,
                                HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step1: Upload you Matrix</div>")),
                            column(width = 1, align = "right", offset = 3,
                                shinyWidgets::dropdownButton(
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("Upload gene expression matrix data, the file format is only supported by .csv .xlsx"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    column(width = 12, align = "center", downloadButton("download_deg_matrix_template_wgcna", label = "Template matrix", style = "border-color: #ccc;font-size:12px;text-align: center;")),
                                    circle = TRUE, status = "default",
                                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )),
                            column(width = 8, align="left", offset = 0,
                                fileInput(inputId = "wgcna_upload_matrix", label = NULL, accept = list(".csv", ".xlsx"), width = "340px"))
                        ),# close fluidRow
                        br(),
                        fluidRow(
                            column(width = 8, align="left", offset = 4,
                                    HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step2: Download you Group info</div>")),
                            column(width = 1, align = "right", offset = 3,
                                shinyWidgets::dropdownButton(
                                tags$h4(p(icon("check",lib = "font-awesome"),em("Download your group information table, modify the group column (the sample column does not need to be modified), and upload it in Step 3."),style="color:#337ab7;text-align:center;font-size:15px;")),
                                circle = TRUE, status = "default",
                                icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )
                            ),
                            column(width = 8, align="left", offset = 0,
                                    downloadButton(outputId = "wgcna_download_groupinfo", label = "Download", icon = icon("download"), style = "width: 340px;margin-bottom: -10px;"))
                        ),# close fluidRow
                        br(),br(),br(),
                        fluidRow(
                            column(width = 8, align="left", offset = 4,
                                    HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step3: Upload you Group info</div>")),
                            column(width = 1, align = "right", offset = 3,
                                shinyWidgets::dropdownButton(
                                tags$h4(p(icon("check",lib = "font-awesome"),em("Upload your sample group file (download the step2 file and modify the group column, set the same group name for the same group of samples)。"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                column(width = 12, align = "center", downloadButton("download_deg_group_template2", label = "Template groupinfo", style = "border-color: #ccc;font-size:12px;text-align: center;")),
                                circle = TRUE, status = "default",
                                icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )
                            ),
                            column(width = 8, align="left", offset = 0,
                                    fileInput(inputId = "wgcna_upload_groupinfo", label = NULL, accept = ".csv", width = "340px"))
                        ),# close fluidRow
                    ), #close cloumn
                    br(), 
                    column(width = 4, offset = 4,
                            column(width = 6, align="left", offset = 0, style="margin-bottom: 80px;margin-top: 40px;",
                                    actionButton(inputId = "reset_wgcna", label = "RESET", icon = icon("rotate-left"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;")),
                            column(width = 6, align="right", offset = 0, style="margin-bottom: 80px;margin-top: 40px;",
                                    actionButton(inputId = "enter_wgcna", label = "ENTER", icon = icon("check"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;"))
                    )
            )# close fluidRow
        })
    } else if (wgcna_process$value == "running") {
       output$ui_wgcna <- renderUI({
            fluidRow(
                br(),
                column(width = 12, h3("First step : Data preprocessing")),
                HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
                column(width = 12,
                        column(width = 7, 
                        column(width = 12, align = "left", style="background-color: #0000001f;height: 50px;font-weight: 900;", 
                                        HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right"),"Before", "</p>"))),
                        column(width = 12, DT::dataTableOutput(outputId = "wgcna_matrix_before", width = "100%"), style="background-color: #AEEEEE;margin-bottom: 20px;"),

                        column(width = 2, checkboxInput("wgcna_cpm", "CPM Normalization", value = TRUE), style="margin-bottom: 0px;font-size: 15px;margin-top: 15px;"),
                        column(width = 2, numericInput("wgcna_filterlow_cutoff", label = "Cutoff low expression", value = 2, width = "100%"), style="margin-bottom: 0px;border-left: 3px solid #ddd"),
                        column(width = 2, numericInput("wgcna_filterlow_sample_cutoff", label = "Low expression sample", value = 3, width = "100%"), style="margin-bottom: ;border-right: 3px solid #ddd;"),
                        column(width = 2, selectInput("wgcna_select_filter_mode", "Select a filtering method", choices = c("mad", "var", "mean"), width = "100%"), style="margin-bottom: 0px"),
                        column(width = 3, numericInput("wgcna_select_filter_value", label = "A few percent smaller than the method will be filtered out", value = 98, min = 1, max = 100, step = 5, width = "100%"), style="margin-bottom: 0px;border-right: 3px solid #ddd"),
                        column(width = 1, numericInput("wgcna_filterlow_setseed_color", label = "Random colors", value = 1, width = "100%"), style="margin-bottom: 0px"),

                        column(width = 12, align = "left", style="background-color: #0000001f;height: 50px;font-weight: 900;", 
                                        HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right"),"After", "</p>"))),
                        column(width = 12, DT::dataTableOutput(outputId = "wgcna_matrix_after", width = "100%"), style="background-color: #AEEEEE;margin-bottom: 20px;")
                    ),
                    column(width = 5, imageOutput(outputId = "sample_dendrogram", height = "600px",width = "100%") %>% withSpinner()),
                    
                ),

                uiOutput("wgcna_step2")
            )
       })
    }
})

#upload stat
wgcna_upload <- reactiveValues(file = NULL, group = NULL)
observeEvent(input$wgcna_upload_matrix, {wgcna_upload$file = "uploaded"})
observeEvent(input$wgcna_upload_groupinfo, {wgcna_upload$group = "uploaded"})

# enter_wgcna
observeEvent(input$enter_wgcna, {
    req(wgcna_upload$file, wgcna_upload$group)
    if(wgcna_upload$file == "uploaded" & wgcna_upload$group == "uploaded"){

        # sample name detect
        if (!check_column_names(input$wgcna_upload_matrix$datapath)) {
            shinyjs::runjs('alert("ERROR: The sample name can only be composed of <a~z A~Z 0~9 _ .>, and only the beginning of the letter is supported. Please check that your sample name is named correctly")')
            shinyjs::reset("wgcna_upload_matrix")
            shinyjs::reset("wgcna_upload_groupinfo")
            wgcna_upload$file = NULL
            wgcna_upload$group = NULL
            return(NULL)
        }

        # check format
        fileformat <- strsplit(input$wgcna_upload_matrix$datapath, "\\.")[[1]][-1]
        if (fileformat == "csv") {
            file <- read.csv(input$wgcna_upload_matrix$datapath, header = T, row.names = 1)
        }else if (fileformat == "xlsx") {
            file <- openxlsx::read.xlsx(input$wgcna_upload_matrix$datapath, colNames = TRUE, rowNames = TRUE)
        }



        group <- read.csv(input$wgcna_upload_groupinfo$datapath, header = T, row.names = 1)
        #限制样本数需要大于10
        if (nrow(group) < 10) {
            shinyjs::runjs('alert("Failed: Sample sizes below 10 are not recommended for WGCNA.")')
            shinyjs::reset("wgcna_upload_matrix")
            shinyjs::reset("wgcna_upload_groupinfo")
            wgcna_upload$file = NULL
            wgcna_upload$group = NULL
            return(NULL)
        }
        
        #判断groupname样品是否在data里
        for (i in rownames(group)) {
            if (i %in% colnames(file) == FALSE) {
                shinyjs::runjs('alert("ERROR: The sample name in group info does not correspond to the sample name in matrix.")')
                shinyjs::reset("wgcna_upload_matrix")
                shinyjs::reset("wgcna_upload_groupinfo")
                wgcna_upload$file = NULL
                wgcna_upload$group = NULL
                return(NULL)
            }
        }


        wgcna_process$value = "running"
    }else {
        shinyjs::runjs('alert("ERROR: Please upload matrix and group info.")')
        shinyjs::reset("wgcna_upload_matrix")
        shinyjs::reset("wgcna_upload_groupinfo")
        wgcna_upload$file = NULL
        wgcna_upload$group = NULL
    }
})

# reset_wgcna
observeEvent(input$reset_wgcna, {
    shinyjs::reset("wgcna_upload_matrix")
    shinyjs::reset("wgcna_upload_groupinfo")
    wgcna_upload$file = NULL
    wgcna_upload$group = NULL
})

# download group_info
output$wgcna_download_groupinfo <- downloadHandler(#下载groupinfo模板
  filename = function() {"groupinfo.csv"},
  content = function(file) {
    if(wgcna_upload$file == "uploaded") {
        
        fileformat <- strsplit(input$wgcna_upload_matrix$datapath, "\\.")[[1]][-1]
        if (fileformat == "csv") {
            group <- read.csv(input$wgcna_upload_matrix$datapath, header = T, row.names = 1)
        }else if (fileformat == "xlsx") {
            group <- openxlsx::read.xlsx(input$wgcna_upload_matrix$datapath, colNames = TRUE, rowNames = TRUE)
        }

        group <- colnames(group)

        group <- data.frame(group)
        rownames(group) <- group$group
        write.csv(group, file, row.names = TRUE, fileEncoding="GBK", quote = FALSE)
    }else {
        shinyjs::runjs('alert("ERROR: Please upload matrix and group info.")')
        return(NULL)
    }
  }
)

# wgcna_matrix
wgcna_matrix_before <- reactive({
    req(input$wgcna_upload_matrix, input$wgcna_upload_groupinfo)
    if(wgcna_process$value == "running"){

        fileformat <- strsplit(input$wgcna_upload_matrix$datapath, "\\.")[[1]][-1]
        if (fileformat == "csv") {
            wgcna_matrix_before <- read.csv(input$wgcna_upload_matrix$datapath, header = T, row.names = 1)
        }else if (fileformat == "xlsx") {
            wgcna_matrix_before <- openxlsx::read.xlsx(input$wgcna_upload_matrix$datapath, colNames = TRUE, rowNames = TRUE)
        }

        return(wgcna_matrix_before)
    }else {
       return(NULL)
    }
})
output$wgcna_matrix_before <- DT::renderDataTable(wgcna_matrix_before(), rownames = T, options = list(dom = 'fltipr', pageLength = 2, scrollX=TRUE, orderClasses = T,
                                                                        columnDefs = list(
                                                                                        list(className = 'dt-center', 
                                                                                                targets = '_all')
                                                                                        )
                                                                        ), selection = "none")
#

wgcna_matrix_after <- reactive({
    req(input$wgcna_upload_matrix, input$wgcna_upload_groupinfo)
    if(wgcna_process$value == "running"){
        wgcna_matrix_before <- wgcna_matrix_before()
        # wgcna_matrix_before <- wgcna_matrix_before[!apply(wgcna_matrix_before, 1, function(x){sum(floor(x)==0)>0}),]
        if(input$wgcna_cpm == T){
            group_list <- wgcna_group()[,1]
            wgcna_matrix_before <- edgeR::DGEList(wgcna_matrix_before, group = group_list)
            wgcna_matrix_before <- edgeR::cpm(wgcna_matrix_before, group = group_list)
            wgcna_matrix_before <- data.frame(wgcna_matrix_before)
        }
        wgcna_matrix_before <- wgcna_matrix_before[apply(wgcna_matrix_before, 1, function(x){sum(x>input$wgcna_filterlow_cutoff)>=input$wgcna_filterlow_sample_cutoff}),] 

        wgcna_matrix_before$mad <- apply(wgcna_matrix_before, 1, input$wgcna_select_filter_mode)
        wgcna_matrix_before <- wgcna_matrix_before[order(wgcna_matrix_before$mad, decreasing = T),]

        wgcna_matrix_after <- wgcna_matrix_before[wgcna_matrix_before$mad >= quantile(wgcna_matrix_before$mad, (input$wgcna_select_filter_value/100)),][1:(ncol(wgcna_matrix_before)-1)]
        return(wgcna_matrix_after)
    }else {
       return(NULL)
    }
})
output$wgcna_matrix_after <- DT::renderDataTable(wgcna_matrix_after(), rownames = T, options = list(dom = 'fltipr', pageLength = 2, scrollX=TRUE, orderClasses = T,
                                                                        columnDefs = list(
                                                                                        list(className = 'dt-center', 
                                                                                                targets = '_all')
                                                                                        )
                                                                        ), selection = "none")
#

# wgcna_group
wgcna_group <- reactive({
    req(input$wgcna_upload_matrix, input$wgcna_upload_groupinfo)
    if(wgcna_process$value == "running"){
        wgcna_group <- read.csv(input$wgcna_upload_groupinfo$datapath, header = T, row.names = 1)
        return(wgcna_group)
    }else {
       return(NULL)
    }
})

# 新建wgcna的tmp文件夹
wgcna_tmp_filepath <- reactive({
    req(input$enter_wgcna)
    if(wgcna_process$value == "running"){
        prjpath <- prjpath("wgcna")
        return(prjpath)
    }else {
       return(NULL)
    }
})

## ==================step 1
#sample dendrogram
sample_dendrogram <- reactive({
    req(wgcna_matrix_after(), wgcna_group(), wgcna_tmp_filepath())
    data_filter <- wgcna_matrix_after()
    group_list <- wgcna_group()[,1]

    data_Expr <- log2(t(data_filter)+1)
    datTraits <- group_list
    datExpr_tree<-hclust(dist(data_Expr), method = "average")

    set.seed(input$wgcna_filterlow_setseed_color)
    random_colors <- sample(colors(), length(unique(group_list)))
    sample_colors1 <- WGCNA::numbers2colors(as.numeric(factor(datTraits)), 
                                 colors = random_colors, signed = FALSE)
    col=as.matrix(data.frame(group_list=sample_colors1))
    par(mar = c(1,4,3,1),cex=0.8)
    png(paste0(wgcna_tmp_filepath(),"/step1-sample-subtype-cluster.png"),width = 700,height = 600)
    WGCNA::plotDendroAndColors(datExpr_tree, col,
                    groupLabels = colnames(sample),
                    cex.dendroLabels = 0.8,
                    marAll = c(2, 4, 6, 1),
                    cex.rowText = 0.01,
                    main = "Sample dendrogram and trait heatmap")
    dev.off()
    pdf(paste0(wgcna_tmp_filepath(),"/step1-sample-subtype-cluster.pdf"),width = 7,height = 6)
    WGCNA::plotDendroAndColors(datExpr_tree, col,
                    groupLabels = colnames(sample),
                    cex.dendroLabels = 0.8,
                    marAll = c(2, 4, 6, 1),
                    cex.rowText = 0.01,
                    main = "Sample dendrogram and trait heatmap")
    dev.off()
    return(paste0(wgcna_tmp_filepath(),"step1-sample-subtype-cluster.png"))
})
output$sample_dendrogram <- renderImage({
    if(!is.null(sample_dendrogram())){
        list(src = sample_dendrogram(), contentType = 'image/png', style = "max-width:100%; max-height:100%;", deleteFile = FALSE)
    }
})


## ==================step 2
observeEvent(input$wgcna_step1_ok, {wgcna_process$step2 = "start"})
output$wgcna_step2 <- renderUI({
    if(wgcna_process$step2 == "start"){
        column(width = 12,
            br(),
            column(width = 12, h3("Second step : Power estimate")),
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
            column(width = 4, offset = 1, align = "center", tableOutput(outputId = "wgcna_sft_fitIndices"), style = "font-size: 11px;"),
            column(width = 6, offset = 0, align = "center", imageOutput(outputId = "wgcna_powerEstimate", height = "600px",width = "100%") %>% withSpinner(), style = "margin-top: -20px;"),
            column(width = 6, offset = 3, align = "center", selectInput("wgcna_power_input", "Power(Soft thresholds) selection", choices = wgcna_powerEstimate()$sft, width = "100%")),
            uiOutput("wgcna_warnning_power_ui"),
            uiOutput("wgcna_step3")
        )
    }else {
       column(width = 12, 
            column(width = 12, actionButton("wgcna_step1_ok", "!!!! Click to confirm that the data cleansing is complete and proceed to the second step !!!!", width = "100%", style = "background-color: pink;"), style="margin-bottom: 50px;")
       )
    }
})

output$wgcna_warnning_power_ui <- renderUI({
    if(length(wgcna_powerEstimate()$sft) == 30){
        column(width = 12, align = "center", style = "margin-bottom: 30px;",
            HTML("<p style='font-size:20px;color:red' class='title'>WARNNING: No suitable soft threshold to choose from, and it is recommended to adjust the data.<br/>"),
            HTML("<p style='font-size:20px;color:red' class='title'>You can use empirical soft thresholds: Samples<20 Power = 9; Samples<30 Power = 8; Samples<40 Power = 7;"))
    }else {
       column(width = 12, align = "center", style = "margin-bottom: 30px;",
            HTML("<p style='font-size:20px;color:black' class='title'>Recommendation of Power: SFT.R.sq ≥ 0.8; slope ≤ -0.5; mean.k ≤ 150<br/>"))
    }
})

wgcna_powerEstimate <- reactive({
    req(input$wgcna_step1_ok)
    WGCNA::allowWGCNAThreads()
    withProgress(message = 'Running step 2 : Power estimate',
      detail = 'This may take a few minutes...', value = 1, {
        for (i in 1: 1) {

            data_filter <- wgcna_matrix_after()
            group_list <- wgcna_group()[,1]
            data_Expr <- log2(t(data_filter)+1)
            datTraits <- group_list

            powers = c(c(1:10), seq(from = 12, to=30, by=2))
            sft = WGCNA::pickSoftThreshold(data_Expr, powerVector = powers, verbose = 5)

            png(paste0(wgcna_tmp_filepath(), "step2-beta-value.png"),width = 800,height = 600)
                par(mfrow = c(1,2));
                cex1 = 0.7;
                plot(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
                xlab=("Soft Threshold (power)"),ylab="Scale Free Topology Model Fit,signed R^2",type="n",
                main = paste("Scale independence"));
                text(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
                labels=powers,cex=cex1,col="red");
                # abline(h=0.85,col="red")
                plot(sft$fitIndices[,1], sft$fitIndices[,5],
                    xlab="Soft Threshold (power)",ylab="Mean Connectivity", type="n",
                    main = paste("Mean connectivity"))
                text(sft$fitIndices[,1], sft$fitIndices[,5], labels=powers, cex=cex1,col="red")
            dev.off()
            pdf(paste0(wgcna_tmp_filepath(), "step2-beta-value.pdf"),width = 8,height = 6)
                par(mfrow = c(1,2));
                cex1 = 0.7;
                plot(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
                xlab=("Soft Threshold (power)"),ylab="Scale Free Topology Model Fit,signed R^2",type="n",
                main = paste("Scale independence"));
                text(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
                labels=powers,cex=cex1,col="red");
                # abline(h=0.85,col="red")
                plot(sft$fitIndices[,1], sft$fitIndices[,5],
                    xlab="Soft Threshold (power)",ylab="Mean Connectivity", type="n",
                    main = paste("Mean connectivity"))
                text(sft$fitIndices[,1], sft$fitIndices[,5], labels=powers, cex=cex1,col="red")
            dev.off()
            my_list <- list()
            a <- data.frame(t(sft$fitIndices))
            for (i in a) {
                if(i[2]>=0.8 & i[3]<0 & i[5]<=150){
                    my_list[[length(my_list) + 1]] <- i[1]
                }
            }

            if(length(my_list) == 0){
                my_list <- c(c(1:30))
            }
        }
      }
    )
    return(list(sft = unlist(my_list), imgfile = paste0(wgcna_tmp_filepath(), "step2-beta-value.png"), fitIndices = sft$fitIndices))
})

output$wgcna_sft_fitIndices <- renderTable(wgcna_powerEstimate()$fitIndices , colnames = T)

output$wgcna_powerEstimate <- renderImage({
    if(!is.null(wgcna_powerEstimate())){
        list(src = wgcna_powerEstimate()$imgfile, contentType = 'image/png', style = "max-width:100%; max-height:100%;", deleteFile = FALSE)
    }
})


##  ==================step 3
observeEvent(input$wgcna_step2_ok, {wgcna_process$step3 = "start"})
output$wgcna_step3 <- renderUI({
    if(wgcna_process$step3 == "start"){
        column(width = 12,
            br(),
            column(width = 12, h3("Third step : Build the co-expression network"), style = "margin-top: 20px;"),
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
            column(width = 3, offset = 0, align = "center", numericInput("wgcna_minModuleSize", label = "minModuleSize", value = 30, min = 1, max = 1000, step = 1, width = "100%")),
            column(width = 3, offset = 0, align = "center", numericInput("wgcna_maxBlockSize", label = "maxBlockSize", value = 10000, min = 5000, max = 30000, step = 1, width = "100%")),
            column(width = 3, offset = 0, align = "center", numericInput("wgcna_mergeCutHeight", label = "mergeCutHeight", value = 0, min = 0, max = 0.5, step = 0.01, width = "100%")),
            column(width = 3, offset = 0, actionButton("wgcna_step3_network", "Click to start building the co-expression network", width = "100%", style = "height: 60px ;font-size: 18px;background-color: lightgreen;")),
            uiOutput("wgcna_step3_network")
        )
    }else {
       column(width = 12, 
            column(width = 12, actionButton("wgcna_step2_ok", "!!!! Click to confirm the power and proceed to the third step !!!!", width = "100%", style = "background-color: pink;"), style="margin-bottom: 50px;")
        )
    }
})

wgcna_step3_network <- reactiveValues(process = "stop")
observeEvent(input$wgcna_step3_network, {wgcna_step3_network$process = "start"})
output$wgcna_step3_network <- renderUI({
    if(wgcna_step3_network$process == "stop"){
        fluidRow(
            column(width = 12, align = "center", HTML("<p style='font-size:30px;color:black' class='title'>Note: <br/> Building a co-expression network will consume a lot of computing resources for computing, please do not operate during the calculation, please wait patiently and press the green button to start building. <br/>"),)
        )
    }else {
       fluidRow(style = "margin-bottom: 50px;margin-top: 30px;",
            column(width = 4, offset = 0, align = "center", imageOutput(outputId = "wgcna_module_dendrogram", height = "500px",width = "100%") %>% withSpinner(), style = ""),
            column(width = 4, offset = 0, align = "center", imageOutput(outputId = "wgcna_mes_col", height = "500px",width = "100%") %>% withSpinner(), style = ""),
            column(width = 4, offset = 0, align = "center", imageOutput(outputId = "wgcna_TOMplot", height = "500px",width = "100%") %>% withSpinner(), style = ""),
            br(),
            uiOutput("wgcna_step4")
       )
    }
})

# bulid co-network
wgcna_blockwiseModules <- reactive({

    WGCNA::allowWGCNAThreads()
    cor <- WGCNA::cor
    withProgress(message = 'Running step 3 : Build the co-expression network',
      detail = 'This will take a lot of time, be patient !', value = 1, {
        for (i in 1: 1) {
            
            WGCNA::allowWGCNAThreads()
            data_filter <- wgcna_matrix_after()
            group_list <- wgcna_group()[,1]
            data_Expr <- log2(t(data_filter)+1)
            datTraits <- group_list
            print("checkpoint0")
            net = WGCNA::blockwiseModules(
                data_Expr,
                power = as.integer(input$wgcna_power_input),
                TOMType = "unsigned",
                minModuleSize = input$wgcna_minModuleSize,
                reassignThreshold = 0,
                maxBlockSize = input$wgcna_maxBlockSize,
                mergeCutHeight = input$wgcna_mergeCutHeight,#需要合并模块的阈值
                numericLabels = TRUE, #以数字作为模块的名字
                pamRespectsDendro = FALSE,
                saveTOMs = T,
                saveTOMFileBase = paste0(wgcna_tmp_filepath(), "WGCNA_TOM"),
                verbose = 3
            )
            # print(table(net$colors))
            print("checkpoint1")
            #模块可视化
            moduleColors <- WGCNA::labels2colors(net$colors)
            png(paste0(wgcna_tmp_filepath(), "step3-Cluster Dendrogram.png"),width = 500,height = 500)
                WGCNA::plotDendroAndColors(
                    net$dendrograms[[1]],
                    moduleColors[net$blockGenes[[1]]],
                    "Module colors",
                    dendroLabels = FALSE,
                    hang = 0.03,
                    addGuide = TRUE,
                    guideHang = 0.05
                )
            dev.off()
            print("checkpoint2")
            pdf(paste0(wgcna_tmp_filepath(), "step3-Cluster Dendrogram.pdf"), width = 10, height = 10)
                WGCNA::plotDendroAndColors(
                    net$dendrograms[[1]],
                    moduleColors[net$blockGenes[[1]]],
                    "Module colors",
                    dendroLabels = FALSE,
                    hang = 0.03,
                    addGuide = TRUE,
                    guideHang = 0.05
                )
            dev.off()
            #TOM可视化
            print("checkpoint3")
            load(net$TOMFiles[1], verbose = TRUE)
            TOM <- as.matrix(TOM)
            dissTOM <- 1-TOM
            plotTOM <- dissTOM^7
            diag(plotTOM) <- NA
            print("checkpoint4")
            mycolor <- gplots::colorpanel(250, 'red', "orange", 'lemonchiffon')
            png(paste0(wgcna_tmp_filepath(), "step3-Network heatmap plot all genes.png"),width = 500,height = 500)
                WGCNA::TOMplot(
                    plotTOM,
                    # 层次聚类的第一个
                    net$dendrograms[[1]],
                    # 取对应块基因的颜色
                    moduleColors[net$blockGenes[[1]]],
                    main = "Network heatmap plot_all genes",
                    col = mycolor
                )
            dev.off()
            print("checkpoint5")
            #各模块间相关性
            MEs_col <- net$MEs
            if(ncol(MEs_col)==1){
                shinyjs::runjs('alert("Failed: Only one module is detected in WGCNA.")')
                shinyjs::runjs('alert("Possible Causes: 1.Low gene expression variance; 2.Inappropriate soft-threshold power selection; 3.MergeCutHeight is too low; 4.Insufficient sample size")')
                wgcna_step3_network$process = "stop"
                return(NULL)
            }
            print(MEs_col)
            print("checkpoint5.1")
            colnames(MEs_col) <- paste0("ME", WGCNA::labels2colors(as.numeric(stringr::str_replace_all(colnames(MEs_col),"ME",""))))
            print("checkpoint5.2")
            MEs_col <- WGCNA::orderMEs(MEs_col)
            print(MEs_col)
            print("checkpoint5.3")
            png(paste0(wgcna_tmp_filepath(), "step3-Eigengene adjacency heatmap.png"),width = 500,height = 500)
                WGCNA::plotEigengeneNetworks(
                    MEs_col,
                    "Eigengene adjacency heatmap",
                    marDendro = c(0.5, 3, 2, 4),
                    marHeatmap = c(2, 3, 0, 2),
                    plotDendrograms = T,
                    xLabelsAngle = 90)#输出6x6的pdf
            dev.off()
            print("checkpoint6")
            pdf(paste0(wgcna_tmp_filepath(), "step3-Eigengene adjacency heatmap.pdf"),width = 10,height = 10)
                WGCNA::plotEigengeneNetworks(
                    MEs_col,
                    "Eigengene adjacency heatmap",
                    marDendro = c(0.5, 3, 2, 4),
                    marHeatmap = c(2, 3, 0, 2),
                    plotDendrograms = T,
                    xLabelsAngle = 90)#输出6x6的pdf
            dev.off()

            # 写入个模块基因
            print("checkpoint7")
            modulegenes <- data.frame(gene_id = names(net$colors), moduleColors = moduleColors)
            write.table(modulegenes, paste0(wgcna_tmp_filepath(), "modlegenes.txt"), sep = "\t", quote = F, row.names = F)

            # 写入catoscape
            cytoscape_file <- paste0(wgcna_tmp_filepath(), "cytoscape/")
            dir.create(cytoscape_file)
            TOM <- WGCNA::TOMsimilarityFromExpr(data_Expr, power = input$wgcna_power_input)
            module <- unique(moduleColors)
            for(i in module) {
                probes = colnames(data_Expr)
                inModule = (moduleColors==i)
                modProbes = probes[inModule]

                modTOM = TOM[inModule, inModule]
                dimnames(modTOM) = list(modProbes, modProbes)
                cyt <- WGCNA::exportNetworkToCytoscape(
                    modTOM,
                    edgeFile = paste(cytoscape_file, i, ".edges.txt", sep=""),
                    nodeFile = paste(cytoscape_file, i, ".nodes.txt", sep=""),
                    weighted = TRUE,
                    threshold = 0.02,
                    nodeNames = modProbes, 
                    nodeAttr = moduleColors[inModule]
                )
            }
            print("checkpoint8")
            # 计算kME module membership
            datKME <- WGCNA::signedKME(data_Expr, MEs_col, outputColumnName="kME_MM.")
            write.csv(datKME, paste0(wgcna_tmp_filepath(), "kME_MM.csv"))

            ## 计算所有基因的连接度
            ADJ <- abs(cor(data_Expr, use="p"))^6
            Alldegrees <- WGCNA::intramodularConnectivity(ADJ, moduleColors)
            write.csv(Alldegrees, file = paste0(wgcna_tmp_filepath(), "intramodularConnectivity.csv"))
            print("checkpoint9")
            return(list(net = net, 
                dendrogram = paste0(wgcna_tmp_filepath(),"step3-Cluster Dendrogram.png"),
                tomplot = paste0(wgcna_tmp_filepath(), "step3-Network heatmap plot all genes.png"),
                mes_col = paste0(wgcna_tmp_filepath(), "step3-Eigengene adjacency heatmap.png"),
                cytoscape = paste0(wgcna_tmp_filepath(), "cytoscape/"),
                group = unique(moduleColors),
                datKME = paste0(wgcna_tmp_filepath(), "kME_MM.csv"),
                data_Expr = data_Expr,
                datTraits = datTraits,
                moduleColors = moduleColors,
                connectivity = paste0(wgcna_tmp_filepath(), "intramodularConnectivity.csv")
            ))
        }
      }
    )

})
output$wgcna_module_dendrogram <- renderImage({
    if(!is.null(wgcna_blockwiseModules())){
        list(src = wgcna_blockwiseModules()$dendrogram, contentType = 'image/png', style = "max-width:100%; max-height:100%;", deleteFile = FALSE)
    }
})
output$wgcna_TOMplot <- renderImage({
    if(!is.null(wgcna_blockwiseModules())){
        list(src = wgcna_blockwiseModules()$tomplot, contentType = 'image/png', style = "max-width:100%; max-height:100%;", deleteFile = FALSE)
    }
})
output$wgcna_mes_col <- renderImage({
    if(!is.null(wgcna_blockwiseModules())){
        list(src = wgcna_blockwiseModules()$mes_col, contentType = 'image/png', style = "max-width:100%; max-height:100%;", deleteFile = FALSE)
    }
})




##  ==================step 4
observeEvent(input$wgcna_step3_ok, {wgcna_process$step4 = "start"})
output$wgcna_step4 <- renderUI({
    if(wgcna_process$step4 == "start"){
        column(width = 12, style = "margin-top: 50px;",
            br(),
            column(width = 12, h3("Fourth step : Correlation analysis between trait and module"), style = "margin-top: 20px;"),
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
            column(width = 6, align = "center", imageOutput(outputId = "wgcna_trait_module_cor", height = "500px",width = "100%") %>% withSpinner(), style = "border-right: 1px solid #ddd;margin-top: 80px;"),
            uiOutput("wgcna_trait2_ui"),
            uiOutput("wgcna_step5")
        )
    }else {
       column(width = 12, style = "margin-top: 50px;",
            column(width = 12, actionButton("wgcna_step3_ok", "!!!! Click to perform trait and module correlation analysis !!!!", width = "100%", style = "background-color: pink;"), style="margin-bottom: 50px;margin-top: 80px;")
        )
    }
})
trait_module_cor <- reactive({
    req(wgcna_blockwiseModules())
    datTraits <- wgcna_blockwiseModules()$datTraits
    data_Expr <- wgcna_blockwiseModules()$data_Expr
    moduleColors <- wgcna_blockwiseModules()$moduleColors

    group <- factor(datTraits, levels = unique(datTraits))
    datTraits <-  data.frame(samples=rownames(data_Expr),subtype=group)
    design <- model.matrix(~0 + datTraits$subtype)
    colnames(design) <- levels(factor(datTraits$subtype))

    MEs0 <- WGCNA::moduleEigengenes(data_Expr, moduleColors)$eigengene#计算各基因在不同颜色模块中的第一主成分ME。
    MEs = WGCNA::orderMEs(MEs0)
    moduleTraitCor = cor(MEs, design , use = "p")#计算不同模块和样本的相关性。
    nSamples = nrow(data_Expr)
    moduleTraitPvalue = WGCNA::corPvalueStudent(moduleTraitCor, nSamples)#计算不同模块和样本性状相关性的p值。
    textMatrix = paste(signif(moduleTraitCor, 2), "\n(",signif(moduleTraitPvalue, 1), ")", sep = "")
    dim(textMatrix) = dim(moduleTraitCor)
    png(paste0(wgcna_tmp_filepath(), "step4-Correlation between trait and module.png"),width = 500,height = 500)
        par(mar = c(6, 8, 3, 2));#下、左、上、右的边距
        WGCNA::labeledHeatmap(Matrix = moduleTraitCor,
                        xLabels = colnames(design),
                        yLabels = names(MEs),
                        ySymbols = names(MEs),
                        colorLabels = FALSE,
                        colors = WGCNA::blueWhiteRed(50),
                        textMatrix = textMatrix,
                        setStdMargins = FALSE,
                        cex.text = 0.8,
                        zlim = c(-1,1),
                        main = paste("Module-trait relationships"))
    dev.off()
    return(list(
        file = paste0(wgcna_tmp_filepath(), "step4-Correlation between trait and module.png"),
        design = design
    ))
})
output$wgcna_trait_module_cor <- renderImage({
    if(!is.null(trait_module_cor())){
        list(src = trait_module_cor()$file, contentType = 'image/png', style = "max-width:100%; max-height:100%;", deleteFile = FALSE)
    }
})

#trait2
output$wgcna_trait2_ui <- renderUI({
    fluidRow(
        column(width = 6, align="center", offset = 0, fileInput(inputId = "wgcna_trait2", label = "Optional: Upload trait2", accept = c(".csv"), width = "50%")),
        uiOutput("wgcna_trait2_cor_ui")
    )
})
wgcna_trait2 <- reactive({
    req(input$wgcna_trait2, wgcna_blockwiseModules())
    data_Expr <- wgcna_blockwiseModules()$data_Expr
    data_Expr <- rownames(data_Expr)

    trait <- read.csv(input$wgcna_trait2$datapath, row.names = 1)
    trait_name <- colnames(trait)
    if(identical(data_Expr, trait_name)){
        return(trait)
    }else {
       shinyjs::runjs('alert("ERROR: The trait sample name does not match the gene matrix sample name.")')
       return(NULL)
    }
})
trait2_module_cor <- reactive({
    req(wgcna_trait2(), wgcna_blockwiseModules())

    datTraits <- wgcna_trait2()
    datTraits_t <- t(datTraits)
    datTraits_t <- as.data.frame(datTraits_t)
    colnames(datTraits_t) <- rownames(datTraits)
    rownames(datTraits_t) <- colnames(datTraits)
    datTraits <- datTraits_t
    datTraits <- as.data.frame(lapply(datTraits, as.numeric))

    moduleColors <- wgcna_blockwiseModules()$moduleColors
    data_Expr <- wgcna_blockwiseModules()$data_Expr

    MEs0 <- WGCNA::moduleEigengenes(data_Expr, moduleColors)$eigengene#计算各基因在不同颜色模块中的第一主成分ME。
    MEs = WGCNA::orderMEs(MEs0)

    moduleTraitCor = cor(MEs, datTraits , use = "p")#计算不同模块和样本的相关性。

    nSamples = nrow(data_Expr)
    moduleTraitPvalue = WGCNA::corPvalueStudent(moduleTraitCor, nSamples)#计算不同模块和样本性状相关性的p值。
    textMatrix = paste(signif(moduleTraitCor, 2), "\n(",signif(moduleTraitPvalue, 1), ")", sep = "")
    dim(textMatrix) = dim(moduleTraitCor)
    png(paste0(wgcna_tmp_filepath(), "step4-Correlation between trait2 and module.png"),width = 500,height = 500)
        par(mar = c(6, 8, 3, 2));#下、左、上、右的边距
        WGCNA::labeledHeatmap(Matrix = moduleTraitCor,
                        xLabels = colnames(datTraits),
                        yLabels = names(MEs),
                        ySymbols = names(MEs),
                        colorLabels = FALSE,
                        colors = WGCNA::blueWhiteRed(50),
                        textMatrix = textMatrix,
                        setStdMargins = FALSE,
                        cex.text = 0.8,
                        zlim = c(-1,1),
                        main = paste("Module-trait relationships"))
    dev.off()
    pdf(paste0(wgcna_tmp_filepath(), "step4-Correlation between trait2 and module.pdf"), width = 5, height = 5)
        par(mar = c(6, 8, 3, 2));#下、左、上、右的边距
        WGCNA::labeledHeatmap(Matrix = moduleTraitCor,
                        xLabels = colnames(datTraits),
                        yLabels = names(MEs),
                        ySymbols = names(MEs),
                        colorLabels = FALSE,
                        colors = WGCNA::blueWhiteRed(50),
                        textMatrix = textMatrix,
                        setStdMargins = FALSE,
                        cex.text = 0.8,
                        zlim = c(-1,1),
                        main = paste("Module-trait relationships"))
    dev.off()
    return(list(
        file = paste0(wgcna_tmp_filepath(), "step4-Correlation between trait2 and module.png"),
        design = datTraits
    ))

})
output$wgcna_trait2_cor_ui <- renderUI({
    column(width = 6, align = "center", imageOutput(outputId = "wgcna_trait2_module_cor", height = "500px",width = "100%") %>% withSpinner(), style = "border-right: 1px solid #ddd;margin-top: -20px;")
})
output$wgcna_trait2_module_cor <- renderImage({
    if(!is.null(trait_module_cor())){
        list(src = trait2_module_cor()$file, contentType = 'image/png', style = "max-width:100%; max-height:100%;", deleteFile = FALSE)
    }
})




##  ==================step 5
observeEvent(input$wgcna_step4_ok, {wgcna_process$step5 = "start"})
output$wgcna_step5 <- renderUI({
    if(wgcna_process$step5 == "start"){
        column(width = 12, style = "margin-top: 50px;",
            br(),
            column(width = 12, h3("Fourth step : Hub genes screening"), style = "margin-top: 20px;"),
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
            column(width = 3, selectInput("wgcna_select_NetworkToCytoscape_input", label = "Module genes selection", choices = wgcna_blockwiseModules()$group), style = "font-size: 20px;"),
            column(width = 9, tableOutput(outputId = "wgcna_group_stat_table"), align = "left", style = "font-size: 20px;"),
            column(width = 12, uiOutput("wgcna_hubgenes_ui1")),

            column(width = 12, h4("Method 1: Through Intra-module connection weights"), style = "margin-bottom: -10px;margin-top: 50px;"),
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
            column(width = 6, uiOutput("wgcna_hubgenes_ui2")),

            column(width = 6,#network PPI graph
                fluidRow(
                    column(width = 2, numericInput("wgcna_ppi_count", label = "No. of weight top", value = 200)),
                    column(width = 2, selectInput("wgcna_ppi_layout", label = "Layout", choices = c("kk", "stress", "fr", "lgl", "graphopt", "circle"), selected = "circle")),
                    uiOutput("wgcna_ppi_circle_layout"),
                    column(width = 2, numericInput("wgcna_ppi_node_num", label = "Display node", value = 5)),
                    column(width = 4, selectInput("wgcna_ppi_order", label = "Node order", choices = c("Connection count", "Total weight"), selected = "Connection count"))
                ),
                fluidRow(
                    column(width = 4, colourpicker::colourInput("wgcna_ppi_hight_col", label = "High No. of connections", "#DD3E2C")),
                    column(width = 4, colourpicker::colourInput("wgcna_ppi_low_col", label = "Low No. of connections", "blue")),
                    column(width = 4, colourpicker::colourInput("wgcna_ppi_edge_col", label = "Colour of edges", "grey60"))
                )
            ),
            column(width = 6, offset = 0, align = "center", plotOutput(outputId = "wgcna_ppi_net", height = "500px",width = "100%") %>% withSpinner(), style = ""),
            column(width = 6,
                fluidRow(
                    column(width = 3, numericInput("wgcna_ppi_width", label = "Width", value = 13)),
                    column(width = 3, numericInput("wgcna_ppi_height", label = "Height", value = 8)),
                    column(width = 3, selectInput(inputId = "wgcna_ppi_format", label = "Format", choices = c("png", "pdf"),width = "100%")),
                    column(width = 3, downloadButton("download_wgcna_ppi", label = "Download", style = "width: 100%;"), style = "margin-top: 23px;")
                )
            ),
            br(),

            column(width = 12, h4("Method 2: Through Module Membership(|MM| ≥ 0.8)"), style = "margin-bottom: -10px; margin-top: 20px;"),
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
            uiOutput("wgcna_mm_ui"),
            column(width = 6, align = "center", plotOutput(outputId = "wgcna_mm_conn_plot", height = "500px",width = "500px") %>% withSpinner(), style = "margin-top: 130px;"),
            uiOutput("wgcna_step6"),
            br(),

            column(width = 12, h4("Method 3: Through Gene trait Significance(GS) and Module Membership(MM) or Connectivity "), style = "margin-bottom: -10px; margin-top: 20px;"),
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
            uiOutput("wgcna_gs_ui_1"),
            uiOutput("wgcna_gs_ui_2"),

            column(width = 12, style = "margin-top: 80px;",
                downloadButton("wgcna_download_result", "If the WGCNA analysis is complete, click to download all the analysis results.", style = "width:100%;background-color: pink;"))
        
        )
    }else {
       column(width = 12, style = "margin-top: 50px;",
            column(width = 12, actionButton("wgcna_step4_ok", "!!!! Click for fifth step to select hub genes !!!!", width = "100%", style = "background-color: pink;"), style="margin-bottom: 50px;")
        )
    }
})

wgcna_select_modulegene <- reactive({
    file_module <- read.csv(paste0(wgcna_tmp_filepath(), "modlegenes.txt"), sep = "\t")
    return(file_module)
})
filtered_wgcna_select_modulegene <- reactive({
    req(input$wgcna_select_NetworkToCytoscape_input)

    # 读取选择的文件
    data <- wgcna_select_modulegene()
    subset(data, 
            moduleColors == input$wgcna_select_NetworkToCytoscape_input
            )
})
output$wgcna_select_modulegene <- DT::renderDataTable(filtered_wgcna_select_modulegene(), rownames = FALSE, options = list(dom = 'fltipr', pageLength = 5, scrollX=TRUE, orderClasses = T,
                                                                        columnDefs = list(
                                                                                        list(className = 'dt-center', 
                                                                                                targets = '_all')
                                                                                        )
                                                                        ), selection = "none")
#


#module genes
wgcna_net_num <- reactive({
    req(wgcna_blockwiseModules())
    net <- wgcna_blockwiseModules()$net
    group <- WGCNA::labels2colors(net$colors)
    group <- data.frame(table(group))
    group <- t(group)
    return(group)
})
output$wgcna_group_stat_table <- renderTable(rownames = TRUE, wgcna_net_num(), colnames = F)

#module connectivity
wgcna_connectivity <- reactive({
    req(wgcna_blockwiseModules(), filtered_wgcna_select_modulegene())
    data <- wgcna_blockwiseModules()$connectivity
    data <- read.csv(data)
    colnames(data) <- c("gene_id", "kTotal", "kWithin", "kOut", "kDiff")

    select_module <- filtered_wgcna_select_modulegene()
    select_module <- unlist(select_module[,"gene_id"])
    data <- data[data$gene_id %in% select_module, ]
    return(data)
})
output$wgcna_select_connectivity <- DT::renderDataTable(wgcna_connectivity(), rownames = FALSE, options = list(dom = 'fltipr', pageLength = 5, scrollX=TRUE, orderClasses = T,
                                                                        columnDefs = list(
                                                                                        list(className = 'dt-center', 
                                                                                                targets = '_all')
                                                                                        )
                                                                        ), selection = "none")
#

#ui_modulegenes & connectivity
output$wgcna_hubgenes_ui1 <- renderUI({
    fluidRow(
        column(width = 6,
                    column(width = 12, align = "left", style="background-color: #0000001f;height: 50px;font-weight: 900;", 
                                HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right"), paste0(input$wgcna_select_NetworkToCytoscape_input, " Module genes"), "</p>"))),
                    column(width = 12, DT::dataTableOutput(outputId = "wgcna_select_modulegene", width = "100%") %>% withSpinner(), style="background-color: #AEEEEE;margin-bottom: 20px;")
                ),
        column(width = 6,
                    column(width = 12, align = "left", style="background-color: #0000001f;height: 50px;font-weight: 900;", 
                                HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right"), paste0(input$wgcna_select_NetworkToCytoscape_input, " Module connectivity"), "</p>"))),
                    column(width = 12, DT::dataTableOutput(outputId = "wgcna_select_connectivity", width = "100%") %>% withSpinner(), style="background-color: #AEEEEE;margin-bottom: 20px;")
                )
    )
})


#NetworkToCytoscape
wgcna_select_NetworkToCytoscape <- reactive({
    file <- read.csv(paste0(wgcna_tmp_filepath(), "cytoscape/", input$wgcna_select_NetworkToCytoscape_input, ".edges.txt"), sep = "\t")
    file <- file[,1:3]
    return(file)
})
output$wgcna_select_NetworkToCytoscape <- DT::renderDataTable(wgcna_select_NetworkToCytoscape(), rownames = FALSE, options = list(dom = 'fltipr', pageLength = 15, scrollX=TRUE, orderClasses = T,
                                                                        columnDefs = list(
                                                                                        list(className = 'dt-center', 
                                                                                                targets = '_all')
                                                                                        )
                                                                        ), selection = "none")
#
output$wgcna_hubgenes_ui2 <- renderUI({
    fluidRow(
        column(width = 12,
            column(width = 12, align = "left", style="background-color: #0000001f;height: 50px;font-weight: 900;", 
                        HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right"), paste0(input$wgcna_select_NetworkToCytoscape_input, " Module Undirected network"), "</p>"))),
            column(width = 12, DT::dataTableOutput(outputId = "wgcna_select_NetworkToCytoscape", width = "100%"), style="background-color: #AEEEEE;margin-bottom: 20px;")
        )
    )
})


# #ppi network graph
wgcna_ppi_net <- reactive({
    req(wgcna_select_NetworkToCytoscape())

    data <- wgcna_select_NetworkToCytoscape()
    weight_num <- ifelse(input$wgcna_ppi_count > 500, 500, input$wgcna_ppi_count)
    weight_num <- ifelse(nrow(data) < input$wgcna_ppi_count, nrow(data), input$wgcna_ppi_count)
    data <- head(arrange(data, desc(data$weight)), weight_num)
    data$module <- paste0(input$wgcna_select_NetworkToCytoscape_input, " module weight top ", weight_num)
    graph <- igraph::graph_from_data_frame(data, directed = FALSE)
    node_degrees <- igraph::degree(graph, mode = "all")
    degree_info <- data.frame(Node = names(node_degrees), Degree = node_degrees)
    #统计total weight
    edge_list <- data.frame(Node1 = data$fromNode, Node2 = data$toNode, weight = data$weight)
    edge_list_long <- tidyr::pivot_longer(edge_list, cols = c("Node1", "Node2"), names_to = "NodeType", values_to = "Node")
    weight_sums <- edge_list_long %>%
                    group_by(Node) %>%
                    summarize(TotalWeight = sum(weight))
    degree_info <- left_join(degree_info, weight_sums, by = "Node")
    #select top node order
    if(input$wgcna_ppi_order == "Connection count"){
        top_nodes <- degree_info[order(-degree_info$Degree), "Node"][1:input$wgcna_ppi_node_num]
    }else {
       top_nodes <- degree_info[order(-degree_info$TotalWeight), "Node"][1:input$wgcna_ppi_node_num]
    }
    
    degree_info$is_top <- ifelse(degree_info$Node %in% top_nodes, TRUE, FALSE)
    # library(ggraph)
    if(input$wgcna_ppi_layout == "circle"){
        value1 <- ifelse(input$wgcna_ppi_circle_value == NULL, 100, input$wgcna_ppi_circle_value)
        g <- ggraph(graph, layout = 'kk', maxiter = input$wgcna_ppi_circle_value) + 
            geom_edge_link(color = input$wgcna_ppi_edge_col) + 
            geom_node_point(aes(size = degree_info$Degree, color = degree_info$TotalWeight)) +  # 设置节点颜色为连接数
            scale_color_continuous(low = input$wgcna_ppi_low_col, high = input$wgcna_ppi_hight_col, name = "Total Weight") +  # 设置颜色映射范围
            # geom_node_text(aes(label = ifelse(degree_info$is_top, degree_info$Node, ""), size = 12), repel = TRUE) +  # 仅为前五个节点添加标签
            scale_size_continuous(range = c(5, 10), name = "Connection Count") +  # 设置节点大小范围
            theme_void()+
            # facet_edges(~module) +
            theme_graph(foreground = 'black', fg_text_colour = 'white',strip_text_size = 18) +
            # ggtitle(paste0(input$wgcna_select_NetworkToCytoscape_input, " module weight top ", weight_num)) +
            theme(text = element_text(family = "sans"))
    }else {
        g <- ggraph(graph, layout = input$wgcna_ppi_layout) + 
            geom_edge_link(color = input$wgcna_ppi_edge_col) + 
            geom_node_point(aes(size = degree_info$Degree, color = degree_info$TotalWeight)) +  # 设置节点颜色为连接数
            scale_color_continuous(low = input$wgcna_ppi_low_col, high = input$wgcna_ppi_hight_col, name = "Total Weight") +  # 设置颜色映射范围
            # geom_node_text(aes(label = ifelse(degree_info$is_top, degree_info$Node, ""), size = 12), repel = TRUE) +  # 仅为前五个节点添加标签
            scale_size_continuous(range = c(5, 10), name = "Connection Count") +  # 设置节点大小范围
            theme_void()+
            # facet_edges(~module) +
            theme_graph(foreground = 'black', fg_text_colour = 'white',strip_text_size = 18) +
            # ggtitle(paste0(input$wgcna_select_NetworkToCytoscape_input, " module weight top ", weight_num)) +
            theme(text = element_text(family = "sans"))
    }

    if(input$wgcna_ppi_node_num <= 0){
        g <- g
    }else {
       g <- g + geom_node_text(aes(label = ifelse(degree_info$is_top, degree_info$Node, ""), size = 12), repel = TRUE)# 仅为前五个节点添加标签
    }

    ggsave(paste0(wgcna_tmp_filepath(), paste0(input$wgcna_select_NetworkToCytoscape_input, " module weight top ", weight_num), "_network.png"), g, width = 8, height = 8)

    
    return(g)
})
output$wgcna_ppi_net <- renderPlot({wgcna_ppi_net()})
output$wgcna_ppi_circle_layout <- renderUI({
    if(input$wgcna_ppi_layout == "circle"){
        column(width = 2, numericInput("wgcna_ppi_circle_value", label = "Circle layout", value = 1))
    }else {
       return(NULL)
    }
})
output$download_wgcna_ppi <- downloadHandler(#下载
    filename = function() { paste("wgcna_network_graph", input$wgcna_ppi_format, sep = ".")},
    content = function(file) {
    p <- wgcna_ppi_net()
    ggsave(file, p, width = input$wgcna_ppi_width, height = input$wgcna_ppi_height)
})

#module membership (MM) 
output$wgcna_mm_ui <- renderUI({
    column(width = 6,
        column(width = 12, align = "left", style="background-color: #0000001f;height: 50px;font-weight: 900;", 
                    HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right"), paste0(input$wgcna_select_NetworkToCytoscape_input, " Module Membership for all genes"), "</p>"))),
        column(width = 12, DT::dataTableOutput(outputId = "wgcna_k_me", width = "100%"), style="background-color: #AEEEEE;margin-bottom: 20px;"),

        column(width = 12, align = "left", style="background-color: #0000001f;height: 50px;font-weight: 900;", 
                    HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right"), paste0(input$wgcna_select_NetworkToCytoscape_input, " Module Membership for all genes ≥ 0.8 or ≤ -0.8"), "</p>"))),
        column(width = 12, DT::dataTableOutput(outputId = "wgcna_k_me_hub", width = "100%"), style="background-color: #AEEEEE;margin-bottom: 20px;")
    )
})
wgcna_k_me <- reactive({
    file_k_me <- read.csv(paste0(wgcna_tmp_filepath(), "kME_MM.csv"), sep = ",")
    colnames(file_k_me) <- c("gene_id", colnames(file_k_me)[2:ncol(file_k_me)])

    select_module <- input$wgcna_select_NetworkToCytoscape_input
    select_module <- paste0("kME_MM.", select_module)
    file_k_me <- file_k_me[,c("gene_id", select_module)]


    return(file_k_me)
})
output$wgcna_k_me <- DT::renderDataTable(wgcna_k_me(), rownames = FALSE, options = list(dom = 'fltipr', pageLength = 5, scrollX=TRUE, orderClasses = T,
                                        columnDefs = list(
                                                        list(className = 'dt-center', 
                                                                targets = '_all')
                                                        )
                                        ), selection = "none")
#
wgcna_k_me_hub <- reactive({
    req(wgcna_k_me())
    mm <- wgcna_k_me()
    mm <- mm[abs(mm[, 2]) > 0.8, ]
    return(mm)
})
output$wgcna_k_me_hub <- DT::renderDataTable(wgcna_k_me_hub(), rownames = FALSE, options = list(dom = 'fltipr', pageLength = 5, scrollX=TRUE, orderClasses = T,
                                        columnDefs = list(
                                                        list(className = 'dt-center', 
                                                                targets = '_all')
                                                        )
                                        ), selection = "none")
#
wgcna_mm_conn_plot <- reactive({
    req(wgcna_k_me(), wgcna_connectivity())
    mm_hub <- wgcna_k_me()
    connectivity <- wgcna_connectivity()
    data <- dplyr::inner_join(mm_hub, connectivity, by = "gene_id")

    cor_test <- cor.test(data[,2], data[,3])
    cor_value <- abs(round(cor_test$estimate, 2))
    p_value <- cor_test$p.value
    p_value <- formatC(p_value, format = "e", digits = 1)

    title_text <- paste("Correlation: ", cor_value, " |  P-value: ", p_value)

    p <- ggplot(data, aes(x = data[,3], y = abs(data[,2]))) +
        geom_point(color = input$wgcna_select_NetworkToCytoscape_input) +
        geom_smooth(method = "lm", col = "black", se = FALSE) +  # 黑色回归线，不显示置信区间
        labs(title = title_text) +
        theme_bw() +
        theme(panel.grid = element_blank(),
                axis.title = element_text(colour = "black", size = 20),
                axis.text = element_text(colour = "black", size = 15),
                plot.title = element_text(colour = "black", size = 20) ) +
        geom_hline(yintercept = 0.8, linetype = "dashed", color = "black", size = 0.5) +
        xlab("Connectivity") + 
        ylab(paste0(input$wgcna_select_NetworkToCytoscape_input, " Module Membership (MM)")) +
        ylim(0,1)

    ggsave(paste0(wgcna_tmp_filepath(),input$wgcna_select_NetworkToCytoscape_input, " module MM_Connectivity.png"), p, width = 8, height = 8)
    ggsave(paste0(wgcna_tmp_filepath(),input$wgcna_select_NetworkToCytoscape_input, " module MM_Connectivity.pdf"), p, width = 8, height = 8)

    return(p)
})
output$wgcna_mm_conn_plot <- renderPlot({wgcna_mm_conn_plot()})


#gene trait significance (GS)
wgcna_gs1 <- reactive({
    req(wgcna_blockwiseModules(), trait_module_cor())
    data_Expr <- wgcna_blockwiseModules()$data_Expr
    weight <- trait_module_cor()$design
    
    geneTraitSignificance = as.data.frame(cor(data_Expr, weight, use = "p"))
    GSPvalue = as.data.frame(WGCNA::corPvalueStudent(as.matrix(geneTraitSignificance), nrow(data_Expr)))
    names(geneTraitSignificance) = paste("GS.", colnames(weight), sep="")
    names(GSPvalue) = paste("p.GS.", colnames(weight), sep="")

    write.csv(geneTraitSignificance, paste0(wgcna_tmp_filepath(), "geneTraitSignificance(GS).csv"))
    write.csv(GSPvalue, paste0(wgcna_tmp_filepath(), "GSPvalue.csv"))

    return(list(
        geneTraitSignificance = geneTraitSignificance,
        GSPvalue = GSPvalue

    ))
})
wgcna_gs1_selected <- reactive({
    req(wgcna_gs1(), input$wgcna_gs_trait_1)
    gs1 <- wgcna_gs1()$geneTraitSignificance
    gs1$gene_id <- rownames(gs1)
    select <- input$wgcna_gs_trait_1

    gs1 <- gs1[,c("gene_id", select)]
    return(gs1)
})
output$wgcna_gs1_selected <- DT::renderDataTable(wgcna_gs1_selected(), rownames = FALSE, options = list(dom = 'fltipr', pageLength = 5, scrollX=TRUE, orderClasses = T,
                                        columnDefs = list(
                                                        list(className = 'dt-center', 
                                                                targets = '_all')
                                                        )
                                        ), selection = "none")
#

output$wgcna_gs_ui_1 <- renderUI({
    column(width = 6,style = "border-right: 1px solid #ddd;",
        uiOutput("wgcna_gs1_select"),
        column(width = 12, align = "left", style="background-color: #0000001f;height: 50px;font-weight: 900;", 
                    HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right"), paste0("Significance between genes and trait"), "</p>"))),
        column(width = 12, DT::dataTableOutput(outputId = "wgcna_gs1_selected", width = "100%"), style="background-color: #AEEEEE;margin-bottom: 20px;"),
        column(width = 6, align = "left", plotOutput(outputId = "wgcna_gs1_conn_plot", height = "350px",width = "350px") %>% withSpinner()),
        column(width = 6, align = "left", plotOutput(outputId = "wgcna_gs1_mm_plot", height = "350px",width = "350px") %>% withSpinner())
    )
})
output$wgcna_gs1_select <- renderUI({
    column(width = 12, selectInput("wgcna_gs_trait_1", label = "Select trait", choices = colnames(wgcna_gs1()$geneTraitSignificance)))
})

wgcna_gs1_mm_plot <- reactive({
    req(wgcna_gs1_selected(), wgcna_k_me())

    gs1 <- wgcna_gs1_selected()
    gs1_name <- input$wgcna_gs_trait_1
    mm <- wgcna_k_me()
    mm_name <- input$wgcna_select_NetworkToCytoscape_input
    data <- dplyr::inner_join(gs1, mm, by = "gene_id")
    
    cor_test <- cor.test(data[,2], data[,3])
    cor_value <- abs(round(cor_test$estimate, 2))
    p_value <- cor_test$p.value
    p_value <- formatC(p_value, format = "e", digits = 1)

    title_text <- paste("Correlation: ", cor_value, " |  P-value: ", p_value)

    p <- ggplot(data, aes(x = abs(data[,2]), y = abs(data[,3]))) +
        geom_point(color = input$wgcna_select_NetworkToCytoscape_input) +
        geom_smooth(method = "lm", col = "black", se = FALSE) +  # 黑色回归线，不显示置信区间
        labs(title = title_text) +
        theme_bw() +
        theme(panel.grid = element_blank(),
                axis.title = element_text(colour = "black", size = 16),
                axis.text = element_text(colour = "black", size = 15),
                plot.title = element_text(colour = "black", size = 16) ) +
        xlab(paste0("Gene trait Significance (", gs1_name, ")")) + 
        ylab(paste0(mm_name, " Module Membership (MM)")) +
        xlim(0, 1) + ylim(0, 1)

    p
    ggsave(paste0(wgcna_tmp_filepath(),input$wgcna_gs_trait_1, "_MM.",input$wgcna_select_NetworkToCytoscape_input,".png"), p, width = 8, height = 8)
    ggsave(paste0(wgcna_tmp_filepath(),input$wgcna_gs_trait_1, "_MM.",input$wgcna_select_NetworkToCytoscape_input,".pdf"), p, width = 8, height = 8)
    return(p)
})
output$wgcna_gs1_mm_plot <- renderPlot({wgcna_gs1_mm_plot()})

wgcna_gs1_conn_plot <- reactive({
    req(wgcna_gs1_selected(), wgcna_connectivity())

    gs1 <- wgcna_gs1_selected()
    gs1_name <- input$wgcna_gs_trait_1
    connectivity <- wgcna_connectivity()
    data <- dplyr::inner_join(gs1, connectivity, by = "gene_id")

    cor_test <- cor.test(data[,2], data[,3])
    cor_value <- abs(round(cor_test$estimate, 2))
    p_value <- cor_test$p.value
    p_value <- formatC(p_value, format = "e", digits = 1)

    title_text <- paste("Correlation: ", cor_value, " |  P-value: ", p_value)

    p <- ggplot(data, aes(x = abs(data[,2]), y = data[,3])) +
        geom_point(color = input$wgcna_select_NetworkToCytoscape_input) +
        geom_smooth(method = "lm", col = "black", se = FALSE) +  # 黑色回归线，不显示置信区间
        labs(title = title_text) +
        theme_bw() +
        theme(panel.grid = element_blank(),
                axis.title = element_text(colour = "black", size = 16),
                axis.text = element_text(colour = "black", size = 15),
                plot.title = element_text(colour = "black", size = 16) ) +
        xlab(paste0("Gene trait Significance (", gs1_name, ")")) + 
        ylab("Connectivity") +
        xlim(0,1)
    p
    ggsave(paste0(wgcna_tmp_filepath(),input$wgcna_gs_trait_1, " _Connectivity.png"), p, width = 8, height = 8)
    ggsave(paste0(wgcna_tmp_filepath(),input$wgcna_gs_trait_1, " _Connectivity.pdf"), p, width = 8, height = 8)

    return(p)
})
output$wgcna_gs1_conn_plot <- renderPlot({wgcna_gs1_conn_plot()})


#gene trait significance2 (GS2)
wgcna_gs2 <- reactive({
    req(wgcna_blockwiseModules(), trait2_module_cor())
    data_Expr <- wgcna_blockwiseModules()$data_Expr
    weight <- trait2_module_cor()$design
    
    geneTraitSignificance = as.data.frame(cor(data_Expr, weight, use = "p"))
    GSPvalue = as.data.frame(WGCNA::corPvalueStudent(as.matrix(geneTraitSignificance), nrow(data_Expr)))
    names(geneTraitSignificance) = paste("GS.", colnames(weight), sep="")
    names(GSPvalue) = paste("p.GS.", colnames(weight), sep="")

    write.csv(geneTraitSignificance, paste0(wgcna_tmp_filepath(), "geneTraitSignificance2(GS2).csv"))
    write.csv(GSPvalue, paste0(wgcna_tmp_filepath(), "GS2Pvalue.csv"))

    return(list(
        geneTraitSignificance = geneTraitSignificance,
        GSPvalue = GSPvalue

    ))
})
wgcna_gs2_selected <- reactive({
    req(wgcna_gs2(), input$wgcna_gs_trait_2)
    gs1 <- wgcna_gs2()$geneTraitSignificance
    gs1$gene_id <- rownames(gs1)
    select <- input$wgcna_gs_trait_2

    gs1 <- gs1[,c("gene_id", select)]
    return(gs1)
})
output$wgcna_gs2_selected <- DT::renderDataTable(wgcna_gs2_selected(), rownames = FALSE, options = list(dom = 'fltipr', pageLength = 5, scrollX=TRUE, orderClasses = T,
                                        columnDefs = list(
                                                        list(className = 'dt-center', 
                                                                targets = '_all')
                                                        )
                                        ), selection = "none")
#

output$wgcna_gs_ui_2 <- renderUI({
    if(!is.null(trait2_module_cor())){
        column(width = 6,style = "border-right: 1px solid #ddd;",
            uiOutput("wgcna_gs2_select"),
            column(width = 12, align = "left", style="background-color: #0000001f;height: 50px;font-weight: 900;", 
                        HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right"), paste0("Significance between genes and trait2"), "</p>"))),
            column(width = 12, DT::dataTableOutput(outputId = "wgcna_gs2_selected", width = "100%"), style="background-color: #AEEEEE;margin-bottom: 20px;"),
            column(width = 6, align = "left", plotOutput(outputId = "wgcna_gs2_conn_plot", height = "350px",width = "350px") %>% withSpinner()),
            column(width = 6, align = "left", plotOutput(outputId = "wgcna_gs2_mm_plot", height = "350px",width = "350px") %>% withSpinner())
        )
    }else {
        column(width = 6, HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right"), paste0("Please upload trait2 in fourth step."), "</p>")))
    }
})
output$wgcna_gs2_select <- renderUI({
    column(width = 12, selectInput("wgcna_gs_trait_2", label = "Select trait", choices = colnames(wgcna_gs2()$geneTraitSignificance)))
})

wgcna_gs2_mm_plot <- reactive({
    req(wgcna_gs2_selected(), wgcna_k_me())

    gs1 <- wgcna_gs2_selected()
    gs1_name <- input$wgcna_gs_trait_2
    mm <- wgcna_k_me()
    mm_name <- input$wgcna_select_NetworkToCytoscape_input
    data <- dplyr::inner_join(gs1, mm, by = "gene_id")
    
    cor_test <- cor.test(data[,2], data[,3])
    cor_value <- abs(round(cor_test$estimate, 2))
    p_value <- cor_test$p.value
    p_value <- formatC(p_value, format = "e", digits = 1)

    title_text <- paste("Correlation: ", cor_value, " |  P-value: ", p_value)

    p <- ggplot(data, aes(x = abs(data[,2]), y = abs(data[,3]))) +
        geom_point(color = input$wgcna_select_NetworkToCytoscape_input) +
        geom_smooth(method = "lm", col = "black", se = FALSE) +  # 黑色回归线，不显示置信区间
        labs(title = title_text) +
        theme_bw() +
        theme(panel.grid = element_blank(),
                axis.title = element_text(colour = "black", size = 16),
                axis.text = element_text(colour = "black", size = 15),
                plot.title = element_text(colour = "black", size = 16) ) +
        xlab(paste0("Gene trait Significance (", gs1_name, ")")) + 
        ylab(paste0(mm_name, " Module Membership (MM)")) +
        xlim(0, 1) + ylim(0, 1)

    p
    ggsave(paste0(wgcna_tmp_filepath(),input$wgcna_gs_trait_2, "_MM.",input$wgcna_select_NetworkToCytoscape_input,".png"), p, width = 8, height = 8)
    ggsave(paste0(wgcna_tmp_filepath(),input$wgcna_gs_trait_2, "_MM.",input$wgcna_select_NetworkToCytoscape_input,".pdf"), p, width = 8, height = 8)
    return(p)
})
output$wgcna_gs2_mm_plot <- renderPlot({wgcna_gs2_mm_plot()})

wgcna_gs2_conn_plot <- reactive({
    req(wgcna_gs2_selected(), wgcna_connectivity())

    gs1 <- wgcna_gs2_selected()
    gs1_name <- input$wgcna_gs_trait_2
    connectivity <- wgcna_connectivity()
    data <- dplyr::inner_join(gs1, connectivity, by = "gene_id")

    cor_test <- cor.test(data[,2], data[,3])
    cor_value <- abs(round(cor_test$estimate, 2))
    p_value <- cor_test$p.value
    p_value <- formatC(p_value, format = "e", digits = 1)

    title_text <- paste("Correlation: ", cor_value, " |  P-value: ", p_value)

    p <- ggplot(data, aes(x = abs(data[,2]), y = data[,3])) +
        geom_point(color = input$wgcna_select_NetworkToCytoscape_input) +
        geom_smooth(method = "lm", col = "black", se = FALSE) +  # 黑色回归线，不显示置信区间
        labs(title = title_text) +
        theme_bw() +
        theme(panel.grid = element_blank(),
                axis.title = element_text(colour = "black", size = 16),
                axis.text = element_text(colour = "black", size = 15),
                plot.title = element_text(colour = "black", size = 16) ) +
        xlab(paste0("Gene trait Significance (", gs1_name, ")")) + 
        ylab("Connectivity") +
        xlim(0,1)
    p
    ggsave(paste0(wgcna_tmp_filepath(),input$wgcna_gs_trait_2, " _Connectivity.png"), p, width = 8, height = 8)
    ggsave(paste0(wgcna_tmp_filepath(),input$wgcna_gs_trait_2, " _Connectivity.pdf"), p, width = 8, height = 8)

    return(p)
})
output$wgcna_gs2_conn_plot <- renderPlot({wgcna_gs2_conn_plot()})

## download wgcna result
#download result wgcna_download_result
output$wgcna_download_result <- downloadHandler(
  filename = function() {"WGCNA.zip"},
  content = function(file) {
    directory_to_compress <- wgcna_tmp_filepath()
    files_and_folders <- list.files(directory_to_compress, full.names = TRUE, recursive = TRUE)
    zip::zip(file, files_and_folders)
  }
)
