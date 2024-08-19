library(shiny)
# sidebar panels
output$getkegg_option <- renderUI({
    fluidPage(
        column(width = 12,
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
            column(width = 12, style = "font-size:12px",
                selectInput("getkegg_species", "Genomic selection", choices = unlist(list(list.files("enrich_DB/genome_bg/KEGG"), "Upload other")))),
            column(width = 12, uiOutput(outputId = "ui_getkegg_option"))

        )
    )
})

# main panels
output$getkegg_main_panel <- renderUI({
           fluidPage(
                column(width = 10, align = "left", style="background-color: #0000001f;height: 50px;font-weight: 900;", 
                        HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right")," Class C: ", input$select3, "</p>"))),
                column(width = 2, align = "right", downloadButton("download_getkegg_classc", label = "Download class C", style = "text-align: center;margin-top: 6px;font-size:13px"),style="background-color: #0000001f;height: 50px;"),
                column(width = 12, DT::dataTableOutput(outputId = "table1", width = "100%"), style="background-color: #AEEEEE;margin-bottom: 20px;"),
                br(),
                column(width = 10, align = "left", style="background-color: #0000001f;height: 50px;font-weight: 900;", 
                        HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right")," Class B: ", input$select2, "</p>"))),
                column(width = 2, align = "right", downloadButton("download_getkegg_classb", label = "Download class B", style = "text-align: center;margin-top: 6px;font-size:13px"),style="background-color: #0000001f;height: 50px;"),
                column(width = 12, DT::dataTableOutput(outputId = "table2", width = "100%"), style="background-color: #87CEFA;margin-bottom: 20px;"),
                br(),
                column(width = 10, align = "left", style="background-color: #0000001f;height: 50px;font-weight: 900;", 
                        HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right")," Class A: ", input$select1, "</p>"))),
                column(width = 2, align = "right", downloadButton("download_getkegg_classa", label = "Download class A", style = "text-align: center;margin-top: 6px;font-size:13px"),style="background-color: #0000001f;height: 50px;"),
                column(width = 12, DT::dataTableOutput(outputId = "table3", width = "100%"), style="background-color: #98FB98;margin-bottom: 20px;"),
           )
})

# getkegg_option_ui
output$ui_getkegg_option <- renderUI({
    if(input$getkegg_species == "Upload other"){
        fluidRow(
            column(width = 12, fileInput(inputId = "getkegg_gene_background_kegg", label = NULL, accept = list(".txt")), style = "font-size:12px"),
            column(width = 12, selectInput("getkegg_db_version_kegg", "KEGG db version", choices = list.files("enrich_DB/DB_version/KEGG/")), style = "font-size:12px"),
            column(width = 12, selectInput("select1", "Class A selection", choices = NULL), style = "font-size:12px"),
            column(width = 12, selectInput("select2", "Class B selection", choices = NULL), style = "font-size:12px"),
            column(width = 12, selectInput("select3", "Class C selection", choices = NULL), style = "font-size:12px")
        )
    }else {
        fluidRow(
            column(width = 12, selectInput("select1", "Class A selection", choices = NULL), style = "font-size:12px"),
            column(width = 12, selectInput("select2", "Class B selection", choices = NULL), style = "font-size:12px"),
            column(width = 12, selectInput("select3", "Class C selection", choices = NULL), style = "font-size:12px")
        )  
    }

})

# kegg_db
kegg_db <- reactive({
    if(!is.null(input$getkegg_species)){
        if(input$getkegg_species == "Upload other" & !is.null(input$getkegg_db_version_kegg)& !is.null(input$getkegg_gene_background_kegg)){
           genome_bg <- read.csv(input$getkegg_gene_background_kegg$datapath, sep = "\t", header = F)
           colnames(genome_bg) <- c("gene_id", "des_id")
           kegg_db <- read.csv(paste0("enrich_DB/DB_version/KEGG/", input$getkegg_db_version_kegg, "/", input$getkegg_db_version_kegg), sep = "\t", header = T)
        #    enrich_bg <- list(genome_bg = genome_bg, kegg_db = kegg_db)
        }else if(input$getkegg_species != "Upload other"){
           genome_bg <- read.csv(paste0("enrich_DB/genome_bg/KEGG/", input$getkegg_species), sep = "\t", header = F)
           colnames(genome_bg) <- c("gene_id", "des_id")
           kegg_db <- stringr::str_sub(genome_bg[1, 1], 2, -1)
           kegg_db <- read.csv(paste0("enrich_DB/DB_version/KEGG/", kegg_db, "/", kegg_db), sep = "\t", header = T) #des_id#des#pathway_id#classc#classa#classb
        #    enrich_bg <- list(genome_bg = genome_bg, kegg_db = kegg_db)
        }else {
           return(NULL)
        }
        keggfile <- dplyr::inner_join(data.frame(gene_id = genome_bg$gene_id, des_id = genome_bg$des_id), kegg_db, by = "des_id", relationship = "many-to-many")
        keggfile <- data.frame(gene_id = keggfile$gene_id, des = keggfile$des, des_id = keggfile$des_id, pathway_id = keggfile$pathway_id, classc = keggfile$classc, classb = keggfile$classb, classa = keggfile$classa)
        return(keggfile)
    }else{
       return(NULL)
    }
})

observe({
        req(kegg_db())

        # 读取选择的文件
        data <- kegg_db()
        
        # 填充选择框选项
        updateSelectInput(session, "select1", choices = unique(data$classa))
        updateSelectInput(session, "select2", choices = unique(data$classb))
        updateSelectInput(session, "select3", choices = unique(data$classc))
  })

observe({
        req(kegg_db())

        # 读取选择的文件
        data <- kegg_db()
        filtered_data <- subset(data, classa == input$select1)

        # 更新第二个选择框
        updateSelectInput(session, "select2", choices = unique(filtered_data$classb))
        updateSelectInput(session, "select3", choices = unique(filtered_data$classc))

})

observe({
        req(kegg_db())

        # 读取选择的文件
        data <- kegg_db()
        filtered_data <- subset(data, classb == input$select2)

        # 更新第二个选择框
        updateSelectInput(session, "select3", choices = unique(filtered_data$classc))

})

output$table1 <- DT::renderDataTable(filtered_data_classc(), rownames = FALSE, options = list(dom = 'fltipr', pageLength = 3, scrollX=TRUE, orderClasses = T,
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

output$table2 <- DT::renderDataTable(filtered_data_classb(), rownames = FALSE, options = list(dom = 'fltipr', pageLength = 3, scrollX=TRUE, orderClasses = T,
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
                                        
output$table3 <- DT::renderDataTable(filtered_data_classa(), rownames = FALSE, options = list(dom = 'fltipr', pageLength = 3, scrollX=TRUE, orderClasses = T,
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
# 数据过滤
filtered_data_classc <- reactive({
    req(kegg_db(), input$select1, input$select2, input$select3)

    # 读取选择的文件
    data <- kegg_db()
    subset(data, 
            classa == input$select1 &
                classb == input$select2 &
                classc == input$select3)
})
filtered_data_classb <- reactive({
    req(kegg_db(), input$select1, input$select2)

    # 读取选择的文件
    data <- kegg_db()
    subset(data, 
            classa == input$select1 &
                classb == input$select2)
})
filtered_data_classa <- reactive({
    req(kegg_db(), input$select1)

    # 读取选择的文件
    data <- kegg_db()
    subset(data, 
            classa == input$select1)
})

#download table
output$download_getkegg_classc <- downloadHandler(
    filename = function() {"classC.txt"},
    content = function(file) {
      write.table(filtered_data_classc(), file, row.names = FALSE, sep="\t", quote = F)
    }
  )
output$download_getkegg_classb <- downloadHandler(
    filename = function() {"classB.txt"},
    content = function(file) {
      write.table(filtered_data_classb(), file, row.names = FALSE, sep="\t", quote = F)
    }
  )
output$download_getkegg_classa <- downloadHandler(
    filename = function() {"classA.txt"},
    content = function(file) {
      write.table(filtered_data_classa(), file, row.names = FALSE, sep="\t", quote = F)
    }
  )