library(shiny)
# sidebar panels
output$getgo_option <- renderUI({
    fluidPage(
        column(width = 12,
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
            column(width = 12, style = "font-size:12px",
                selectInput("getgo_species", "Genomic selection", choices = unlist(list(list.files("enrich_DB/genome_bg/GO"), "Upload other")))),
            column(width = 12, uiOutput(outputId = "ui_getgo_option"))

        )
    )
})

# main panels
output$getgo_main_panel <- renderUI({
           fluidPage(
                column(width = 10, align = "left", style="background-color: #0000001f;height: 50px;font-weight: 900;", 
                        HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right")," Pathway: ", input$select2_GO, "</p>"))),
                column(width = 2, align = "right", downloadButton("download_getgo_pathway", label = "Download pathway", style = "text-align: center;margin-top: 6px;font-size:13px"),style="background-color: #0000001f;height: 50px;"),
                column(width = 12, DT::dataTableOutput(outputId = "filtered_data_pathway", width = "100%"), style="background-color: #AEEEEE;margin-bottom: 20px;"),
                br(),
                column(width = 10, align = "left", style="background-color: #0000001f;height: 50px;font-weight: 900;", 
                        HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right")," Ontology: ", input$select1_GO, "</p>"))),
                column(width = 2, align = "right", downloadButton("download_getgo_ontology", label = "Download ontology", style = "text-align: center;margin-top: 6px;font-size:13px"),style="background-color: #0000001f;height: 50px;"),
                column(width = 12, DT::dataTableOutput(outputId = "filtered_data_ontology", width = "100%"), style="background-color: #87CEFA;margin-bottom: 20px;")
           )
})

# getgo_option_ui
output$ui_getgo_option <- renderUI({
    if(input$getgo_species == "Upload other"){
        fluidRow(
            column(width = 12, fileInput(inputId = "getgo_gene_background_go", label = NULL, accept = list(".txt")), style = "font-size:12px"),
            column(width = 12, selectInput("getgo_db_version_go", "GO db version", choices = list.files("enrich_DB/DB_version/GO")), style = "font-size:12px"),
            column(width = 12, selectInput("select1_GO", "Ontology selection", choices = NULL), style = "font-size:12px"),
            column(width = 12, selectInput("select2_GO", "Pathway selection", choices = NULL), style = "font-size:12px")
        )
    }else {
        fluidRow(
            column(width = 12, selectInput("getgo_db_version_go", "GO db version", choices = list.files("enrich_DB/DB_version/GO")), style = "font-size:12px"),
            column(width = 12, selectInput("select1_GO", "Ontology selection", choices = NULL), style = "font-size:12px"),
            column(width = 12, selectInput("select2_GO", "Pathway selection", choices = NULL), style = "font-size:12px")
        )  
    }

})

# go_db
go_db <- reactive({
    if(!is.null(input$getgo_species)){
        if(input$getgo_species == "Upload other" & !is.null(input$getgo_gene_background_go) & !is.null(input$getgo_db_version_go)){
           genome_bg <- read.csv(input$getgo_gene_background_go$datapath, sep = "\t", header = F)
           colnames(genome_bg) <- c("gene_id", "go_id")
           go_db <- read.table(paste0("enrich_DB/DB_version/GO/", input$getgo_db_version_go), sep = "\t", header = T)

        }else if(input$getgo_species != "Upload other" & !is.null(input$getgo_db_version_go)){
           genome_bg <- read.csv(paste0("enrich_DB/genome_bg/GO/", input$getgo_species), sep = "\t", header = F)
           colnames(genome_bg) <- c("gene_id", "go_id")
           go_db <- read.table(paste0("enrich_DB/DB_version/GO/", input$getgo_db_version_go), sep = "\t", header = T)

        }else {
           return(NULL)
        }
        gofile <- dplyr::inner_join(data.frame(gene_id = genome_bg$gene_id,go_id = genome_bg$go_id), go_db, by = "go_id", relationship = "many-to-many")

        gofile <- data.frame(gene_id = gofile$gene_id, go_id = gofile$go_id, pathway = gofile$pathway, ontology = gofile$ontology)
        return(gofile)
    }else{
       return(NULL)
    }
})

observe({
        req(go_db())

        # 读取选择的文件
        data <- go_db()
        
        # 填充选择框选项
        updateSelectInput(session, "select1_GO", choices = unique(data$ontology))
        updateSelectInput(session, "select2_GO", choices = unique(data$pathway))
  })

observe({
        req(go_db())

        # 读取选择的文件
        data <- go_db()
        filtered_data <- subset(data, ontology == input$select1_GO)

        # 更新第二个选择框
        updateSelectInput(session, "select2_GO", choices = unique(filtered_data$pathway))

})

output$filtered_data_ontology <- DT::renderDataTable(filtered_data_ontology(), rownames = FALSE, options = list(dom = 'fltipr', pageLength = 3, scrollX=TRUE, orderClasses = T, 
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

output$filtered_data_pathway <- DT::renderDataTable(filtered_data_pathway(),  rownames = FALSE, options = list(dom = 'fltipr', pageLength = 3, scrollX=TRUE, orderClasses = T,
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
filtered_data_ontology <- reactive({
    req(go_db(), input$select1_GO)

    # 读取选择的文件
    data <- go_db()
    subset(data, 
            ontology == input$select1_GO)
})

filtered_data_pathway <- reactive({
    req(go_db(), input$select1_GO, input$select2_GO)

    # 读取选择的文件
    data <- go_db()
    subset(data, 
            ontology == input$select1_GO &
                pathway == input$select2_GO)
})


# download table
output$download_getgo_ontology <- downloadHandler(
    filename = function() {"Ontology.txt"},
    content = function(file) {
      write.table(filtered_data_ontology(), file, row.names = FALSE, sep="\t", quote = F)
    }
  )
output$download_getgo_pathway <- downloadHandler(
    filename = function() {"Pathway.txt"},
    content = function(file) {
      write.table(filtered_data_pathway(), file, row.names = FALSE, sep="\t", quote = F)
    }
  )
