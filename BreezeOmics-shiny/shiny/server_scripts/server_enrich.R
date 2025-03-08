library(shiny)
library(DT)
library(shinydashboard)

observeEvent(input$enrich_ui, {
    if(input$enrich_ui == "<div style='font-size:30px;font-weight:900;line-height: 1.5em;font-family:FontAwesome;'>KEGG</div>"){
        output$ui_enerichanalysis <- renderUI(uiOutput("ui_enerichanalysis_kegg"))
    }else if (input$enrich_ui == "<div style='font-size:30px;font-weight:900;line-height: 1.5em;font-family:FontAwesome;'>Gene Ontology</div>") {
        output$ui_enerichanalysis <- renderUI(uiOutput("ui_enerichanalysis_go"))
    }
})

##################################################################    KEGG    ##################################################################
#render UI_KEGG
enrich_process_kegg <- reactiveValues(value = "start")
observeEvent(enrich_process_kegg$value, {
    if(enrich_process_kegg$value == "start"){
        output$ui_enerichanalysis_kegg <- renderUI({
            fluidRow(
                br(),
                fluidRow(
                        column(width = 6, offset = 4, align="center", style = "margin-left: 39%;",
                                tags$head(tags$style(HTML('.selectize-input {border-color: black;line-height:23px; font-size:14px;height:38px}'))),
                                br(),br(),
                                column(width = 11, align="left", offset = 1, 
                                        HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step1: Upload you gene set</div>")),
                                column(width = 1, align = "right", offset = 0,
                                        shinyWidgets::dropdownButton(
                                                tags$h4(p(icon("check",lib = "font-awesome"),em("Upload a list of gene sets, excluding column names, with the tab \"\\t\" as the separator, and refer to the following template file for the specific format."),style="color:#337ab7;text-align:center;font-size:15px;")),
                                                column(width = 12, align = "center", downloadButton("download_geneset_template", label = "Template geneset", style = "width:200px;border-color: #ccc;font-size:12px;text-align: center;")),
                                                column(width = 12, align = "center", downloadButton("download_geneset_template_log2fc", label = "Template geneset_log2fc", style = "width:200px;border-color: #ccc;font-size:12px;text-align: center;")),
                                                circle = TRUE, status = "default",
                                                icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                                )
                                        ),
                                column(width = 11, align="left", offset = 0, style = "margin-bottom: -15px;",
                                        fileInput(inputId = "enrich_gene_set_kegg", label = NULL, accept = ".txt", width = "340px")),

                                column(width = 11, align="left", offset = 1,
                                        HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step2: Contain Log2FC column</div>")),
                                column(width = 1, align = "right", offset = 0,
                                        shinyWidgets::dropdownButton(
                                                tags$h4(p(icon("check",lib = "font-awesome"),em("If the first column of the gene set file uploaded in Step1 is the gene and the second column is the log2fc value, select Contain, otherwise Not Contain."),style="color:#337ab7;text-align:center;font-size:15px;")),
                                                circle = TRUE, status = "default",
                                                icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                                )
                                        ),
                                column(width = 11, align="left", offset = 0, 
                                        shinyWidgets::radioGroupButtons(inputId = "enrich_log2fc_kegg", label = NULL, choices = c("Not contain", "Contain"), individual = TRUE, width = "100%",
                                        checkIcon = list(yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"), no = tags$i(class = "fa fa-square-o",style = "color: steelblue")))),
                                
                                column(width = 11, align="left", offset = 1,
                                        HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step3: Choose the species for KEGG</div>")),
                                column(width = 1, align = "right", offset = 0,
                                        shinyWidgets::dropdownButton(
                                                tags$h4(p(icon("check",lib = "font-awesome"),em("Select the species information corresponding to your gene set (there are different genome versions of the same species, pay attention to distinguish)"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                                circle = TRUE, status = "default",
                                                icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                                )
                                        ),
                                column(width = 11, align="left", offset = 0, style = "font-size: 14px;",
                                        selectInput(inputId = "enrichanalysis_genome_kegg", label = NULL, choices = unlist(list(list.files("enrich_DB/genome_bg/KEGG"), "Upload other")), width = "340px")),
                                br()
                        ),
                        uiOutput(outputId = "ui_enrich_option_kegg")
                )
            )# close fluidRow
        })
    }else if (enrich_process_kegg$value == "processing") {
        if(!is.null(kegg_enrich())) {
                output$ui_enerichanalysis_kegg <- renderUI({
                        fluidRow(
                                br(),br(),
                                tags$head(tags$style(HTML('.form-control {height: 37.5px;border: 1px solid #000;}'))),
                                tags$head(tags$style(HTML('.selectize-input {border-color: black;line-height:23px; font-size:14px;height:38px}'))),
                                HTML('<hr style="width: 90%;border-top: 2px solid #000;" />'),
                                column(width = 12, align = "center",style = "width:40%;margin-left:30%;margin-bottom: 20px;", 
                                        htmlOutput(outputId = "enrich_info_kegg", style = "font-size:15px;"),
                                        column(width = 12, align = "center", downloadButton("download_enrich_bg_kegg", label = "Download background file"), style = "margin-bottom: 20px;")),
                                HTML('<hr style="width: 90%;border-top: 2px solid #000;" />'),
                                column(width = 12, align = "left", style = "width:80%;margin-left:10%;",
                                        column(width = 3, align = "left", h3("KEGG pathway enrichment")),# close column
                                        column(width = 1, align = "left", offset = 0, style = "left:-60px;top: 15px;",
                                                shinyWidgets::dropdownButton(
                                                tags$h4(p(icon("check",lib = "font-awesome"),em("KEGG enrichment analysis, KEGG is divided into classA, classB, classC, and the pathways displayed in general belong to classC (also known as pathway_id or ko number)"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                                tags$h4(p(icon("check",lib = "font-awesome"),em("Each gene should correspond to a des_id (e.g., K21848), and the pathway is composed of multiple des_id."),style="color:#337ab7;text-align:center;font-size:15px;")),
                                                tags$h4(p(icon("check",lib = "font-awesome"),em("The logical relationship between KEGG is as follows: classA→ classB→ classC (corresponding to pathway), →des_id (corresponding to gene)"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                                tags$h4(p(icon("check",lib = "font-awesome"),em("Generatio (number of genes in the gene set, genes in this pathway/number of genes in the gene set)"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                                tags$h4(p(icon("check",lib = "font-awesome"),em("BgRatio (number of pathway genes in the background/number of all genes in the background)"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                                tags$h4(p(icon("check",lib = "font-awesome"),em("Pvalue (significance)"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                                tags$h4(p(icon("check",lib = "font-awesome"),em("P. adjast (corrected value)"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                                tags$h4(p(icon("check",lib = "font-awesome"),em("RichFactor (number of genes in the pathway in the gene set/number of genes in the pathway in the background)"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                                tags$h4(p(icon("check",lib = "font-awesome"),em("The mapping will be sorted by Pvalue first, and then the corresponding number of channels will be displayed according to the user's needs."),style="color:#337ab7;text-align:center;font-size:15px;")),
                                                circle = TRUE, status = "default",
                                                icon = icon("question"), width = "300px", size = "sm", right = F, up = T, margin = "10px")
                                        ),
                                        HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'), br(),
                                        column(width = 12,offset = 0, align = "left", uiOutput("keggtablesetting_ui")), # close column
                                        column(width = 12, style = "margin-bottom: 30px", uiOutput("kegg_table_type")), # close column
                                        
                                        HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'), br(),
                                        column(width = 12, align = "left", uiOutput("kegg_plot_ui"), style = "margin-bottom: 20px;") # close column
                                ), # close column
                                HTML('<hr style="width: 90%;border-top: 2px solid #000;" />'),
                                column(width = 12, align = "center", style = "margin-top: 30px;margin-bottom: 50px;",
                                        actionButton(inputId = "reset_enrich_kegg", label = "RESET", icon = icon("rotate-left"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;"))
                        )
                })
        }else {
           output$ui_enerichanalysis_kegg <- renderUI({
                fluidRow(
                        column(width = 12, align = "center", style = "",  
                        HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Running...</div>"))
                )
           })
        }
    }
})
output$ui_enrich_option_kegg <- renderUI({
    if(input$enrichanalysis_genome_kegg != "Upload other"){
        column(width = 4, offset = 4,
                column(width = 6, align="left", offset = 0, style="margin-bottom: 30px;margin-top: 50px;",
                        actionButton(inputId = "reset_enrich_kegg", label = "RESET", icon = icon("rotate-left"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;")),
                column(width = 6, align="right", offset = 0, style="margin-bottom: 30px;margin-top: 50px;",
                        actionButton(inputId = "enter_enrich_kegg", label = "ENTER", icon = icon("check"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;"))
        )
    } else {
        column(width = 12,
                column(width = 6, offset = 4,align="center", style="margin-left: 38.8%;",
                        column(width = 11, align="left", offset = 1,
                                HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step4: Upload you Species information</div>")),
                        column(width = 1, align = "right", offset = 0,
                                shinyWidgets::dropdownButton(
                                        tags$h4(p(icon("check",lib = "font-awesome"),em("Upload the KEGG annotation information of the species, a total of two columns, the first column is the gene number, the second column is the KO number, with the tab \"\\t\" as the separator, the specific format refers to the following file."),style="color:#337ab7;text-align:center;font-size:15px;")),
                                        column(width = 12, align = "center", downloadButton("download_kegg_gemone_template", label = "Template Species information", style = "width:200px;border-color: #ccc;font-size:12px;text-align: center;")),
                                        circle = TRUE, status = "default",
                                        icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                        )
                                ),
                        column(width = 11, align="left", offset = 0,
                                fileInput(inputId = "enrich_gene_background_kegg", label = NULL, accept = list(".txt"), width = "340px")),

                        column(width = 11, align="left", offset = 1,
                                HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step5: Choose the database version</div>")),
                        column(width = 1, align = "right", offset = 0,
                                shinyWidgets::dropdownButton(
                                        tags$h4(p(icon("check",lib = "font-awesome"),em("Select the KEGG background pathway and collect it on the KEGG official website"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                        circle = TRUE, status = "default",
                                        icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                        )
                                ),
                        column(width = 11, align="left", offset = 0,
                                selectInput(inputId = "enrichanalysis_db_version_kegg", label = NULL, choices = list.files("enrich_DB/DB_version/KEGG/"), width = "340px")),

                        column(width = 11, align="left", offset = 1,
                                HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step6: Select KEGG pathway</div>")),
                        column(width = 1, align = "right", offset = 0,
                                shinyWidgets::dropdownButton(
                                        tags$h4(p(icon("check",lib = "font-awesome"),em("Select the pathway category you want to be enriched into"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                        circle = TRUE, status = "default",
                                        icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                        )
                                ),
                        column(width = 11, align = "left", offset = 0, 
                                shinyWidgets::pickerInput(inputId = "Id087", label = NULL, , multiple = TRUE, width = "340px",
                                selected = c("Brite Hierarchies", "Cellular Processes", "Environmental Information Processing","Genetic Information Processing","Human Diseases","Metabolism","Not Included in Pathway or Brite","Organismal Systems"),
                                choices = c("Brite Hierarchies", "Cellular Processes", "Environmental Information Processing","Genetic Information Processing","Human Diseases","Metabolism","Not Included in Pathway or Brite","Organismal Systems")))
                ),
                column(width = 4, offset = 4,align="center", style="margin-left: 601px;",
                        column(width = 6, align="left", offset = 0, style="margin-bottom: 30px;margin-top: 50px;",
                                actionButton(inputId = "reset_enrich_kegg", label = "RESET", icon = icon("rotate-left"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;")),
                        column(width = 6, align="right", offset = 0, style="margin-bottom: 30px;margin-top: 50px;",
                                actionButton(inputId = "enter_enrich_kegg", label = "ENTER", icon = icon("check"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;"))
                )
        )
    }
})

#download template
output$download_geneset_template <- downloadHandler(#下载enrich_geneset模板
    filename = function() {"geneset_template.txt"},
    content = function(file) {
      data <- read.table("template/geneset_template.txt")
      write.table(data, file, row.names = F, fileEncoding="GBK", quote = FALSE, col.names = F, sep = "\t")
    }
)
output$download_geneset_template_log2fc <- downloadHandler(#下载enrich_geneset_log2fc模板
    filename = function() {"geneset_template_log2fc.txt"},
    content = function(file) {
      data <- read.table("template/geneset_template_log2fc.txt")
      write.table(data, file, row.names = F, fileEncoding="GBK", quote = FALSE, col.names = F, sep = "\t")
    }
)
output$download_kegg_gemone_template <- downloadHandler(#下载kegg_gemone_template模板
    filename = function() {"kegg_gemone_template[TAIR10].txt"},
    content = function(file) {
      data <- read.table("template/kegg_gemone_template[TAIR10].txt", sep = "\t")
      write.table(data, file, row.names = F, fileEncoding="GBK", quote = FALSE, col.names = F, sep = "\t")
    }
)


#file upload status
#KEGG
values_enrich <- reactiveValues(upload_enrich_gene_set_kegg = "NULL", upload_enrich_background_kegg = "uploaded")
observeEvent(input$enrich_gene_set_kegg, {values_enrich$upload_enrich_gene_set_kegg <- 'uploaded'})
observeEvent(input$enrichanalysis_genome_kegg, {
    if(input$enrichanalysis_genome_kegg == "Upload other"){
        values_enrich$upload_enrich_background_kegg <- 'NULL'
    }else if (input$enrichanalysis_genome_kegg != "Upload other") {
       values_enrich$upload_enrich_background_kegg <- 'uploaded'
    }
})
observeEvent(input$enrich_gene_background_kegg, {values_enrich$upload_enrich_background_kegg <- 'uploaded'})

#ENTER
observeEvent(input$enter_enrich_kegg, {
  if (values_enrich$upload_enrich_gene_set_kegg == "uploaded" & values_enrich$upload_enrich_background_kegg == 'uploaded') {
    if(input$enrichanalysis_genome_kegg == "Upload other"){
        genome_bg <- read.table(input$enrich_gene_background_kegg$datapath, sep = "\t", header = F)

        if(ncol(genome_bg)!=2) {shinyjs::runjs('alert("The species information must be in two columns, the first is the gene_id and the second is the KEGG des_id")');return(NULL)}
        colnames(genome_bg) <- c("gene_id", "des_id")
        kegg <- length(genome_bg[substr(genome_bg$des_id, start = 1, stop = 1)=="K","gene_id"])

        if(kegg < 100) {shinyjs::runjs('alert("The KEGG des_id in your species information ERROR, correct format is K*****")');return(NULL)}
        kegg_bg <- genome_bg[genome_bg$des_id != "", "gene_id"]
        }else {
        genome_bg <- read.csv(paste0("enrich_DB/genome_bg/KEGG/", input$enrichanalysis_genome_kegg), sep = "\t", header = F)
        kegg_bg <- genome_bg[,1]
        kegg_bg <- unique(kegg_bg)
        }

    geneset <- read.csv(input$enrich_gene_set_kegg$datapath, sep = "\t", header = F)
    if(input$enrich_log2fc_kegg == "Contain" & ncol(geneset) < 2 ) {shinyjs::runjs('alert("The log2FC value for the second column is not detected in the geneset file")');return(NULL)}
    if(input$enrich_log2fc_kegg == "Contain" ) {if(all(is.numeric(geneset[,2])) == FALSE) {shinyjs::runjs('alert("The log2FC vcolumn must be numeric")');return(NULL)}}
    if(length(geneset[,1])>20000) {shinyjs::runjs('alert("The number of genes in your gene set cannot be greater than 20,000")');return(NULL)}

    kegg_bg <- length(intersect(kegg_bg, geneset[,1]))
    if(kegg_bg < 20) {
        shinyjs::runjs('alert("The num of intersections between your gene set and the species information gene set is less than 20, please check again")');return(NULL)
    }
    enrich_process_kegg$value <- "processing"
  } else {
     shinyjs::runjs('alert("ERROR: Please upload gene set file or species information file first.")')
     enrich_process_kegg$value <- "start"
        return(NULL)
  }
})
# RESET
observeEvent(input$reset_enrich_kegg, {
  enrich_process_kegg$value <- "start"
  shinyjs::reset("enrich_gene_set_kegg")
  shinyjs::reset("enrich_log2fc_kegg")
  shinyjs::reset("enrich_gene_background_kegg")
  shinyjs::reset("enrichanalysis_genome_kegg")
  shinyjs::reset("enrichanalysis_db_version_kegg")
  values_enrich$upload_enrich_gene_set_kegg = "NULL"
  values_enrich$upload_enrich_background_kegg = "uploaded"
})
#source enrich base
kegg_base <- reactive({
    if(enrich_process_kegg$value == "processing") {
        geneset <- read.csv(input$enrich_gene_set_kegg$datapath, sep = "\t", header = F)[,1]

        if(input$enrichanalysis_genome_kegg == "Upload other"){
                genome_bg <- read.csv(input$enrich_gene_background_kegg$datapath, sep = "\t", header = F)
                colnames(genome_bg) <- c("gene_id", "des_id")
                kegg_db <- read.csv(paste0("enrich_DB/DB_version/KEGG/", input$enrichanalysis_db_version_kegg, "/", input$enrichanalysis_db_version_kegg), sep = "\t", header = T)
                print(kegg_db)
                kegg_db <- kegg_db[kegg_db$classa %in% input$Id087,]
                # print(input$Id087)
                keggfile <- dplyr::inner_join(data.frame(gene_id = genome_bg$gene_id, des_id = genome_bg$des_id), kegg_db, by = "des_id", relationship = "many-to-many")
                keggfile <- keggfile[!duplicated(keggfile),]

                TERM2GENE <- data.frame(id = keggfile$pathway_id, gene = keggfile$gene_id)
                TERM2GENE <- TERM2GENE[!duplicated(TERM2GENE),]
                TERM2NAME <- data.frame(id = kegg_db$pathway_id, pathway = kegg_db$classc)
                TERM2NAME <- TERM2NAME[!duplicated(TERM2NAME),]
        }else {
           genome_bg <- read.csv(paste0("enrich_DB/genome_bg/KEGG/", input$enrichanalysis_genome_kegg), sep = "\t", header = F)
           colnames(genome_bg) <- c("gene_id", "des_id")
           kegg_db <- stringr::str_sub(genome_bg[1, 1], 2, -1)
           kegg_db <- read.csv(paste0("enrich_DB/DB_version/KEGG/", kegg_db, "/", kegg_db), sep = "\t", header = T)
           keggfile <- dplyr::inner_join(data.frame(gene_id = genome_bg$gene_id, des_id = genome_bg$des_id), kegg_db, by = "des_id", relationship = "many-to-many")
           keggfile <- keggfile[!duplicated(keggfile),]
        #    colnames(keggfile) <- c("gene_id", "des_id", "des", "pathway_id", "classc", "classb", "classa")
           TERM2GENE <- data.frame(id = keggfile$pathway_id, gene = keggfile$gene_id)
           TERM2GENE <- TERM2GENE[!duplicated(TERM2GENE),]
           TERM2NAME <- data.frame(id = keggfile$pathway_id, pathway = keggfile$classc)
           TERM2NAME <- TERM2NAME[!duplicated(TERM2NAME),]
        }



        enrich_bg <- list(TERM2GENE = TERM2GENE, TERM2NAME = TERM2NAME)
        return(enrich_bg)
    }else {
        return(NULL)
    }
})

#kegg enrich
output$enrich_info_kegg <- renderText({#enrich INFO UI
       if(enrich_process_kegg$value == "processing" & !is.null(kegg_base())) {
        geneset_filename <- paste0("File name: ", input$enrich_gene_set_kegg[1])
        if(input$enrichanalysis_genome_kegg != "Upload other"){
                bg_file <- paste0("Background species: ", input$enrichanalysis_genome_kegg)
        }else {
           bg_file <- paste0("Background species: Uploaded")
        }
        if(input$enrichanalysis_genome_kegg != "Upload other"){
                genome_bg <- read.csv(paste0("enrich_DB/genome_bg/KEGG/", input$enrichanalysis_genome_kegg), sep = "\t", header = F)
                genome_bg <- stringr::str_sub(genome_bg[1, 1], 2, -1)
                db_version <- paste0("Data base version: ", genome_bg)
        }else {
           db_version <- paste0("Data base version: ", input$enrichanalysis_db_version_kegg)
        }
        

        gene_set_num <- read.csv(input$enrich_gene_set_kegg$datapath, sep = "\t", header = T)[,1]
        gene_set_num <- paste0("Num of genes (gene set): ", length(unique(gene_set_num)))

        kegg_bg_num <- kegg_base()$TERM2GENE$gene
        kegg_bg_num <- paste0("Num of genes (KEGG background): ", length(unique(kegg_bg_num)))


        a <- HTML(paste0("<div style='font-size:20px;font-weight:600;line-height: 1em;'><br/>KEGG enrich basic information</div>",
                        "<br/>", geneset_filename, "<br/>", bg_file, "<br/>", db_version, "<br/>", gene_set_num, "<br/>", kegg_bg_num,"<br/><br/>"))
       }else {
          return(NULL)
       }
})
output$download_enrich_bg_kegg <- downloadHandler(#下载enrich DB
    filename = function() {ifelse(input$enrichanalysis_genome_kegg == "Upload other", paste(input$enrichanalysis_db_version_kegg, ".tar.gz"), paste(input$enrichanalysis_genome_kegg, ".tar.gz"))},
    content = function(file) {
        if(input$enrichanalysis_genome_kegg != "Upload other"){
                bg_file <- paste0("enrich_DB/genome_bg/KEGG/", input$enrichanalysis_genome_kegg)
                data <- read.csv(bg_file, header = F)
                db_file <- stringr::str_sub(data[1, 1], 2, -1)
                db_file <- paste0("enrich_DB/DB_version/KEGG/", db_file, "/", db_file)
                # print(bg_file)
                # print(db_file)
                utils::tar(file, c(bg_file, db_file), compression = 'gzip', tar="tar")
        }else {
           bg_file <- paste0("enrich_DB/DB_version/KEGG/", input$enrichanalysis_db_version_kegg, "/", input$enrichanalysis_db_version_kegg)
           R.utils::gzip(filename = bg_file, destname = file, remove = F)
        }
    }
)


kegg_enrich <- reactive({
    if(enrich_process_kegg$value == "processing" & !is.null(kegg_base())){
        if(input$enrich_log2fc_kegg == "Contain"){

                geneset <- read.csv(input$enrich_gene_set_kegg$datapath, sep = "\t", header = T)
                colnames(geneset) <- c("gene_id", "log2fc")
                upset <- geneset[geneset$log2fc > 0 , "gene_id"]
                downset <- geneset[geneset$log2fc < 0 , "gene_id"]

                TERM2GENE <- kegg_base()$TERM2GENE
                TERM2NAME <- kegg_base()$TERM2NAME

                rich_up <- clusterProfiler::enricher(upset,TERM2GENE=TERM2GENE,
                        TERM2NAME=TERM2NAME,
                        pvalueCutoff=1,
                        qvalueCutoff =1,
                        pAdjustMethod = "BH")
                rich_up <- as.data.frame(rich_up)
                rich_up <- mutate(rich_up, RichFactor = Count / as.numeric(sub("/\\d+", "", BgRatio)))
                rich_up <- data.frame(Pathway_ID = rich_up$ID, Description = rich_up$Description, GeneRatio = rich_up$GeneRatio, BgRatio = rich_up$BgRatio, Pvalue = rich_up$pvalue, P.adjust = rich_up$p.adjust, RichFactor = rich_up$RichFactor, Genes = rich_up$geneID)
                

                rich_down <- clusterProfiler::enricher(downset,TERM2GENE=TERM2GENE,
                        TERM2NAME=TERM2NAME,
                        pvalueCutoff=1,
                        qvalueCutoff =1,
                        pAdjustMethod = "BH")
                rich_down <- as.data.frame(rich_down)
                rich_down <- mutate(rich_down, RichFactor = Count / as.numeric(sub("/\\d+", "", BgRatio)))
                rich_down <- data.frame(Pathway_ID = rich_down$ID, Description = rich_down$Description, GeneRatio = rich_down$GeneRatio, BgRatio = rich_down$BgRatio, Pvalue = rich_down$pvalue, P.adjust = rich_down$p.adjust, RichFactor = rich_down$RichFactor, Genes = rich_down$geneID)
                

                if(input$enrichanalysis_genome_kegg != "Upload other"){
                        genome_bg <- read.csv(paste0("enrich_DB/genome_bg/KEGG/", input$enrichanalysis_genome_kegg), sep = "\t", header = F)
                        kegg_db <- stringr::str_sub(genome_bg[1, 1], 2, -1)
                        kegg_db <- read.csv(paste0("enrich_DB/DB_version/KEGG/", kegg_db, "/", kegg_db), sep = "\t", header = T)
                        kegg_db <- data.frame(classa = kegg_db$classa, classb = kegg_db$classb, classc = kegg_db$classc, Pathway_ID = kegg_db$pathway_id)

                }else {
                kegg_db <- read.csv(paste0("enrich_DB/DB_version/KEGG/", input$enrichanalysis_db_version_kegg, "/",  input$enrichanalysis_db_version_kegg), sep = "\t", header = T)
                kegg_db <- data.frame( classa = kegg_db$classa, classb=kegg_db$classb, classc=kegg_db$classc, Pathway_ID = kegg_db$pathway_id)
                }

                kegg_db <- kegg_db[!duplicated(kegg_db),]
                # names(kegg_db)[names(kegg_db) == "pathway_id"] <- "Pathway_ID"



                rich_up <- dplyr::inner_join(kegg_db, rich_up, by = "Pathway_ID", relationship = "many-to-many")# add classA, classB, classC
                rich_down <- dplyr::inner_join(kegg_db, rich_down, by = "Pathway_ID", relationship = "many-to-many")# add classA, classB, classC



                rich <- list(rich_up = rich_up, rich_down = rich_down)
                return(rich)



        }else if (input$enrich_log2fc_kegg == "Not contain") {
                geneset <- read.csv(input$enrich_gene_set_kegg$datapath, sep = "\t", header = F)[,1]
                TERM2GENE <- kegg_base()$TERM2GENE
                TERM2NAME <- kegg_base()$TERM2NAME
                # print(head(TERM2GENE)) #for debug
                # print(head(TERM2NAME)) #for debug

                rich <- clusterProfiler::enricher(geneset,TERM2GENE=TERM2GENE,
                        TERM2NAME=TERM2NAME,
                        pvalueCutoff=1,
                        qvalueCutoff =1,
                        pAdjustMethod = "BH")
                rich <- as.data.frame(rich)
                rich <- mutate(rich, RichFactor = Count / as.numeric(sub("/\\d+", "", BgRatio)))
                rich <- data.frame(Pathway_ID = rich$ID, Description = rich$Description, GeneRatio = rich$GeneRatio, BgRatio = rich$BgRatio, Pvalue = rich$pvalue, P.adjust = rich$p.adjust, RichFactor = rich$RichFactor, Genes = rich$geneID)
                
                if(input$enrichanalysis_genome_kegg != "Upload other"){
                        genome_bg <- read.csv(paste0("enrich_DB/genome_bg/KEGG/", input$enrichanalysis_genome_kegg), sep = "\t", header = F)
                        kegg_db <- stringr::str_sub(genome_bg[1, 1], 2, -1)
                        kegg_db <- read.csv(paste0("enrich_DB/DB_version/KEGG/", kegg_db, "/", kegg_db), sep = "\t", header = T)
                        kegg_db <- data.frame(classa = kegg_db$classa, classb = kegg_db$classb, classc = kegg_db$classc, Pathway_ID = kegg_db$pathway_id)

                }else {
                kegg_db <- read.csv(paste0("enrich_DB/DB_version/KEGG/", input$enrichanalysis_db_version_kegg, "/",  input$enrichanalysis_db_version_kegg), sep = "\t", header = T)
                kegg_db <- data.frame( classa = kegg_db$classa, classb=kegg_db$classb, classc=kegg_db$classc, Pathway_ID = kegg_db$pathway_id)
                }

                kegg_db <- kegg_db[!duplicated(kegg_db),]
                # names(kegg_db)[names(kegg_db) == "pathway_id"] <- "Pathway_ID"
                
                rich <- dplyr::inner_join(kegg_db, rich, by = "Pathway_ID", relationship = "many-to-many")# add classA, classB, classC
                return(rich)
        }
    }else {
       return(NULL)
    }
})

output$kegg_table_type <- renderUI({
        if(input$enrich_log2fc_kegg == "Contain"){
                fluidPage(
                        column(width = 2, align = "left", style="background-color: #ff000014;height: 50px;width: 300px;font-weight: 900;", HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("circle-up")," UP regulation</p>"))),
                        column(width = 12, DT::dataTableOutput(outputId = "kegg_resault", width = "100%"), style="background-color: #ff000014;margin-bottom: 20px;"),
                        column(width = 2, align = "left", style="background-color: #00acff14;height: 50px;width: 300px;font-weight: 900;", HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("circle-down")," DOWN regulation</p>"))),
                        column(width = 12, DT::dataTableOutput(outputId = "kegg_resault_2", width = "100%"), style="background-color: #00acff14;")
                )
        }else {
           DT::dataTableOutput(outputId = "kegg_resault", width = "102%")
        }
})


kegg_table <- reactive({
    if(enrich_process_kegg$value == "processing" & !is.null(kegg_enrich())){
        pvaluecuttoff <- ifelse(is.null(input$kegg_pvaluecuttoff), 1, input$kegg_pvaluecuttoff)
        padjcuttoff <- ifelse(is.null(input$kegg_padjcuttoff), 1, input$kegg_padjcuttoff)

        if(input$enrich_log2fc_kegg == "Contain"){
                rich_up <- kegg_enrich()$rich_up
                rich_down <- kegg_enrich()$rich_down
                rich_up <- rich_up[rich_up$Pvalue <= pvaluecuttoff & rich_up$P.adjust <= padjcuttoff,]
                rich_down <- rich_down[rich_down$Pvalue <= pvaluecuttoff & rich_down$P.adjust <= padjcuttoff,]
                rich_up<-rich_up[order(rich_up$Pvalue),]
                rich_down<-rich_down[order(rich_down$Pvalue),]


                if(nrow(rich_up)>1){rownames(rich_up) <- 1:nrow(rich_up)} # nolint
                if(nrow(rich_down)>1){rownames(rich_down) <- 1:nrow(rich_down)} # nolint
                rich <- list(rich_up = rich_up, rich_down = rich_down)
                return(rich)
        }else {
                rich <- kegg_enrich()
                rich <- rich[rich$Pvalue <= pvaluecuttoff & rich$P.adjust <= padjcuttoff,]
                rich<-rich[order(rich$Pvalue),]
                if(nrow(rich)>1){rownames(rich) <- 1:nrow(rich)} # nolint
                rich <- list(rich_up = rich, rich_down = NULL)
                return(rich)
        }

    }else {
       return(NULL)
    }
})
output$keggtablesetting_ui <- renderUI({
        if(!is.null(kegg_enrich())) {
                column(width = 12, 
                        column(width = 12,
                                column(width = 2, align = "left", offset = 5, style="", numericInput(inputId = "kegg_pvaluecuttoff", label = HTML("<p style='height: 1px;font-size: 10px;'>Pvalue cutoff</p>"), min = 0, value = 1, max = 1)),
                                column(width = 2, align = "left", style="",  numericInput(inputId = "kegg_padjcuttoff", label = HTML("<p style='height: 1px;font-size: 10px;'>P.adjust cutoff</p>"), min = 0, value = 1, max = 1)),
                                column(width = 3, align = "right", style = "margin-bottom:20px;top: 20px;", downloadButton(outputId = "download_kegg_table", label = "Download Full KEGG enrich table")),    
                              )
                        )
        }else {
           return(NULL)
        }
})
output$kegg_resault <- DT::renderDataTable(kegg_table()$rich_up[,-c(1,2,3,11)], options = list(dom = 'fltipr', pageLength = 10, scrollX=TRUE, orderClasses = T, 
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
output$kegg_resault_2 <- DT::renderDataTable(kegg_table()$rich_down[,-c(1,2,3,11)], options = list(dom = 'fltipr', pageLength = 10, scrollX=TRUE, orderClasses = T, 
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
output$kegg_plot_ui <- renderUI({
        if(!is.null(kegg_table())){
                fluidRow(
                        column(width = 12, style = "margin-bottom: 60px;margin-top: 20px;",
                                column(width = 8,offset = 2, align = "center", plotOutput("kegg_plot_bar", width = "100%", height = "450px"))
                              ),
                        HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'), br(),
                        column(width = 12, style = "margin-bottom: 60px;margin-top: 20px;",
                                column(width = 8, offset = 0, plotOutput("kegg_plot_dot", width = "100%", height = "550px")),
                                column(width = 3,
                                        column(width = 12, shinyWidgets::radioGroupButtons(
                                                inputId = "kegg_plot", label = NULL, choices = c("Dot plot", "Bar plot"), individual = F, justified = T, width = '100%',
                                                checkIcon = list(yes = tags$i(class = "fa fa-circle", style = "color: steelblue"),
                                                                 no = tags$i(class = "fa fa-circle-o", style = "color: steelblue")))),
                                        column(width = 6, colourpicker::colourInput("color_kegg_plot_hight", label = NULL, "red"), style = "margin-bottom: -0px;"),
                                        column(width = 6, colourpicker::colourInput("color_kegg_plot_low", label = NULL, "darkblue"), style = "margin-bottom: -0px;"),
                                        column(width = 6, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Item</div>")),
                                        column(width = 6, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Rank by</div>")),
                                        column(width = 6, numericInput("kegg_num_of_item", label = NULL, value = 10, max = 20, min = 5), style = "margin-bottom: -5px;"),
                                        column(width = 6, selectInput(inputId = "kegg_rank_by", label = NULL, choices = c("RichFactor", "Pvalue", "P.adjust", "Count")), style = "margin-bottom: -5px;"),
                                        column(width = 6, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>X-axis text size</div>")),
                                        column(width = 6, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Y-axis text size</div>")),
                                        column(width = 6, numericInput("kegg_x_text_size", label = NULL, value = 8, min = 0), style = "margin-bottom: -5px;"),
                                        column(width = 6, numericInput("kegg_y_text_size", label = NULL, value = 14, min = 0), style = "margin-bottom: -5px;"),
                                        column(width = 6, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>X-axis title size</div>")),
                                        column(width = 6, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Y-axis title size</div>")),
                                        column(width = 6, numericInput("kegg_x_title_size", label = NULL, value = 14, min = 0), style = "margin-bottom: -5px;"),
                                        column(width = 6, numericInput("kegg_y_title_size", label = NULL, value = 20, min = 0), style = "margin-bottom: -5px;"),
                                        column(width = 6, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Y text length</div>")),
                                        # column(width = 6, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Reset</div>")),
                                        column(width = 6, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Show pathway id</div>")),
                                        column(width = 6, numericInput("kegg_y_text_length", label = NULL, value = 60, min = 0), style = "margin-bottom: -5px;"),
                                        # column(width = 6, actionButton(inputId = "reset_kegg_plot", label = NULL, icon = icon("arrows-rotate"),style = "font-size: 17px;margin-bottom: -10px;WIDTH: 100%;margin-bottom: 10px;")),
                                        column(width = 6, shinyWidgets::radioGroupButtons(inputId = "kegg_show_pathway",label = NULL, choices = c("YES", "NO"),justified = TRUE), style = "margin-bottom: -5px;"),
                                        column(width = 6, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Graph width</div>")),
                                        column(width = 6, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Graph height</div>")),
                                        column(width = 6, numericInput("kegg_width", label = NULL, value = 10), style = "margin-bottom: -10px;", style = "margin-bottom: -5px;"),
                                        column(width = 6, numericInput("kegg_height", label = NULL, value = 6), style = "margin-bottom: -10px;", style = "margin-bottom: -5px;"),
                                        column(width = 6, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Graph format</div>")),
                                        column(width = 6, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Download</div>")),
                                        column(width = 6, selectInput(inputId = "kegg_plot_format", label = NULL, choices = c("PNG" = "png", "PDF" = "pdf")), style = "margin-bottom: -10px;"),
                                        column(width = 6, downloadButton("download_kegg_bar_plot", label = NULL, style = "width:100%;"))
                                )
                              )
                )#close fluiRow
        }else {
           return(NULL)
        }
})
output$download_kegg_table <- downloadHandler(#download kegg full table
  filename = function() {paste0("KEGG_enrich.xlsx")},
  content = function(file) {
    if(!is.null(kegg_enrich())){
        if(input$enrich_log2fc_kegg=="Contain"){
                rich_up <- kegg_enrich()$rich_up
                rich_up <- na.omit(rich_up)
                rich_down <- kegg_enrich()$rich_down
                rich_down <- na.omit(rich_down)
                list_data <- list("KEGG_up_regulation" = rich_up, "KEGG_down_regulation" = rich_down)
                openxlsx::write.xlsx(list_data, file, fileEncoding="GBK")

        }else {
                a <- kegg_enrich()
                a <- na.omit(a)
                openxlsx::write.xlsx(a, file, sheetName = "KEGG", fileEncoding="GBK")
        }
      
    }
  }
)
observeEvent(input$reset_kegg_plot, {
        shinyjs::reset("color_kegg_plot_hight")
        shinyjs::reset("color_kegg_plot_low")
        shinyjs::reset("kegg_rank_by")
        shinyjs::reset("kegg_num_of_item")
        shinyjs::reset("kegg_x_text_size")
        shinyjs::reset("kegg_y_text_size")
        shinyjs::reset("kegg_x_title_size")
        shinyjs::reset("kegg_y_title_size")
        shinyjs::reset("kegg_width")
        shinyjs::reset("kegg_height")
        shinyjs::reset("kegg_plot")
})



##################################################################     GO      ##################################################################
#render UI_GO
enrich_process_go <- reactiveValues(value = "start")
observeEvent(enrich_process_go$value, {
    if(enrich_process_go$value == "start"){
       output$ui_enerichanalysis_go <- renderUI({
                output$ui_enerichanalysis_go <- renderUI({
                        fluidRow(
                                br(),
                                fluidRow(
                                        column(width = 6, offset = 4, align="center", style = "margin-left: 39%;",
                                                tags$head(tags$style(HTML('.selectize-input {border-color: black;line-height:23px; font-size:14px;height:38px}'))),
                                                br(),br(),
                                                column(width = 11, align="left", offset = 1, 
                                                        HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step1: Upload you gene set</div>")),
                                                column(width = 1, align = "right", offset = 0,
                                                        shinyWidgets::dropdownButton(
                                                                tags$h4(p(icon("check",lib = "font-awesome"),em("Upload a list of gene sets, excluding column names, with the tab \"\\t\" as the separator, and refer to the following template file for the specific format."),style="color:#337ab7;text-align:center;font-size:15px;")),
                                                                column(width = 12, align = "center", downloadButton("download_geneset_template", label = "Template geneset", style = "width:200px;border-color: #ccc;font-size:12px;text-align: center;")),
                                                                column(width = 12, align = "center", downloadButton("download_geneset_template_log2fc", label = "Template geneset_log2fc", style = "width:200px;border-color: #ccc;font-size:12px;text-align: center;")),
                                                                circle = TRUE, status = "default",
                                                                icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                                                )),
                                                column(width = 11, align="left", offset = 0, style = "margin-bottom: -15px;",
                                                        fileInput(inputId = "enrich_gene_set_go", label = NULL, accept = ".txt", width = "340px")),
                                                        
                                                column(width = 11, align="left", offset = 1,
                                                        HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step2: Contain Log2FC column</div>")),
                                                column(width = 1, align = "right", offset = 0,
                                                        shinyWidgets::dropdownButton(
                                                                tags$h4(p(icon("check",lib = "font-awesome"),em("If the first column of the gene set file uploaded in Step1 is the gene and the second column is the log2fc value, select Contain, otherwise Not Contain."),style="color:#337ab7;text-align:center;font-size:15px;")),
                                                                circle = TRUE, status = "default",
                                                                icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                                                )),
                                                column(width = 11, align="left", offset = 0, 
                                                        shinyWidgets::radioGroupButtons(inputId = "enrich_log2fc_go", label = NULL, choices = c("Not contain", "Contain"), individual = TRUE, width = "100%",
                                                        checkIcon = list(yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"), no = tags$i(class = "fa fa-square-o",style = "color: steelblue")))),
                                                
                                                column(width = 11, align="left", offset = 1,
                                                        HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step3: Choose the species for GO</div>")),
                                                column(width = 1, align = "right", offset = 0,
                                                        shinyWidgets::dropdownButton(
                                                                tags$h4(p(icon("check",lib = "font-awesome"),em("Select the GO information of the species corresponding to your gene set (the same species has different genome versions, pay attention to distinguish)"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                                                circle = TRUE, status = "default",
                                                                icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                                                )),
                                                column(width = 11, align="left", offset = 0, style = "font-size: 14px;",
                                                        selectInput(inputId = "enrichanalysis_genome_go", label = NULL, choices = unlist(list(list.files("enrich_DB/genome_bg/GO"), "Upload other")), width = "340px")),
                                                br()
                                        ),
                                        uiOutput(outputId = "ui_enrich_option_go")
                                )
                        )# close fluidRow
                })
       })
    }else if (enrich_process_go$value == "processing") {
        if(!is.null(go_enrich())) {
                output$ui_enerichanalysis_go <- renderUI({
                        fluidRow(
                                br(),br(),
                                tags$head(tags$style(HTML('.form-control {height: 37.5px;border: 1px solid #000;}'))),
                                tags$head(tags$style(HTML('.selectize-input {border-color: black;line-height:23px; font-size:14px;height:38px}'))),
                                HTML('<hr style="width: 90%;border-top: 2px solid #000;" />'),
                                column(width = 12, align = "center",style = "width:40%;margin-left:30%;margin-bottom: 20px;", 
                                        htmlOutput(outputId = "enrich_info_go", style = "font-size:15px;"),
                                        column(width = 12, align = "center", downloadButton("download_enrich_bg_go", label = "Download background file"), style = "margin-bottom: 20px;")),
                                HTML('<hr style="width: 90%;border-top: 2px solid #000;" />'),
                                column(width = 12, align = "left", style = "width:80%;margin-left:10%;",
                                        column(width = 3, align = "left", h3("Gene ontology enrichment")),# close column
                                        column(width = 1, align = "left", offset = 0, style = "left:-60px;top: 15px;",
                                                shinyWidgets::dropdownButton(
                                                tags$h4(p(icon("check",lib = "font-awesome"),em("GO enrichment analysis, GO has three subclasses are Molecular Function, Biological Process, Cellular_Component, and each pathway belongs to a subclass"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                                tags$h4(p(icon("check",lib = "font-awesome"),em("GeneRatio (number of genes in the pathway/number of genes in the gene set)"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                                tags$h4(p(icon("check",lib = "font-awesome"),em("BgRatio (number of pathway genes in the background/number of all genes in the background)"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                                tags$h4(p(icon("check",lib = "font-awesome"),em("Pvalue (significance)"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                                tags$h4(p(icon("check",lib = "font-awesome"),em("P.adjust (corrected P-value)"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                                tags$h4(p(icon("check",lib = "font-awesome"),em("Richie Falktor (number of genes for this pathway in gene set/number of genes for this pathway in background)"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                                tags$h4(p(icon("check",lib = "font-awesome"),em("The mapping will be sorted by Pvalue first, and then the corresponding number of channels will be displayed according to the user's needs."),style="color:#337ab7;text-align:center;font-size:15px;")),
                                                circle = TRUE, status = "default",
                                                icon = icon("question"), width = "300px", size = "sm", right = F, up = T)
                                        ),
                                        HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'), br(),
                                        column(width = 12,offset = 0, align = "left", uiOutput("gotablesetting_ui")), # close column
                                        column(width = 12, style = "margin-bottom: 30px", uiOutput("go_table_type")), # close column
                                        
                                        HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'), br(),
                                        column(width = 12, align = "left", uiOutput("go_plot_ui"), style = "margin-bottom: 20px;") # close column
                                ), # close column
                                HTML('<hr style="width: 90%;border-top: 2px solid #000;" />'),
                                column(width = 12, align = "center", style = "margin-top: 30px;margin-bottom: 50px;",
                                        actionButton(inputId = "reset_enrich_go", label = "RESET", icon = icon("rotate-left"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;"))
                        )
                })
        }else {
           output$ui_enerichanalysis_go <- renderUI({
                fluidRow(
                        column(width = 12, align = "center", style = "",  
                        HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Running...</div>"))
                )
           })
        }
    }
})

output$ui_enrich_option_go <- renderUI({
    if(input$enrichanalysis_genome_go != "Upload other"){
        column(width = 12,
                column(width = 6, offset = 4,align="center", style="margin-left: 38.8%;",
                        column(width = 11, align="left", offset = 1,
                                HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step4: Choose the GO base version</div>")),
                        column(width = 1, align = "right", offset = 0,
                                shinyWidgets::dropdownButton(
                                        tags$h4(p(icon("check",lib = "font-awesome"),em("Select all the item information of GO, collect it on the official website of GO, and releases_date the official release date of the version information"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                        circle = TRUE, status = "default",
                                        icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                        )
                                ),
                        column(width = 11, align="left", offset = 0,
                                selectInput(inputId = "enrichanalysis_db_version_go", label = NULL, choices = unlist(list(list.files("enrich_DB/DB_version/GO"))), width = "340px"))
                ),
                column(width = 4, offset = 4,
                        column(width = 6, align="left", offset = 0, style="margin-bottom: 30px;margin-top: 50px;",
                                actionButton(inputId = "reset_enrich_go", label = "RESET", icon = icon("rotate-left"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;")),
                        column(width = 6, align="right", offset = 0, style="margin-bottom: 30px;margin-top: 50px;",
                                actionButton(inputId = "enter_enrich_go", label = "ENTER", icon = icon("check"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;"))
                )
        )
    } else {
        column(width = 12,
                column(width = 6, offset = 4,align="center", style="margin-left: 38.8%;",
                        column(width = 11, align="left", offset = 1,
                                HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step4: Upload GO annotation information</div>")),
                        column(width = 1, align = "right", offset = 0,
                                shinyWidgets::dropdownButton(
                                        tags$h4(p(icon("check",lib = "font-awesome"),em("Upload the GO annotation information of the species, a total of two columns, the first column is the gene number, the second column is the GO number, with the tab \"\\t\" as the separator, the specific format refers to the following file."),style="color:#337ab7;text-align:center;font-size:15px;")),
                                        column(width = 12, align = "center", downloadButton("download_go_gemone_template", label = "Template Species information", style = "width:200px;border-color: #ccc;font-size:12px;text-align: center;")),
                                        circle = TRUE, status = "default",
                                        icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                        )
                                ),
                        column(width = 11, align="left", offset = 0,
                                fileInput(inputId = "enrich_gene_background_go", label = NULL, accept = list(".txt"), width = "340px")),

                        column(width = 11, align="left", offset = 1,
                                HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step5: Choose the GO base version</div>")),
                        column(width = 1, align = "right", offset = 0,
                                shinyWidgets::dropdownButton(
                                        tags$h4(p(icon("check",lib = "font-awesome"),em("Select all the item information of GO, collect it on the official website of GO, and releases_date the official release date of the version information"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                        circle = TRUE, status = "default",
                                        icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                        )
                                ),
                        column(width = 11, align="left", offset = 0,
                                selectInput(inputId = "enrichanalysis_db_version_go", label = NULL, choices = unlist(list(list.files("enrich_DB/DB_version/GO"))), width = "340px"))
                ),
                column(width = 4, offset = 4,align="center", style="margin-left: 601px;",
                        column(width = 6, align="left", offset = 0, style="margin-bottom: 30px;margin-top: 50px;",
                                actionButton(inputId = "reset_enrich_go", label = "RESET", icon = icon("rotate-left"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;")),
                        column(width = 6, align="right", offset = 0, style="margin-bottom: 30px;margin-top: 50px;",
                                actionButton(inputId = "enter_enrich_go", label = "ENTER", icon = icon("check"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;"))
                )
        )
    }
})

#download template
output$download_go_gemone_template <- downloadHandler(#下载go_gemone_template模板
    filename = function() {"go_gemone_template[IRGSP].txt"},
    content = function(file) {
      data <- read.table("template/Oryza sativa japonica (IRGSP) [UP000059680]", sep = "\t")
      write.table(data, file, row.names = F, fileEncoding="GBK", quote = FALSE, col.names = F, sep = "\t")
    }
)

#file upload status
#GO
values_enrich <- reactiveValues(upload_enrich_gene_set_go = "NULL", upload_enrich_background_go = "uploaded")
observeEvent(input$enrich_gene_set_go, {values_enrich$upload_enrich_gene_set_go <- 'uploaded'})
observeEvent(input$enrichanalysis_genome_go, {
    if(input$enrichanalysis_genome_go == "Upload other"){
        values_enrich$upload_enrich_background_go <- 'NULL'
    }else if (input$enrichanalysis_genome_go != "Upload other") {
       values_enrich$upload_enrich_background_go <- 'uploaded'
    }
})
observeEvent(input$enrich_gene_background_go, {values_enrich$upload_enrich_background_go <- 'uploaded'})

#ENTER
observeEvent(input$enter_enrich_go, {
  if (values_enrich$upload_enrich_gene_set_go == "uploaded" & values_enrich$upload_enrich_background_go == 'uploaded') {
    if(input$enrichanalysis_genome_go == "Upload other"){
        genome_bg <- read.table(input$enrich_gene_background_go$datapath, sep = "\t", header = F)

        if(ncol(genome_bg)!=2) {shinyjs::runjs('alert("The GO annotation information must be in two columns, the first is the gene_id and the second is the GO id")');return(NULL)}
        colnames(genome_bg) <- c("gene_id", "go_id")
        go <- length(genome_bg[substr(genome_bg$go_id, start = 1, stop = 1)=="G","gene_id"])

        if(go < 100) {shinyjs::runjs('alert("The GO id in your GO annotation information ERROR, correct format is GO:*******")');return(NULL)}
        go_bg <- genome_bg[genome_bg$go_id != "", "gene_id"]

        }else {
        genome_bg <- read.csv(paste0("enrich_DB/genome_bg/GO/", input$enrichanalysis_genome_go), sep = "\t", header = F)
        go_bg <- genome_bg[,1]
        go_bg <- unique(go_bg)
        }

    geneset <- read.csv(input$enrich_gene_set_go$datapath, sep = "\t", header = F)

    if(input$enrich_log2fc_go == "Contain" & ncol(geneset) < 2 ) {shinyjs::runjs('alert("The log2FC value for the second column is not detected in the geneset file")');return(NULL)}
    if(input$enrich_log2fc_go == "Contain" ) {if(all(is.numeric(geneset[,2])) == FALSE) {shinyjs::runjs('alert("The log2FC vcolumn must be numeric")');return(NULL)}}
    
    if(length(geneset[,1])>20000) {shinyjs::runjs('alert("The number of genes in your gene set cannot be greater than 20,000")');return(NULL)}

    go_bg <- length(intersect(go_bg, geneset[,1]))
    if(go_bg < 20) {
        shinyjs::runjs('alert("The num of intersections between your gene set and the GO annotation information gene set is less than 20, please check again")');return(NULL)
    }

    enrich_process_go$value <- "processing"
  } else {
     shinyjs::runjs('alert("ERROR: Please upload gene set file or species information file first.")')
     enrich_process_go$value <- "start"
        return(NULL)
  }
})
# RESET
observeEvent(input$reset_enrich_go, {
  enrich_process_go$value <- "start"
  shinyjs::reset("enrich_gene_set_go")
  shinyjs::reset("enrich_log2fc_go")
  shinyjs::reset("enrich_gene_background_go")
  shinyjs::reset("enrichanalysis_genome_go")
  shinyjs::reset("enrichanalysis_db_version_go")
  values_enrich$upload_enrich_gene_set_go = "NULL"
  values_enrich$upload_enrich_background_go = "uploaded"
})
#source enrich base
go_base <- reactive({
    if(enrich_process_go$value == "processing") {
        geneset <- read.csv(input$enrich_gene_set_go$datapath, sep = "\t", header = F)[,1]
        go_db <- read.table(paste0("enrich_DB/DB_version/GO/", input$enrichanalysis_db_version_go), sep = "\t", header = T)
        if(input$enrichanalysis_genome_go == "Upload other"){
                genome_bg <- read.table(input$enrich_gene_background_go$datapath, sep = "\t", header = F)
                colnames(genome_bg) <- c("gene_id", "go_id")
        }else {
                genome_bg <- read.table(paste0("enrich_DB/genome_bg/GO/", input$enrichanalysis_genome_go), sep = "\t", header = F)
                colnames(genome_bg) <- c("gene_id", "go_id")
        }
        TERM2GENE <- data.frame(id = genome_bg$go_id, gene = genome_bg$gene_id)
        TERM2GENE <- TERM2GENE[!duplicated(TERM2GENE),]
        TERM2NAME <- data.frame(id = go_db$go_id, pathway = go_db$pathway)
        TERM2NAME <- TERM2NAME[!duplicated(TERM2NAME),]

        enrich_bg <- list(TERM2GENE = TERM2GENE, TERM2NAME = TERM2NAME)

        return(enrich_bg)
    }else {
        return(NULL)
    }
})
#go enrich
output$enrich_info_go <- renderText({#enrich INFO UI
       if(enrich_process_go$value == "processing" & !is.null(go_base())) {
        geneset_filename <- paste0("File name: ", input$enrich_gene_set_go[1])
        if(input$enrichanalysis_genome_go != "Upload other"){
                bg_file <- paste0("Background species: ", input$enrichanalysis_genome_go)
        }else {
           bg_file <- paste0("Background species: Uploaded")
        }

        db_version <- paste0("Data base version: ", input$enrichanalysis_db_version_go)

        gene_set_num <- read.csv(input$enrich_gene_set_go$datapath, sep = "\t", header = T)[,1]
        gene_set_num <- paste0("Num of genes (gene set): ", length(unique(gene_set_num)))

        go_bg_num <- go_base()$TERM2GENE$gene
        go_bg_num <- paste0("Num of genes (GO background): ", length(unique(go_bg_num)))


        a <- HTML(paste0("<div style='font-size:20px;font-weight:600;line-height: 1em;'><br/>GO enrich basic information</div>",
                        "<br/>", geneset_filename, "<br/>", bg_file, "<br/>", db_version, "<br/>", gene_set_num, "<br/>", go_bg_num,"<br/><br/>"))
       }else {
          return(NULL)
       }
})
output$download_enrich_bg_go <- downloadHandler(#下载enrich DB
    filename = function() {ifelse(input$enrichanalysis_genome_go == "Upload other", paste(input$enrichanalysis_db_version_go, ".tar.gz"), paste(input$enrichanalysis_genome_go, ".tar.gz"))},
    content = function(file) {
        if(input$enrichanalysis_genome_go != "Upload other"){
                bg_file <- paste0("enrich_DB/genome_bg/GO/", input$enrichanalysis_genome_go)
                db_file <- paste0("enrich_DB/DB_version/GO/", input$enrichanalysis_db_version_go)
                # print(bg_file)
                # print(db_file)
                utils::tar(file, c(bg_file, db_file), compression = 'gzip', tar="tar")
        }else {
           bg_file <- paste0("enrich_DB/DB_version/GO/", input$enrichanalysis_db_version_go)
           R.utils::gzip(filename = bg_file, destname = file, remove = F)
        }
    }
)
go_enrich <- reactive({
    if(enrich_process_go$value == "processing" & !is.null(go_base())){
        if(input$enrich_log2fc_go == "Contain"){

                geneset <- read.csv(input$enrich_gene_set_go$datapath, sep = "\t", header = T)
                colnames(geneset) <- c("gene_id", "log2fc")
                upset <- geneset[geneset$log2fc > 0 , "gene_id"]
                downset <- geneset[geneset$log2fc < 0 , "gene_id"]

                TERM2GENE <- go_base()$TERM2GENE
                TERM2NAME <- go_base()$TERM2NAME

                rich_up <- clusterProfiler::enricher(upset,TERM2GENE=TERM2GENE,
                        TERM2NAME=TERM2NAME,
                        pvalueCutoff=1,
                        qvalueCutoff =1,
                        pAdjustMethod = "BH")
                rich_up <- as.data.frame(rich_up)
                rich_up <- mutate(rich_up, RichFactor = Count / as.numeric(sub("/\\d+", "", BgRatio)))
                rich_up <- data.frame(Pathway_ID = rich_up$ID, Description = rich_up$Description, GeneRatio = rich_up$GeneRatio, BgRatio = rich_up$BgRatio, Pvalue = rich_up$pvalue, P.adjust = rich_up$p.adjust, RichFactor = rich_up$RichFactor, Genes = rich_up$geneID)
                

                rich_down <- clusterProfiler::enricher(downset,TERM2GENE=TERM2GENE,
                        TERM2NAME=TERM2NAME,
                        pvalueCutoff=1,
                        qvalueCutoff =1,
                        pAdjustMethod = "BH")
                rich_down <- as.data.frame(rich_down)
                rich_down <- mutate(rich_down, RichFactor = Count / as.numeric(sub("/\\d+", "", BgRatio)))
                rich_down <- data.frame(Pathway_ID = rich_down$ID, Description = rich_down$Description, GeneRatio = rich_down$GeneRatio, BgRatio = rich_down$BgRatio, Pvalue = rich_down$pvalue, P.adjust = rich_down$p.adjust, RichFactor = rich_down$RichFactor, Genes = rich_down$geneID)
                
                go_db <- read.table(paste0("enrich_DB/DB_version/GO/", input$enrichanalysis_db_version_go), sep = "\t", header = T)

                go_db <- go_db[!duplicated(go_db),]
                names(go_db)[names(go_db) == "go_id"] <- "Pathway_ID"

                rich_up <- dplyr::inner_join(go_db, rich_up, by = "Pathway_ID", relationship = "many-to-many")# add classA, classB, classC
                rich_down <- dplyr::inner_join(go_db, rich_down, by = "Pathway_ID", relationship = "many-to-many")# add classA, classB, classC

                rich <- list(rich_up = rich_up, rich_down = rich_down)
                return(rich)

        }else if (input$enrich_log2fc_go == "Not contain") {
                geneset <- read.csv(input$enrich_gene_set_go$datapath, sep = "\t", header = F)[,1]
                TERM2GENE <- go_base()$TERM2GENE
                TERM2NAME <- go_base()$TERM2NAME
                # print(head(TERM2GENE)) #for debug
                # print(head(TERM2NAME)) #for debug

                rich <- clusterProfiler::enricher(geneset,TERM2GENE=TERM2GENE,
                        TERM2NAME=TERM2NAME,
                        pvalueCutoff=1,
                        qvalueCutoff =1,
                        pAdjustMethod = "BH")
                rich <- as.data.frame(rich)
                rich <- mutate(rich, RichFactor = Count / as.numeric(sub("/\\d+", "", BgRatio)))
                rich <- data.frame(Pathway_ID = rich$ID, Description = rich$Description, GeneRatio = rich$GeneRatio, BgRatio = rich$BgRatio, Pvalue = rich$pvalue, P.adjust = rich$p.adjust, RichFactor = rich$RichFactor, Genes = rich$geneID)
                
                go_db <- read.table(paste0("enrich_DB/DB_version/GO/", input$enrichanalysis_db_version_go), sep = "\t", header = T)
                go_db <- go_db[!duplicated(go_db),]
                names(go_db)[names(go_db) == "go_id"] <- "Pathway_ID"   
                
                rich <- dplyr::inner_join(go_db, rich, by = "Pathway_ID", relationship = "many-to-many")# add pathway ontology
                return(rich)
        }
    }else {
       return(NULL)
    }
})

output$go_table_type <- renderUI({
        if(input$enrich_log2fc_go == "Contain"){
                fluidPage(
                        column(width = 2, align = "left", style="background-color: #ff000014;height: 50px;width: 300px;font-weight: 900;", HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("circle-up")," UP regulation</p>"))),
                        column(width = 12, DT::dataTableOutput(outputId = "go_resault", width = "100%"), style="background-color: #ff000014;margin-bottom: 20px;"),
                        column(width = 2, align = "left", style="background-color: #00acff14;height: 50px;width: 300px;font-weight: 900;", HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("circle-down")," DOWN regulation</p>"))),
                        column(width = 12, DT::dataTableOutput(outputId = "go_resault_2", width = "100%"), style="background-color: #00acff14;")
                )
        }else {
           DT::dataTableOutput(outputId = "go_resault", width = "102%")
        }
})
go_table <- reactive({
    if(enrich_process_go$value == "processing" & !is.null(go_enrich())){
        pvaluecuttoff <- ifelse(is.null(input$go_pvaluecuttoff), 1, input$go_pvaluecuttoff)
        padjcuttoff <- ifelse(is.null(input$go_padjcuttoff), 1, input$go_padjcuttoff)

        if(input$enrich_log2fc_go == "Contain"){
                rich_up <- go_enrich()$rich_up
                rich_down <- go_enrich()$rich_down
                rich_up <- rich_up[rich_up$Pvalue <= pvaluecuttoff & rich_up$P.adjust <= padjcuttoff,]
                rich_down <- rich_down[rich_down$Pvalue <= pvaluecuttoff & rich_down$P.adjust <= padjcuttoff,]
                rich_up<-rich_up[order(rich_up$Pvalue),]
                rich_down<-rich_down[order(rich_down$Pvalue),]


                if(nrow(rich_up)>1){rownames(rich_up) <- 1:nrow(rich_up)} # nolint
                if(nrow(rich_down)>1){rownames(rich_down) <- 1:nrow(rich_down)} # nolint
                rich <- list(rich_up = rich_up, rich_down = rich_down)
                return(rich)
        }else {
                rich <- go_enrich()
                rich <- rich[rich$Pvalue <= pvaluecuttoff & rich$P.adjust <= padjcuttoff,]
                rich<-rich[order(rich$Pvalue),]
                if(nrow(rich)>1){rownames(rich) <- 1:nrow(rich)} # nolint
                rich <- list(rich_up = rich, rich_down = NULL)
                return(rich)
        }

    }else {
       return(NULL)
    }
})
output$gotablesetting_ui <- renderUI({
        if(!is.null(go_enrich())) {
                column(width = 12, 
                        column(width = 12,
                                column(width = 2, align = "left", offset = 5, style="", numericInput(inputId = "go_pvaluecuttoff", label = HTML("<p style='height: 1px;font-size: 10px;'>Pvalue cutoff</p>"), min = 0, value = 1, max = 1)),
                                column(width = 2, align = "left", style="",  numericInput(inputId = "go_padjcuttoff", label = HTML("<p style='height: 1px;font-size: 10px;'>P.adjust cutoff</p>"), min = 0, value = 1, max = 1)),
                                column(width = 3, align = "right", style = "margin-bottom:20px;top: 20px;", downloadButton(outputId = "download_go_table", label = "Download Full GO enrich table"))
                              )
                        )
        }else {
           return(NULL)
        }
})
output$go_resault <- DT::renderDataTable(go_table()$rich_up[,-c(2,10)], options = list(dom = 'fltipr', pageLength = 10, scrollX=TRUE, orderClasses = T, 
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
output$go_resault_2 <- DT::renderDataTable(go_table()$rich_down[,-c(2,10)], options = list(dom = 'fltipr', pageLength = 10, scrollX=TRUE, orderClasses = T, 
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

output$go_plot_ui <- renderUI({
        if(!is.null(go_table())){
                fluidRow(
                        column(width = 12, style = "margin-bottom: 60px;margin-top: 20px;",
                                column(width = 12, offset = 0, plotOutput("go_plot_dot", width = "100%", height = "800px")),
                                
                                column(width = 12,style = "margin-top: 50px;",
                                        column(width = 6, shinyWidgets::radioGroupButtons(
                                                inputId = "go_plot", label = NULL, choices = c("Dot plot", "Bar plot"), individual = F, justified = T, width = '641px',
                                                checkIcon = list(yes = tags$i(class = "fa fa-circle", style = "color: steelblue"),
                                                                 no = tags$i(class = "fa fa-circle-o", style = "color: steelblue")))),
                                        column(width = 3, colourpicker::colourInput("color_go_plot_hight", label = NULL, "red"), style = "margin-bottom: -0px;"),
                                        column(width = 3, colourpicker::colourInput("color_go_plot_low", label = NULL, "darkblue"), style = "margin-bottom: -0px;"),
                                ),
                                column(width = 12,
                                        column(width = 3, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Item</div>")),
                                        column(width = 3, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Rank by</div>")),
                                        column(width = 3, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Y text length</div>")),
                                        column(width = 3, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Reset</div>")),
                                        column(width = 3, numericInput("go_num_of_item", label = NULL, value = 10, max = 20, min = 5), style = "margin-bottom: -5px;"),
                                        column(width = 3, selectInput(inputId = "go_rank_by", label = NULL, choices = c("RichFactor", "Pvalue", "P.adjust", "Count")), style = "margin-bottom: -5px;"),
                                        column(width = 3, numericInput("go_y_text_length", label = NULL, value = 60, min = 0), style = "margin-bottom: -5px;"),
                                        column(width = 3, actionButton(inputId = "reset_go_plot", label = NULL, icon = icon("arrows-rotate"),style = "font-size: 17px;margin-bottom: -10px;WIDTH: 300px;margin-bottom: 10px;")),
                                ),
                                column(width = 12,
                                        column(width = 3, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>X-axis text size</div>")),
                                        column(width = 3, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Y-axis text size</div>")),
                                        column(width = 3, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>X-axis title size</div>")),
                                        column(width = 3, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Y-axis title size</div>")),
                                        column(width = 3, numericInput("go_x_text_size", label = NULL, value = 8, min = 0), style = "margin-bottom: -5px;"),
                                        column(width = 3, numericInput("go_y_text_size", label = NULL, value = 14, min = 0), style = "margin-bottom: -5px;"), 
                                        column(width = 3, numericInput("go_x_title_size", label = NULL, value = 14, min = 0), style = "margin-bottom: -5px;"),
                                        column(width = 3, numericInput("go_y_title_size", label = NULL, value = 20, min = 0), style = "margin-bottom: -5px;"),
                                ),
                                column(width = 12,
                                        column(width = 3, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Graph width</div>")),
                                        column(width = 3, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Graph height</div>")),
                                        column(width = 3, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Graph format</div>")),
                                        column(width = 3, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Download</div>")),
                                        column(width = 3, numericInput("go_width", label = NULL, value = 10), style = "margin-bottom: -10px;", style = "margin-bottom: -5px;"),
                                        column(width = 3, numericInput("go_height", label = NULL, value = 6), style = "margin-bottom: -10px;", style = "margin-bottom: -5px;"),
                                        column(width = 3, selectInput(inputId = "go_plot_format", label = NULL, choices = c("PNG" = "png", "PDF" = "pdf")), style = "margin-bottom: -10px;"),
                                        column(width = 3, downloadButton("download_go_bar_plot", label = NULL, style = "width:300px;"))
                                )
                              )
                )#close fluiRow
        }else {
           return(NULL)
        }
})
output$download_go_table <- downloadHandler(#download go full table
  filename = function() {paste0("GO_enrich.xlsx")},
  content = function(file) {
    if(!is.null(go_enrich())){
        if(input$enrich_log2fc_go=="Contain"){
                rich_up <- go_enrich()$rich_up
                rich_up <- na.omit(rich_up)
                rich_down <- go_enrich()$rich_down
                rich_down <- na.omit(rich_down)
                list_data <- list("GO_up_regulation" = rich_up, "GO_down_regulation" = rich_down)
                openxlsx::write.xlsx(list_data, file, fileEncoding="GBK")

        }else {
                a <- go_enrich()
                a <- na.omit(a)
                openxlsx::write.xlsx(a, file, sheetName = "GO", fileEncoding="GBK")
        }
      
    }
  }
)