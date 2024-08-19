#Shiny-candidater
library(shiny)
candidater_process <- reactiveValues()
candidater_process$value <- "start"

output$ui_coanalysis3 <- renderUI({
    if(candidater_process$value == "start") {
        fluidRow(
            br(),
            fluidRow(
                column(width = 4, align="left", offset = 4,
                    HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step1: Upload peaks anno file by 'Peak annotation' module</div>")),
                column(width = 4, offset = 4, align = "center",  fileInput("candidater_upload_peakanno", label = NULL, width = "100%", accept = ".xlsx"))
            ),br(),
            fluidRow(
                column(width = 4, align="left", offset = 4,
                    HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step2: Upload DGEs result table by 'DEGs' module</div>")),
                column(width = 4, align="center", offset = 4, 
                    shinyWidgets::radioGroupButtons(inputId = "candidater_degs_count",label = NULL, choices = c(1, 2, 3), selected = 1, justified = TRUE,checkIcon = list(yes = icon("ok", lib = "glyphicon"))))
            ),br(),
            uiOutput("candidater_upload_degs_ui"),br(),
            fluidRow(
                column(width = 4, align="left", offset = 4,
                    HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step3: Select you species genomes</div>")),
                column(width = 4, offset = 4, align = "center",  selectInput(inputId = "candidater_species", label = NULL, choices = unlist(list(list.files("txdb/txdb"))), width = "100%"))
            ),br(),
            fluidRow(
                column(width = 4, align="left", offset = 4,
                    HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step4: Select the gene region you are interested in</div>")),
                column(width = 4, align="center", offset = 4,
                    shinyWidgets::radioGroupButtons(inputId = "candidater_gene_region",label = NULL, choices = c("Promoter", "Exon", "Intron", "Downstream"), selected = "Promoter", justified = TRUE,checkIcon = list(yes = icon("ok", lib = "glyphicon"))))
            ),br(),
            fluidRow(style = "margin-top: 50px;margin-bottom: 30px;",
                column(width = 3, align="center", offset = 3,
                    actionButton(inputId = "reset_candidater", label = "RESET", icon = icon("rotate-left"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;")),
                column(width = 3, align="center", offset = 0,
                    actionButton(inputId = "enter_candidater", label = "ENTER", icon = icon("check"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;"))
            )# close fluidRow
        )
    }else if (candidater_process$value == "process") {
       fluidRow(
            HTML('<hr style="width: 100%;border-top: 2px solid #000;" />'),
            column(width = 12, align = "left", style="background-color: #00000012;font-weight: 900;border-radius: 15px;", htmlOutput(outputId = "candidater_info", style = "font-size:15px;")),
            column(width = 12, align = "center", style="background-color: #00000012;border-radius: 15px;", uiOutput("candidater_cutoff_ui")),
            column(width = 12, DT::dataTableOutput(outputId = "candidater_result", width = "100%"), style="background-color: #d3f2ff;margin-bottom: 20px; font-size: 12px;border-radius: 15px"),
            br(),
            uiOutput("candidater_structure_ui"),
            HTML('<hr style="width: 100%;border-top: 2px solid #000;" />'),
       
       )
    }
})

output$candidater_upload_degs_ui <- renderUI({
    if(input$candidater_degs_count == 1){
        fluidRow(
            column(width = 1, offset = 4, align="center", textInput("candidater_degs1_name", label = "Table 1:", value = "DEGs 1")),
            column(width = 3, offset = 0, align = "center",  fileInput("candidater_upload_degs1", label = "", width = "100%", accept = ".xlsx")),
        )
    } else if (input$candidater_degs_count == 2) {
        fluidRow(
            column(width = 1, align="center", offset = 4, textInput("candidater_degs1_name", label = "Table 1:", value = "DEGs 1")),
            column(width = 3, offset = 0, align = "center",  fileInput("candidater_upload_degs1", label = "", width = "100%", accept = ".xlsx")),
            column(width = 1, align="center", offset = 4, textInput("candidater_degs2_name", label = "Table 2:", value = "DEGs 2")),
            column(width = 3, offset = 0, align = "center",  fileInput("candidater_upload_degs2", label = "", width = "100%", accept = ".xlsx"))
        )
    }else {
        fluidRow(
            column(width = 1, align="center", offset = 4, textInput("candidater_degs1_name", label = "Table 1:", value = "DEGs 1")),
            column(width = 3, offset = 0, align = "center",  fileInput("candidater_upload_degs1", label = "", width = "100%", accept = ".xlsx")),
            column(width = 1, align="center", offset = 4, textInput("candidater_degs2_name", label = "Table 2:", value = "DEGs 2")),
            column(width = 3, offset = 0, align = "center",  fileInput("candidater_upload_degs2", label = "", width = "100%", accept = ".xlsx")),
            column(width = 1, align="center", offset = 4, textInput("candidater_degs3_name", label = "Table 3:", value = "DEGs 3")),
            column(width = 3, offset = 0, align = "center",  fileInput("candidater_upload_degs3", label = "", width = "100%", accept = ".xlsx"))
        )
    }
})

#
candidater_values <- reactiveValues(upload_anno = "NULL", upload_degs1 = "NULL", upload_degs2 = "NULL", upload_degs3 = "NULL")
observeEvent(input$candidater_upload_peakanno, {candidater_values$upload_anno <- "uploaded"})
observeEvent(input$candidater_upload_degs1, {candidater_values$upload_degs1 <- "uploaded"})
observeEvent(input$candidater_upload_degs2, {candidater_values$upload_degs2 <- "uploaded"})
observeEvent(input$candidater_upload_degs3, {candidater_values$upload_degs3 <- "uploaded"})

#ENTER
observeEvent(input$enter_candidater, {
    if(candidater_values$upload_anno == "uploaded"){
        peak_anno <- openxlsx::read.xlsx(input$candidater_upload_peakanno$datapath, colNames = TRUE, rowNames = F)
        if(ncol(peak_anno) != 13 & ncol(peak_anno) != 17 & ncol(peak_anno) != 14 & ncol(peak_anno) != 18){
            print(ncol(peak_anno))
            candidater_process$value = "start"
            shinyjs::runjs('alert("STEP1 ERROR: Please upload the Peak Anno file analyzed by the <Peak annotation> module!")')
            shinyjs::reset("candidater_upload_peakanno")
            shinyjs::reset("candidater_degs_count")
            shinyjs::reset("candidater_upload_degs1")
            shinyjs::reset("candidater_upload_degs2")
            shinyjs::reset("candidater_upload_degs3")
            candidater_values = "NULL"
            return(NULL)
        }
        peak_genes = unlist(peak_anno[, "geneId"])
        gff_gene <- unlist(read.table(paste0("txdb/",input$candidater_species), sep = "\t"))
        gene_set <- intersect(peak_genes, gff_gene)

        if(input$candidater_degs_count==1 & candidater_values$upload_degs1=="uploaded"){
            deg1 <- openxlsx::read.xlsx(input$candidater_upload_degs1$datapath, colNames = TRUE, rowNames = F)
            if (ncol(deg1) == 11) {
                deg1_gene <- unlist(deg1[, "Gene"])
                gene_set <- intersect(gene_set, deg1_gene)
                if(length(gene_set) < 10){
                    candidater_process$value = "start"
                    shinyjs::runjs('alert("ERROR: At present, only rice, maize, and Arabidopsis thaliana genomes are supported, please check whether the genome is selected correctly!")')
                    shinyjs::reset("candidater_upload_peakanno")
                    shinyjs::reset("candidater_degs_count")
                    shinyjs::reset("candidater_upload_degs1")
                    shinyjs::reset("candidater_upload_degs2")
                    shinyjs::reset("candidater_upload_degs3")
                    candidater_values = "NULL"
                    return(NULL)
                }
                candidater_process$value = "process"
            }else {
                candidater_process$value = "start"
                shinyjs::runjs('alert("STEP2 ERROR: Please upload the DEGs result file analyzed by the <DEGs> module!")')
                shinyjs::reset("candidater_upload_peakanno")
                shinyjs::reset("candidater_degs_count")
                shinyjs::reset("candidater_upload_degs1")
                shinyjs::reset("candidater_upload_degs2")
                shinyjs::reset("candidater_upload_degs3")
                candidater_values = "NULL"
                return(NULL)
            }
        }else if (input$candidater_degs_count==2 & candidater_values$upload_degs1=="uploaded" & candidater_values$upload_degs2=="uploaded") {
            deg1 <- openxlsx::read.xlsx(input$candidater_upload_degs1$datapath, colNames = TRUE, rowNames = F)
            deg2 <- openxlsx::read.xlsx(input$candidater_upload_degs2$datapath, colNames = TRUE, rowNames = F)
            if (ncol(deg1) == 11 & ncol(deg2) == 11) {
                deg1_gene <- unlist(deg1[, "Gene"])
                gene_set <- intersect(gene_set, deg1_gene)
                deg2_gene <- unlist(deg2[, "Gene"])
                gene_set <- intersect(gene_set, deg2_gene)
                if(length(gene_set) < 10){
                    candidater_process$value = "start"
                    shinyjs::runjs('alert("ERROR: At present, only rice, maize, and Arabidopsis thaliana genomes are supported, please check whether the genome is selected correctly!")')
                    shinyjs::reset("candidater_upload_peakanno")
                    shinyjs::reset("candidater_degs_count")
                    shinyjs::reset("candidater_upload_degs1")
                    shinyjs::reset("candidater_upload_degs2")
                    shinyjs::reset("candidater_upload_degs3")
                    candidater_values = "NULL"
                    return(NULL)
                }
                candidater_process$value = "process"
            }else {
                candidater_process$value = "start"
                shinyjs::runjs('alert("STEP2 ERROR: Please upload the DEGs result file analyzed by the <DEGs> module!")')
                shinyjs::reset("candidater_upload_peakanno")
                shinyjs::reset("candidater_degs_count")
                shinyjs::reset("candidater_upload_degs1")
                shinyjs::reset("candidater_upload_degs2")
                shinyjs::reset("candidater_upload_degs3")
                candidater_values = "NULL"
                return(NULL)
            }
        }else if (candidater_values$upload_degs1=="uploaded" & candidater_values$upload_degs2=="uploaded" & candidater_values$upload_degs3=="uploaded" ) {
            deg1 <- openxlsx::read.xlsx(input$candidater_upload_degs1$datapath, colNames = TRUE, rowNames = F)
            deg2 <- openxlsx::read.xlsx(input$candidater_upload_degs2$datapath, colNames = TRUE, rowNames = F)
            deg3 <- openxlsx::read.xlsx(input$candidater_upload_degs3$datapath, colNames = TRUE, rowNames = F)
            if (ncol(deg1) == 11 & ncol(deg2) == 11 & ncol(deg3) == 11) {
                deg1_gene <- unlist(deg1[, "Gene"])
                gene_set <- intersect(gene_set, deg1_gene)
                deg2_gene <- unlist(deg2[, "Gene"])
                gene_set <- intersect(gene_set, deg2_gene)
                deg3_gene <- unlist(deg3[, "Gene"])
                gene_set <- intersect(gene_set, deg3_gene)
                if(length(gene_set) < 10){
                    candidater_process$value = "start"
                    shinyjs::runjs('alert("ERROR: At present, only rice, maize, and Arabidopsis thaliana genomes are supported, please check whether the genome is selected correctly!")')
                    shinyjs::reset("candidater_upload_peakanno")
                    shinyjs::reset("candidater_degs_count")
                    shinyjs::reset("candidater_upload_degs1")
                    shinyjs::reset("candidater_upload_degs2")
                    shinyjs::reset("candidater_upload_degs3")
                    candidater_values = "NULL"
                    return(NULL)
                }
                candidater_process$value = "process"
            }else {
                candidater_process$value = "start"
                shinyjs::runjs('alert("STEP2 ERROR: Please upload the DEGs result file analyzed by the <DEGs> module!")')
                shinyjs::reset("candidater_upload_peakanno")
                shinyjs::reset("candidater_degs_count")
                shinyjs::reset("candidater_upload_degs1")
                shinyjs::reset("candidater_upload_degs2")
                shinyjs::reset("candidater_upload_degs3")
                candidater_values = "NULL"
                return(NULL)
            }
        }else {
            candidater_process$value = "start"
            shinyjs::runjs('alert("ERROR: Please upload your peak annotation file and DEGs table!")')
            shinyjs::reset("candidater_upload_peakanno")
            shinyjs::reset("candidater_degs_count")
            shinyjs::reset("candidater_upload_degs1")
            shinyjs::reset("candidater_upload_degs2")
            shinyjs::reset("candidater_upload_degs3")
            candidater_values = "NULL"
            return(NULL)
        }
    }else {
        candidater_process$value = "start"
        shinyjs::runjs('alert("ERROR: Please upload your peak annotation file and DEGs table!")')
        shinyjs::reset("candidater_upload_peakanno")
        shinyjs::reset("candidater_degs_count")
        shinyjs::reset("candidater_upload_degs1")
        shinyjs::reset("candidater_upload_degs2")
        shinyjs::reset("candidater_upload_degs3")
        candidater_values = "NULL"
        return(NULL)
    }
})
#RESET
observeEvent(input$reset_candidater, {
        candidater_process$value = "start"
        shinyjs::runjs('alert("ERROR: Please upload your peak annotation file and DEGs table!")')
        shinyjs::reset("candidater_upload_peakanno")
        shinyjs::reset("candidater_degs_count")
        shinyjs::reset("candidater_upload_degs1")
        shinyjs::reset("candidater_upload_degs2")
        shinyjs::reset("candidater_upload_degs3")
        candidater_values = "NULL"
})

#co analysis
co_analysis <- reactive({
    if(candidater_process$value == "process"){
        peak_anno <- openxlsx::read.xlsx(input$candidater_upload_peakanno$datapath, colNames = TRUE, rowNames = F)
        gene_region <- input$candidater_gene_region
        gff_file <- paste0("txdb/gff/", input$candidater_species)

        peak_anno <- peak_anno %>%
            dplyr::mutate(annotation = case_when(
                stringr::str_detect(annotation, "Exon") ~ "Exon",
                stringr::str_detect(annotation, "Intron") ~ "Intron",
                stringr::str_detect(annotation, "Promoter") ~ "Promoter",
                stringr::str_detect(annotation, "Downstream") ~ "Downstream",
                TRUE ~ annotation
            ))
        
        peak_anno <- peak_anno[peak_anno$annotation == gene_region,]
        peak_anno <- as.data.frame(peak_anno)

        if (ncol(peak_anno) == 13 | ncol(peak_anno) == 14) {
            peak_anno <- data.frame(Gene = peak_anno$geneId,
                                    # Gene_chr = peak_anno$geneChr,
                                    # Gene_stand = peak_anno$geneStrand,
                                    # Gene_length = peak_anno$geneLength,
                                    # Gene_start = peak_anno$geneStart,
                                    # Gene_end = peak_anno$geneEnd,
                                    Peak_distToTSS = peak_anno$distanceToTSS,
                                    Peak_pvalue = round(10^(-peak_anno$'-log10pvalue'),10)
                                    )
        }else {
            peak_anno <- data.frame(Gene = peak_anno$geneId,
                                    # Gene_chr = peak_anno$geneChr,
                                    # Gene_stand = peak_anno$geneStrand,
                                    # Gene_length = peak_anno$geneLength,
                                    # Gene_start = peak_anno$geneStart,
                                    # Gene_end = peak_anno$geneEnd,
                                    Peak_distToTSS = peak_anno$distanceToTSS,
                                    Peak_enrich = peak_anno$'fold.change',
                                    Peak_pvalue = round(10^(-peak_anno$'-log10pvalue'),10)
                                    )
        }

        if (!is.null(input$candidater_upload_degs1)) {
            deg1 <- openxlsx::read.xlsx(input$candidater_upload_degs1$datapath, colNames = TRUE, rowNames = F)
            deg1$mean_cpm <- apply(deg1[, 6:ncol(deg1)], 1, mean)
            colnames(deg1)[1:5] <- c("Gene", paste0(input$candidater_degs1_name, "_Change"), 
                                             paste0(input$candidater_degs1_name, "_log2fc"), 
                                             paste0(input$candidater_degs1_name, "_pvalue"), 
                                             paste0(input$candidater_degs1_name, "_padj")
                    )
            colnames(deg1)[ncol(deg1)] <- paste0(input$candidater_degs1_name, "_meanCPM")
            deg1 <- deg1[,c(1,3,4, ncol(deg1))]
            deg1[, 2:4] <- round(deg1[, 2:4], 3)
            deg1 <- deg1[deg1$Gene %in% peak_anno$Gene, ]
            peak_anno <- dplyr::inner_join(peak_anno, deg1, by = "Gene")
        }
        if (!is.null(input$candidater_upload_degs2)) {
            deg2 <- openxlsx::read.xlsx(input$candidater_upload_degs2$datapath, colNames = TRUE, rowNames = F)
            deg2$mean_cpm <- apply(deg2[, 6:ncol(deg2)], 1, mean)
            colnames(deg2)[1:5] <- c("Gene", paste0(input$candidater_degs2_name, "_Change"), 
                                             paste0(input$candidater_degs2_name, "_log2fc"),
                                             paste0(input$candidater_degs2_name, "_pvalue"),
                                             paste0(input$candidater_degs2_name, "_padj")
                    )
            colnames(deg2)[ncol(deg2)] <- paste0(input$candidater_degs2_name, "_meanCPM")
            deg2 <- deg2[,c(1,3,4, ncol(deg2))]
            deg2[, 2:4] <- round(deg2[, 2:4], 3)
            deg2 <- deg2[deg2$Gene %in% peak_anno$Gene, ]
            peak_anno <- dplyr::inner_join(peak_anno, deg2, by = "Gene")
        }
        if (!is.null(input$candidater_upload_degs3)) {
            deg3 <- openxlsx::read.xlsx(input$candidater_upload_degs3$datapath, colNames = TRUE, rowNames = F)
            deg3$mean_cpm <- apply(deg3[, 6:ncol(deg3)], 1, mean)
            colnames(deg3)[1:5] <- c("Gene", paste0(input$candidater_degs3_name, "_Change"), 
                                             paste0(input$candidater_degs3_name, "_log2fc"), 
                                             paste0(input$candidater_degs3_name, "_pvalue"), 
                                             paste0(input$candidater_degs3_name, "_padj")
                    )
            colnames(deg3)[ncol(deg3)] <- paste0(input$candidater_degs3_name, "_meanCPM")
            deg3 <- deg3[,c(1,3,4, ncol(deg3))]
            deg3[, 2:4] <- round(deg3[, 2:4], 3)
            deg3 <- deg3[deg3$Gene %in% peak_anno$Gene, ]
            peak_anno <- dplyr::inner_join(peak_anno, deg3, by = "Gene")
        }



        return(peak_anno)

    }else {
       return(NULL)
    }
})
# candidater info
output$candidater_info <- renderText({
    req(co_analysis())
    base <- paste("<div style='font-size:30px;font-weight:600;line-height: 1em;'><br/>Candidater explore run info:<br/>")
    anno_file <- paste("<div style='font-size:15px;font-weight:500;line-height: 1em;'><br/>Peak anno file: ",input$candidater_upload_peakanno$name,"<br/>")
    if(!is.null(input$candidater_upload_degs1)){deg1_file <- paste("<div style='font-size:15px;font-weight:500;line-height: 1em;'><br/>DEGs 1 file: ",input$candidater_upload_degs1$name,"<",input$candidater_degs1_name,">","<br/>")}
    if(!is.null(input$candidater_upload_degs2)){deg2_file <- paste("<div style='font-size:15px;font-weight:500;line-height: 1em;'><br/>DEGs 2 file: ",input$candidater_upload_degs2$name,"<",input$candidater_degs2_name,">","<br/>")}
    if(!is.null(input$candidater_upload_degs3)){deg3_file <- paste("<div style='font-size:15px;font-weight:500;line-height: 1em;'><br/>DEGs 3 file: ",input$candidater_upload_degs3$name,"<",input$candidater_degs3_name,">","<br/>")}
    genome <- paste("<div style='font-size:15px;font-weight:500;line-height: 1em;'><br/>Genome: ",input$candidater_species,"<br/>")
    region <- paste("<div style='font-size:15px;font-weight:500;line-height: 1em;'><br/>Interesting region: ",input$candidater_gene_region,"<br/>")
    if(input$candidater_degs_count == 1){
        a <- HTML(paste0(base, anno_file, deg1_file, genome, region,"<br/><br/><br/>"))
    }else if (input$candidater_degs_count == 2) {
       a <- HTML(paste0(base, anno_file, deg1_file, deg2_file, genome, region,"<br/><br/><br/>"))
    }else if (input$candidater_degs_count == 3) {
       a <- HTML(paste0(base, anno_file, deg1_file, deg2_file, deg3_file, genome, region,"<br/><br/><br/>"))
    }
})
# candidater_cutoff_ui
output$candidater_cutoff_ui <- renderUI({
    req(co_analysis())
    if(!is.null(input$candidater_upload_degs1) & is.null(input$candidater_upload_degs2) & is.null(input$candidater_upload_degs3)){
        fluidRow(
            column(width = 1, align = "center", numericInput("candidater_fc_cutoff", label = "Log2FC cutoff", value = 1)),
            column(width = 2, align = "center", selectInput("candidater_degs1_regulation", label = input$candidater_degs1_name, choices = c("up regulation"=1, "down regulation"=-1))),
            column(width = 2, align = "cenetr", downloadButton("download_candidater_table", label = "Download table", style = "margin-top: 25px;width: 100%;border-color: #80808057;height: 36px;"))
        )
    }else if (!is.null(input$candidater_upload_degs1) & !is.null(input$candidater_upload_degs2) & is.null(input$candidater_upload_degs3)) {
        fluidRow(
            column(width = 1, align = "center", numericInput("candidater_fc_cutoff", label = "Log2FC cutoff", value = 1)),
            column(width = 2, align = "center", selectInput("candidater_degs1_regulation", label = input$candidater_degs1_name, choices = c("up regulation"=1, "down regulation"=-1))),
            column(width = 2, align = "center", selectInput("candidater_degs2_regulation", label = input$candidater_degs2_name, choices = c("up regulation"=1, "down regulation"=-1))),
            column(width = 2, align = "cenetr", downloadButton("download_candidater_table", label = "Download table", style = "margin-top: 25px;width: 100%;border-color: #80808057;height: 36px;"))
        )
    }else if (!is.null(input$candidater_upload_degs1) & !is.null(input$candidater_upload_degs2) & !is.null(input$candidater_upload_degs3)) {
        fluidRow(
            column(width = 1, align = "center", numericInput("candidater_fc_cutoff", label = "Log2FC cutoff", value = 1)),
            column(width = 2, align = "center", selectInput("candidater_degs1_regulation", label = input$candidater_degs1_name, choices = c("up regulation"=1, "down regulation"=-1))),
            column(width = 2, align = "center", selectInput("candidater_degs2_regulation", label = input$candidater_degs2_name, choices = c("up regulation"=1, "down regulation"=-1))),
            column(width = 2, align = "center", selectInput("candidater_degs3_regulation", label = input$candidater_degs3_name, choices = c("up regulation"=1, "down regulation"=-1))),
            column(width = 2, align = "cenetr", downloadButton("download_candidater_table", label = "Download table", style = "margin-top: 25px;width: 100%;border-color: #80808057;height: 36px;"))
        )
    }
})

# candidater table
candidater_table <- reactive({
    req(co_analysis(), input$candidater_degs1_regulation, input$candidater_fc_cutoff)
    table <- co_analysis()
    if(!is.null(input$candidater_upload_degs1) & is.null(input$candidater_upload_degs2) & is.null(input$candidater_upload_degs3)){#degs1
        if(input$candidater_degs1_regulation == "1"){
            if (ncol(table)==6) {table <- subset(table, table[, 4] >= input$candidater_fc_cutoff )}else if (ncol(table)==7) {table <- subset(table, table[, 5] >= input$candidater_fc_cutoff )}
        }else {
           if (ncol(table)==6) {table <- subset(table, table[, 4] <= -input$candidater_fc_cutoff )}else if (ncol(table)==7) {table <- subset(table, table[, 5] <= -input$candidater_fc_cutoff )}
        }

    }else if (!is.null(input$candidater_upload_degs1) & !is.null(input$candidater_upload_degs2) & is.null(input$candidater_upload_degs3)) {#degs1 & 2
        if(input$candidater_degs1_regulation == "1"){
            if (ncol(table)==9) {table <- subset(table, table[, 4] >= input$candidater_fc_cutoff )}else if (ncol(table)==10) {table <- subset(table, table[, 5] >= input$candidater_fc_cutoff )}
        }else {
           if (ncol(table)==9) {table <- subset(table, table[, 4] <= -input$candidater_fc_cutoff )}else if (ncol(table)==10) {table <- subset(table, table[, 5] <= -input$candidater_fc_cutoff )}
        }

        if(input$candidater_degs2_regulation == "1"){
            if (ncol(table)==9) {table <- subset(table, table[, 7] >= input$candidater_fc_cutoff )}else if (ncol(table)==10) {table <- subset(table, table[, 8] >= input$candidater_fc_cutoff )}
        }else {
           if (ncol(table)==9) {table <- subset(table, table[, 7] <= -input$candidater_fc_cutoff )}else if (ncol(table)==10) {table <- subset(table, table[, 8] <= -input$candidater_fc_cutoff )}
        }

    }else if (!is.null(input$candidater_upload_degs1) & !is.null(input$candidater_upload_degs2) & !is.null(input$candidater_upload_degs3)) {#degs1 & 2 & 3
        if(input$candidater_degs1_regulation == "1"){
            if (ncol(table)==12) {table <- subset(table, table[, 4] >= input$candidater_fc_cutoff )}else if (ncol(table)==13) {table <- subset(table, table[, 5] >= input$candidater_fc_cutoff )}
        }else {
           if (ncol(table)==12) {table <- subset(table, table[, 4] <= -input$candidater_fc_cutoff )}else if (ncol(table)==13) {table <- subset(table, table[, 5] <= -input$candidater_fc_cutoff )}
        }

        if(input$candidater_degs2_regulation == "1"){
            if (ncol(table)==12) {table <- subset(table, table[, 7] >= input$candidater_fc_cutoff )}else if (ncol(table)==13) {table <- subset(table, table[, 8] >= input$candidater_fc_cutoff )}
        }else {
           if (ncol(table)==12) {table <- subset(table, table[, 7] <= -input$candidater_fc_cutoff )}else if (ncol(table)==13) {table <- subset(table, table[, 8] <= -input$candidater_fc_cutoff )}
        }

        if(input$candidater_degs3_regulation == "1"){
            if (ncol(table)==12) {table <- subset(table, table[, 10] >= input$candidater_fc_cutoff )}else if (ncol(table)==13) {table <- subset(table, table[, 11] >= input$candidater_fc_cutoff )}
        }else {
           if (ncol(table)==12) {table <- subset(table, table[, 10] <= -input$candidater_fc_cutoff )}else if (ncol(table)==13) {table <- subset(table, table[, 11] <= -input$candidater_fc_cutoff )}
        }
    }
    table <- table[order(table$Peak_pvalue),]
    return(table)
})

# output table
output$candidater_result <- DT::renderDataTable(candidater_table(), rownames = F, selection = list(mode = 'single'), options = list(dom = 'fltipr', pageLength = 10, scrollX=TRUE, orderClasses = T,
                                                                        columnDefs = list(
                                                                                        list(className = 'dt-center', 
                                                                                                targets = '_all')
                                                                                        )
                                                                        )
)
output$download_candidater_table <- downloadHandler(#下载allset
  filename = function() {paste0("candidater_explore.xlsx")},
  content = function(file) {
    if(!is.null(candidater_table())){
      a <- candidater_table()
      openxlsx::write.xlsx(a, file, sheetName = "candidate_explore", fileEncoding="GBK")
    }
  }
)

# plot
df1 <- reactive({
    req(co_analysis(), candidater())
    df1 <- openxlsx::read.xlsx(input$candidater_upload_peakanno$datapath, colNames = TRUE, rowNames = F)
    df1 <- df1 %>%
        dplyr::mutate(annotation = case_when(
            stringr::str_detect(annotation, "Exon") ~ "Exon",
            stringr::str_detect(annotation, "Intron") ~ "Intron",
            stringr::str_detect(annotation, "Promoter") ~ "Promoter",
            stringr::str_detect(annotation, "Downstream") ~ "Downstream",
            TRUE ~ annotation
        ))
    df1 <- df1[df1$annotation == input$candidater_gene_region,]
    candidater <- candidater()
    df1 <- df1[df1$geneId == candidater, c("start", "end", "peak_name")]
    colnames(df1) <- c("Start", "End", "name")
    rownames(df1) <- 1:nrow(df1) # nolint # nolint
    return(df1)
})
output$candidater_peak_table <- renderDataTable(df1(), options = list(dom = 'tipr'))

candidater <- reactive({
    req(candidater_table(), input$candidater_result_rows_selected)
    candidater_table <- candidater_table()
    candidater <- input$candidater_result_rows_selected
    candidater <- candidater_table[candidater,1]
    return(candidater)
})

gff <- reactive({
    req(input$candidater_species, candidater())
    candidater <- candidater()
    gff <- read.table(paste0("txdb/gff/", input$candidater_species), sep = "\t")
    if(grepl("IRGSP", input$candidater_species)){
        result <- gff %>%
            dplyr::filter(V3 == "exon" & grepl(candidater, V9))
        result <- data.frame(
                    Chromosome = result$V1,
                    Start = result$V4,
                    End = result$V5,
                    Strand = result$V7,
                    Transcript = regmatches(result$V9, regexpr("transcript_id\\s(\\S+);", result$V9, perl=TRUE)) 
                    )
    }else if (grepl("B73", input$candidater_species)) {
        result <- gff %>%
            dplyr::filter(V3 == "exon" & grepl(candidater, V9))
        result <- data.frame(
                    Chromosome = result$V1,
                    Start = result$V4,
                    End = result$V5,
                    Strand = result$V7,
                    Transcript = regmatches(result$V9, regexpr("transcript:[^;]+", result$V9, perl=TRUE)) 
                    )
    }else if (grepl("TAIR10", input$candidater_species)) {
        result <- gff %>%
            dplyr::filter(V3 == "exon" & grepl(candidater, V9))
        result <- data.frame(
                    Chromosome = result$V1,
                    Start = result$V4,
                    End = result$V5,
                    Strand = result$V7,
                    Transcript = regmatches(result$V9, regexpr("Parent=([A-Za-z0-9.]+)", result$V9, perl = TRUE)) 
                    )
    }

    return(result)
})
output$candidater_gene_table <- renderDataTable(gff(), options = list(dom = 'tipr'))

promoter <- reactive({
    req(df1, gff)
    gff <- gff()
    gff <- c(as.numeric(unlist(gff$Start)), as.numeric(unlist(gff$End)))
    gff_max <- max(gff)
    gff_min <- min(gff)

    df1 <- df1()
    df1 <- c(as.numeric(unlist(df1$Start)), as.numeric(unlist(df1$End)))
    df1_max <- max(df1)
    df1_min <- min(df1)

    max <- abs(gff_max - df1_max)
    min <- abs(gff_min - df1_min)
    value <- min(c(max, min))+500
    return(value)
})

gene_structure_plot <- reactive({
    req(co_analysis(), df1(), candidater())
    candidater <- candidater()#基因号
    df1 <- df1()#peak信息
    result <- gff()#筛选过基因号的gff

    df <- result
    df$y <- as.numeric(as.factor(df$Transcript))
    df$ymin <- df$y-0.25
    df$ymax <- df$y+0.25
    df$Transcript <- as.factor(df$Transcript)

    # 获取每个转录本的转录起始位点和最后一个外显子，包括 y 坐标信息
    transcript_starts <- df %>%
        dplyr::group_by(Transcript) %>%
        dplyr::summarize(Start = ifelse(Strand[which.min(Start)] == "+", min(Start), max(End)),
                y = dplyr::first(y),  # 获取对应的 y 坐标
                Strand = unique(Strand))  # 获取正反链信息
    last_exon <- df %>%
        dplyr::group_by(Transcript) %>%
        dplyr::slice(ifelse(Strand=="+", which.max(End), which.min(End))) %>%
        dplyr::ungroup()
    last_exon$ymin <- last_exon$y-0.25
    last_exon$ymax <- last_exon$y+0.25
    last_exon <- unique(last_exon)
    # 生成五边形的顶点
    generate_polygon <- function(start,end,transcript_y,ymin,ymax,strand) {
        if (strand == "+") {
            data.frame(
            x = c(start, start+(end-start)*2/3, end , start+(end-start)*2/3, start),
            y = c(ymin, ymin, transcript_y, ymax, ymax)
            )
        } else {
            data.frame(
            x = c(start, start+(end-start)*1/3, end, end, start+(end-start)*1/3),
            y = c(transcript_y, ymin, ymin, ymax, ymax)
            )
        }
    }
    # 创建包含五边形的顶点的数据框
    polygon_data <- do.call(rbind, lapply(1:nrow(last_exon), function(i) { # nolint
        row <- last_exon[i, ]
        poly_data <- generate_polygon(row$Start, row$End, row$Transcript, row$ymin, row$ymax, row$Strand)
        poly_data$Transcript <- row$Transcript
        return(poly_data)
    }))
    #tss
    tss_region <- promoter()
    #xlim
    xmin <- min(df$Start)-tss_region
    xmin <- ifelse(xmin < 0 , 0, xmin)
    xmax <- max(df$End)+tss_region
    xrange <- 1000
    # 绘制基因结构
    plot <- ggplot() +
        # 绘制连接线
        geom_line(data = df, aes(x = Start, y = y, group = Transcript), size = 1, color = "black") +
        # 绘制外显子
        geom_rect(data = df %>% dplyr::filter(!Start %in% last_exon$Start), aes(xmin = Start, xmax = End, ymin = ymin, ymax = ymax),color="black", fill = input$exon_color)+
        # 添加延伸线
        geom_segment(data = transcript_starts, 
                    aes(x = ifelse(Strand == "+", Start - tss_region, Start + tss_region), 
                        xend = Start, y = y, yend = y), color = "black", size = 1) +
        geom_segment(data = last_exon, 
                    aes(x = End, xend = ifelse(Strand == "+", End + tss_region, Start - tss_region), 
                        y = y, yend = y), color = "black", size = 1) +
        # 添加标签
        geom_label(data = df, aes(x = xmin+(xmax-xmin)/2, y = ymax+0.2, label = Transcript), 
                    color = "black", size = 6, label.size = 0)+
        # 绘制五边形
        geom_polygon(data = polygon_data, aes(x = x, y = y, group = Transcript), fill = input$exon_color, color = "black") +
        scale_fill_brewer(palette = "Set1") +
        labs(x = paste0(df$Chromosome[1]," (", xrange," bp per block",")"), y = paste0("Transcript (", candidater, ")")) +
        theme_bw() +
        theme(legend.position = "none", 
                axis.title = element_text(size = 15, color = "black"), 
                axis.text = element_text(size = 10, color = "black"),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.ticks.length = unit(0.3,'cm'),
                panel.grid = element_blank(),
                panel.border = element_rect(color = "black", size = 1)) +
        #axis range setting
        xlim(xmin, xmax) +
        scale_y_continuous(expand = expansion(mult = c(0.2, 0.2)))+
        scale_x_continuous(limits = c(xmin, xmax),  breaks = c(seq(xmin,xmax,by=xrange)), position = "top")+
        ##peak 
        geom_hline(yintercept = 0.5, size = 0.5, color = "black")+
        # peak line
        geom_point(data=df1, aes(x=Start, y=0.4), shape=17, size = 5, color = "darkred") +
        geom_segment(data = df1, 
                    aes(x = Start, xend = Start, y = 0.5, yend = Inf), color = "darkred", size = 0.5, linetype = "dashed") +
        geom_label(data = df1, 
                    aes(x = Start, y = 0.3, label = name), color = "white", size = 5, fill = "darkred")
        # # rnaseq 
        # annotate("label", x = xmin+(xmax-xmin)/2, y = -Inf, label = "Up regulation in Sample1 compare with Sample2: Log2FC = 1.5  P  = 0.002", 
        #         color = "black", fill = "pink", size = 5, label.size = 0.,vjust = -0.1 , 
        #         label.padding = unit(0.25, "lines"))
    return(plot)
})
output$gene_structure_plot <- renderPlot(gene_structure_plot())

# gene stucture ui
output$candidater_structure_ui  <- renderUI({
    if(!is.null(input$candidater_result_rows_selected)){
        fluidRow(
            fluidRow(
                column(width = 12, align = "center", plotOutput("gene_structure_plot", width = "80%", height = "600px")%>% withSpinner()),
                column(width = 2, offset = 1, colourpicker::colourInput("exon_color", label = "Color of gene structure", "lightblue")),
                column(width = 2, numericInput("candidater_plot_width", label = "Width", value = 16)),
                column(width = 2, numericInput("candidater_plot_height", label = "Height", value = 8)),
                column(width = 2, selectInput(inputId = "candidater_plot_format", label = "Format", choices = c("PNG" = "png", "PDF" = "pdf"))),
                column(width = 2, align = "center", downloadButton("candidater_plot_download", label = "Download",style = "width: 100%;border-color: #ccc;text-align: center;margin-top: 25px;height: 37px;"))
            ),
            br(),
            fluidRow(
                column(width = 6, offset = 3, align = "left", style="background-color: #00000012;height: 50px;width: 50%;font-weight: 900;line-height: 2.5em;font-size: 20px;border-radius: 15px;", HTML("Candidate gene structure:")),
                column(width = 6, offset = 3, DT::dataTableOutput(outputId = "candidater_gene_table", width = "100%"), style="background-color: #00000012;font-weight: 900;border-radius: 15px;")
                ),
            br(),
            fluidRow(
                column(width = 6, offset = 3, align = "left", style="background-color: #00000012;height: 50px;width: 50%;font-weight: 900;line-height: 2.5em;font-size: 20px;border-radius: 15px;", HTML("Peak binding site:")),
                column(width = 6, offset = 3, DT::dataTableOutput(outputId = "candidater_peak_table", width = "100%"), style="background-color: #00000012;font-weight: 900;border-radius: 15px;")
                )
        )
    }else {
       return(NULL)
    }
})
output$candidater_plot_download <- downloadHandler(#下载
    filename = function() { paste0(candidater(), "_gene_structure.", input$candidater_plot_format)},
    content = function(file) {
    p <- gene_structure_plot()
    ggsave(file, p, width = input$candidater_plot_width, height = input$candidater_plot_height)
})