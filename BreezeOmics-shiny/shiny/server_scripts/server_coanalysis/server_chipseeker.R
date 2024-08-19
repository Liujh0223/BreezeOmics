#Shiny-ChIPseeker
chipseeker_process <- reactiveValues()
chipseeker_process$value <- "start"

output$ui_coanalysis1 <- renderUI({
    if(chipseeker_process$value == "start"){
        fluidRow(
            br(),
            fluidRow(
                column(width = 4, align="left", offset = 4,HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step1: Please upload you call peaks file (BED format)</div>")),
                column(width = 4, offset = 4, align = "center",  fileInput("chipseeker_upload_bed", label = NULL, width = "100%"))
            ),br(),
            fluidRow(
                column(width = 4, align="left", offset = 4,HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step2: What kind of BED file you loaded</div>")),
                column(width = 4, offset = 4, align = "center",  selectInput(inputId = "chipseeler_bed_format", label = NULL, width = "100%", choices = list(
                    "BED5(chr, start, end, name, score): NAME_summits.bed" = 5,
                    "BED4+6(chr, start, end, name, -10*log10qvalue, strand, FC, -log10pvalue, -log10qvalue, distanceToSummit): NAME_peaks.narrowPeak" = 10,
                    "Other BED file(base:chr, start, end)" = 3
                )), style = "margin-bottom: 30px;")
            ),
            fluidRow(
                column(width = 4, align="left", offset = 4,HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step3: Select Transcript or Gene</div>")),
                column(width = 4, offset = 4, align = "center", shinyWidgets::radioGroupButtons(inputId = "chipseeker_t_g",label = NULL,choices = c("Transcript"="transcript", "Gene"="gene"), justified = TRUE))
            ),br(),
            fluidRow(
                column(width = 4, align="left", offset = 4,HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step4: Select your TSS Region</div>")),
                column(width = 4, offset = 4, align = "center", sliderInput("chipseeker_tssregion", label = NULL, min = -5000, max = 2000, value = c(-3000, 500), step = 100, width = "100%"))
            ),br(),
            fluidRow(
                column(width = 4, align="left", offset = 4,HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step5: Select you species genomes</div>")),
                column(width = 4, offset = 4, align = "center",  selectInput(inputId = "chipseeler_species", label = NULL, choices = unlist(list(list.files("txdb/txdb/"))), width = "100%"))
            ),
            uiOutput("chipseeker_upload_gff_ui"),
            fluidRow(style = "margin-top: 50px;margin-bottom: 30px;",
                column(width = 3, align="center", offset = 3,
                    actionButton(inputId = "reset_chipseeker", label = "RESET", icon = icon("rotate-left"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;")),
                column(width = 3, align="center", offset = 0,
                    actionButton(inputId = "enter_chipseeker", label = "ENTER", icon = icon("check"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;"))
            )# close fluidRow
        )
    }else if(chipseeker_process$value == "process"){
       fluidRow(
            HTML('<hr style="width: 100%;border-top: 2px solid #000;" />'),
            column(width = 12, align = "left", style="background-color: #00000012;font-weight: 900;border-radius: 15px;", htmlOutput(outputId = "chipseeker_info", style = "font-size:15px;")),
            column(width = 12, DT::dataTableOutput(outputId = "chipseeker_result", width = "100%"), style="background-color: #d3f2ff;margin-bottom: 10px; font-size: 12px;border-radius: 15px;margin-top: -25px"),
            uiOutput("download_chipseeker_result_ui"),
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),

            fluidRow(
                column(width = 12, plotOutput("chipseeker_covplot", height = "800px") %>% withSpinner())
            ),
            uiOutput("download_covplot_ui"),
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),

            # fluidRow(
            #     column(width = 12, plotOutput("chipseeker_plotAvgProf", height = "800px") %>% withSpinner())
            # ),
            # uiOutput("download_plotAvgProf_ui"),
            # HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),

            fluidRow(
                column(width = 3, align = "right", tableOutput(outputId = "chipseeker_stat_table"), style = "margin-top: 120px;"),
                column(width = 6, align = "center", plotOutput("chipseeker_pieplot", height = "600px", width = "600px") %>% withSpinner()),
                column(width = 3,
                    column(width = 12, shinyWidgets::radioGroupButtons(
                                                inputId = "chipseeker_statplot_type", label = NULL, choices = c("Pie chart", "Bar plot", "Stacked plot"), individual = F, justified = T, width = '100%',
                                                checkIcon = list(yes = tags$i(class = "fa fa-circle", style = "color: steelblue"),
                                                                 no = tags$i(class = "fa fa-circle-o", style = "color: steelblue")))),
                    column(width = 12, colourpicker::colourInput("chipseeker_statplot_col1", label = NULL, "blue")),
                    column(width = 12, colourpicker::colourInput("chipseeker_statplot_col2", label = NULL, "yellow")),
                    column(width = 12, colourpicker::colourInput("chipseeker_statplot_col3", label = NULL, "green")),
                    column(width = 12, colourpicker::colourInput("chipseeker_statplot_col4", label = NULL, "grey")),
                    column(width = 12, colourpicker::colourInput("chipseeker_statplot_col5", label = NULL, "pink")),
                    column(width = 12, colourpicker::colourInput("chipseeker_statplot_col6", label = NULL, "red")),
                    column(width = 12, colourpicker::colourInput("chipseeker_statplot_col7", label = NULL, "purple")),
                    uiOutput("chipseeker_statbarplot_ui"),
                    column(width = 3, numericInput("chipseeker_statbarplot_width", label = "Width", value = 8), style = "left:-15px"),
                    column(width = 3, numericInput("chipseeker_statbarplot_height", label = "Height", value = 8), style = "left:-40px"),
                    column(width = 3, selectInput("chipseeker_statbarplot_format", label = "Format", choices = c("png", "pdf")), style = "top: -2px;"),
                    column(width = 3, downloadButton("chipseeker_statbarplot_download", label = "Download"), style = "left: -26px;top: 30px;")
                    
                )
            ),
            fluidRow(
                column(width = 2, align = "center", downloadButton("chipseeker_promoter_set", label = "Promoter set", style = "width:100%;"), offset = 1),
                column(width = 1, align = "center", downloadButton("chipseeker_5utr_set", label = "5'UTR set", style = "width:100%;")),
                column(width = 1, align = "center", downloadButton("chipseeker_exon_set", label = "Exon set", style = "width:100%;")),
                column(width = 1, align = "center", downloadButton("chipseeker_intron_set", label = "Intron set", style = "width:100%;")),
                column(width = 1, align = "center", downloadButton("chipseeker_3utr_set", label = "3'UTR set", style = "width:100%;")),
                column(width = 2, align = "center", downloadButton("chipseeker_downstream_set", label = "Downstream set", style = "width:100%;")),
                column(width = 2, align = "center", downloadButton("chipseeker_intergenic_set", label = "Distal intergenic set", style = "width:100%;"))
            ),
            uiOutput("chipseeker_pieplot_ui"),
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
       
       )
    }

})

output$chipseeker_upload_gff_ui <- renderUI({
    if(input$chipseeler_species == "Upload GFF/GTF"){
        fluidRow(
            br(),
            column(width = 4, align="left", offset = 4,HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step6: Selection GTF or GFF</div>")),
            column(width = 4, offset = 4, align = "center",  shinyWidgets::radioGroupButtons(inputId = "chipseeker_gff_gtf",label = NULL,choices = c("GTF", "GFF"), justified = TRUE)),
            br(),
            column(width = 4, align="left", offset = 4,HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step7: Please upload GFF/GTF annotation file</div>"), style = "margin-top: 40px;"),
            column(width = 4, offset = 4, align = "center",  fileInput("chipseeker_upload_gtfgff", label = NULL, width = "100%"))
        )
    }else {
       return(NULL)
    }
})

chipseeker_values <- reactiveValues(upload_bed = "NULL", upload_gtfgff = "uploaded")
observeEvent(input$chipseeker_upload_bed, {chipseeker_values$upload_bed <- "uploaded"})
observeEvent(input$chipseeker_upload_gtfgff, {chipseeker_values$upload_gtfgff <- "uploaded"})
observeEvent(input$chipseeler_species, {
    if(input$chipseeler_species == "Upload GFF/GTF"){
        chipseeker_values$upload_gtfgff <- "NULL"
    }else {
       chipseeker_values$upload_gtfgff <- "uploaded"
    }
})
#ENTER
observeEvent(input$enter_chipseeker, {
    if(chipseeker_values$upload_bed == "uploaded" & chipseeker_values$upload_gtfgff == "uploaded"){
        bed <- read.table(input$chipseeker_upload_bed$datapath)
        if(input$chipseeler_bed_format != ncol(bed)){
            shinyjs::runjs('alert("ERROR: The bed file format you uploaded does not match the type you selected in step 2")')
            chipseeker_process$value <- "start"
            shinyjs::reset("chipseeker_upload_bed")
            shinyjs::reset("chipseeker_upload_gtfgff")
            shinyjs::reset("chipseeler_species")
            chipseeker_values$upload_bed = "NULL"
            chipseeker_values$upload_gtfgff = "NULL"
            return(NULL)
        }else {
           chipseeker_process$value = "process"
        }
    }else {
        shinyjs::runjs('alert("ERROR: Please upload bed file and annotation file.")')
        chipseeker_process$value <- "start"
        shinyjs::reset("chipseeker_upload_bed")
        shinyjs::reset("chipseeker_upload_gtfgff")
        shinyjs::reset("chipseeler_species")
        chipseeker_values$upload_bed = "NULL"
        chipseeker_values$upload_gtfgff = "NULL"
       return(NULL)
    }
})
#RESET
observeEvent(input$reset_chipseeker, {
  chipseeker_process$value <- "start"
  shinyjs::reset("chipseeker_upload_bed")
  shinyjs::reset("chipseeker_upload_gtfgff")
  shinyjs::reset("chipseeler_species")
  chipseeker_values$upload_bed = NULL
  chipseeker_values$upload_gtfgff = NULL
})

#chipseeker
chipseeker <- reactive({
    if(input$chipseeler_species == "Upload GFF/GTF"){
        txdb <- GenomicFeatures::makeTxDbFromGFF(input$chipseeker_upload_gtfgff$datapath, format = input$chipseeker_gff_gtf)
    }else {
        txdb <- paste0("txdb/txdb/", input$chipseeler_species)
        txdb <- AnnotationDbi::loadDb(txdb)
    }
    peak <- ChIPseeker::readPeakFile(input$chipseeker_upload_bed$datapath)
    bed1 <- ChIPseeker::annotatePeak(peak,
                                    TxDb = txdb,
                                    level = input$chipseeker_t_g,
                                    tssRegion=c(input$chipseeker_tssregion[1], input$chipseeker_tssregion[2]))
    bed <- as.data.frame(bed1)
    print(colnames(bed))
    if(input$chipseeker_t_g == "gene"){
        coln <- colnames(bed)[(length(colnames(bed))-7):length(colnames(bed))]
    }else {
       coln <- colnames(bed)[(length(colnames(bed))-8):length(colnames(bed))]
    }
    
    if(input$chipseeler_bed_format == 3){
        coln1 <- c("seqnames", "start", "end", "width", coln)
        bed <- bed[, coln1]
        colnames(bed) <- c("chr", "start", "end", coln)
        print(colnames(bed))
    }else if (input$chipseeler_bed_format == 5) {
        coln1 <- c("seqnames", "start", "end", "V4", "V5", coln)
        bed <- bed[, coln1]
        colnames(bed) <- c("chr", "start", "end", "peak_name", "-log10pvalue", coln)
        print(colnames(bed))
    }else if (input$chipseeler_bed_format == 10) {
        coln1 <- c("seqnames", "start", "end", "width", "V4", "V7", "V8", "V9", "V10", coln)
        bed <- bed[,coln1]
        colnames(bed) <- c("chr", "start", "end", "width", "peak_name", "fold change", "-log10pvalue", "-log10qvalue", "distanceToSummit", coln)
        print(colnames(bed))
    }

    return(list(bed1 = bed1,
                bed = bed,
                peak = peak,
                txdb = txdb  
    ))
})
output$chipseeker_info <- renderText({
    req(chipseeker())
    base <- paste("<div style='font-size:30px;font-weight:600;line-height: 1em;'><br/>ChIPseeker run info:<br/>")
    bed_file <- paste("<div style='font-size:15px;font-weight:500;line-height: 1em;'><br/>BED file: ",input$chipseeker_upload_bed$name,"<br/>")
    genome <- paste("<div style='font-size:15px;font-weight:500;line-height: 1em;'><br/>Genome: ",input$chipseeler_species,"<br/>")
    level <- paste("<div style='font-size:15px;font-weight:500;line-height: 1em;'><br/>Annotation type: ",input$chipseeker_t_g,"<br/>")
    tssregion <- paste("<div style='font-size:15px;font-weight:500;line-height: 1em;'><br/>TSS region: ",input$chipseeker_tssregion[1],"~",input$chipseeker_tssregion[2],"bp<br/>")
    a <- HTML(paste0(base, bed_file, genome, level, tssregion,"<br/><br/><br/>"))
})

#chipseeker result
output$chipseeker_result <- DT::renderDataTable(chipseeker()$bed, rownames = T, options = list(dom = 'fltipr', pageLength = 10, scrollX=TRUE, orderClasses = T,
                                                                        columnDefs = list(
                                                                                        list(className = 'dt-center', 
                                                                                                targets = '_all')
                                                                                        )
                                                                        ), selection = "none"
)

#download_chipseeker_result_ui
output$download_chipseeker_result_ui <- renderUI({
    if(!is.null(chipseeker())){
        column(width = 12, align = "center", downloadButton("download_chipseeker_result", label = "Download ChIPseeker annotation result", width = "100%"), style = "margin-bottom:50px;")
    }
    
})
output$download_chipseeker_result <- downloadHandler(#下载allset
  filename = function() {"ChIPseeker_annotation.xlsx"},
  content = function(file) {
    if(!is.null(chipseeker()$bed)){
      a <- chipseeker()$bed
      openxlsx::write.xlsx(a, file, sheetName = "chipseeker", fileEncoding = "GBK")
    }
    
  }
)

#chipseeker covplot
chipseeker_covplot <- reactive({
    req(chipseeker())
    options(scipen=200)
    peak <- chipseeker()$peak
    a <- ChIPseeker::covplot(peak, weightCol = 2) +
        theme_bw() + theme(axis.title.x = element_text(size = 20, color = "black"), 
                            plot.title = element_text(size = 20, color = "black"), 
                            panel.grid = element_blank(),
                            axis.text.y = element_blank(),
                            axis.text = element_text(size = 15, color = "black"),
                            axis.ticks.y = element_blank(),
                            strip.text = element_text(size = 15, color = "black")
                            )
    return(a)
})
output$chipseeker_covplot <- renderPlot({chipseeker_covplot()})
output$download_covplot_ui <- renderUI({
    if(!is.null(chipseeker_covplot())){
        fluidRow(style = "margin-top: 30px;margin-bottom: 50px;",
            column(width = 1, offset = 4, numericInput("covplot_width", label = "Width", value = 12, width = "100%")),
            column(width = 1, numericInput("covplot_height", label = "Height", value = 8, width = "100%")),
            column(width = 1, selectInput(inputId = "covplot_format", label = "Format", choices = c("PNG" = "png", "PDF" = "pdf"), width = "100%")),
            column(width = 1, align = "center", downloadButton("download_covplot", label = "Download plot"), style = "margin-top: 30px;"),
        )
    }
})
output$download_covplot <- downloadHandler(#下载
    filename = function() { paste("covplot", input$covplot_format, sep = ".")},
    content = function(file) {
    p <- chipseeker_covplot()
    ggsave(file, p, width = input$covplot_width, height = input$covplot_height)
})

#tag tssregion
chipseeker_plotAvgProf <- reactive({
    req(chipseeker())
    txdb <- chipseeker()$txdb
    peak <- chipseeker()$peak

    promoter <- ChIPseeker::getPromoters(TxDb=txdb, upstream=1500, downstream=1500)
    tagMatrix <- ChIPseeker::getTagMatrix(peak, windows = promoter)

    a <- ChIPseeker::plotAvgProf(tagMatrix, xlim=c(-1500, 1500),
            conf=0.95,resample = 1000,
            xlab="Genomic Region (5'->3')", ylab = "Read Count Frequency")
    return(a)
})
output$chipseeker_plotAvgProf <- renderPlot(chipseeker_plotAvgProf())

#barplot
chipseeker_pieplot <- reactive({
    req(chipseeker())
    bed <- chipseeker()$bed

    bed_stat <- bed %>%
        mutate(annotation = case_when(
            stringr::str_detect(annotation, "Exon") ~ "Exon",
            stringr::str_detect(annotation, "Intron") ~ "Intron",
            stringr::str_detect(annotation, "Promoter") ~ paste0("Promoter(<=", abs(input$chipseeker_tssregion[1])/1000, "kb)"),
            stringr::str_detect(annotation, "Downstream") ~ "Downstream",
            TRUE ~ annotation
        ))
    bed_stat3 <- bed_stat

    bed_stat2 <- data.frame(gene = bed_stat$geneId, annotation = bed_stat$annotation)
    bed_stat2 <- unique(bed_stat2)
    bed_stat <- as.data.frame(table(bed_stat2[, "annotation"]))

    colnames(bed_stat) <- c("annotation", "count")
    bed_stat1 <- bed_stat
    bed_stat$annotation <- factor(bed_stat$annotation, levels = c(paste0("Promoter(<=", abs(input$chipseeker_tssregion[1])/1000, "kb)"), "5' UTR", "Exon", "Intron", "3' UTR", "Downstream", "Distal Intergenic"))
    bed_stat$proportion <- bed_stat$count / sum(bed_stat$count)

    col <- c(input$chipseeker_statplot_col1, 
                input$chipseeker_statplot_col2, 
                input$chipseeker_statplot_col3, 
                input$chipseeker_statplot_col4, 
                input$chipseeker_statplot_col5,
                input$chipseeker_statplot_col6,
                input$chipseeker_statplot_col7)
    if(input$chipseeker_statplot_type == "Pie chart"){
            plot <- ggplot(bed_stat, aes(x = "", y = proportion, fill = annotation)) +
                geom_bar(width = 1, stat = "identity", color = "black") +
                coord_polar(theta = "y") +
                labs(title = NULL) +
                theme_void() + 
                theme(legend.title = element_blank(), legend.text = element_text(size = 15)) + 
                scale_fill_manual(labels = function(x) {
                                label <- paste(x, scales::percent(bed_stat$proportion[match(x, bed_stat$annotation)]), sep = ": ")
                                return(label)
                            },
                            values = col)

    }else if (input$chipseeker_statplot_type == "Bar plot") {
            plot <- ggplot(bed_stat, aes(x=annotation, y=count, fill = annotation)) +
                geom_bar(stat="identity", color = "black") + theme_classic() + 
                labs(title=NULL, x=NULL, y="Counts") + 
                theme(legend.title = element_blank(), legend.text = element_text(size = 15),
                        axis.text.x = element_text(size = input$chipseeker_barplot_text_size, angle = input$chipseeker_barplot_x_angle, hjust = 1, vjust = 1, color = "black"),
                        axis.text.y = element_text(size = input$chipseeker_barplot_text_size, color = "black"),
                        axis.title = element_text(size = input$chipseeker_barplot_title_size, color = "black"),
                        plot.margin = unit(c(1, 1, 1, 1), "cm")
                     ) + 
                scale_y_continuous(expand = c(0, 0), limits = c(0, input$chipseeker_barplot_ylim))+
                scale_fill_manual(labels = function(x) {
                                label <- paste(x, scales::percent(bed_stat$proportion[match(x, bed_stat$annotation)]), sep = ": ")
                                return(label)
                            },
                            values = col)

    }else if(input$chipseeker_statplot_type == "Stacked plot"){
            plot <- ggplot(bed_stat, aes(x = "", y = proportion, fill = annotation)) +
                geom_bar(width = 1, stat = "identity", color = "black") +
                labs(title = NULL) +
                theme_void() + 
                theme(legend.title = element_blank(), legend.text = element_text(size = 15)) + 
                scale_fill_manual(labels = function(x) {
                                label <- paste(x, scales::percent(bed_stat$proportion[match(x, bed_stat$annotation)]), sep = ": ")
                                return(label)
                            },
                            values = col)
                
    }else {
       return(NULL)
    }


    return(list(plot = plot, bed_stat1 = bed_stat1, bed_stat3 = bed_stat3))
})
output$chipseeker_pieplot <- renderPlot(chipseeker_pieplot()$plot)
output$chipseeker_stat_table <- renderTable(rownames = F, chipseeker_pieplot()$bed_stat1, colnames = T)

#chipseeker_stat_barplot
output$chipseeker_statbarplot_ui <- renderUI({
    if(input$chipseeker_statplot_type == "Bar plot"){
        fluidRow(
            column(width = 6, numericInput("chipseeker_barplot_ylim", "Y limit", value = 8000)),
            column(width = 6, numericInput("chipseeker_barplot_x_angle", "X text angle", value = 45)),
            column(width = 6, numericInput("chipseeker_barplot_title_size", "Title size", value = 20)),
            column(width = 6, numericInput("chipseeker_barplot_text_size", "Text size", value = 20))
        )
    }else {
       return(NULL)
    }
})
output$chipseeker_statbarplot_download <- downloadHandler(#下载
    filename = function() { paste("chipseeker_statbarplot", input$chipseeker_statbarplot_format, sep = ".")},
    content = function(file) {
    p <- chipseeker_pieplot()$plot
    ggsave(file, p, width = input$chipseeker_statbarplot_width, height = input$chipseeker_statbarplot_height)
})

#chipseeker_download set
output$chipseeker_promoter_set <- downloadHandler(#下载
    filename = function() {"Promoter_set.txt"},
    content = function(file) {
    bed <- chipseeker_pieplot()$bed_stat3
    print(colnames(bed))
    bed <- bed[bed$annotation == paste0("Promoter(<=", abs(input$chipseeker_tssregion[1])/1000, "kb)"), "geneId"]
    bed <- unique(bed)
    write.table(bed, file, quote = F, sep = "\t", row.names = F, col.names = F)
})
output$chipseeker_5utr_set <- downloadHandler(#下载
    filename = function() {"5UTR_set.txt"},
    content = function(file) {
    bed <- chipseeker_pieplot()$bed_stat3
    print(colnames(bed))
    bed <- bed[bed$annotation == "5' UTR", "geneId"]
    bed <- unique(bed)
    write.table(bed, file, quote = F, sep = "\t", row.names = F, col.names = F)
})
output$chipseeker_exon_set <- downloadHandler(#下载
    filename = function() {"Exon_set.txt"},
    content = function(file) {
    bed <- chipseeker_pieplot()$bed_stat3
    print(colnames(bed))
    bed <- bed[bed$annotation == "Exon", "geneId"]
    bed <- unique(bed)
    write.table(bed, file, quote = F, sep = "\t", row.names = F, col.names = F)
})
output$chipseeker_intron_set <- downloadHandler(#下载
    filename = function() {"Intron_set.txt"},
    content = function(file) {
    bed <- chipseeker_pieplot()$bed_stat3
    print(colnames(bed))
    bed <- bed[bed$annotation == "Intron", "geneId"]
    bed <- unique(bed)
    write.table(bed, file, quote = F, sep = "\t", row.names = F, col.names = F)
})
output$chipseeker_3utr_set <- downloadHandler(#下载
    filename = function() {"3UTR_set.txt"},
    content = function(file) {
    bed <- chipseeker_pieplot()$bed_stat3
    print(colnames(bed))
    bed <- bed[bed$annotation == "3' UTR", "geneId"]
    bed <- unique(bed)
    write.table(bed, file, quote = F, sep = "\t", row.names = F, col.names = F)
})
output$chipseeker_downstream_set <- downloadHandler(#下载
    filename = function() {"Downstream_set.txt"},
    content = function(file) {
    bed <- chipseeker_pieplot()$bed_stat3
    print(colnames(bed))
    bed <- bed[bed$annotation == "Downstream", "geneId"]
    bed <- unique(bed)
    write.table(bed, file, quote = F, sep = "\t", row.names = F, col.names = F)
})
output$chipseeker_intergenic_set <- downloadHandler(#下载
    filename = function() {"Distal Intergenic_set.txt"},
    content = function(file) {
    bed <- chipseeker_pieplot()$bed_stat3
    print(colnames(bed))
    bed <- bed[bed$annotation == "Distal Intergenic", "geneId"]
    bed <- unique(bed)
    write.table(bed, file, quote = F, sep = "\t", row.names = F, col.names = F)
})