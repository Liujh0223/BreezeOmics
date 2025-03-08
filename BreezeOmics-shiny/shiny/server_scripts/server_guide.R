output$ui_guide <- renderUI({
    fluidPage(style = "margin-top:10px;margin-bottom:30px;",
        bs_accordion(id = "guide_bsplus") %>%
            bs_set_opts(panel_type = "info", use_heading_link = TRUE) %>%
            bs_append(
            title = HTML("<div style='font-size:17px;line-height: 2em;color: black;'> >> What can BreezeOmics-Vis do ?</div>"),
            content = uiOutput("guide_bsplus_1"),
            ) %>%
            bs_append(
            title = HTML("<div style='font-size:17px;line-height: 2em;color: black;'> >> What are the advantages of BreezeOmics-Vis ?</div>"),
            content = uiOutput("guide_bsplus_2"),
            ) %>%
            bs_append(
            title = HTML("<div style='font-size:17px;line-height: 2em;color: black;'> >> Sample datas for testing.</div>"),
            content = uiOutput("guide_bsplus_3"),
            ) %>%
            bs_append(
            title = HTML(paste0("<div style='font-size:17px;line-height: 2em;color: black;display:inline-block;'> >> What the &nbsp</div>",
                                "<div style='font-size:17px;line-height: 2em;color: black;text-decoration: underline;display:inline-block;'> \"Quality control\" </div>",
                                "<div style='font-size:17px;line-height: 2em;color: black;display:inline-block;'> &nbsp module can do and how to do it ?</div>"
                                )),
            content = uiOutput("guide_bsplus_4"),
            ) %>%
            bs_append(
            title = HTML(paste0("<div style='font-size:17px;line-height: 2em;color: black;display:inline-block;'> >> What the &nbsp</div>",
                                "<div style='font-size:17px;line-height: 2em;color: black;text-decoration: underline;display:inline-block;'> \"DEGs\" </div>",
                                "<div style='font-size:17px;line-height: 2em;color: black;display:inline-block;'> &nbsp module can do and how to do it ?</div>"
                                )),
            content = uiOutput("guide_bsplus_5"),
            ) %>%
            bs_append(
            title = HTML(paste0("<div style='font-size:17px;line-height: 2em;color: black;display:inline-block;'> >> What the &nbsp</div>",
                                "<div style='font-size:17px;line-height: 2em;color: black;text-decoration: underline;display:inline-block;'> \"Time series analysis\" </div>",
                                "<div style='font-size:17px;line-height: 2em;color: black;display:inline-block;'> &nbsp module can do and how to do it ?</div>"
                                )),
            content = uiOutput("guide_bsplus_6"),
            ) %>%
            bs_append(
            title = HTML(paste0("<div style='font-size:17px;line-height: 2em;color: black;display:inline-block;'> >> What the &nbsp</div>",
                                "<div style='font-size:17px;line-height: 2em;color: black;text-decoration: underline;display:inline-block;'> \"WGCNA\" </div>",
                                "<div style='font-size:17px;line-height: 2em;color: black;display:inline-block;'> &nbsp module can do and how to do it ?</div>"
                                )),
            content = uiOutput("guide_bsplus_7"),
            ) %>%
            bs_append(
            title = HTML(paste0("<div style='font-size:17px;line-height: 2em;color: black;display:inline-block;'> >> What the &nbsp</div>",
                                "<div style='font-size:17px;line-height: 2em;color: black;text-decoration: underline;display:inline-block;'> \"Enrichment analysis\" </div>",
                                "<div style='font-size:17px;line-height: 2em;color: black;display:inline-block;'> &nbsp module can do and how to do it ?</div>"
                                )),
            content = uiOutput("guide_bsplus_8"),
            ) %>%
            bs_append(
            title = HTML(paste0("<div style='font-size:17px;line-height: 2em;color: black;display:inline-block;'> >> What the &nbsp</div>",
                                "<div style='font-size:17px;line-height: 2em;color: black;text-decoration: underline;display:inline-block;'> \"Co-analysis\" </div>",
                                "<div style='font-size:17px;line-height: 2em;color: black;display:inline-block;'> &nbsp module can do and how to do it ?</div>"
                                )),
            content = uiOutput("guide_bsplus_12"),
            ) %>%
            bs_append(
            title = HTML(paste0("<div style='font-size:17px;line-height: 2em;color: black;display:inline-block;'> >> What the &nbsp</div>",
                                "<div style='font-size:17px;line-height: 2em;color: black;text-decoration: underline;display:inline-block;'> \"Tools\" </div>",
                                "<div style='font-size:17px;line-height: 2em;color: black;display:inline-block;'> &nbsp module can do and how to do it ?</div>"
                                )),
            content = uiOutput("guide_bsplus_9"),
            ) %>%
            bs_append(
            title = HTML("<div style='font-size:17px;line-height: 2em;color: black;'> >> Contact me</div>"),
            content = uiOutput("guide_bsplus_10"),
            ) %>%
            bs_append(
            title = HTML("<div style='font-size:17px;line-height: 2em;color: black;'> >> Update history</div>"),
            content = uiOutput("guide_bsplus_11"),
            ),
        # activate tooltips, popovers, accordion-sidebar and MathJax
        use_bs_tooltip(),
        use_bs_popover(),
        use_bs_accordion_sidebar(), # needs to be at end, for some reason
        withMathJax()
    )
})

## guide_bsplus_1, What can ShinyRNAseq do ?
output$guide_bsplus_1 <- renderUI({
    fluidPage(
        column(width = 12,
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />')
        ),
        column(width = 12, align = "center",
            imageOutput("guide_rnaseq_pipeline", width = "100%", height = "700px")%>% withSpinner()
        ),
        column(width = 12,
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />')
        )
    )
})
output$guide_rnaseq_pipeline <- renderImage({
    list(src = "template/RNAseq_pipeline.jpg", contentType = 'image/png', style = "max-width:100%; max-height: 100%;", deleteFile = FALSE)
})


## guide_bsplus_2, What are the advantages of ShinyRNAseq ?
output$guide_bsplus_2 <- renderUI({
    fluidPage(
        column(width = 12,
            HTML(paste0(
                '<hr style="width: 100%;border-top: 1px solid #ddd;" />',
                "<div style='font-size:17px;line-height: 2em;color: black'> 1. The platform employs a fully graphical user interface design, enabling code-free downstream analysis of transcriptomes. </div>",
                "<div style='font-size:17px;line-height: 2em;color: black'> 2. The platform are designed in a modular fashion, where each module can be used independently or combined with others to form a complete analysis workflow. </div>",
                "<div style='font-size:17px;line-height: 2em;color: black'> 3. Each analysis function provides commonly optional parameters to meet users' customization needs. </div>",
                "<div style='font-size:17px;line-height: 2em;color: black'> 4. Based on interactive design, parameters can be modified in real-time for secondary analysis. </div>",
                "<div style='font-size:17px;line-height: 2em;color: black'> 5. The platform can be deployed on a server or personal computer without the need for additional software installation. </div>",
                "<div style='font-size:17px;line-height: 2em;color: black'> 6. The platform covers major analysis methods for downstream transcriptomic analysis. </div>",
                '<hr style="width: 100%;border-top: 1px solid #ddd;" />'
            ))
        )
    )
})

## guide_bsplus_3, Sample datas for testing.
output$guide_bsplus_3 <- renderUI({
    fluidPage(
        column(width = 12,
            HTML(paste0(
                '<hr style="width: 100%;border-top: 1px solid #ddd;" />',
                "<div style='font-size:17px;line-height: 2em;color: black'> Here we provide three example datas for testing that have been published from the NCBI public database, covering 3 different species. </div>",
                "<div style='font-size:17px;line-height: 2em;color: black'> And because BreezeOmics-Vis is used for downstream analysis, these datas have been aligned, assembled and quantified in advance to form gene expression matrices. </div>",
                "<div style='font-size:17px;line-height: 2em;color: black'> The example datas and upstream analysis method are as follows: </div>"
            ))
        ),
        column(width = 5,
            column(width = 12, 
                HTML(paste0(
                    '<hr style="width: 100%;border-top: 1px solid #ddd;" />',
                    "<div style='font-size:17px;line-height: 2em;color: black;'> Project ID: PRJNA610422 </div>",
                    "<div style='font-size:17px;line-height: 2em;color: black;'> Species: Rice </div>",
                    "<div style='font-size:17px;line-height: 2em;color: black;'> Repeat: 3 </div>",
                    "<div style='font-size:17px;line-height: 2em;color: black;'> Genome: IRGSP1.0 (https://rapdb.dna.affrc.go.jp/) </div>",
                    "<div style='font-size:17px;line-height: 2em;color: black;'> Reference: Pan Y, Liang H, Gao L, et al. Transcriptomic profiling of germinating seeds under cold stress and characterization of the cold-tolerant gene LTG5 in rice [J]. BMC Plant Biology, 2020, 20 (1): 371. </div>"
                )),
                downloadButton("download_PRJNA610422", label = "Download PRJNA610422_gene_conut_matrix.csv", style = "width:500px;margin-bottom: 10px;")
            ),
            column(width = 12, 
                HTML(paste0(
                    '<hr style="width: 100%;border-top: 1px solid #ddd;" />',
                    "<div style='font-size:17px;line-height: 2em;color: black;'> Project ID: PRJNA779257 </div>",
                    "<div style='font-size:17px;line-height: 2em;color: black;'> Species: Zea may </div>",
                    "<div style='font-size:17px;line-height: 2em;color: black;'> Repeat: 3 </div>",
                    "<div style='font-size:17px;line-height: 2em;color: black;'> Genome: Zm-B73-REFERENCE-NAM-5.0 (https://maizegdb.org/) </div>",
                    "<div style='font-size:17px;line-height: 2em;color: black;'> Reference: Jiang H, Shi Y, Liu J, et al. Natural polymorphism of ZmICE1 contributes to amino acid metabolism that impacts cold tolerance in maize [J]. Nature Plants, 2022, 8 (10): 1176–1190. </div>"
                )),
                downloadButton("download_PRJNA779257", label = "Download PRJNA779257_gene_conut_matrix.csv", style = "width:500px;margin-bottom: 10px;"),
                downloadButton("download_PRJNA779257_narrorPeak", label = "Download PRJNA779257_ZmICE1_chip.narrowPeak", style = "width:500px;margin-bottom: 10px;"),
                downloadButton("download_PRJNA779257_summitsBed", label = "Download PRJNA779257_ZmICE1_chip_summits.bed", style = "width:500px;margin-bottom: 10px;")
            ),
            column(width = 12, 
                HTML(paste0(
                    '<hr style="width: 100%;border-top: 1px solid #ddd;" />',
                    "<div style='font-size:17px;line-height: 2em;color: black;'> Project ID: PRJNA806968 </div>",
                    "<div style='font-size:17px;line-height: 2em;color: black;'> Species: Arabidopsis </div>",
                    "<div style='font-size:17px;line-height: 2em;color: black;'> Repeat: 3 </div>",
                    "<div style='font-size:17px;line-height: 2em;color: black;'> Genome: TAIR10 (https://www.arabidopsis.org/) </div>",
                    "<div style='font-size:17px;line-height: 2em;color: black;'> Reference: Burko Y, Willige B C, Seluzicki A, et al. PIF7 is a master regulator of thermomorphogenesis in shade[J]. Nature Communications, 2022, 13(1): 4942. </div>"
                )),
                downloadButton("download_PRJNA806968", label = "Download PRJNA806968_gene_conut_matrix.csv", style = "width:500px;margin-bottom: 10px;"),
                HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />')
            ),
        ),
        column(width = 6, offset = 1, style = "margin-bottom:20px",
            shinyWidgets::radioGroupButtons(inputId = "guide_methods",label = NULL, choices = c("RNA-seq Upstream Methods", "ChIP-seq Upstream Methods"), justified = TRUE, width = "100%", checkIcon = list(yes = icon("ok", lib = "glyphicon")))
        ),
        column(width = 6, offset = 1, uiOutput("guide_methods_ui"))
    )
})
output$guide_rnaseq_upstream_method <- renderImage({
    list(src = "template/upstream_analysis_methods.jpg", contentType = 'image/png', style = "max-width:85%", deleteFile = FALSE)
})
output$guide_chipseq_upstream_method <- renderImage({
    list(src = "template/chipseq_upstream_analysis_methods.jpg", contentType = 'image/png', style = "max-width:85%", deleteFile = FALSE)
})
output$download_PRJNA610422 <- downloadHandler(
    filename = function() {"PRJNA610422_gene_conut_matrix.csv" },
    content = function(file) {file.copy("template/PRJNA610422_gene_count_matrix.csv", file)}
)
output$download_PRJNA779257 <- downloadHandler(
    filename = function() {"PRJNA779257_gene_conut_matrix.csv" },
    content = function(file) {file.copy("template/PRJNA779257_gene_count_matrix.csv", file)}
)
output$download_PRJNA779257_narrorPeak <- downloadHandler(
    filename = function() {"PRJNA779257_ZmICE1_chip.narrowPeak" },
    content = function(file) {file.copy("template/PRJNA779257_ZmICE1_chip.narrowPeak", file)}
)
output$download_PRJNA779257_summitsBed <- downloadHandler(
    filename = function() {"PRJNA779257_ZmICE1_chip_summits.bed" },
    content = function(file) {file.copy("template/PRJNA779257_ZmICE1_chip_summits.bed", file)}
)
output$download_PRJNA806968 <- downloadHandler(
    filename = function() {"PRJNA806968_gene_conut_matrix.csv" },
    content = function(file) {file.copy("template/PRJNA806968_gene_count_matrix.csv", file)}
)

output$guide_methods_ui <- renderUI({
    if(input$guide_methods == "RNA-seq Upstream Methods"){
        imageOutput("guide_rnaseq_upstream_method", width = "100%")%>% withSpinner()
    }else if (input$guide_methods == "ChIP-seq Upstream Methods") {
       imageOutput("guide_chipseq_upstream_method", width = "100%")%>% withSpinner()
    }else {
       return(NULL)
    }
})

## guide_bsplus_4, DATA CHECK
output$guide_bsplus_4 <- renderUI({
    fluidPage(
        column(width = 12,
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />')
        ),
        column(width = 12, align = "center",
            imageOutput("S4", width = "100%", height = "1000px")%>% withSpinner()
        ),  
        column(width = 12,
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />')
        )
    )
})
output$S4 <- renderImage({
    list(src = "template/Fig_S4.jpg", contentType = 'image/png', style = "max-width:100%; max-height: 100%;", deleteFile = FALSE)
})

## guide_bsplus_5, DIFFERENCE ANAYLSIS
output$guide_bsplus_5 <- renderUI({
    fluidPage(
        column(width = 12,
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />')
        ),
        column(width = 12, align = "center",
            imageOutput("S5", width = "100%", height = "1400px")%>% withSpinner()
        ),  
        column(width = 12,
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />')
        )
    )
})
output$S5 <- renderImage({
    list(src = "template/Fig_S5.jpg", contentType = 'image/png', style = "max-width:100%; max-height: 100%;", deleteFile = FALSE)
})

## guide_bsplus_6, TIME SERIES ANALYSIS
output$guide_bsplus_6 <- renderUI({
    fluidPage(
        column(width = 12,
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />')
        ),
        column(width = 12, align = "center",
            imageOutput("S6", width = "100%", height = "1200px")%>% withSpinner()
        ),  
        column(width = 12,
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />')
        )
    )
})
output$S6 <- renderImage({
    list(src = "template/Fig_S6.jpg", contentType = 'image/png', style = "max-width:100%; max-height: 100%;", deleteFile = FALSE)
})

## guide_bsplus_7, WGCNA
output$guide_bsplus_7 <- renderUI({
    fluidPage(
        column(width = 12,
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />')
        ),
        column(width = 12, align = "center",
            imageOutput("S7", width = "100%", height = "1400px")%>% withSpinner()
        ),  
        column(width = 12,
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />')
        )
    )
})
output$S7 <- renderImage({
    list(src = "template/Fig_S7.jpg", contentType = 'image/png', style = "max-width:100%; max-height: 100%;", deleteFile = FALSE)
})

## guide_bsplus_8, ENRICHMENT ANALYSIS
output$guide_bsplus_8 <- renderUI({
    fluidPage(
        column(width = 12,
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />')
        ),
        column(width = 12, align = "center",
            imageOutput("S8", width = "100%", height = "800px")%>% withSpinner()
        ),  
        column(width = 12,
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />')
        )
    )
})
output$S8 <- renderImage({
    list(src = "template/Fig_S8.jpg", contentType = 'image/png', style = "max-width:100%; max-height: 100%;", deleteFile = FALSE)
})

## guide_bsplus_9, TOOLS
output$guide_bsplus_9 <- renderUI({
    fluidPage(
        column(width = 12,
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />')
        ),
        column(width = 12, align = "center",
            imageOutput("S10", width = "100%", height = "1400px")%>% withSpinner()
        ),  
        column(width = 12,
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />')
        )
    )
})
output$S10 <- renderImage({
    list(src = "template/Fig_S10.jpg", contentType = 'image/png', style = "max-width:100%; max-height: 100%;", deleteFile = FALSE)
})

## guide_bsplus_10, Contact me
output$guide_bsplus_10 <- renderUI({
    fluidPage(
        column(width = 12,
            HTML(paste0(
                '<hr style="width: 100%;border-top: 1px solid #ddd;" />',
                "<div style='font-size:17px;line-height: 2em;color: black; display:inline-block;'> The source code of this project has been uploaded to GitHub </div>",
                "<a style='color: #008aff;;font-size: 17px;' href='https://github.com/Liujh0223/BreezeOmics' target='_blank'>", " (https://github.com/Liujh0223/BreezeOmics) ", "</a>",
                "<div style='font-size:17px;line-height: 2em;color: black'> To improve the user experience, we recommend that you clone the project to run it locally. </div>",
                "<div style='font-size:17px;line-height: 2em;color: black'> If you have any questions or suggestions during use, please leave a message on GitHub or contact liujianhong0223@gmail.com/364832885@qq.com by email. </div>",
                '<hr style="width: 100%;border-top: 1px solid #ddd;" />'
            ))
        )
    )
})

## guide_bsplus_11, Update history
output$guide_bsplus_11 <- renderUI({
    fluidPage(
        column(width = 12,
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
            HTML(update_history()),
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />')
        )
    )
})

## guide_bsplus_12, Multi-Omics co-analysis
output$guide_bsplus_12 <- renderUI({
    fluidPage(
        column(width = 12,
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />')
        ),
        column(width = 12, align = "center",
            imageOutput("S9", width = "100%", height = "1400px")%>% withSpinner()
        ),  
        column(width = 12,
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />')
        )
    )
})
output$S9 <- renderImage({
    list(src = "template/Fig_S9.jpg", contentType = 'image/png', style = "max-width:100%; max-height: 100%;", deleteFile = FALSE)
})

update_history <- reactive({
    file_path <- "Update_history.txt"
    file_content <- readLines(file_path)
    formatted_lines <- lapply(file_content, function(line) {
      formatted_line <- paste0("<span style='color: blue; font-size: 14px;'>", line, "</span>")
      formatted_line
    })
    formatted_text <- paste(formatted_lines, collapse = "<br>")
    HTML(formatted_text)
})