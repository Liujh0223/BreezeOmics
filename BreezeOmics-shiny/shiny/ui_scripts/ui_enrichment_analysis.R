enrichmentanalysis <- tabPanel(title = list(icon("chart-gantt"),"ENRICHMENT ANALYSIS"), value = "enrichmentanalysis",
                      fluidPage(style= "margin-top: 30px;margin-bottom: 3px;",
                        column(width = 6, align = "center", offset = 3,
                          shinyWidgets::radioGroupButtons(inputId = "enrich_ui", label = NULL, size = "lg",justified = T, status = "info",
                          choices = c(HTML("<div style='font-size:30px;font-weight:900;line-height: 1.5em;font-family:FontAwesome;'>KEGG</div>"),
                                      HTML("<div style='font-size:30px;font-weight:900;line-height: 1.5em;font-family:FontAwesome;'>Gene Ontology</div>")))
                          ),
                        column(width = 12, offset = 0,style = "margin-bottom: 30px;",
                          uiOutput(outputId = "ui_enerichanalysis")
                          ),
                        column(width = 6, offset = 4,
                          column(width = 12, align = "left", align = "letf", HTML("<div style='font-size:20px;line-height: 2em;'>Relevant database address:</div>")),
                          column(width = 12, align = "left", align = "letf", HTML(paste0("<a style='color: black;font-size: 15px;' href='https://www.genome.jp/kegg/' target='_blank'>", "Kyoto Encyclopedia of Genes and Genomes (KEGG) - https://www.genome.jp/kegg/", "</a>"))),
                          column(width = 12, align = "letf", align = "letf", HTML(paste0("<a style='color: black;font-size: 15px;' href='https://www.genome.jp/kegg-bin/get_htext?ko00001' target='_blank'>", " KEGG Orthology (KO) - https://www.genome.jp/kegg-bin/get_htext?ko00001", "</a>"))),
                          column(width = 12, align = "letf", align = "letf", HTML(paste0("<a style='color: black;font-size: 15px;' href='https://www.geneontology.org/' target='_blank'>", " Gene Ontology (GO) - https://www.geneontology.org/", "</a>"))),
                          column(width = 12, align = "letf", align = "letf", HTML(paste0("<a style='color: black;font-size: 15px;' href='https://current.geneontology.org/ontology/go-basic.obo' target='_blank'>", " go-basic.obo - https://current.geneontology.org/ontology/go-basic.obo", "</a>"))),
                          column(width = 12, align = "letf", align = "letf", HTML(paste0("<a style='color: black;font-size: 15px;' href='https://www.uniprot.org/' target='_blank'>", " UniProt - https://www.uniprot.org/", "</a>"))),
                          column(width = 12, align = "letf", align = "letf", HTML(paste0("<a style='color: black;font-size: 15px;' href='https://www.ebi.ac.uk/interpro/' target='_blank'>", " InterPro - https://www.ebi.ac.uk/interpro/", "</a>")))
                        )
 
                      )
)