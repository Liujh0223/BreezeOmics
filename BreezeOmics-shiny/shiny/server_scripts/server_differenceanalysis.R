differenceanalysis_process <- reactiveValues()
differenceanalysis_process$value <- "start"
library(shiny)
library(DT)
# render UI
observeEvent(differenceanalysis_process$value, {
  if (differenceanalysis_process$value == "start") {
    output$ui_differenceanalysis <- renderUI({
      fluidRow(style = "height:750px;",
               br(), br(), br(), br(), br(),
               column(width = 12, style = "left:-50px;margin-top: -70px;",
                      fluidRow(
                        tags$head(tags$style(HTML('.selectize-input {border-color: black;line-height:23px; font-size:14px;height:38px}'))),
                        column(width = 7, align="left", offset = 5,
                               HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step1: Biological replicates</div>")),
                        column(width = 7, align="left", offset = 5, style="margin-bottom: -30px;", 
                               selectInput(inputId = "differenceanalysis_replicates", label = NULL, choices = list("Multiple biological replicates", "No biological replicates"), width = "340px"))
                      ),# close fluidRow
                      br(),
                      fluidRow(
                        column(width = 7, align="left", offset = 5,
                               HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step2: Upload you Matrix</div>")),

                        column(width = 1, align = "right", offset = 4,
                            shinyWidgets::dropdownButton(
                              tags$h4(p(icon("check",lib = "font-awesome"),em("Upload gene expression matrix data (the value must be an integer read value), and the file format is only supported by .csv .xlsx"),style="color:#337ab7;text-align:center;font-size:15px;")),
                              column(width = 12, align = "center", downloadButton("download_deg_matrix_template", label = "Template matrix", style = "border-color: #ccc;font-size:12px;text-align: center;")),
                              circle = TRUE, status = "default",
                              icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                            )
                          ),
                        column(width = 7, align="left", offset = 0, style="margin-bottom: -42px;",
                               fileInput(inputId = "differenceanalysis_upload_matrix", label = NULL, accept = list(".csv", ".xlsx"), width = "340px"))
                      ),# close fluidRow
                      br(),
                      uiOutput("differenceanalysis_ui")
               )#, #close cloumn
               #column(width = 6, slickROutput("differenceanalysis_slickR"), style = "right:200px;"
               #) #close cloumn
      )# close fluidRow
    })
  } else if (differenceanalysis_process$value == "processing" ) {
    output$ui_differenceanalysis <- renderUI({
      fluidRow(
        br(),br(),
        HTML('<hr style="width: 90%;border-top: 2px solid #ddd;" />'),
        column(width = 12, align = "left", style = "width:80%;margin-left:10%;height:600px;",
               h2("This page will be automatically updated, please wait a few minutes."),
               h2("This page will be automatically updated, please wait a few minutes.")),
        HTML('<hr style="width: 90%;border-top: 2px solid #ddd;" />')#close cloumn
      )# close fluidRow
    })# close output$ui_datacheck
  } else if (differenceanalysis_process$value == "end1") {
     output$ui_differenceanalysis <- renderUI({
      fluidRow(
        br(), br(),
        HTML('<hr style="width: 90%;border-top: 2px solid #ddd;" />'),
        column(width = 12, align = "left", style = "width:80%;margin-left:10%;height:auto;",
               h2("Please check the information and press \"Start\" ... "),
               htmlOutput(outputId = "deg_wait_time", style = "font-size:15px;")
               ),#close cloumn
        HTML('<hr style="width: 90%;border-top: 2px solid #ddd;" />'),
        column(width = 6, align = "center",
               actionButton(inputId = "reste_differenceanallysis_2", label = "Cancel", icon = icon("xmark"),
                            style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;margin-bottom: 10px")
               ),#close cloumn
        column(width = 6, align = "center",
                actionButton(inputId = "run_differenceanalysis", label = "Start", icon = icon("check"),
                            style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;margin-bottom: 10px")
               )#close cloumn
      )# close fluidRow
     })# close output$ui_datacheck
  } else if (differenceanalysis_process$value == "end2") {
    output$ui_differenceanalysis <- renderUI({#Table of DEG UI
      if(!is.null(deg())) {
        fluidRow(
          br(),br(),
          HTML('<hr style="width: 90%;border-top: 2px solid #000;" />'),
          column(width = 12,style = "width:80%;margin-left:10%;height:auto;",
            column(width = 3, align = "left", h3("Differential Expression Analysis")),
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'), br(),
            column(width = 5, align = "center", style = "border: 2px solid #0006;border-radius: 30px;margin-bottom:20px;", htmlOutput(outputId = "deg_wait_time", style = "font-size:15px;")),
            column(width = 5, align = "center", plotOutput("summary_DEGs", width = "100%", height = "700px")),
            column(width = 2, align = "left", style = "top:50px;",
              column(width = 12, colourpicker::colourInput("color_up_summary_DEGs", label = NULL, "pink"),style = "margin-bottom:-13px;"),
              column(width = 12, colourpicker::colourInput("color_down_summary_DEGs", label = NULL, "skyblue"),style = "margin-bottom:-13px;"),
              column(width = 12, colourpicker::colourInput("color_not_summary_DEGs", label = NULL, "grey"),style = "margin-bottom:-13px;"),
              column(width = 12, colourpicker::colourInput("color_text_summary_DEGs", label = NULL, "black"),style = "margin-bottom:10px;"),
              column(width = 12, numericInput("genes_size_summary_DEGs", label = "Count size", value = 10, width = "100%")),
              column(width = 12, numericInput("title_size_summary_DEGs", label = "Title size", value = 15, width = "100%")),
              column(width = 6, numericInput("text_x_summary_DEGs", label = "Text X", value = 9, width = "100%")),
              column(width = 6, numericInput("text_y_summary_DEGs", label = "Text Y", value = 12, width = "100%")),
              column(width = 12, numericInput("legend_text_size_summary_DEGs", label = "Legend text size", value = 15, width = "100%")),
              column(width = 5, numericInput("width_summary_DEGs", label = "Width", value = 7), style = "width:39%"),
              column(width = 5, numericInput("height_summary_DEGs", label = "Height", value = 8), style = "left:-29px; width:39%;"),
              column(width = 6, selectInput(inputId = "summary_DEGs_format", label = "Format", choices = c("PNG" = "png", "PDF" = "pdf"),width = "80%"),style = "position: absolute;top: 491px;right: -3px;"),
              column(width = 12, align = "center", downloadButton("download_summary_DEGs", label = "Download",style = "width: 188px;border-color: #ccc;font-size:10px;text-align: center;margin-top: 10px;"))
              
            ),
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'), br(),
            column(width = 12,
              column(width = 2, HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Select Group:</div>"), offset = 0, align = "center"),# close column
              column(width = 4, align = "left", selectInput(inputId = "selectgroupname", label = NULL, choices = list.files(deg()))),# close column
              column(width = 2, align = "left", style = "margin-top: -0px;margin-left: -40px;",
                      shinyWidgets::dropdownButton(
                        circle = FALSE, status = "default", size = "sm", inputId = "degtablesetting",
                        icon = icon("gear"), width = "430px",
                        tags$h4(p(icon("eye",lib = "font-awesome"),em("Settings"),style="color:#337ab7;text-align:center;font-size:15px;")), hr(),
                        column(width = 5, h5("Log2FC cutoff:")),
                        column(width = 7, numericInput(inputId = "log2fccutoff1", label = NULL, min = 0, value = 1)),
                        column(width = 5, h5("Pvalue cutoff:")),
                        column(width = 7, numericInput(inputId = "pvaluecutoff1", label = NULL, min = 0, value = 0.05)),
                        hr(),
                        HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
                        column(width = 6, HTML("<div style='font-size:14px;'>Filter low-expr genes by:</div>")),
                        column(width = 6,
                              shinyWidgets::radioGroupButtons(inputId = "filter_low_gene", label = NULL, choices = c("Mean", "Percentage"), individual = TRUE, width = "80%", size = "xs",
                                        checkIcon = list(yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"), no = tags$i(class = "fa fa-square-o",style = "color: steelblue")))),
                        column(width = 12, uiOutput("filter_low_gene_ui")),
                        HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
                        column(width = 4, align = "center", downloadButton(outputId = "downup1", label = "UP set", style = "width:130px;"),style = "height:40px;"),
                        column(width = 4, align = "center", downloadButton(outputId = "downdown1", label = "DOWN set")),
                        column(width = 4, align = "center", downloadButton(outputId = "downdiff1", label = "DIFF set"), style = "margin-bottom:7px;"),
                        column(width = 12, align = "center", style = "margin-bottom:20px;", downloadButton(outputId = "downall1", label = "Download ALL", style = "width:385px;")),
                        HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />')
                      )
                    ), # close column
              
              column(width = 3, align = "left", style = "margin-left: -110px;margin-top: -1px;", actionButton('refresh_deg_table', 'Refresh Data', icon = icon('refresh'))),
              column(width = 1, align = "left", offset = 0, style = "left: -100px;",
                shinyWidgets::dropdownButton(
                  tags$h4(p(icon("check",lib = "font-awesome"),em("This table is a summary table of difference analysis, which can adjust the difference relationship by setting the threshold of log2FC and Pvalue, and can filter out low-expression genes by percentage or mean."),style="color:#337ab7;text-align:center;font-size:15px;")),
                  tags$h4(p(icon("check",lib = "font-awesome"),em("Some genes do not appear in the table because the filter is set to filter the low-expression genes are filtered out, or the log2FC value cannot be calculated for the gene and is automatically excluded by the system."),style="color:#337ab7;text-align:center;font-size:15px;")),
                  tags$h4(p(icon("check",lib = "font-awesome"),em("Gene number; Gene Name (Symbol); Change; difference magnification (log2FC); pvalue; P-value corrected value (padj); Expression level after sample homogenization (sample_CPM)"),style="color:#337ab7;text-align:center;font-size:15px;")),
                  circle = TRUE, status = "default",
                  icon = icon("question"), width = "300px", size = "sm", right = F, up = T)),

              column(width = 12, align = "center", style = "margin-left: 0px;margin-bottom: 40px;", DT::dataTableOutput(outputId = "degresulttable", width = "100%"))# close column
              ), # close column
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'), br(),
            column(width = 5, style = "border-right: 1px solid #ddd;margin-bottom: 20px;",
              column(width = 8, plotOutput(outputId = "vol_plot", width = "100%", height = "450px") %>% withSpinner),# close column
              column(width = 4,
                column(width = 12, align = "center", HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Settings</div>"), style = "MARGIN-TOP: -10px;"),
                column(width = 12, selectInput(inputId = "vol_plot_type", label = NULL, choices = c("Volcano plot" = "vol", "Rank plot" = "rank")), style = "margin-bottom: -10px"),
                column(width = 12, colourpicker::colourInput("color_vol_plot_up", label = NULL, "red"), style = "margin-bottom: -10px;"),
                column(width = 12, colourpicker::colourInput("color_vol_plot_down", label = NULL, "blue"), style = "margin-bottom: -10px;"),
                column(width = 12, colourpicker::colourInput("color_vol_plot_not", label = NULL, "grey"), style = "margin-bottom: -10px;"),
                column(width = 6, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Width</div>")),
                column(width = 6, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Height</div>")),
                column(width = 6, numericInput("vol_plot_width", label = NULL, value = 5.65), style = "margin-bottom: -10px;width:59%"),
                column(width = 6, numericInput("vol_plot_height", label = NULL, value = 6.15), style = "margin-bottom: -10px;position: absolute;top: 220px;width: 50%;left: 80px;"),
                column(width = 6, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Legend X</div>"), style = "width: 60%;"),
                column(width = 6, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Legend Y</div>"), style = "position: absolute;top: 265px;left: 90px;"),
                column(width = 6, numericInput("vol_plot_legend_x", label = NULL, value = 0.89), style = "margin-bottom: -10px;width: 59%;"),
                column(width = 6, numericInput("vol_plot_legend_y", label = NULL, value = 0.9), style = "margin-bottom: -10px;left: 80px;position: absolute;top: 282px;"),
                column(width = 6, checkboxInput("label_vol_plot", label = " Label", value = FALSE), align = "center", style = "margin-bottom: 15px; top: 10px;"),
                column(width = 6, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Format</div>")),
                column(width = 6, selectInput(inputId = "vol_plot_format", label = NULL, choices = c("PNG" = "png", "PDF" = "pdf")), style = "margin-bottom: -10px;position: absolute; top: 345px;left: 80px;"),
                column(width = 12, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Download</div>")),
                column(width = 12, align = "center", downloadButton("download_vol_plot", label = NULL, style = "width:100%;")),
                )# close column
              ),# close column
            column(width = 7, style = "margin-bottom:20px;margin-left: 0px;",
              column(width = 9, offset = 1, align="left",
                column(width = 4, uiOutput("deg_bar_ui1")),
                column(width = 4, uiOutput("deg_bar_ui2")),
                column(width = 4, uiOutput("deg_bar_ui3")),
                column(width = 4, uiOutput("deg_bar_ui4")),
                column(width = 4, uiOutput("deg_bar_ui5")),
                column(width = 4, uiOutput("deg_bar_ui6"))),
              column(width = 2,
                column(width = 12, align = "center", HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Settings</div>"), style = "MARGIN-TOP: -10px;width: 116px"),
                column(width = 12, colourpicker::colourInput("color_deg_bar", label = NULL, "grey"), style = "margin-bottom: 110px;width: 116px"),
                column(width = 12, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Width</div>")),
                column(width = 12, numericInput("deg_bar_width", label = NULL, value = 3), style = "margin-bottom: -10px;width: 116px"),
                column(width = 12, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Height</div>")),
                column(width = 12, numericInput("deg_bar_height", label = NULL, value = 4.5), style = "margin-bottom: -10px;width: 116px"),
                column(width = 12, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Format</div>")),
                column(width = 12, selectInput(inputId = "deg_bar_format", label = NULL, choices = c("PNG" = "png", "PDF" = "pdf")), style = "margin-bottom: -10px;width: 116px"),
                column(width = 12, HTML("<div style='font-size:13px;font-weight:600;line-height: 1.5em;'>Download</div>")),
                column(width = 12, align = "center", downloadButton("download_deg_bar", label = NULL, style = "width:86px"))
                )
            ),# close column
            column(width = 12, style = "margin-bottom: 50px;", verbatimTextOutput('deg_selected'))# close column
            ), # close column
          HTML('<hr style="width: 90%;border-top: 2px solid #000;" />'),
          column(width = 12, align = "center",
                actionButton(inputId = "allback_differenceanallysis", label = "BACK", icon = icon("arrow-left"),
                              style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #337ab7;color: white;margin-bottom: 10px")
            )#close cloumn
        )#close fluidRow
      }
    })# close output$ui_differenceanalysis
  }
})
#difference_ui
output$differenceanalysis_ui <- renderUI({
  if(input$differenceanalysis_replicates == "Multiple biological replicates"){
    column(style = "left:-5px;", width = 12,
      fluidRow(
        column(width = 7, align="left", offset = 5,
                HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step3: Download you Group info</div>")),
        column(width = 1, align = "right", offset = 4,
            shinyWidgets::dropdownButton(
              tags$h4(p(icon("check",lib = "font-awesome"),em("Download your group information table, modify the group column (the sample column does not need to be modified), and upload it to Step 4."),style="color:#337ab7;text-align:center;font-size:15px;")),
              circle = TRUE, status = "default",
              icon = icon("question"), width = "300px", size = "sm", right = F, up = T
            )
          ),
        column(width = 7, align="left", offset = 0, style="margin-bottom: -0px;",
                downloadButton(outputId = "differenceanalysis_download_groupinfo", label = "Download", icon = icon("download"), style = "width: 340px;margin-bottom: -10px;"))
      ),# close fluidRow
      br(),
      fluidRow(
        column(width = 7, align="left", offset = 5,
                HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step4: Upload you Group info</div>")),
        column(width = 1, align = "right", offset = 4,
            shinyWidgets::dropdownButton(
              tags$h4(p(icon("check",lib = "font-awesome"),em("Upload your sample group file (download the step3 file and modify the group column, set the same group name for the same group of samples)."),style="color:#337ab7;text-align:center;font-size:15px;")),
              column(width = 12, align = "center", downloadButton("download_deg_group_template", label = "Template groupinfo", style = "border-color: #ccc;font-size:12px;text-align: center;")),
              circle = TRUE, status = "default",
              icon = icon("question"), width = "300px", size = "sm", right = F, up = T
            )
          ),
        column(width = 7, align="left", offset = 0, style="margin-bottom: -40px;",
                fileInput(inputId = "differenceanalysis_upload_groupinfo", label = NULL, accept = ".csv", width = "340px"))
      ),# close fluidRow
      br(),
      fluidRow(
        column(width = 7, align="left", offset = 5,
                HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step5: Select the TR and the CO</div>")),
        column(width = 1, align = "right", offset = 4,
            shinyWidgets::dropdownButton(
              tags$h4(p(icon("check",lib = "font-awesome"),em("According to the experimental design, the TR (Treatment group) and the CO (control group) were set, and the final difference analysis results were as follows: relative to the control group, the difference multiplier of the experimental group."),style="color:#337ab7;text-align:center;font-size:15px;")),
              tags$h4(p(icon("check",lib = "font-awesome"),em("Steps: First, make sure that the matrix file and group file have been uploaded, enter the GroupName, select the experimental group and the control group, press Add to add to the list, and so on."),style="color:#337ab7;text-align:center;font-size:15px;")),
              circle = TRUE, status = "default",
              icon = icon("question"), width = "300px", size = "sm", right = F, up = T
            )
          ),
        column(width = 7, align="left", offset = 0, style="margin-bottom: 12px;",
                actionButton(inputId = "differenceanalysis_tr_co", label = p(icon("table"),("Click to select")), width = 340, style="height:38px;font-size:17px"))
      ),# close fluidRow
      uiOutput("differenceanalysis_software_ui"),
      br(),
      fluidRow(
        column(width = 2, align="center", offset = 4,
                actionButton(inputId = "reset_differenceanallysis", label = "RESET", icon = icon("rotate-left"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;")),
        column(width = 3, align="center", offset = 0,
                actionButton(inputId = "enter_differenceanallysis", label = "ENTER", icon = icon("check"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;"))
      )# close fluidRow
    )
  } else {
    column(style = "left:-5px;", width = 12,
      fluidRow(
        column(width = 7, align="left", offset = 5,
                HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step3: Select the TR and the CO</div>")),
        column(width = 1, align = "right", offset = 4,
            shinyWidgets::dropdownButton(
              tags$h4(p(icon("check",lib = "font-awesome"),em("According to the experimental design, the TR (Treatment group) and the CO (control group) were set, and the final difference analysis results were as follows: relative to the control group, the difference multiplier of the experimental group."),style="color:#337ab7;text-align:center;font-size:15px;")),
              tags$h4(p(icon("check",lib = "font-awesome"),em("Steps: First, make sure that the matrix file and group file have been uploaded, enter the GroupName, select the experimental group and the control group, press Add to add to the list, and so on."),style="color:#337ab7;text-align:center;font-size:15px;")),
              circle = TRUE, status = "default",
              icon = icon("question"), width = "300px", size = "sm", right = F
            )
          ),
        column(width = 7, align="left", offset = 0, style="margin-bottom: 12px;",
                actionButton(inputId = "differenceanalysis_tr_co", label = p(icon("table"),("Click to select")), width = 340, style="height:38px;font-size:17px"))
      ),# close fluidRow
      uiOutput("differenceanalysis_software_ui"),
      br(),
      fluidRow(
        column(width = 7, align="left", offset = 5,
                HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step5: Set the BCV for edgeR</div>")),
        column(width = 1, align = "right", offset = 4,
            shinyWidgets::dropdownButton(
              tags$h4(p(icon("check",lib = "font-awesome"),em("Booker (Biologi Karcofi Sint, Ben Variation) represents the coefficient of biological variation, which is used to adjust the negative binomial distribution model to more accurately estimate differentially expressed genes."),style="color:#337ab7;text-align:center;font-size:15px;")),
              tags$h4(p(icon("check",lib = "font-awesome"),em("If the sample is human, set BCV = 0.4 and model organism to 0.1. If there are too few differential genes, down-regulation of BCV can be tried"),style="color:#337ab7;text-align:center;font-size:15px;")),
              circle = TRUE, status = "default",
              icon = icon("question"), width = "300px", size = "sm", right = F
            )
          ),
        column(width = 7, align="left", offset = 0, style="margin-bottom: 12px;",
                selectInput(inputId = "differenceanalysis_bcv", label = NULL, choices = c("0.1"=0.1, "0.2"=0.2, "0.3"=0.3, "0.4"=0.4, "0.5"=0.5), selected = "0.2", width = "340px")),
        column(width = 7, align="left", offset = 5, style="margin-bottom: -0px;font-size:18px;",
                checkboxInput("differenceanalysis_filterbyexp", "Filter low expression genes by edgeR", value = TRUE, width = "100%"))
      ),# close fluidRow
      br(),
      fluidRow(
        column(width = 2, align="center", offset = 4,
                actionButton(inputId = "reset_differenceanallysis", label = "RESET", icon = icon("rotate-left"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;")),
        column(width = 3, align="center", offset = 0,
                actionButton(inputId = "enter_differenceanallysis", label = "ENTER", icon = icon("check"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;"))
      )# close fluidRow
    )
  }
})

#differenceanalysis_software_ui
output$differenceanalysis_software_ui <- renderUI({
  if(input$differenceanalysis_replicates == "Multiple biological replicates") {
        fluidRow(
                  column(width = 7, align="left", offset = 5,
                          HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step6: Select the analysis software</div>")),
                  column(width = 7, align="left", offset = 5, style="margin-bottom: -0px;",
                          selectInput(inputId = "differenceanalysis_software", label = NULL, choices = c("DESeq2","edgeR"), width = "340px")),
                  column(width = 7, align="left", offset = 5, style="margin-bottom: -0px; font-size:18px;",
                          checkboxInput("differenceanalysis_filterbyexp_rep", "Filter low expression genes by edgeR", value = TRUE, width = "100%"))
                )# close fluidRow
  } else {
        fluidRow(
                      column(width = 7, align="left", offset = 5,
                              HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step4: Select the analysis software</div>")),
                      column(width = 1, align = "right", offset = 4,
                          shinyWidgets::dropdownButton(
                            tags$h4(p(icon("check",lib = "font-awesome"),em("The current software used without biological replicates is edgeR, which is different from biological replicates: edgeR without biological replicates uses the officially recommended LRT test, and there are biological replicates with QL testing."),style="color:#337ab7;text-align:center;font-size:15px;")),
                            circle = TRUE, status = "default",
                            icon = icon("question"), width = "300px", size = "sm", right = F
                          )
                        ),
                      column(width = 7, align="left", offset = 0, style="margin-bottom: -25px;",
                              selectInput(inputId = "differenceanalysis_software", label = NULL, choices = c("edgeR"), width = "340px", )),
                    )# close fluidRow
  }


})

#download template
output$download_deg_matrix_template <- downloadHandler(#下载deg matrix 模板
    filename = function() {"gene_count_matrix_template.csv"},
    content = function(file) {
      data <- read.csv("template/gene_count_matrix_template.csv")
      write.csv(data, file, row.names = F, fileEncoding="GBK", quote = FALSE)
    }
)
output$download_deg_group_template <- downloadHandler(#下载deg matrix 模板
    filename = function() {"groupinfo_template.csv"},
    content = function(file) {
      data <- read.csv("template/groupinfo_template.csv")
      colnames(data) <- c("sample", "group")
      write.csv(data, file, row.names = F, fileEncoding="GBK", quote = FALSE)
    }
)

# RESET
observeEvent(input$reset_differenceanallysis,{
  shinyjs::reset("differenceanalysis_upload_matrix")
  shinyjs::reset("differenceanalysis_upload_groupinfo")
  values$upload_state1 = NULL
  values$upload_state2 = NULL
})
observeEvent(input$reste_differenceanallysis_2,{
  differenceanalysis_process$value <- "start"
  shinyjs::reset("differenceanalysis_upload_matrix")
  shinyjs::reset("differenceanalysis_upload_groupinfo")
  values$upload_state1 = NULL
  values$upload_state2 = NULL
})
# creat temp dir
deg_prjpath <- reactive({
  if(! is.null(input$differenceanalysis_upload_matrix$datapath) & values$upload_state1 == 'uploaded' & values$upload_state2 == 'uploaded'){
    prjpath <- prjpath("differentanalysis")
    deggroupfile <- data.frame(GroupName = character(), TR = character(), CO = character())
    filename <- paste0(prjpath,"deggroup.csv")
    write.csv(deggroupfile, filename)
    return(prjpath)
  } else {
     return(NULL)
  }
})
# upload_state
values <- reactiveValues(upload_state1 = "NULL", upload_state2 = "NULL")
observeEvent(input$differenceanalysis_upload_matrix, {values$upload_state1 <- 'uploaded'})
observeEvent(input$differenceanalysis_upload_groupinfo, {values$upload_state2 <- 'uploaded'})
observeEvent(input$differenceanalysis_replicates, {
  if(input$differenceanalysis_replicates == "No biological replicates"){values$upload_state2 <- 'uploaded'}
})

# read countdata 
readscountdata <- reactive({#读取readscount矩阵，返回datafarme，header=T， rowname=F
  countdata <- input$differenceanalysis_upload_matrix
  if (is.null(countdata) & values$upload_state1 != "uploaded" ) {
    return(NULL)
  }else {
    shinyjs::reset("differenceanalysis_upload_groupinfo")
    fileformat <- strsplit(countdata$datapath, "\\.")[[1]][-1]
    if (fileformat == "csv") {
        countdata <- read.csv(countdata$datapath, header = TRUE)
    }else if (fileformat == "xlsx") {
        countdata <- openxlsx::read.xlsx(countdata$datapath, colNames = TRUE, rowNames = F)
    }else {
          shinyjs::runjs('alert("ERROR: Please check the format of matrix (.csv / .xlsx).")')
          shinyjs::reset("differenceanalysis_upload_matrix")
          shinyjs::reset("differenceanalysis_upload_groupinfo")
          return(NULL)
    }
  }

  # sample name formay detect
  if (!check_column_names(input$differenceanalysis_upload_matrix$datapath)) {
    shinyjs::reset("differenceanalysis_upload_matrix")
    shinyjs::reset("differenceanalysis_upload_groupinfo")
    shinyjs::runjs('alert("ERROR: The sample name can only be composed of <a~z A~Z 0~9 _ .>, and only the beginning of the letter is supported. Please check that your sample name is named correctly")')
    return(NULL)
  }

  countdata
})
# download groupinfo
deggroupinfotemplate <- reactive({#deggroupinfo模板(生物学重复分组), 返回datafarme header=T，rowname=1
  if (is.null(readscountdata())) {
    return(NULL)
  }else {
      readscountdata <- readscountdata()
      group <- colnames(readscountdata)[-1]
      group <- data.frame(group)
      rownames(group) <- group$group
      group
  }
})
output$differenceanalysis_download_groupinfo <- downloadHandler(#下载groupinfo模板
  filename = function() {"groupinfo.csv"},
  content = function(file) {
    if(is.null(readscountdata())) {
        shinyjs::runjs('alert("ERROR: Please upload matrix first")')
        
      }else {
        data <- deggroupinfotemplate()[-1,]
        write.csv(deggroupinfotemplate(), file, row.names = TRUE, fileEncoding="GBK", quote = FALSE)
    }
  }
)
# read groupinfo
deggroupinfo <- reactive({
  if(input$differenceanalysis_replicates == "Multiple biological replicates") {
    groupinfo <- input$differenceanalysis_upload_groupinfo
    if(!is.null(groupinfo) & values$upload_state2 == "uploaded"){
      fileformat <- strsplit(groupinfo$datapath, "\\.")[[1]][-1]
      if (fileformat == "csv") {
        groupinfo <- read.csv(groupinfo$datapath, header = TRUE)
      }else if (fileformat == "xlsx") {
        groupinfo <- openxlsx::read.xlsx(groupinfo$datapath, colNames = TRUE, rowNames = F)
      }
      colnames(groupinfo) <- c("sample", "group")
      if(nrow(groupinfo) >= 1000){
        shinyjs::runjs('alert("ERROR: Check you group info file (There are more than thousand lines).")')
      }
    } else {
      return(NULL)
    }
  } else {
     if(!is.null(readscountdata())) {
      groupinfo <- data.frame(sample = colnames(readscountdata())[-1], group = colnames(readscountdata())[-1])
     } else {
        shinyjs::runjs('alert("ERROR: Please upload you Matrix first.")')
        return(NULL)
     }
  }
  groupinfo
})
# select tr co
editcounts <- reactiveValues(deggroupcount = 0)#定义一个reactivevalues，每次操作表格都改变，用于触发result_degfile等
observeEvent(input$close, {#触发close按钮
  removeModal()
})
observeEvent(input$addgroup, {#触发add按钮
  if(!is.null(input$Id082) & !is.null(input$groupname)) {
    if(length(input$Id082) == 2) {
      if(input$Id082[1] == input$Id082[2]){shinyjs::runjs('alert("TR cannot be the same as CO")'); return(NULL)}
      olddeggroupfile <- read.csv(paste0(deg_prjpath(),"deggroup.csv"), row.names = 1)
      newdeggroupfile <- data.frame(GroupName = input$groupname, TR = input$Id082[1], CO = input$Id082[2])
      groupfile <- rbind(olddeggroupfile, newdeggroupfile)
      write.csv(groupfile, paste0(deg_prjpath(), "deggroup.csv"))
      editcounts$deggroupcount <- editcounts$deggroupcount + 1
    }
  }
})
observeEvent(input$deleterow, {#触发delete按钮
  if(!is.null(result_degfile()) & input$deleterowid <= nrow(result_degfile())) {
    if(nrow(result_degfile()) != 0) {
      row <- input$deleterowid
      olddeggroupfile <- read.csv(paste0(deg_prjpath(), "deggroup.csv"))
      newdeggroupfile <- olddeggroupfile[-row, ]
      groupfile <- newdeggroupfile[, -1]
      if (nrow(groupfile) != 0){rownames(groupfile) <- c(1: nrow(groupfile))} # nolint
      write.csv(groupfile, paste0(deg_prjpath(), "deggroup.csv"))
      editcounts$deggroupcount <- editcounts$deggroupcount - 1
      #print(editcounts$deggroupcount)#for debug
    }
  }
})
observeEvent(input$reset, {#触发重置DEGgroupInfo按钮
  if (!is.null(deg_prjpath())) {
    editcounts$deggroupcount <- 0
    deggroupfile <- data.frame(GroupName = character(), TR = character(), CO = character())
    filename <- paste0(deg_prjpath(), "deggroup.csv")
    write.csv(deggroupfile, filename)
  }
})
result_degfile <- reactive({#最终deggroupinfo, rowname = F
  if(editcounts$deggroupcount != 0){
    table <- read.csv(paste0(deg_prjpath(), "deggroup.csv"))
    colnames(table)[1] <- "ID"
  }else{return(NULL)}
  table
})
output$degtable <- renderTable(#输出result_degfile的控件
  result_degfile()
)
observeEvent(input$differenceanalysis_tr_co,{#DEGgroupInfo按钮的UI
    showModal(
      modalDialog(
        title = "Add the different gene grouping information",
        uiOutput(outputId = "DEGgroupInfo"),
        easyClose = FALSE,
        size = "m",
        footer = tagList(
          column(6, align = "left", actionButton("reset", "Reset")),
          column(6, align = "right", actionButton("close", "Confirm"))
        )
      )
    )
})
output$DEGgroupInfo <- renderUI({#DEGgroupInfo UI
  if (!is.null(deggroupinfo())){
    fluidRow(
      tags$head(tags$style(HTML('.form-control {height: 37.5px;border: 1px solid #000;}'))),
      column(2, h5("GroupName:"), style = "top:2px;"),
      column(3, textInput(inputId = "groupname", label = NULL, value = NULL), style = "top:1px;"),
      column(5, shinyWidgets::pickerInput(
                          inputId = "Id082",
                          label = NULL,
                          choices = list(TR = unique(deggroupinfo()[,2]), CO = unique(deggroupinfo()[,2])),
                          multiple = TRUE,
                          options = list("max-options-group" = 1)
                        )),
      column(2, actionButton(inputId = "addgroup", label = "Add")),
      column(12, align = "center", tableOutput("degtable")),
      column(12, align = "center",
            if(!is.null(result_degfile())){
              column(12, align = "center",
                column(2, offset = 2, h5("Row ID:")),
                column(4, numericInput(inputId = "deleterowid", label = NULL, value = 1, min = 1)),
                column(1, actionButton("deleterow", label = "Delete"))
              )
            }
          )
    )
  }
})
# ENTER
observeEvent(input$enter_differenceanallysis,{
  print(head(readscountdata()))
  print(head(deggroupinfo()))
  print(head(result_degfile()))
  write.csv(readscountdata(), paste0(deg_prjpath(), "readscountdata.csv"), quote = F, row.names = F)
  write.csv(deggroupinfo(), paste0(deg_prjpath(), "deggroupinfo.csv"), quote = F, row.names = F)

  if (!is.null(readscountdata()) & !is.null(deggroupinfo()) & !is.null(result_degfile())) {
    data <- unlist(matrix(readscountdata()[, -1]))
    if (all(data == floor(data)) == FALSE & input$differenceanalysis_software == "DESeq2") {#判断整数
      shinyjs::runjs('alert("ERROR: The gene reads must be an integer.")')
      shinyjs::reset("differenceanalysis_upload_matrix")
      shinyjs::reset("differenceanalysis_upload_groupinfo")
      editcounts$deggroupcount <- 0
      return(NULL)
    }
    print("checkpoint1")
    matrix_sample <- colnames(readscountdata())
    groupinfo_sample <- deggroupinfo()$sample
    print("checkpoint2")
    for (i in groupinfo_sample) {#检查分组文件样品名是否与矩阵中样品名一致
      if (i %in% matrix_sample == FALSE) {
        shinyjs::runjs('alert("ERROR: Samples in groupinfo are not match with your matrix samples.")')
        shinyjs::reset("differenceanalysis_upload_matrix")
        shinyjs::reset("differenceanalysis_upload_groupinfo")
        editcounts$deggroupcount <- 0
        return(NULL)
      }
    }
    print("checkpoint3")
    differenceanalysis_process$value <- "processing"
  } else {
     shinyjs::runjs('alert("ERROR: Please upload matrix, group info and then set the TR & CO.")')
     differenceanalysis_process$value <- "start"
  }
})

# write running info file
deg_process <- reactive({
  if(differenceanalysis_process$value == "processing"){
    if(input$differenceanalysis_replicates == "No biological replicates") {
      groupinfo = data.frame(sample = colnames(readscountdata())[-1], group = colnames(readscountdata())[-1])
      write.csv(groupinfo, paste0(deg_prjpath(), "groupinfo.csv"), row.names = F, quote = F )
    print("checkpoint4")
      deg_process_file <- data.frame(path = deg_prjpath(),
                                    replicates = input$differenceanalysis_replicates,
                                    software = input$differenceanalysis_software,
                                    matrix = input$differenceanalysis_upload_matrix$datapath,
                                    groupinfo = paste0(deg_prjpath(), "groupinfo.csv"),
                                    degfile = paste0(deg_prjpath(),"deggroup.csv"))
                      
      write.table(deg_process_file, paste0(deg_prjpath(), "deg_process.info"), row.names = F, quote = F, sep = "\t")
      differenceanalysis_process$value = "end1"
    print("checkpoint5")
    }else {
      deg_process_file <- data.frame(path = deg_prjpath(),
                                    replicates = input$differenceanalysis_replicates,
                                    software = input$differenceanalysis_software,
                                    # matrix = input$differenceanalysis_upload_matrix$datapath,
                                    matrix = paste0(deg_prjpath(), "readscountdata.csv"),
                                    #groupinfo = input$differenceanalysis_upload_groupinfo$datapath,
                                    groupinfo = paste0(deg_prjpath(), "deggroupinfo.csv"),
                                    degfile = paste0(deg_prjpath(),"deggroup.csv"))
                      
      write.table(deg_process_file, paste0(deg_prjpath(), "deg_process.info"), row.names = F, quote = F, sep = "\t")
      differenceanalysis_process$value = "end1"
    print("checkpoint6")
    }

    return("okkk")
  }else {
     return(NULL)
  }
})
# check running info 
deg_check <- reactive({
  if(!is.null(deg_process())) {
    running_info <- read.table(paste0(deg_prjpath(), "deg_process.info"), header = TRUE, sep = "\t")

    replicates = running_info$replicates[1]
    software = running_info$software
    matrix = read.csv(running_info$matrix[1], header = TRUE)
    groupinfo = read.csv(running_info$groupinfo[1],header = TRUE)
    degfile = read.csv(running_info$degfile[1],header = TRUE)
    print("checkpoint7")
    Num_genes = nrow(matrix)
    Num_sample = nrow(groupinfo)
    group = unique(groupinfo$group)
    print("checkpoint8")
    running_info$Num_genes <- Num_genes
    running_info$Num_sample <- Num_sample
    running_info$Num_group <- length(group)
    running_info$time <- Sys.time() 
    running_info$software <- input$differenceanalysis_software
    print("checkpoint9")
    running_info_groupinfo <- data.frame(group = "sample")
    for(i in group){ 
      running_info_groupinfo$i <- paste0("<div style='font-size:15px;font-weight:600;display:inline-block;'>", i,": &nbsp &nbsp</div>",  paste0(paste(unlist(groupinfo[groupinfo$group == i ,1]),collapse = "; "), "<br/>"))
      colnames(running_info_groupinfo)[ncol(running_info_groupinfo)] <- paste0(i, ": ")
    }

    write.table(running_info, paste0(deg_prjpath(), "deg_process.info"), row.names = F, quote = F, sep = "\t")
    write.table(running_info_groupinfo, paste0(deg_prjpath(), "deg_process_group.info"), row.names = F, quote = F, sep = "\t")
    result <- list(running_info, running_info_groupinfo)
    return(result)
  }else {
     return(NULL)
  }
})
# print the deg info 
output$deg_wait_time <- renderText({
  print("checkpoint10")
  if(differenceanalysis_process$value == "end1" | differenceanalysis_process$value == "end2"){
    running_info <- read.table(paste0(deg_prjpath(), "deg_process.info"), header = TRUE, sep = "\t")
    deg_process_group <- read.table(paste0(deg_prjpath(), "deg_process_group.info"), header = TRUE, sep = "\t")
    Num_genes <- paste0("Number of genes: ", running_info$Num_genes)
    Num_samples <- paste0("Number of samples: ", running_info$Num_sample)
    Num_groups <- paste0("Number of groups: ", running_info$Num_group)
    prj_time <- paste0("Date: ", running_info$time)
    software <- paste0("Software: ", running_info$software)
    Basic_information <- paste("<div style='font-size:20px;font-weight:600;line-height: 1em;'><br/>Basic information</div>",prj_time, Num_genes, Num_samples, Num_groups, software, sep = '<br/>')
    
    running_info_groupinfo <- read.table(paste0(deg_prjpath(), "deg_process_group.info"), header = TRUE, sep = "\t", row.names = 1)
    Sample_information <- paste0("<div style='font-size:20px;font-weight:600;line-height: 1em;'><br/>Sample information<br/><br/></div>", paste(unlist(running_info_groupinfo[1,]), collapse=""))
    print("checkpoint11")
    deggroup <- read.csv(paste0(deg_prjpath(), "deggroup.csv"), header = T, row.names = 1)
    deggroup$all <- paste0("<div style='font-size:15px;font-weight:600;display:inline-block;'>", deggroup[,1], ": &nbsp &nbsp</div>", 
                            "<div style='font-size:15px;font-weight:600;display:inline-block;'>", deggroup[,2], "&nbsp</div>",
                            "vs", 
                            "<div style='font-size:15px;font-weight:600;display:inline-block;'>&nbsp", deggroup[,3], "&nbsp</div>","<br/>")

    Group_information <- paste0("<div style='font-size:20px;font-weight:600;line-height: 1em;'><br/>Group information<br/><br/></div>", paste(unlist(deggroup$all), collapse=""))

    sink(paste0(deg_prjpath(), "deg.info"))
    print(prj_time);print(Num_genes);print(Num_samples);print(Num_groups);print(software)
    sink()
    print("checkpoint12")
    a <- HTML(paste0("<br/>", Basic_information, "<br/>", Sample_information, "<br/>", Group_information, "<br/><br/>"))
    a
  }else {
     return(NULL)
  }
})
observe({
  if(!is.null(deg_check())){
        differenceanalysis_process$value = "end1"
  }
})

## start difference analysis
# running status
deg_status <- reactiveValues(status = 0, status2 = 0)
observeEvent(input$run_differenceanalysis, {
  deg_status$status  <- 1
  differenceanalysis_process$value = "end2"
})
deg <- reactive({
  if(deg_status$status == 1 & deg_status$status2 == 0) {
    running_info <- read.table(paste0(deg_prjpath(), "deg_process.info"), header = TRUE, sep = "\t")
    replicates = running_info$replicates[1]
    software = running_info$software
    fileformat <- strsplit(running_info$matrix[1], "\\.")[[1]][-1]
    if (fileformat == "csv") {
        matrix = read.csv(running_info$matrix[1], header = TRUE, row.names = 1)
    }else if (fileformat == "xlsx") {
        matrix <- openxlsx::read.xlsx(running_info$matrix[1], colNames = TRUE, rowNames = TRUE)
    }
    groupinfo = read.csv(running_info$groupinfo[1],header = TRUE, row.names = 1)
    deggroupinfo = read.csv(running_info$degfile[1],header = TRUE, row.names = 1)

    readscount <- matrix; readscount <- readscount[order(names(readscount))]
    samplegroupinfo <- groupinfo; samplegroupinfo <- t(samplegroupinfo); samplegroupinfo <- as.data.frame(samplegroupinfo); samplegroupinfo <- samplegroupinfo[order(names(samplegroupinfo))]

    readscount <- rbind(readscount, samplegroupinfo)
    # 创建文件夹
      path <- paste0(deg_prjpath(), "deg", "/")
      if (dir.exists(paste0(deg_prjpath(), "deg"))) {unlink(paste0(deg_prjpath(), "deg"), recursive = T)}
      dir.create(path)

    #开始分析
    withProgress(message = 'Running differential expression analysis',
      detail = 'This may take a few minutes...', value = 0, {
        for (i in 1: nrow(deggroupinfo)) { # nolint
            incProgress(1/nrow(deggroupinfo))

            name <- deggroupinfo[i, 1]
            print(paste0("正在进行", name))
            TR <- as.character(deggroupinfo[i,2])
            CO <- as.character(deggroupinfo[i,3])
            name <- paste0(name, "(TR=", TR, ", CO=", CO, ")")

            # TRreadscount <- readscount[, apply(readscount,2,function(x) any(x==TR))]
            # COreadscount <- readscount[, apply(readscount,2,function(x) any(x==CO))]
            # degreadscount <- cbind(TRreadscount, COreadscount)

            degreadscount <- readscount[, apply(readscount,2,function(x) any(x==TR | x== CO))]
            # print(degreadscount)
            group <- t(degreadscount[nrow(degreadscount),])#group文件必须打竖
            # print(group)#for debug;

            degreadscount2 <- degreadscount
            degreadscount2["group",] <- ifelse(degreadscount2["group",] == TR, paste0("TR:",TR), ifelse(degreadscount2["group",] == CO, paste0("CO:",CO), "NA"))
            

            degreadscount <- degreadscount[-nrow(degreadscount),]
            degreadscount <- as.data.frame(lapply(degreadscount, as.numeric), row.names = rownames(degreadscount))
            

            #DESeq2 & Multiple biological replicates #有生物学重复组的正在用(必须为整数，)
            if(software == "DESeq2" & replicates == "Multiple biological replicates"){
              print("DESeq2")#for debug

              if(input$differenceanalysis_filterbyexp_rep == TRUE){
                dge <- edgeR::DGEList(counts=degreadscount)
                keep.exprs <- edgeR::filterByExpr(dge) #自动筛选过滤低表达基因
                dge <- dge[keep.exprs,,keep.lib.sizes=FALSE]
                degreadscount <- data.frame(dge$counts)
              }

              dds <- DESeq2::DESeqDataSetFromMatrix(countData=degreadscount, colData=group, design=~group)
              # keep <- rowSums(DESeq2::counts(dds)) >= 5*ncol(degreadscount)#过滤掉总reads小于5倍样本数量的基因
              # dds <- dds[keep,]

              dds <- DESeq2::DESeq(dds)#多线程
              # print(TR)#for debug
              # print(CO)#for debug
              DEG <- DESeq2::results(dds, contrast=c("group", TR, CO))#处理组在前，对照组在后
              DEG <- as.data.frame(DEG)
              deseq2DEG <- data.frame(gene = rownames(DEG), Change = rownames(DEG), DESeq2_log2FC = DEG$log2FoldChange, DESeq2_pvalue = DEG$pvalue,  DESeq2_padj = DEG$padj)
              result <- deseq2DEG
            }

            #EdfeR 有生物学重复官方推荐用拟合广义线性模型（QL-test #弃用
            if(software == "edgeR" & replicates == "Multiple biological replicates"){
              print("有生物重复，edgeR")
              group_test <- ifelse(group == TR, "TR", group)
              group_test <- ifelse(group_test == CO, "CO", group_test)
              group_edger <- data.frame(group_test)
              group_edger <- group_edger$group

              dge <- edgeR::DGEList(counts=degreadscount, group=group_edger)
              if(input$differenceanalysis_filterbyexp_rep == TRUE){
                keep.exprs <- edgeR::filterByExpr(dge) #自动筛选过滤低表达基因
                dge <- dge[keep.exprs,,keep.lib.sizes=FALSE] 
              }

              dge <- edgeR::calcNormFactors(dge, method = 'TMM') #归一化因子用于 normalizes the library sizes
              dge <- edgeR::estimateDisp(dge, robust=T) 
              fit <- edgeR::glmQLFit(dge, robust=T)  #拟合模型 
              lt <- edgeR::glmQLFTest(fit)

              tempDEG <- edgeR::topTags(lt, n = Inf)
              tempDEG <- as.data.frame(tempDEG)
              edgeRDEG <- data.frame(gene = rownames(tempDEG), Change = rownames(tempDEG), edgeR_log2FC = tempDEG$logFC, edgeR_pvalue = tempDEG$PValue, edgeR_FDR = tempDEG$FDR)
              result <- edgeRDEG
            }

            #limma(voom) & Multiple biological replicates #弃用
            if(software == "limma" & replicates == "Multiple biological replicates"){
              express_rec <- degreadscount
              express_rec[express_rec==0] <- 1
              limmagroup <- data.frame(group)
              limmagroup <- factor(limmagroup$group,levels = c(TR, CO))
              design <- model.matrix(~0+limmagroup)
              colnames(design) <- c(TR,CO)
              rownames(design) <- rownames(group)
              contrast.matrix <- limma::makeContrasts(contrasts=paste0(TR,'-',CO), levels=design)

              group_test <- gsub(TR,"TR",group); group_test <- gsub(CO,"CO",group_test)
              group_edger <- data.frame(group_test)
              group_edger <- group_edger$group
              dge <- edgeR::DGEList(counts=degreadscount, group=group_edger)
              keep.exprs <- edgeR::filterByExpr(dge) #自动筛选过滤低表达基因
              dge <- dge[keep.exprs,,keep.lib.sizes=FALSE] 
              dge <- edgeR::calcNormFactors(dge, method = 'TMM') #归一化因子用于 normalizes the library sizes
              dge <- edgeR::estimateDisp(dge, robust=T)

              de <- limma::voom(dge,design,plot=TRUE, normalize="quantile")#voom
              fit1 <- limma::lmFit(de, design)               #线性拟合
              fit2 <- limma::contrasts.fit(fit1, contrast.matrix) #统计检验
              efit <- limma::eBayes(fit2, trend=F)
              tempDEG <- limma::topTable(efit, coef=paste0(TR,'-',CO), n=Inf)
              tempDEG  <- na.omit(tempDEG)
              limmaDEG <- data.frame(gene = rownames(tempDEG), Change = rownames(tempDEG), limma_log2FC = tempDEG$logFC, limma_pvalue = tempDEG$P.Value, limma_padj = tempDEG$adj.P.Val)
              result <- limmaDEG
            }

            #EdgeR 无生物学重复 likelihood ratio test #无生物学重复组的正在用
            if(software == "edgeR" & replicates == "No biological replicates"){
              
              group_test <- gsub(TR,"TR",group); group_test <- gsub(CO,"CO",group_test)
              group_edger <- data.frame(group_test)
              group_edger <- group_edger$group
              # print(group_edger) #for debug
              dge <- edgeR::DGEList(counts=degreadscount, group=group_edger)

              if(input$differenceanalysis_filterbyexp == TRUE){
                keep.exprs <- edgeR::filterByExpr(dge) #自动筛选过滤低表达基因
                dge <- dge[keep.exprs,,keep.lib.sizes=FALSE]
              }
              
              dge <- edgeR::calcNormFactors(dge, method = 'TMM') #归一化因子用于 normalizes the library sizes
              # print(head(dge)) # for debug
              dge <- edgeR::estimateDisp(dge, robust=T)
              fit <- edgeR::glmFit(dge, robust=T, dispersion = as.numeric(input$differenceanalysis_bcv) ^ 2)  #拟合模型 
              lt <- edgeR::glmLRT(fit)
              tempDEG <- edgeR::topTags(lt, n = Inf)
              tempDEG <- as.data.frame(tempDEG)
              edgeRDEG <- data.frame(gene = rownames(tempDEG), Change = rownames(tempDEG), edgeR_log2FC = tempDEG$logFC, edgeR_pvalue = tempDEG$PValue, edgeR_FDR = tempDEG$FDR)
              result <- edgeRDEG
            }

            deg_status$status2 == 1
            # write result

            result["group",] <- c(rep("NA1", ncol(result)))




            cpm <- t(t(degreadscount) / colSums(degreadscount) * 1000000)
            cpm <- data.frame(cpm)
            colnames(cpm) <- paste0(colnames(cpm),"_CPM")
            cpm$gene <- rownames(cpm)
            cpm <- cpm[cpm$gene %in% result$gene,]


            result <- dplyr::inner_join(result, cpm, by = "gene")
            result <- na.omit(result)

            colnames(result)[1] <- "Gene"
            result["group",] <- c(rep("NO",5),degreadscount2["group",])


            file <- paste0(path, name)
            write.csv(result, file = file, quote = F, row.names = F)
          }
      })
    
    #分析完成
    
    return(path)
  } else {
     return(NULL)
  }
})

# deg table
getdegtable <- reactive({#DEGstable
  if (!is.null(deg())){

    a <- read.csv(paste0(deg(), input$selectgroupname))
    log2fccutoff1 <- input$log2fccutoff1
    pvaluecutoff1 <- input$pvaluecutoff1

    a <- a[-nrow(a),]
    a[,3:ncol(a)] <- lapply(a[,3:ncol(a)], as.numeric)

    a$Change <- ifelse(a[, 4] <= pvaluecutoff1 & abs(a[,3]) >= log2fccutoff1, ifelse(a[, 3] >= log2fccutoff1 , "UP", "DOWN"), "NOT")

    # running_info <- read.table(paste0(deg_prjpath(), "deg_process.info"), header = TRUE, sep = "\t")
    # readscount = read.csv(running_info$matrix[1], header = TRUE, row.names = 1)
    # readscount <- readscountdata()
    # rownames(readscount) <- readscount[,1]
    # readscount <- readscount[,-1]
    # cpm <- t(t(readscount) / colSums(readscount) * 1000000)
    # cpm <- data.frame(cpm)
    # colnames(cpm) <- paste0(colnames(cpm),"_CPM")
    # cpm$gene <- rownames(cpm)
    # cpm <- cpm[cpm$gene %in% a$gene,]
    # a <- dplyr::inner_join(a, cpm, by = "gene")

    a <- na.omit(a)
    #print(a[1:10,])#for debug

    #添加symbol for rice (IRGSP, MSU, MH63RS3)

    # if( unique(stringr::str_detect(a$Gene,'Os*g*')) == TRUE ){#IRGSP
    #   CGSNL_symbol <- read.csv("symbol/Oryza sativa(IRGSP)/loc_os_annotation.txt", header = T, sep = "\t")
    #   CGSNL_symbol <- data.frame(Gene = CGSNL_symbol$Os, Symbol = CGSNL_symbol$CGSNL_symbol)
    #   CGSNL_symbol <- CGSNL_symbol[CGSNL_symbol$Gene %in% a$Gene & CGSNL_symbol$Symbol != "",]
    #   a <- dplyr::full_join(CGSNL_symbol[CGSNL_symbol$Gene %in% a$Gene,], a, by = "Gene")
    #   a[is.na(a)]="-"
    # } else
    # if ( unique(stringr::str_detect(a$Gene,'LOC_Os*g\\d\\d\\d\\d\\d')) == TRUE ) {#MSU
    #   CGSNL_symbol <- read.csv("symbol/Oryza sativa(IRGSP)/loc_os_annotation.txt", header = T, sep = "\t")
    #   CGSNL_symbol <- data.frame(Gene = CGSNL_symbol$LOC, Symbol = CGSNL_symbol$CGSNL_symbol)
    #   CGSNL_symbol <- CGSNL_symbol[CGSNL_symbol$Gene %in% a$Gene & CGSNL_symbol$Symbol != "" & CGSNL_symbol$Symbol != " " & CGSNL_symbol$Symbol != "-",]
    #   # CGSNL_symbol <- data.table::as.data.table(CGSNL_symbol)
    #   # CGSNL_symbol <- CGSNL_symbol[, .(Symbol = paste(Symbol, collapse = ",")), by = Gene]
    #   # CGSNL_symbol <- as.data.frame(CGSNL_symbol)
    #   a <- dplyr::full_join(CGSNL_symbol[CGSNL_symbol$Gene %in% a$Gene,], a, by = "Gene")
    #   # a <- a[,-3]
    #   a[is.na(a)]="-"
    # } else
    # if ( unique(stringr::str_detect(a$Gene,'OsMH_*G*')) == TRUE ) {#MH63RS3
    #   CGSNL_symbol <- read.csv("symbol/Oryza sativa(IRGSP)/mh63RS3_loc.pair.info.txt", header = T, sep = "\t")
    #   CGSNL_symbol <- data.frame(Gene = CGSNL_symbol$MH63RS3, Symbol = CGSNL_symbol$CGSNL_symbol)
    #   CGSNL_symbol <- CGSNL_symbol[CGSNL_symbol$Gene %in% a$Gene & CGSNL_symbol$Symbol != "",]
    #   a <- dplyr::full_join(CGSNL_symbol[CGSNL_symbol$Gene %in% a$Gene,], a, by = "Gene")
    #   a[is.na(a)]="-"
    # }
    # #

    a <- a[order(a$Gene),]

    # print(colnames(a))

    rownames(a) <- 1:nrow(a) # nolint

    
    #过滤低表达基因
    rank_a <- data.frame(a[,stringr::str_ends(colnames(a), "_CPM")], row.names = a$Gene)
    rank_a <- data.frame(gene = rownames(rank_a), cpm = apply(rank_a, 1, mean))
    if(input$filter_low_gene == "Mean"){
      cutoff = ifelse(is.null(input$deg_cpm_mean_cut_off), 0, input$deg_cpm_mean_cut_off)
      gene_list <- rank_a[rank_a$cpm >= cutoff, "gene"]
      a <- a[a$Gene %in% gene_list, ]
    }else if (input$filter_low_gene == "Percentage") {
      cutoff = ifelse(is.null(input$deg_cpm_percentage_cut_off), 0, input$deg_cpm_percentage_cut_off)
      cutoff <- quantile(rank_a$cpm, input$deg_cpm_percentage_cut_off)
      gene_list <- rank_a[rank_a$cpm >= cutoff, "gene"]
      a <- a[a$Gene %in% gene_list, ]
    }
  

  }else{return(NULL)}
  a
})
output$degresulttable <- DT::renderDataTable(getdegtable(), rownames = T, selection = list(mode = 'multiple'), options = list(dom = 'fltipr', pageLength = 10, scrollX=TRUE, orderClasses = T, 
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
                                                                                                                                                      )
              )

proxy = dataTableProxy('degresulttable', session = shiny::getDefaultReactiveDomain(), deferUntilFlush = TRUE)
observeEvent(input$refresh_deg_table, {
  reloadData(
  proxy,
  resetPaging = TRUE,
  clearSelection = "all")
})
output$downup1 <- downloadHandler(#下载upset
  filename = function() {paste0(input$selectgroupname,"_up.txt")},
  content = function(file) {
    if(!is.null(getdegtable())){
      a <- getdegtable()
      up <- a[a$Change == "UP","Gene"]
      up <- data.frame(up); up <- na.omit(up)
      colnames(up)[1] <- paste0("#",input$selectgroupname,"_up")
      write.table(up, file, row.names = F, fileEncoding="GBK", quote = FALSE,sep = "\t")
    }
  }
)
output$downdown1 <- downloadHandler(#下载downset
  filename = function() {paste0(input$selectgroupname,"_down.txt")},
  content = function(file) {
    if(!is.null(getdegtable())){
      a <- getdegtable()
      up <- a[a$Change == "DOWN","Gene"]
      up <- data.frame(up); up <- na.omit(up)
      colnames(up)[1] <- paste0("#",input$selectgroupname,"_down")
      write.table(up, file, row.names = F, fileEncoding="GBK", quote = FALSE,sep = "\t")
    }
    
  }
)
output$downdiff1 <- downloadHandler(#下载diffset
  filename = function() {paste0(input$selectgroupname,"_diff.txt")},
  content = function(file) {
    if(!is.null(getdegtable())){
      a <- getdegtable()
      up <- a[a$Change == "UP" | a$Change == "DOWN","Gene"]
      up <- data.frame(up); up <- na.omit(up)
      colnames(up)[1] <- paste0("#",input$selectgroupname,"_diff")
      write.table(up, file, row.names = F, fileEncoding="GBK", quote = FALSE,sep = "\t")
    }
    
  }
)
output$downall1 <- downloadHandler(#下载allset
  filename = function() {paste0(input$selectgroupname,"_DEG_all.xlsx")},
  content = function(file) {
    if(!is.null(getdegtable())){
      a <- getdegtable()
      sheetName = ifelse(stringr::str_count(input$selectgroupname) >= 25, paste0(stringr::str_sub(input$selectgroupname,end=25),' ...'), input$selectgroupname)
      openxlsx::write.xlsx(a, file, sheetName = sheetName, fileEncoding="GBK")
      #write.csv(a, file, row.names = F, fileEncoding="GBK", quote = FALSE)
    }
    
  }
)

# all back
observeEvent(input$allback_differenceanallysis,{
  deggroupfile <- data.frame(GroupName = character(), TR = character(), CO = character())
  filename <- paste0(deg_prjpath(), "deggroup.csv")
  shinyjs::reset("differenceanalysis_upload_matrix")
  shinyjs::reset("differenceanalysis_upload_groupinfo")
  differenceanalysis_process$value <- "start"
  deg_status$status = 0
  deg_status$status2 = 0
  values$upload_state1 <- "NULL"
  values$upload_state2 <- "NULL"

  editcounts$deggroupcount <- 0

  write.csv(deggroupfile, filename)
})

#filter_low_gene_ui
output$filter_low_gene_ui <- renderUI({
  if(input$filter_low_gene == "Mean"){
    fluidRow(style = "margin-top: 15px;",
      column(width = 6, HTML("<div style='font-size:14px;margin-top: 10px'>Mean of CPM cutoff:</div>")),
      column(width = 6, numericInput(inputId = "deg_cpm_mean_cut_off", label = NULL, min = 0, value = 0))
    )
  }else if (input$filter_low_gene == "Percentage") {
    fluidRow(style = "margin-top: 15px;",
      column(width = 6, HTML("<div style='font-size:14px;margin-top: 10px'>Percentage of CPM cutoff:</div>")),
      column(width = 6, numericInput(inputId = "deg_cpm_percentage_cut_off", label = NULL, min = 0, value = 0, max = 1, ))
    )
  }
})

# deg plot
output$deg_selected <- renderPrint({
  data <- getdegtable()
  if(is.null(input$degresulttable_rows_selected)){
    print("No genes were selected")
  } else if (length(input$degresulttable_rows_selected) >=6 ) {
     print(paste(unlist(data[input$degresulttable_rows_selected,1][1:6]), collapse = "; "))
     print("Tip: Select up to 6 genes")
  } else {
     print(paste(unlist(data[input$degresulttable_rows_selected,1]), collapse = "; "))
  }
  })
output$vol_plot <- renderPlot(vol_plot())

# deg bar plot
output$deg_bar_ui1 <- renderUI({
  if(length(input$degresulttable_rows_selected)>=1){
    plotOutput("deg_bar_1", width = "100%", height = "225px")
  } else {
     return(NULL)
  }
})

output$deg_bar_ui2 <- renderUI({
  if(length(input$degresulttable_rows_selected)>=2){
    plotOutput("deg_bar_2", width = "100%", height = "225px")
  } else {
     return(NULL)
  }
})

output$deg_bar_ui3 <- renderUI({
  if(length(input$degresulttable_rows_selected)>=3){
    plotOutput("deg_bar_3", width = "100%", height = "225px")
  } else {
     return(NULL)
  }
})

output$deg_bar_ui4 <- renderUI({
  if(length(input$degresulttable_rows_selected)>=4){
    plotOutput("deg_bar_4", width = "100%", height = "225px")
  } else {
     return(NULL)
  }
})

output$deg_bar_ui5 <- renderUI({
  if(length(input$degresulttable_rows_selected)>=5){
    plotOutput("deg_bar_5", width = "100%", height = "225px")
  } else {
     return(NULL)
  }
})

output$deg_bar_ui6 <- renderUI({
  if(length(input$degresulttable_rows_selected)>=6){
    plotOutput("deg_bar_6", width = "100%", height = "225px")
  } else {
     return(NULL)
  }
})