timeseriesanalysis_process <- reactiveValues()
timeseriesanalysis_process$value <- "start"
library(Mfuzz)

# render UI
observeEvent(timeseriesanalysis_process$value, {
    if (timeseriesanalysis_process$value == "start") {
        output$ui_timeseriesanalysis <- renderUI({
            fluidRow(style = ";",
                    br(), br(), br(), br(), br(), br(), br(),
                    column(width = 6, align = "center", style = "left: 450px;margin-top: -70px;",
                        fluidRow(
                            column(width = 8, align="left", offset = 4,
                                HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step1: Upload you Matrix</div>")),
                            column(width = 1, align = "right", offset = 3,
                                shinyWidgets::dropdownButton(
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("Upload gene expression matrix data, the file format is only supported by .csv .xlsx"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    column(width = 12, align = "center", downloadButton("download_deg_matrix_template3", label = "Template matrix", style = "border-color: #ccc;font-size:12px;text-align: center;")),
                                    circle = TRUE, status = "default",
                                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )),
                            column(width = 8, align="left", offset = 0,
                                fileInput(inputId = "tsa_upload_matrix", label = NULL, accept = list(".csv", ".xlsx"), width = "340px"))
                        ),# close fluidRow
                        br(),
                        fluidRow(
                            column(width = 8, align="left", offset = 4, style = "margin-top: -35px;",
                                HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step2: Download you Group info</div>")),
                            column(width = 1, align = "right", offset = 3,
                                shinyWidgets::dropdownButton(
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("Download your group information table, modify the group column (the sample column does not need to be modified), and upload it in Step 3."),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    circle = TRUE, status = "default",
                                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )),
                            column(width = 8, align="left", offset = 0,
                                downloadButton(outputId = "tsa_download_groupinfo", label = "Download", icon = icon("download"), style = "width: 340px;margin-bottom: -10px;"))
                        ),# close fluidRow
                        br(),
                        fluidRow(
                            column(width = 8, align="left", offset = 4,
                                HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step3: Upload you Group info</div>")),
                            column(width = 1, align = "right", offset = 3,
                                shinyWidgets::dropdownButton(
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("Upload your sample group file (download the step2 file and modify the group column, set the same group name for the same group of samples)。"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    column(width = 12, align = "center", downloadButton("download_deg_group_template3", label = "Template groupinfo", style = "border-color: #ccc;font-size:12px;text-align: center;")),
                                    circle = TRUE, status = "default",
                                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )),
                            column(width = 8, align="left", offset = 0,
                                fileInput(inputId = "tsa_upload_groupinfo", label = NULL, accept = ".csv", width = "340px"))
                        ),# close fluidRow
                        br(),
                        fluidRow(
                            column(width = 8, align="left", offset = 4, style = "margin-top: -35px;",
                                HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step4: CPM normalization or not</div>")),
                            column(width = 1, align = "right", offset = 3,
                                shinyWidgets::dropdownButton(
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("If you want to use the normalization method, this method uses the normalization method as CPM by default."),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("Calculation formula: CPM = number of reads aligned to gene A / total reads aligned to all genes * 1,000,000"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("If your gene expression matrix is a reads value matrix, it needs to be normalized, and if it is already normalized, it is not required, such as TPM, FPKM, CPM, RPKM, etc."),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    circle = TRUE, status = "default",
                                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )),
                            column(width = 8, align="left", offset = 0, 
                                    shinyWidgets::radioGroupButtons(inputId = "tsa_cpm", label = NULL, choices = c("YES", "NO"), individual = TRUE, width = "100%",
                                    checkIcon = list(yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"), no = tags$i(class = "fa fa-square-o",style = "color: steelblue"))))
                        ),# close fluidRow
                        br()
                    ), #close cloumn
                    br(),  br(), br(),
                    column(width = 4, offset = 4,
                            column(width = 6, align="left", offset = 0, style="margin-bottom: 80px;margin-top: 80px;",
                                    actionButton(inputId = "reset_tsa", label = "RESET", icon = icon("rotate-left"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;")),
                            column(width = 6, align="right", offset = 0, style="margin-bottom: 80px;margin-top: 80px;",
                                    actionButton(inputId = "enter_tsa", label = "ENTER", icon = icon("check"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;"))
                    )
            )# close fluidRow
        })
    } else if (timeseriesanalysis_process$value == "process") {
       output$ui_timeseriesanalysis <- renderUI({
            fluidRow(
                br(),
                column(width = 12, 
                    column(width = 3, align = "left", h2("Time series analysis")),
                    column(width = 2, align = "left", offset = 0, style = "bottom: -20px;left: -160px;",
                    shinyWidgets::dropdownButton(
                        tags$h4(p(icon("check",lib = "font-awesome"),em("Time series analysis is based on R package: Mfuzz."),style="color:#337ab7;text-align:center;font-size:15px;")),
                        tags$h4(p(icon("check",lib = "font-awesome"),em("In the user-uploaded gene expression matrix, the mean values of each biological replicate are combined for subsequent analysis"),style="color:#337ab7;text-align:center;font-size:15px;")),

                        circle = TRUE, status = "default",
                        icon = icon("question"), width = "300px", size = "sm", right = F, up = F)
                    )
                ),
                HTML('<hr style="width: 100%;border-top: 2px solid #000;" />'),
                br(),
                mainPanel(width = 9, style = "border-right: 1px solid #ddd; margin-bottom: 40px;", 
                    column(width = 12, align = "center", htmlOutput(outputId = "tsa_base_info", style = "font-size:15px; margin-bottom: 30px;margin-top: -25px;")),
                    HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
                    column(width = 12, 
                        column(width = 2, align = "left", style="background-color: #dddddd6b;height: 50px;width: 200px;font-weight: 900;line-height: 2.5em;font-size: 20px;", HTML("After filtration")),
                        column(width = 2, downloadButton(outputId = "download_tsa_after_filter", label = "Download", style = "height:30px;line-height: 0em;"), style="background-color: #dddddd6b;height: 50px;line-height: 3.5em;width: 150px"),
                        column(width = 12, align = "center", DT::dataTableOutput(outputId = "tsa_test2"), style="background-color: #dddddd6b;margin-bottom: 20px;"), 
                    ),
                    br(),
                    HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
                    column(width = 12,
                        column(width = 12, align = "center", imageOutput("tsa_plot") %>% withSpinner()),
                        column(width = 12, align = "center", div(tableOutput(outputId = "tsa_mufzz_stat"), style ="width:100%;"))
                    ),
                    br(),
                    HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
                    column(width = 12,
                        column(width = 2, align = "left", style="background-color: #dddddd6b;height: 50px;width: 200px;font-weight: 900;line-height: 2.5em;font-size: 20px;", HTML("Result")),
                        column(width = 2, downloadButton(outputId = "download_tsa_mfuzz_result", label = "Download", style = "height:30px;line-height: 0em;"), style="background-color: #dddddd6b;height: 50px;line-height: 3.5em;width: 150px"),
                        column(width = 12, align = "center", DT::dataTableOutput(outputId = "tsa_test1"), style="background-color: #dddddd6b;margin-bottom: 20px;")
                    ),
                    
                ),
                sidebarPanel(width = 3,
                        br(),br(),br(),
                        column(width = 12, align = "left", h4("Underlying parameter"), style = "margin-bottom: -10px;margin-left: -10px;"),
                        HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
                        fluidRow(
                            column(width = 2, align = "left", offset = 0,
                                shinyWidgets::dropdownButton(
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("Filter genes that are less than 1 expressed in all samples"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    circle = TRUE, status = "default",
                                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )),
                            column(width = 10, offset = 0, checkboxInput("tsa_filter_low", label = "Filter low expression genes", value = FALSE), style = "font-size: 17px;"), 
                            column(width = 11, align="left", offset = 2, style = "margin-top: -0px;",
                                HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Gene filter</div>")),
                            column(width = 2, align = "left", offset = 0,
                                shinyWidgets::dropdownButton(
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("Gene filtering, 3 kinds of statistics are available for filtering, Var is the variance, Mean is the mean, and Mad is the absolute medium difference."),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("0.25: Top 25%, 0.5: Top 50%, 0.75: Top 75%, 1: All (no filtering)"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    circle = TRUE, status = "default",
                                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )),
                            column(width = 3, align="left", offset = 0,
                                    shinyWidgets::pickerInput(inputId = "tsa_filter_method",label = NULL, choices = c("Var", "Mean", "Mad"), selected = "Var", width = "100%")
                                ),
                            column(width = 7, align="left", offset = 0,
                                    shinyWidgets::pickerInput(inputId = "tsa_filter_value",label = NULL, choices = c(0.25, 0.5, 0.75, 1), selected = 0.25, width = "100%")
                                )
                        ),# close fluidRow
                        fluidRow(
                            column(width = 11, align="left", offset = 2, style = "margin-top: -0px;",
                                HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Number of clusters</div>")),
                            column(width = 2, align = "left", offset = 0,
                                shinyWidgets::dropdownButton(
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("Number of cluster."),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    circle = TRUE, status = "default",
                                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )),
                            column(width = 10, align="left", offset = 0,
                                    shinyWidgets::pickerInput(inputId = "tsa_cluster_num",label = NULL, choices = c(1:10), selected = 10, width = "100%")
                                )
                        ),# close fluidRow
                        fluidRow(
                            column(width = 11, align="left", offset = 2, style = "margin-top: -0px;",
                                HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Set seed </div>")),
                            column(width = 2, align = "center", offset = 0,
                                shinyWidgets::dropdownButton(
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("The function of set seed is to set the seed to generate random numbers, in order to make the result repeatable and reproduce the result."),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("Setting different seeds will give different results."),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    circle = TRUE, status = "default",
                                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )),
                            column(width = 10, align="left", offset = 0,
                                    shinyWidgets::pickerInput(inputId = "tsa_seed",label = NULL, choices = c(1:1000), selected = 123, width = "100%"))
                        ),# close fluidRow
                        br(),br(),
                        column(width = 12, align = "left", h4("Graphical parameter"), style = "margin-bottom: -10px;margin-left: -10px;"),
                        HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
                        fluidRow(
                            column(width = 5, align="left", offset = 2, style = "margin-top: -0px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Axis Y min</div>")),
                            column(width = 5, align="left", offset = 0, style = "margin-top: -0px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Axis Y max</div>")),
                            column(width = 2, align = "left", offset = 0,
                                shinyWidgets::dropdownButton(
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("Set the Y-axis range, when set to 0,0, it will change to automatic mode"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    circle = TRUE, status = "default",
                                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )
                            ),
                            column(width = 5, offset = 0, numericInput("tsa_ylim_min", label = NULL, value = -0, width = "100%")),
                            column(width = 5, offset = 0, numericInput("tsa_ylim_max", label = NULL, value = 0, width = "100%"))
                        ),# close fluidRow
                        fluidRow(
                            column(width = 5, align="left", offset = 2, style = "margin-top: -0px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Images per row</div>")),
                            column(width = 5, align="left", offset = 0, style = "margin-top: -0px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Images per col</div>")),
                            column(width = 2, align = "left", offset = 0,
                                shinyWidgets::dropdownButton(
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("Sets the number of images per row and column"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    circle = TRUE, status = "default",
                                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )),
                            column(width = 5, offset = 0, shinyWidgets::sliderTextInput(inputId = "tsa_line_image",label = NULL, choices = seq(from = 2,to = 5,by = 1), selected = 5, grid = TRUE)),
                            column(width = 5, offset = 0, shinyWidgets::sliderTextInput(inputId = "tsa_col_image",label = NULL, choices = seq(from = 1,to = 3,by = 1), selected = 2, grid = TRUE)),
                        ),# close fluidRow
                        fluidRow(
                            column(width = 5, align="left", offset = 2, style = "margin-top: -0px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>X title</div>")),
                            column(width = 5, align="left", offset = 0, style = "margin-top: -0px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Y title</div>")),
                            column(width = 2, align = "left", offset = 0,
                                shinyWidgets::dropdownButton(
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("The X and Y coordinates of the image"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    circle = TRUE, status = "default",
                                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )),
                            column(width = 5, offset = 0, textInput("tsa_x_title", value = "", label = NULL)),
                            column(width = 5, offset = 0, textInput("tsa_y_title", value = "", label = NULL)),
                        ),# close fluidRow
                        # fluidRow(
                        #     column(width = 10, align="left", offset = 2, style = "margin-top: -0px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Graph colour</div>")),
                        #     column(width = 2, align = "left", offset = 0,
                        #         shinyWidgets::dropdownButton(
                        #             tags$h4(p(icon("check",lib = "font-awesome"),em("To set the image color, the principle is to set these 5 colors evenly to a gradient color"),style="color:#337ab7;text-align:center;font-size:15px;")),
                        #             circle = TRUE, status = "default",
                        #             icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                        #         )),
                        #     column(width = 3, colourpicker::colourInput("tsa_col1", label = NULL, "yellow")),
                        #     column(width = 3, colourpicker::colourInput("tsa_col2", label = NULL, "green")),
                        #     column(width = 3, colourpicker::colourInput("tsa_col3", label = NULL, "blue")),
                        #     column(width = 3, offset = 2, colourpicker::colourInput("tsa_col4", label = NULL, "red")),
                        #     column(width = 3, colourpicker::colourInput("tsa_col5", label = NULL, "black")),
                        #     column(width = 4, checkboxInput("tsa_fancy", label = "Default colour", value = TRUE))
                        # ),# close fluidRow
                        fluidRow(
                            column(width = 3, align="left", offset = 0, style = "margin-top: -0px;",HTML("<div style='font-size:15px;font-weight:600;line-height: 2em;'>Mar.Top</div>")),
                            column(width = 3, align="left", offset = 0, style = "margin-top: -0px;",HTML("<div style='font-size:15px;font-weight:600;line-height: 2em;'>Mar.Bottom</div>")),
                            column(width = 3, align="left", offset = 0, style = "margin-top: -0px;",HTML("<div style='font-size:15px;font-weight:600;line-height: 2em;'>Mar.Letf</div>")),
                            column(width = 3, align="left", offset = 0, style = "margin-top: -0px;",HTML("<div style='font-size:15px;font-weight:600;line-height: 2em;'>Mar.Right</div>")),
                            column(width = 3, offset = 0, numericInput("tsa_mar_top", value = 4, label = NULL)),
                            column(width = 3, offset = 0, numericInput("tsa_mar_bottom", value = 8, label = NULL)),
                            column(width = 3, offset = 0, numericInput("tsa_mar_left", value = 5, label = NULL)),
                            column(width = 3, offset = 0, numericInput("tsa_mar_right", value = 2, label = NULL))
                        ),# close fluidRow
                        fluidRow(
                            column(width = 4, align="left", offset = 0, style = "margin-top: -0px;",HTML("<div style='font-size:15px;font-weight:600;line-height: 2em;'>Title size&col</div>")),
                            column(width = 4, align="left", offset = 0, style = "margin-top: -0px;",HTML("<div style='font-size:15px;font-weight:600;line-height: 2em;'>Label size&col</div>")),
                            column(width = 4, align="left", offset = 0, style = "margin-top: -0px;",HTML("<div style='font-size:15px;font-weight:600;line-height: 2em;'>Text size&col</div>")),
                            column(width = 4, offset = 0, numericInput("tsa_main_size", value = 2, label = NULL)),
                            column(width = 4, offset = 0, numericInput("tsa_lab_size", value = 1, label = NULL)),
                            column(width = 4, offset = 0, numericInput("tsa_axis_size", value = 1, label = NULL)),
                            column(width = 4, colourpicker::colourInput("tsa_main_col", label = NULL, "black"), style = "margin-top:-10px"),
                            column(width = 4, colourpicker::colourInput("tsa_lab_col", label = NULL, "black"), style = "margin-top:-10px"),
                            column(width = 4, colourpicker::colourInput("tsa_axis_col", label = NULL, "black"), style = "margin-top:-10px")
                        ),# close fluidRow
                        fluidRow(
                            column(width = 4, checkboxInput("tsa_center", label = "Show center", value = TRUE), style = "MARGIN-TOP: 15px;font-size: 15px;"),
                            column(width = 4, align="left", offset = 0, style = "margin-top: -0px;",HTML("<div style='font-size:15px;font-weight:600;line-height: 2em;'>Center col</div>")),
                            column(width = 4, align="left", offset = 0, style = "margin-top: -0px;",HTML("<div style='font-size:15px;font-weight:600;line-height: 2em;'>Center size</div>")),
                            column(width = 4, offset = 0, colourpicker::colourInput("tsa_center_col", label = NULL, "black"), style = "margin-top:0px"),
                            column(width = 4, offset = 0, numericInput("tsa_center_size", value = 1, label = NULL))
                        ),# close fluidRow
                        fluidRow(
                            column(width = 4, align="left", offset = 0, style = "margin-top: -0px;",HTML("<div style='font-size:15px;font-weight:600;line-height: 2em;'>Bg col</div>")),
                            column(width = 8, align="left", offset = 0, style = "margin-top: -0px;",HTML("<div style='font-size:15px;font-weight:600;line-height: 2em;'>Text layout</div>")),
                            column(width = 4, offset = 0, colourpicker::colourInput("tsa_bg_col", label = NULL, "white"), style = "margin-top:0px"),
                            column(width = 8, offset = 0, selectInput("tsa_text_layout", choices = list(X_horizontal_Y_horizontal = 1,
                                                                                                        X_horizontal_Y_vertical = 0,
                                                                                                        X_vertical_Y_horizontal = 2,
                                                                                                        X_vertical_Y_vertical = 3), label = NULL, selected = 2))
                        ),# close fluidRow
                        br(),
                        column(width = 12, align = "left", h4("Download graph"), style = "margin-bottom: -10px;margin-left: -10px;"),
                        HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
                        fluidRow(
                            column(width = 5, align="left", offset = 2, style = "margin-top: -0px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Width</div>")),
                            column(width = 5, align="left", offset = 0, style = "margin-top: -0px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Height</div>")),
                            column(width = 2, align = "left", offset = 0,
                                shinyWidgets::dropdownButton(
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("Set the width and height and format of the downloaded image, note: the width and height change will not change on the web side, and it will only change if you view it locally after downloading."),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    circle = TRUE, status = "default",
                                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )),
                            column(width = 5, numericInput("tsa_width", label = NULL, value = 8)),
                            column(width = 5, numericInput("tsa_height", label = NULL, value = 4)),
                            column(width = 5, offset = 2, shinyWidgets::pickerInput(inputId = "tsa_format",label = NULL, choices = c(PDF = "pdf", PNG = "png"), selected = "png", width = "100%")),
                            column(width = 5, offset = 0, align = "center", downloadButton("download_tsa_plot", label = "Download", style = "width:100%; margin-top: 0px;"))
                        ),# close fluidRow
                ),
                HTML('<hr style="width: 100%;border-top: 2px solid #000;" />')
            )
       })
    }
})

# download groupinfo
tsa_download_groupinfo <- reactive({#deggroupinfo模板(生物学重复分组), 返回datafarme header=T，rowname=1
  if (is.null(input$tsa_upload_matrix)) {
    return(NULL)
  }else {
    #读取matrix
    countdata <- input$tsa_upload_matrix
    fileformat <- strsplit(countdata$datapath, "\\.")[[1]][-1]
    if (fileformat == "csv") {
        readscountdata <- read.csv(countdata$datapath, header = TRUE, row.names = 1)
    }else if (fileformat == "xlsx") {
        readscountdata <- openxlsx::read.xlsx(countdata$datapath, colNames = TRUE, rowNames = T)
    }
      group <- colnames(readscountdata)[-1]
      group <- data.frame(group)
      rownames(group) <- group$group
      group
  }
})
output$tsa_download_groupinfo <- downloadHandler(#下载groupinfo模板
  filename = function() {"groupinfo.csv"},
  content = function(file) {
    if(is.null(tsa_download_groupinfo())) {
        shinyjs::runjs('alert("ERROR: Please upload matrix first")')
      }else {
        write.csv(tsa_download_groupinfo(), file, row.names = TRUE, fileEncoding="GBK", quote = FALSE)
    }
  }
)

#file upload stat
values_tsa <- reactiveValues(upload_tsa_matrix = "NULL", upload_tsa_groupinfo = "NULL")
observeEvent(input$tsa_upload_matrix, {values_tsa$upload_tsa_matrix <- 'uploaded'})
observeEvent(input$tsa_upload_groupinfo, {values_tsa$upload_tsa_groupinfo <- 'uploaded'})

#enter
observeEvent(input$enter_tsa, {
    if(values_tsa$upload_tsa_matrix == "uploaded" & values_tsa$upload_tsa_groupinfo == "uploaded"){
        countdata <- input$tsa_upload_matrix
        groupinfo <- input$tsa_upload_groupinfo
        
        #判断matrix格式
        fileformat <- strsplit(countdata$datapath, "\\.")[[1]][-1]
        if (fileformat == "csv") {
        countdata <- read.csv(countdata$datapath, header = TRUE, row.names = 1)
        }else if (fileformat == "xlsx") {
            countdata <- openxlsx::read.xlsx(countdata$datapath, colNames = TRUE, rowNames = T)
        }else {
            shinyjs::runjs('alert("ERROR: Format of matrix file must be <.csv> or <.xlsx>.")')
            shinyjs::reset("tsa_upload_matrix")
            shinyjs::reset("tsa_upload_groupinfo")
            values_tsa$upload_tsa_matrix = "NULL"
            values_tsa$upload_tsa_groupinfo = "NULL"
            return(NULL)
        }
        # sample name detect
        if (!check_column_names(input$tsa_upload_matrix$datapath)) {
            shinyjs::runjs('alert("ERROR: The sample name can only be composed of <a~z A~Z 0~9 _ .>, and only the beginning of the letter is supported. Please check that your sample name is named correctly")')
            shinyjs::reset("tsa_upload_matrix")
            shinyjs::reset("tsa_upload_groupinfo")
            values_tsa$upload_tsa_matrix = "NULL"
            values_tsa$upload_tsa_groupinfo = "NULL"
            return(NULL)
        }
        #判断matrix是否数值
        data <- unlist(matrix(countdata))
        if (typeof(countdata) == "character") {
                shinyjs::runjs('alert("ERROR: The matrix must be numeric.")')
                shinyjs::reset("tsa_upload_matrix")
                shinyjs::reset("tsa_upload_groupinfo")
                values_tsa$upload_tsa_matrix = "NULL"
                values_tsa$upload_tsa_groupinfo = "NULL"
                return(NULL)
        }
        #判断groupinfo格式
        fileformat <- strsplit(groupinfo$datapath, "\\.")[[1]][-1]
        if (fileformat == "csv") {
            groupinfo <- read.csv(groupinfo$datapath, header = TRUE)
        }else if (fileformat == "xlsx") {
            groupinfo <- openxlsx::read.xlsx(groupinfo$datapath, colNames = TRUE, rowNames = F)
        }else {
                shinyjs::runjs('alert("ERROR: Format of group info file must be <.csv> or <.xlsx>.")')
                shinyjs::reset("tsa_upload_groupinfo")
                shinyjs::reset("tsa_upload_matrix")
                values_tsa$upload_tsa_matrix = "NULL"
                values_tsa$upload_tsa_groupinfo = "NULL"
                return(NULL)
        }
        #判断groupinfo的名字和matrix是否符合
        allsample <- groupinfo[, 1]
        cpmsamplename <- colnames(countdata)
        for (i in allsample) {
            if (i %in% cpmsamplename == FALSE) {
            shinyjs::runjs('alert("ERROR: The sample name in group info does not correspond to the sample name in matrix.")')
            shinyjs::reset("tsa_upload_matrix")
            shinyjs::reset("tsa_upload_groupinfo")
            values_tsa$upload_tsa_matrix = "NULL"
            values_tsa$upload_tsa_groupinfo = "NULL"
            return(NULL)
            }
        }
        
        timeseriesanalysis_process$value <- "process"

    }else if (values_tsa$upload_tsa_matrix == "uploaded" & values_tsa$upload_tsa_groupinfo != "uploaded") {
            shinyjs::runjs('alert("Please upload groupinfo file")')
            shinyjs::reset("tsa_upload_matrix")
            shinyjs::reset("tsa_upload_groupinfo")
            values_tsa$upload_tsa_matrix = "NULL"
            values_tsa$upload_tsa_groupinfo = "NULL"
            return(NULL)
    }else if (values_tsa$upload_tsa_matrix != "uploaded" & values_tsa$upload_tsa_groupinfo == "uploaded") {
            shinyjs::runjs('alert("Please upload matrix file")')
            shinyjs::reset("tsa_upload_matrix")
            shinyjs::reset("tsa_upload_groupinfo")
            values_tsa$upload_tsa_matrix = "NULL"
            values_tsa$upload_tsa_groupinfo = "NULL"
            return(NULL)
    }else {
       return(NULL)
    }
})

#读取矩阵，均一化，取均值，输出矩阵
tsa_matrix <- reactive({
    if(timeseriesanalysis_process$value == "process"){
        countdata <- input$tsa_upload_matrix
        groupinfo <- input$tsa_upload_groupinfo
        #读取matrix
        fileformat <- strsplit(countdata$datapath, "\\.")[[1]][-1]
        if (fileformat == "csv") {
            countdata <- read.csv(countdata$datapath, header = TRUE, row.names = 1)
        }else if (fileformat == "xlsx") {
            countdata <- openxlsx::read.xlsx(countdata$datapath, colNames = TRUE, rowNames = T)
        }
        #均一化（自定义步骤）
        if(input$tsa_cpm == "YES"){
            countdata <- t(t(countdata) / colSums(countdata) * 1000000)
            countdata <- data.frame(countdata)
        }
        #读取groupinfo
        fileformat <- strsplit(groupinfo$datapath, "\\.")[[1]][-1]
        if (fileformat == "csv") {
            groupinfo <- read.csv(groupinfo$datapath, header = TRUE, row.names = 1)
        }else if (fileformat == "xlsx") {
            groupinfo <- openxlsx::read.xlsx(groupinfo$datapath, colNames = TRUE, rowNames = T)
        }
        #取每个样品的均值
        countdata <- countdata %>% dplyr::select(rownames(groupinfo))
        colnames(countdata) <- groupinfo$group
        countdata <- as.data.frame( # sapply returns a list here, so we convert it to a data.frame
            sapply(unique(names(countdata)), # for each unique column name
                    function(col) rowMeans(countdata[names(countdata) == col]) # calculate row means
            )
        )
        return(countdata)
    }else {
       return(NULL)
    }
})

#基因过滤
tsa_matrix_filter <- reactive({
    if(!is.null(tsa_matrix())) {
        countdata <- tsa_matrix()

        if(input$tsa_filter_low == TRUE){
            countdata <- countdata[apply(countdata, 1, function(row) !all(row < 1)), ]
        }

        if(input$tsa_filter_method == "Var"){
            countdata$var <- apply(countdata, 1,var)
            filter_vlaue <- ifelse(is.null(input$tsa_filter_value), 0.75, 1-as.numeric(input$tsa_filter_value))
            countdata <- countdata[countdata$var >= quantile(countdata$var, filter_vlaue),][1:(ncol(countdata)-1)]

        }else if (input$tsa_filter_method == "Mean") {
            countdata$var <- apply(countdata, 1,mean)
            filter_vlaue <- ifelse(is.null(input$tsa_filter_value), 0.75, 1-as.numeric(input$tsa_filter_value))
            countdata <- countdata[countdata$var >= quantile(countdata$var, filter_vlaue),][1:(ncol(countdata)-1)]

        }else if (input$tsa_filter_method == "Mad") {
            countdata$var <- apply(countdata, 1,mad)
            filter_vlaue <- ifelse(is.null(input$tsa_filter_value), 0.75, 1-as.numeric(input$tsa_filter_value))
            countdata <- countdata[countdata$var >= quantile(countdata$var, filter_vlaue),][1:(ncol(countdata)-1)]

        }
        return(countdata)
    }else {
       return(NULL)
    }
})

#矩阵基础信息
output$tsa_base_info <- renderText({
    if(!is.null(tsa_matrix()) & !is.null(tsa_matrix_filter())){
        before_filter <- tsa_matrix()
        after_filter <- tsa_matrix_filter()
        
        geneset_filename <- paste0("File name: ", input$tsa_upload_matrix[1])
        filter_method <- ifelse(is.null(input$tsa_filter_method), "Filtration method: Var", paste0("Filtration method: ", input$tsa_filter_method))
        filter_value <- ifelse(is.null(input$tsa_filter_value), "Filtering threshold: 0.25", paste0("Filtering threshold: ", as.numeric(input$tsa_filter_value)))
        before_filter_num <- paste0("Number of genes before filtration: ", nrow(before_filter))
        after_filter_num <- paste0("Number of genes after filtration: ", nrow(after_filter))
        filter_num <- paste0("Number of genes filtered: ", nrow(before_filter) - nrow(after_filter))

        a <- HTML(paste0("<div style='font-size:25px;font-weight:600;line-height: 1em;'><br/>Basic information</div>",
                        "<br/>", geneset_filename, "<br/>", filter_method, "<br/>", filter_value, "<br/>", before_filter_num, "<br/>", after_filter_num, "<br/>", filter_num,"<br/><br/>"))

    }else {
       return(NULL)
    }
})

#过滤后矩阵输出并下载
output$tsa_test2 <- DT::renderDataTable(tsa_matrix_filter(), rownames = T, selection = list(mode = 'multiple'), options = list(dom = 'fltipr', pageLength = 5, scrollX=TRUE, orderClasses = T, 
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
output$download_tsa_after_filter <- downloadHandler(#下载enrich_geneset模板
    filename = function() {"After_filtration.csv"},
    content = function(file) {
      data <- tsa_matrix_filter()
      write.csv(data, file, fileEncoding="GBK", quote = FALSE)
    }
)

#新建tsa的tmp文件夹
tsa_tmp_filepath <- reactive({
    if(!is.null(tsa_matrix_filter())){
        prjpath <- prjpath("timeseriesanalysis")
        return(prjpath)
    }else {
       return(NULL)
    }
})

#Mfuzz
mfuzz <- reactive({
    if(!is.null(tsa_matrix_filter()) & !is.null(tsa_tmp_filepath())){
        data <- tsa_matrix_filter()
        data <- as.matrix(data)
        mfuzz_class <- new('ExpressionSet', exprs = data)

        mfuzz_class <- Mfuzz::filter.NA(mfuzz_class, thres = 0.25)
        mfuzz_class <- Mfuzz::fill.NA(mfuzz_class, mode = 'mean')
        mfuzz_class <- Mfuzz::filter.std(mfuzz_class, min.std = 0, visu = F)
        mfuzz_class <- Mfuzz::standardise(mfuzz_class)
        set.seed(as.numeric(input$tsa_seed))
        cluster_num <- as.numeric(input$tsa_cluster_num)
        mfuzz_cluster <- Mfuzz::mfuzz(mfuzz_class, c = cluster_num, m = mestimate(mfuzz_class))

        cluster <- mfuzz_cluster$cluster
        cluster <- cbind(data[names(cluster), ], cluster)
        cluster <- as.data.frame(cluster)
        cluster$gene_id <- rownames(cluster)
        cluster <- dplyr::select(cluster, c(ncol(cluster), ncol(cluster)-1), everything())#把gene_id列和cluster列排前面
        cluster$cluster <- paste0("Cluster_", cluster$cluster)
        # file_name <- paste0(tsa_tmp_filepath(), "/mfuzz_result.csv")
        # write.csv(cluster, file_name, row.names = F, quote = F)

        return(list(cluster = cluster, data = data, mfuzz_cluster = mfuzz_cluster, mfuzz_class = mfuzz_class))
    }else {
       return(NULL)
    }
})
mfuzz_plot2 <- reactive({
    require(mfuzz())
        cluster <- mfuzz()$cluster
        data <- mfuzz()$data
        mfuzz_cluster <- mfuzz()$mfuzz_cluster
        mfuzz_class <- mfuzz()$mfuzz_class

        height <- ifelse(is.null(input$tsa_height), 4, input$tsa_height)
        width <- ifelse(is.null(input$tsa_width), 8, input$tsa_width)

        png(file = paste0(tsa_tmp_filepath(), "/mfuzz_plot.png"), height = height, width = width, units = "in", res = 200)
        # if(input$tsa_fancy == T){#需在R4.1上运行，4.2会报错
        if(T){#需在R4.1上运行，4.2会报错
            par(mar = c(input$tsa_mar_bottom, input$tsa_mar_left ,input$tsa_mar_top, input$tsa_mar_right))#bottom, left, top, right
            Mfuzz::mfuzz.plot2(mfuzz_class, cl = mfuzz_cluster, time.labels = colnames(data), colo = "fancy", x11 = F,
                                mfrow = c(as.numeric(input$tsa_col_image), as.numeric(input$tsa_line_image)),
                                ylim.set=c(as.numeric(input$tsa_ylim_min), as.numeric(input$tsa_ylim_max)), 
                                xlab = input$tsa_x_title, ylab = input$tsa_y_title,
                                bg = input$tsa_bg_col,
                                col.axis = input$tsa_axis_col,
                                col.lab = input$tsa_lab_col,
                                col.main = input$tsa_main_col,
                                centre = input$tsa_center,
                                centre.col = input$tsa_center_col,
                                centre.lwd = input$tsa_center_size,
                                cex.main = input$tsa_main_size,
                                cex.lab = input$tsa_lab_size,
                                cex.axis = input$tsa_axis_size,
                                las = input$tsa_text_layout
                                )
        }else {
            my_colors <- colorRampPalette(c(input$tsa_col1, input$tsa_col2, input$tsa_col3, input$tsa_col4, input$tsa_col5))
            my_colors <- my_colors(20)
            par(mar = c(input$tsa_mar_bottom, input$tsa_mar_left ,input$tsa_mar_top, input$tsa_mar_right))#bottom, left, top, right
            Mfuzz::mfuzz.plot2(mfuzz_class, cl = mfuzz_cluster, time.labels = colnames(data), colo = input$tsa_col1, x11 = F,
                                mfrow = c(as.numeric(input$tsa_col_image), as.numeric(input$tsa_line_image)),
                                ylim.set=c(as.numeric(input$tsa_ylim_min), as.numeric(input$tsa_ylim_max)), 
                                xlab = input$tsa_x_title, ylab = input$tsa_y_title,
                                bg = input$tsa_bg_col,
                                col.axis = input$tsa_axis_col,
                                col.lab = input$tsa_lab_col,
                                col.main = input$tsa_main_col,
                                centre = input$tsa_center,
                                centre.col = input$tsa_center_col,
                                centre.lwd = input$tsa_center_size,
                                cex.main = input$tsa_main_size,
                                cex.lab = input$tsa_lab_size,
                                cex.axis = input$tsa_axis_size,
                                las = input$tsa_text_layout
                                )
        }
        dev.off()

        pdf(file = paste0(tsa_tmp_filepath(), "/mfuzz_plot.pdf"), height = height, width = width)
        # if(input$tsa_fancy == T){#需在R4.1上运行，4.2会报错
        if(T){#需在R4.1上运行，4.2会报错
            par(mar = c(input$tsa_mar_bottom, input$tsa_mar_left ,input$tsa_mar_top, input$tsa_mar_right))#bottom, left, top, right
            Mfuzz::mfuzz.plot2(mfuzz_class, cl = mfuzz_cluster, time.labels = colnames(data), colo = "fancy", x11 = F,
                                mfrow = c(as.numeric(input$tsa_col_image), as.numeric(input$tsa_line_image)),
                                ylim.set=c(as.numeric(input$tsa_ylim_min), as.numeric(input$tsa_ylim_max)), 
                                xlab = input$tsa_x_title, ylab = input$tsa_y_title,
                                bg = input$tsa_bg_col,
                                col.axis = input$tsa_axis_col,
                                col.lab = input$tsa_lab_col,
                                col.main = input$tsa_main_col,
                                centre = input$tsa_center,
                                centre.col = input$tsa_center_col,
                                centre.lwd = input$tsa_center_size,
                                cex.main = input$tsa_main_size,
                                cex.lab = input$tsa_lab_size,
                                cex.axis = input$tsa_axis_size,
                                las = input$tsa_text_layout
                                )
        }else {
            my_colors <- colorRampPalette(c(input$tsa_col1, input$tsa_col2, input$tsa_col3, input$tsa_col4, input$tsa_col5))
            my_colors <- my_colors(20)
            par(mar = c(input$tsa_mar_bottom, input$tsa_mar_left ,input$tsa_mar_top, input$tsa_mar_right))#bottom, left, top, right
            Mfuzz::mfuzz.plot2(mfuzz_class, cl = mfuzz_cluster, time.labels = colnames(data), colo = my_colors, x11 = F,
                                mfrow = c(as.numeric(input$tsa_col_image), as.numeric(input$tsa_line_image)),
                                ylim.set=c(as.numeric(input$tsa_ylim_min), as.numeric(input$tsa_ylim_max)), 
                                xlab = input$tsa_x_title, ylab = input$tsa_y_title,
                                bg = input$tsa_bg_col,
                                col.axis = input$tsa_axis_col,
                                col.lab = input$tsa_lab_col,
                                col.main = input$tsa_main_col,
                                centre = input$tsa_center,
                                centre.col = input$tsa_center_col,
                                centre.lwd = input$tsa_center_size,
                                cex.main = input$tsa_main_size,
                                cex.lab = input$tsa_lab_size,
                                cex.axis = input$tsa_axis_size,
                                las = input$tsa_text_layout
                                )
        }
        dev.off()

        if(input$tsa_format == "png"){
            plot_path <- paste0(tsa_tmp_filepath(), "/mfuzz_plot.png")
        }else {
           plot_path <- paste0(tsa_tmp_filepath(), "/mfuzz_plot.pdf")
        }
    return(plot_path)
})

output$tsa_plot <- renderImage({
    if(!is.null(mfuzz_plot2()) & !is.null(tsa_tmp_filepath())){
        list(src = paste0(tsa_tmp_filepath(), "/mfuzz_plot.png"), contentType = 'image/png', style = "max-width:100%; max-height:100%;", deleteFile = FALSE)
    }
})
output$download_tsa_plot <- downloadHandler(#下载
    filename = function() {ifelse(input$tsa_format == "png", "tsa_plot.png", "tsa_plot.pdf")},
    content = function(file) {
        if(input$tsa_format == "png"){
            file.copy(paste0(tsa_tmp_filepath(), "/mfuzz_plot.png"), file)
        }else if (input$tsa_format == "pdf") {
           file.copy(paste0(tsa_tmp_filepath(), "/mfuzz_plot.pdf"), file)
        }
    
})

#Mfuzz result table
output$tsa_test1 <- DT::renderDataTable(mfuzz()$cluster, rownames = F, selection = list(mode = 'multiple'), options = list(dom = 'fltipr', pageLength = 5, scrollX=TRUE, orderClasses = T, 
                                                                                                                                                      rowCallback = DT::JS(
                                                                                                                                                                          "function(row, data) {",
                                                                                                                                                                            "for (i = 3; i < data.length; i++) {",
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
Mfuzz_cluster_stat <- reactive({
    if(!is.null(mfuzz())){
        data <- mfuzz()$cluster
        stat <- data.frame(table(data$cluster))
        stat[, 1] <- as.character(stat[, 1])
        stat[, 2] <- as.numeric(stat[, 2])
        stat <- rbind(stat, c("All clusters", sum(stat[,2])))
        stat <- t(stat)
        return(stat)
    }else {
       return(NULL)
    }
})
output$tsa_mufzz_stat <- renderTable(rownames = F, colnames = F, Mfuzz_cluster_stat())
output$download_tsa_mfuzz_result <- downloadHandler(#下载enrich_geneset模板
    filename = function() {"TSA_result.csv"},
    content = function(file) {
      data <- mfuzz()$cluster
      write.csv(data, file, fileEncoding="GBK", quote = FALSE, row.names=F)
    }
)