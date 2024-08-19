
#sidebar panels
output$veendiagram_option <- renderUI({
    fluidPage(
        column(width = 12,
            column(width = 12, actionButton(inputId = "reset_venn_plot", label = "Reset", icon = icon("arrows-rotate"),style = "font-size: 17px;margin-bottom: -10px;WIDTH: 300px;margin-bottom: 10px;")),

            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
            shinyWidgets::dropdownButton(inputId = "mydropdown",label = "Add data set 1",icon = icon("sliders"),status = NULL,circle = FALSE,width = "110%",size = "lg",
                column(width = 6, textAreaInput("input_venn_1",label = "Set 1 (one per line)",height = "260px",width="300px",value="NULL")),
                column(width = 6, fileInput(inputId = "upload_venn_1", label = "or Upload data set 1", accept = ".txt"), style = "margin-bottom:-20px"),
                column(width = 6, colourpicker::colourInput("venn_1_color", label = "Colour of data set 1", "red")),
                column(width = 6, textInput("venn_1_name", value = "Data set 1", label = "Name of data set 1"))
            ),
            br(),
            shinyWidgets::dropdownButton(inputId = "mydropdown",label = "Add data set 2",icon = icon("sliders"),status = NULL,circle = FALSE,width = "110%",size = "lg",
                column(width = 6, textAreaInput("input_venn_2",label = "Set 2 (one per line)",height = "260px",width="300px",value="NULL")),
                column(width = 6, fileInput(inputId = "upload_venn_2", label = "or Upload data set 2", accept = ".txt"), style = "margin-bottom:-20px"),
                column(width = 6, colourpicker::colourInput("venn_2_color", label = "Colour of data set 2", "blue")),
                column(width = 6, textInput("venn_2_name", value = "Data set 2", label = "Name of data set 2"))
            ),
            br(),
            shinyWidgets::dropdownButton(inputId = "mydropdown",label = "Add data set 3",icon = icon("sliders"),status = NULL,circle = FALSE,width = "110%",size = "lg",
                column(width = 6, textAreaInput("input_venn_3",label = "Set 3 (one per line)",height = "260px",width="300px",value="NULL")),
                column(width = 6, fileInput(inputId = "upload_venn_3", label = "or Upload data set 3", accept = ".txt"), style = "margin-bottom:-20px"),
                column(width = 6, colourpicker::colourInput("venn_3_color", label = "Colour of data set 3", "green")),
                column(width = 6, textInput("venn_3_name", value = "Data set 3", label = "Name of data set 3"))
            ),
            br(),
            shinyWidgets::dropdownButton(inputId = "mydropdown",label = "Add data set 4",icon = icon("sliders"),status = NULL,circle = FALSE,width = "110%",size = "lg",
                column(width = 6, textAreaInput("input_venn_4",label = "Set 4 (one per line)",height = "260px",width="300px",value="NULL")),
                column(width = 6, fileInput(inputId = "upload_venn_4", label = "or Upload data set 4", accept = ".txt"), style = "margin-bottom:-20px"),
                column(width = 6, colourpicker::colourInput("venn_4_color", label = "Colour of data set 4", "yellow")),
                column(width = 6, textInput("venn_4_name", value = "Data set 4", label = "Name of data set 4"))
            ),
        )
    )
})

#main panels
output$veendiagram_main_panel <- renderUI({
           fluidPage(
                column(width = 3, DT::dataTableOutput(outputId = "venn_table_1", width = "100%")),
                column(width = 3, DT::dataTableOutput(outputId = "venn_table_2", width = "100%")),
                column(width = 3, DT::dataTableOutput(outputId = "venn_table_3", width = "100%")),
                column(width = 3, DT::dataTableOutput(outputId = "venn_table_4", width = "100%")),
                uiOutput("venn_plot_setting")
           )
})

values_venn <- reactiveValues(upload_venn_1 = "NULL", upload_venn_2 = "NULL", upload_venn_3 = "NULL", upload_venn_4 = "NULL")
observeEvent(input$upload_venn_1, {values_venn$upload_venn_1 <- 'uploaded'})
observeEvent(input$upload_venn_2, {values_venn$upload_venn_2 <- 'uploaded'})
observeEvent(input$upload_venn_3, {values_venn$upload_venn_3 <- 'uploaded'})
observeEvent(input$upload_venn_4, {values_venn$upload_venn_4 <- 'uploaded'})

#venn_table_1
venn_table_1 <- reactive({
    if(values_venn$upload_venn_1 == 'uploaded'){
        table1 <- read.csv(input$upload_venn_1$datapath, header = F, sep = "\t")[1]
    }else if (input$input_venn_1 != "NULL") {
       df <-as.data.frame(matrix(unlist(stringr::str_split(input$input_venn_1,"\n")),ncol=1))
    }else {
       return(NULL)
    }
})
output$venn_table_1 <- DT::renderDataTable(venn_table_1(), options = list(dom = 'fltpr', pageLength = 10, scrollX=TRUE, orderClasses = T), selection = "none")

#venn_table_2
venn_table_2 <- reactive({
    if(values_venn$upload_venn_2 == 'uploaded'){
        table1 <- read.csv(input$upload_venn_2$datapath, header = F, sep = "\t")[1]
    }else if (input$input_venn_2 != "NULL") {
       df <-as.data.frame(matrix(unlist(stringr::str_split(input$input_venn_2,"\n")),ncol=1))
    }else {
       return(NULL)
    }
})
output$venn_table_2 <- DT::renderDataTable(venn_table_2(), options = list(dom = 'fltpr', pageLength = 10, scrollX=TRUE, orderClasses = T), selection = "none")

#venn_table_3
venn_table_3 <- reactive({
    if(values_venn$upload_venn_3 == 'uploaded'){
        table1 <- read.csv(input$upload_venn_3$datapath, header = F, sep = "\t")[1]
    }else if (input$input_venn_3 != "NULL") {
       df <-as.data.frame(matrix(unlist(stringr::str_split(input$input_venn_3,"\n")),ncol=1))
    }else {
       return(NULL)
    }
})
output$venn_table_3 <- DT::renderDataTable(venn_table_3(), options = list(dom = 'fltpr', pageLength = 10, scrollX=TRUE, orderClasses = T), selection = "none")

#venn_table_4
venn_table_4 <- reactive({
    if(values_venn$upload_venn_4 == 'uploaded'){
        table1 <- read.csv(input$upload_venn_4$datapath, header = F, sep = "\t")[1]
    }else if (input$input_venn_4 != "NULL") {
       df <-as.data.frame(matrix(unlist(stringr::str_split(input$input_venn_4,"\n")),ncol=1))
    }else {
       return(NULL)
    }
})
output$venn_table_4 <- DT::renderDataTable(venn_table_4(), options = list(dom = 'fltpr', pageLength = 10, scrollX=TRUE, orderClasses = T), selection = "none")

#venn_plot_setting
output$venn_plot_setting <- renderUI({
    if(!is.null(venn_table_1()) & !is.null(venn_table_2()) ){
        fluidPage(
            column(width = 12, style = "margin-bottom:30px",
                HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
                column(width = 7, align = "center", plotOutput(outputId = "venn_plot", height = "600px", width = "600px"), style = "margin-top: 50px;"),
                column(width = 5, style = "margin-top: 40px;",
                    column(width = 3, shinyWidgets::awesomeCheckbox(inputId = "venn_scale_plot",label = "Scale plot", value = FALSE, width = "100%"), style = "margin-top: 20px;font-size: 15px;"),
                    column(width = 3, colourpicker::colourInput("venn_border_colour", label = "Border", "black")),
                    column(width = 3, colourpicker::colourInput("venn_label_colour", label = "Label", "black")),
                    column(width = 3, colourpicker::colourInput("venn_text_colour", label = "Text", "black")),
                    column(width = 12, shinyWidgets::sliderTextInput(inputId = "venn_label_size",label = "Label size", choices = c(1:10),grid = TRUE, width = "100%", selected = 2)),
                    column(width = 12, shinyWidgets::sliderTextInput(inputId = "venn_text_size",label = "Text size", choices = c(1:10),grid = TRUE, width = "100%", selected = 2)),
                    column(width = 12, shinyWidgets::sliderTextInput(inputId = "venn_border_size",label = "Border size", choices = c(1:10),grid = TRUE, width = "100%", selected = 2)),
                    column(width = 12, shinyWidgets::sliderTextInput(inputId = "venn_alpha",label = "Alpha", choices = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),grid = TRUE, width = "100%", selected = 0.6)),
                    column(width = 4, numericInput("venn_width", label = "Width", value = 6), style = "margin-bottom: -10px;", style = "margin-bottom: -5px;"),
                    column(width = 4, numericInput("venn_height", label = "Height", value = 6), style = "margin-bottom: -10px;", style = "margin-bottom: -5px;"),
                    column(width = 4, selectInput(inputId = "venn_plot_format", label = "Format", choices = c("PDF" = "pdf", "PNG" = "png")), style = "margin-bottom: -10px;"),
                    column(width = 12, downloadButton("download_venn_plot", label = NULL, style = "width:100%;"))
                    )
                ),
                HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
            uiOutput("venn_detail")
        )
    }else {
       fluidPage(
            column(width = 12,style = "margin-bottom:300px; margin-top:200px",
                HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
                column(width = 12, align="center", 
                            HTML("<p class='title'>This section is the Venn diagram plate, and the R package dependency package is\"VennDiagram\"。<br/> 
                            This section currently supports two-set and three-set Venn diagrams<br/> 
                            Enter the corresponding list and the analysis will be automated</p>(The input list is one per row.) The uploaded file will be read as a table with the tab \"\\t\" as the delimiter and without the column name, taking the first one in each row)</p>")),
                HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />')

            )
       )
    }
})

#venn plot
venn_plot <- reactive({
    if(!is.null(venn_table_1()) & !is.null(venn_table_2()) & is.null(venn_table_3()) & is.null(venn_table_4())){
        venn_1 <- venn_table_1(); venn_1 <- venn_1[, 1]
        venn_2 <- venn_table_2(); venn_2 <- venn_2[, 1]
        venn <- list(venn_1 = venn_1, venn_2 = venn_2)
        names(venn) <- c(input$venn_1_name, input$venn_2_name)

        futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
        data <- VennDiagram::venn.diagram(venn, filename = NULL, cat.pos = c(-15,15),cat.dist = c(0.06,0.06),cat.cex = input$venn_label_size,
                                  fill = c(input$venn_1_color, input$venn_2_color), alpha = c(input$venn_alpha, input$venn_alpha),
                                  cat.col = c(input$venn_label_colour, input$venn_label_colour),cex = input$venn_text_size,label.col = input$venn_text_colour,
                                  col = c(input$venn_border_colour, input$venn_border_colour),lwd = input$venn_border_size, output=F, scaled=input$venn_scale_plot)
        p <- cowplot::plot_grid(data)
        return(p)
    }else if(!is.null(venn_table_1()) & !is.null(venn_table_2()) & !is.null(venn_table_3()) & is.null(venn_table_4())) {
        venn_1 <- venn_table_1(); venn_1 <- venn_1[, 1]
        venn_2 <- venn_table_2(); venn_2 <- venn_2[, 1]
        venn_3 <- venn_table_3(); venn_3 <- venn_3[, 1]
        venn <- list(venn_1 = venn_1, venn_2 = venn_2, venn_3 = venn_3)
        names(venn) <- c(input$venn_1_name, input$venn_2_name, input$venn_3_name)

        futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
        data <- VennDiagram::venn.diagram(venn, filename = NULL, cat.pos = c(-15,15,180),cat.dist = c(0.06,0.06,0.06),cat.cex = input$venn_label_size,
                                  fill = c(input$venn_1_color, input$venn_2_color, input$venn_3_color), alpha = c(input$venn_alpha, input$venn_alpha, input$venn_alpha),
                                  cat.col = c(input$venn_label_colour, input$venn_label_colour, input$venn_label_colour),cex = input$venn_text_size, label.col = input$venn_text_colour,
                                  col = c(input$venn_border_colour, input$venn_border_colour, input$venn_border_colour),lwd = input$venn_border_size, output=F, scaled=input$venn_scale_plot)
        p <- cowplot::plot_grid(data)
        return(p)
    }else if(!is.null(venn_table_1()) & !is.null(venn_table_2()) & !is.null(venn_table_3()) & !is.null(venn_table_4())){
        venn_1 <- venn_table_1(); venn_1 <- venn_1[, 1]
        venn_2 <- venn_table_2(); venn_2 <- venn_2[, 1]
        venn_3 <- venn_table_3(); venn_3 <- venn_3[, 1]
        venn_4 <- venn_table_4(); venn_4 <- venn_4[, 1]
        venn <- list(venn_1 = venn_1, venn_2 = venn_2, venn_3 = venn_3, venn_4 = venn_4)
        names(venn) <- c(input$venn_1_name, input$venn_2_name, input$venn_3_name, input$venn_4_name)

        data <- VennDiagram::venn.diagram(venn, filename = NULL, cat.pos = c(-10, 10, 180, 180), cat.dist = c(0.25, 0.25, -0.15, -0.15), cat.cex = input$venn_label_size,
                                  fill = c(input$venn_1_color, input$venn_2_color, input$venn_3_color, input$venn_4_color), 
                                  alpha = c(rep(input$venn_alpha, 4)),
                                  cat.col = c(rep(input$venn_label_colour, 4)),
                                  col = c(rep(input$venn_border_colour, 4)),
                                  cex = input$venn_text_size, label.col = input$venn_text_colour,
                                  lwd = input$venn_border_size, output=F, scaled=input$venn_scale_plot)
        p <- cowplot::plot_grid(data)
        return(p)
    }else {
       return(NULL)
    }
})
output$venn_plot <- renderPlot(venn_plot())

#reset venn plot
observeEvent(input$reset_venn_plot, {
  shinyjs::reset("input_venn_1")
  shinyjs::reset("input_venn_2")
  shinyjs::reset("input_venn_3")
  shinyjs::reset("input_venn_4")
  shinyjs::reset("upload_venn_1")
  shinyjs::reset("upload_venn_2")
  shinyjs::reset("upload_venn_3")
  shinyjs::reset("upload_venn_4")
  shinyjs::reset("venn_1_name")
  shinyjs::reset("venn_2_name")
  shinyjs::reset("venn_3_name")
  shinyjs::reset("venn_4_name")
  shinyjs::reset("venn_1_color")
  shinyjs::reset("venn_2_color")
  shinyjs::reset("venn_3_color")
  shinyjs::reset("venn_4_color")
  values_venn$upload_venn_1 <- 'NULL'
  values_venn$upload_venn_2 <- 'NULL'
  values_venn$upload_venn_3 <- 'NULL'
  values_venn$upload_venn_4 <- 'NULL'
})

#down plot
output$download_venn_plot <- downloadHandler(#下载
    filename = function() { paste("venn", input$venn_plot_format, sep = ".")},
    content = function(file) {
    p <- venn_plot()
    ggsave(file, p, width = input$venn_width, height = input$venn_height)
})

#venn detail
output$venn_detail <- renderUI({
    if(!is.null(venn_table_1()) & !is.null(venn_table_2()) & is.null(venn_table_3()) & is.null(venn_table_4())){
        fluidPage(
            column(width = 12, DT::dataTableOutput(outputId = "venn_only1", width = "100%"))
        )
    }else if (!is.null(venn_table_1()) & !is.null(venn_table_2()) & !is.null(venn_table_3()) & is.null(venn_table_4())) {
        fluidPage(
            column(width = 12, DT::dataTableOutput(outputId = "venn_only1", width = "100%"))
        )
    }else if (!is.null(venn_table_1()) & !is.null(venn_table_2()) & !is.null(venn_table_3()) & !is.null(venn_table_4())){
        fluidPage(
            column(width = 12, DT::dataTableOutput(outputId = "venn_only1", width = "100%"))
        )
    }else {
       return(NULL)
    }
})

tuo_set_detail <- reactive({
    if(!is.null(venn_table_1()) & !is.null(venn_table_2()) & is.null(venn_table_3()) & is.null(venn_table_4())){
        venn_1 <- venn_table_1(); venn_1 <- venn_1[, 1]
        venn_2 <- venn_table_2(); venn_2 <- venn_2[, 1]
        venn <- list(venn_1 = venn_1, venn_2 = venn_2)
        names(venn) <- c(input$venn_1_name, input$venn_2_name)

        futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")

        inter <- VennDiagram::get.venn.partitions(venn)
        for (i in 1:nrow(inter)) inter[i,'values'] <- paste(inter[[i,'..values..']], collapse = ', ') # nolint
        inter <- inter[,-c(3,4)]
        return(inter)
    }else if (!is.null(venn_table_1()) & !is.null(venn_table_2()) & !is.null(venn_table_3()) & is.null(venn_table_4())) {
        venn_1 <- venn_table_1(); venn_1 <- venn_1[, 1]
        venn_2 <- venn_table_2(); venn_2 <- venn_2[, 1]
        venn_3 <- venn_table_3(); venn_3 <- venn_3[, 1]
        venn <- list(venn_1 = venn_1, venn_2 = venn_2, venn_3 = venn_3)
        names(venn) <- c(input$venn_1_name, input$venn_2_name, input$venn_3_name)

        futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")

        inter <- VennDiagram::get.venn.partitions(venn)
        for (i in 1:nrow(inter)) inter[i,'values'] <- paste(inter[[i,'..values..']], collapse = ', ') # nolint
        inter <- inter[,-c(4,5)]
        return(inter)
    }else if (!is.null(venn_table_1()) & !is.null(venn_table_2()) & !is.null(venn_table_3()) & !is.null(venn_table_4())){
        venn_1 <- venn_table_1(); venn_1 <- venn_1[, 1]
        venn_2 <- venn_table_2(); venn_2 <- venn_2[, 1]
        venn_3 <- venn_table_3(); venn_3 <- venn_3[, 1]
        venn_4 <- venn_table_4(); venn_4 <- venn_4[, 1]
        venn <- list(venn_1 = venn_1, venn_2 = venn_2, venn_3 = venn_3, venn_4 = venn_4)
        names(venn) <- c(input$venn_1_name, input$venn_2_name, input$venn_3_name, input$venn_4_name)

        futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")

        inter <- VennDiagram::get.venn.partitions(venn)
        for (i in 1:nrow(inter)) inter[i,'values'] <- paste(inter[[i,'..values..']], collapse = ', ') # nolint
        inter <- inter[,-c(5,6)]
        return(inter)
    }else {
       return(NULL)
    }
})

output$venn_only1 <- DT::renderDataTable(data.frame(tuo_set_detail()), rownames = F, options = list(dom = 'ftpr', pageLength = 10, scrollX=TRUE,scrollY=TRUE, orderClasses = T), selection = "none")