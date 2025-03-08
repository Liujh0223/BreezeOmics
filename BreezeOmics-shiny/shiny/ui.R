##################################################################
#
# Shiny UI script 
# Author: Jianhong Liu
# Date: 2023.1.26
#
##################################################################

# source all tabs scripts
tab_files <- list.files(path = "ui_scripts", full.names = T, recursive = T)
lapply(tab_files, source)

# ui
ui <- shinyUI(
  tagList(
    tags$head(tags$style(HTML('.tab-content {width:1850px;margin-right: auto;margin-left: auto}'))),
    tags$body(style='width:initial;margin: 0 auto;overflow-x: auto;overflow-y: auto;'),
    useShinyjs(),
    navbarPage(
      title = div(img(src = "logo.png", height = "50px"),
                  style = "padding-left:50px;margin-left:-65px;margin-top:-15px;margin-right:-15px;"),
      windowTitle = "BreezeOmics-Vis",
      id = "navbar",
      selected = "home",
      theme = "1.css",
      fluid = TRUE,
      footer = HTML(paste0('
                  <div class = "page-info" style = "background-color:#337ab7;width: 100%;margin-bottom: -150px"> 
                    <h6><br /><h6>
                    <span class = "title">BreezeOmics-Vis v1.0 <br /></span> 
                    <span class = "content">This platform is committed to developing a simple, fast and understandable tool for visualizing omics analysis, and is free for all scientific researchers.
                    <br>Developed based on the shiny framework of R language, it references ggplot2, clusterProfile, DEseq2, edgeR, limma and other related dependency packages. &emsp;&emsp;&emsp;&emsp; &emsp;</span>
                    <hr style="width: 97%;border-top: 1px solid #FFF;" />
                    <span class = "footer">',' Copyright ',icon("copyright"),' 2025 Developed by Jianhong Liu<br /></span>
                    <h6>&emsp;<h6>
                  </div></b>')),
      
      # order of tabs -----------------
      home,
      datacheck,
      differenceanalysis,
      timeseriesanalysis,
      wgcna,
      enrichmentanalysis,
      coanalysis,
      tools,
      guide,
      
    )
  )# colse taglist
  
)# colse shinyui

## END