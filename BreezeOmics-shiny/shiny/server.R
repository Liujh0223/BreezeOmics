##################################################################
#
# Shiny Server script 
# Author: Jianhong Liu
# Date: 2023.1.26
#
##################################################################
library(shiny)
# server
server <- shinyServer(function(input, output, session){
  ##-- HOME ----
  source("server_scripts/server_home.R", local = TRUE)
  ##-- DATA CHECK ----
  source("server_scripts/server_datacheck.R", local = TRUE)
  source("server_scripts/server_plot/plot_datacheck.R", local = TRUE)
  #-- Difference analysis ----
  source("server_scripts/server_differenceanalysis.R", local = TRUE)
  source("server_scripts/server_plot/plot_differenceanalysis.R", local = TRUE)
  ##-- Time Series Analysis -----
  source("server_scripts/server_timeseriesanalysis.R", local = TRUE)
  ##-- Time Series Analysis -----
  source("server_scripts/server_wgcna.R", local = TRUE)
  ##-- Enrichment ----
  source("server_scripts/server_enrich.R", local = TRUE)
  source("server_scripts/server_plot/plot_enrich.R", local = TRUE)
  ##-- Co-analysis ----
  source("server_scripts/server_coanalysis.R", local = TRUE)
  ##-- Tools ----
  source("server_scripts/server_tools.R", local = TRUE)
  ##-- Guide ----
  source("server_scripts/server_guide.R", local = TRUE)
  ##-- Keep the script stops running when the web is closed
  session$onSessionEnded(function() {
    unlink(paste0(getwd(), "/tmp"), recursive = T)
    # stopApp()
  })
})