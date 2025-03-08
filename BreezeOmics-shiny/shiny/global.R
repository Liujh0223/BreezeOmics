##################################################################
#
# Global script 
# Author: Jianhong Liu
# Date: 2023.1.26
#
##################################################################

# contains:-

# 1. required packages
# 2. required funtions
# 3. source ui/server scripts
# 4. running shiny app

##################################################################

# 1. required packages -------------------------------------------
library(shiny)
library(shinyjs)
# library(slickR)(弃用)
library(ggplot2)
library(dplyr)
library(bsplus)
library(shinycssloaders)

# library(shinythemes)
library(shinyWidgets)
library(ggraph)

# library(openxlsx)
# library(reshape2)
# library(aplot)
# library(colourpicker)
# library(stringr)
# library(R.utils)
# library(DT)
# library(edgeR)
# library(DESeq2)
# library(ggrepel)
# # library(factoextra)
# library(ggforce)
# library(clusterProfiler)
# library(data.table)
# library(statmod)
# library(VennDiagram)
# library(shinydashboard)
# library(Mfuzz)
# library(ComplexHeatmap)
# library(WGCNA)
# library(gplots)
# library(zip)

# openxlsx, reshape2, aplot, colourpicker, stringr, R.utils, DT, edgeR, DESeq2, ggrepel, factoextra, ggforce, clusterProfiler, data.table, statmod, VennDiagram, shinydashboard, Mfuzz, ComplexHeatmap, WGCNA, gplots,zip
# gtable, gridExtra, grid, tidygraph, patchwork, igraph, 
# ChIPseeker, GenomicFeatures, TxDb.Hsapiens.UCSC.hg19.knownGene

# 2. required funtions
source('functions/function_tab_voronoys.R') #tab_voronoys
source('functions/function_upload_file.R')
source('functions/function_detectcolname.R')

# 3. source ui/server scripts ------------------------------------
source('ui.R')
options(shiny.maxRequestSize = 20 * 1024^2)#文件上传最大为15M
source('server.R')

# 4.running shiny app --------------------------------------------
shinyApp(ui = ui, server = server)