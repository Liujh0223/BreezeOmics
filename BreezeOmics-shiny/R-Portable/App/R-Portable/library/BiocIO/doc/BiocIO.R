## ----installation, eval=FALSE-------------------------------------------------
#  if (!requireNamespace("BiocManager", quietly = TRUE))
#      install.packages("BiocManager")
#  BiocManager::install("BiocIO")

## ----library------------------------------------------------------------------
library("BiocIO")

## ----importexportGeneirc------------------------------------------------------
getGeneric("import")
getGeneric("export")

## ----warningExample, eval=FALSE-----------------------------------------------
#  file <- tempfile(fileext = ".loom")
#  LoomFile(file)
#  
#  ### LoomFile object
#  ### resource: file.loom
#  ### Warning messages:
#  ### 1: This class is extending the deprecated RTLFile class from
#  ###     rtracklayer. Use BiocFile from BiocIO in place of RTLFile.
#  ### 2: Use BiocIO::resource()

## ----replaceExample, eval=FALSE-----------------------------------------------
#  ## Old
#  setClass('LoomFile', contains='RTLFile')
#  
#  ## New
#  setClass('LoomFile', contains='BiocFile')

## ----defineCSVFile------------------------------------------------------------
.CSVFile <- setClass("CSVFile", contains = "BiocFile")

CSVFile <-
    function(resource)
{
    .CSVFile(resource = resource)
}

## ----defineImportExport-------------------------------------------------------
setMethod("import", "CSVFile",
    function(con, format, text, ...)
{
    read.csv(resource(con), ...)
})

setMethod("export", c("data.frame", "CSVFile"),
    function(object, con, format, ...)
{
    write.csv(object, resource(con), ...)
})

## ----demonstrateCSV-----------------------------------------------------------
temp <- tempfile(fileext = ".csv")
csv <- CSVFile(temp)

export(mtcars, csv)
df <- import(csv)

## ----sessionInfo, echo=FALSE--------------------------------------------------
sessionInfo()

