## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(HDO.db)

## -----------------------------------------------------------------------------
library(AnnotationDbi)

## -----------------------------------------------------------------------------
ls("package:HDO.db")
packageVersion("HDO.db")

## -----------------------------------------------------------------------------
toTable(HDOmetadata)
HDOMAPCOUNTS

## -----------------------------------------------------------------------------
doterm <- toTable(HDOTERM)
head(doterm)

## -----------------------------------------------------------------------------
dotermlist <- as.list(HDOTERM)
head(dotermlist)

## -----------------------------------------------------------------------------
doalias <- as.list(HDOALIAS)
doalias[['DOID:0001816']]

## -----------------------------------------------------------------------------
dosynonym <- as.list(HDOSYNONYM)
dosynonym[['DOID:0001816']]

## -----------------------------------------------------------------------------
anc_table <- toTable(HDOANCESTOR)
head(anc_table)

## -----------------------------------------------------------------------------
anc_list <- AnnotationDbi::as.list(HDOANCESTOR)
anc_list[["DOID:0001816"]]

## -----------------------------------------------------------------------------
parent_table <- toTable(HDOPARENTS)
head(parent_table)

## -----------------------------------------------------------------------------
parent_list <- AnnotationDbi::as.list(HDOPARENTS)
parent_list[["DOID:0001816"]]

## -----------------------------------------------------------------------------
off_list <- AnnotationDbi::as.list(HDO.db::HDOOFFSPRING)
off_list[["DOID:0001816"]]

## -----------------------------------------------------------------------------
child_list <- AnnotationDbi::as.list(HDO.db::HDOCHILDREN)
child_list[["DOID:4"]]

## -----------------------------------------------------------------------------
columns(HDO.db)
## use doid keys
dokeys <- head(keys(HDO.db))
res <- select(x = HDO.db, keys = dokeys, keytype = "doid", 
    columns = c("offspring", "term", "parent"))
head(res)
## use term keys
dokeys <- head(keys(HDO.db, keytype = "term"))
res <- select(x = HDO.db, keys = dokeys, keytype = "term", 
    columns = c("offspring", "doid", "parent"))   
head(res)

## -----------------------------------------------------------------------------
sessionInfo()

