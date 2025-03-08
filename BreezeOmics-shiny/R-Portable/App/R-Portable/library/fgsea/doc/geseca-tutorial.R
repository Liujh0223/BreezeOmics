## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  score <- sum(colSums(E[p, ])**2) / length(p)

## ----message=FALSE------------------------------------------------------------
library(GEOquery)
library(limma)

gse200250 <- getGEO("GSE200250", AnnotGPL = TRUE)[[1]]

es <- gse200250
es <- es[, grep("Th2_", es$title)]
es$time <- as.numeric(gsub(" hours", "", es$`time point:ch1`))
es <- es[, order(es$time)]

exprs(es) <- normalizeBetweenArrays(log2(exprs(es)), method="quantile")

es <- es[order(rowMeans(exprs(es)), decreasing=TRUE), ]
es <- es[!duplicated(fData(es)$`Gene ID`), ]
rownames(es) <- fData(es)$`Gene ID`
es <- es[!grepl("///", rownames(es)), ]
es <- es[rownames(es) != "", ]

fData(es) <- fData(es)[, c("ID", "Gene ID", "Gene symbol")]

es <- es[head(order(rowMeans(exprs(es)), decreasing=TRUE), 12000), ]
head(exprs(es))

## -----------------------------------------------------------------------------
library(msigdbr)
pathwaysDF <- msigdbr("mouse", category="H")
pathways <- split(as.character(pathwaysDF$entrez_gene), pathwaysDF$gs_name)

## ----message=FALSE------------------------------------------------------------
library(fgsea)
set.seed(1)
gesecaRes <- geseca(pathways, exprs(es), minSize = 15, maxSize = 500)

## -----------------------------------------------------------------------------
head(gesecaRes, 10)

## ----fig.width=7, fig.height=4------------------------------------------------
plotCoregulationProfile(pathway=pathways[["HALLMARK_E2F_TARGETS"]], 
                        E=exprs(es), titles = es$title, conditions=es$`time point:ch1`)

## ----fig.width=10, fig.height=4, out.width="100%"-----------------------------
plotCoregulationProfile(pathway=pathways[["HALLMARK_HYPOXIA"]], 
                        E=exprs(es), titles = es$title, conditions=es$`time point:ch1`)



## ----fig.width=10, fig.height=6, out.width="100%"-----------------------------
plotGesecaTable(gesecaRes |> head(10), pathways, E=exprs(es), titles = es$title)

## -----------------------------------------------------------------------------
E <- t(base::scale(t(exprs(es)), scale=FALSE))
pcaRev <- prcomp(E, center=FALSE)
Ered <- pcaRev$x[, 1:10]
dim(Ered)

## -----------------------------------------------------------------------------
set.seed(1)
gesecaResRed <- geseca(pathways, Ered, minSize = 15, maxSize = 500, center=FALSE)
head(gesecaResRed, 10)

## ----fig.width=4, fig.height=4------------------------------------------------
library(ggplot2)
ggplot(data=merge(gesecaRes[, list(pathway, logPvalFull=-log10(pval))],
                  gesecaResRed[, list(pathway, logPvalRed=-log10(pval))])) +
    geom_point(aes(x=logPvalFull, y=logPvalRed)) +
    coord_fixed() + theme_classic()

## ----eval=FALSE---------------------------------------------------------------
#  devtools::install_github('satijalab/seurat-data')

## ----eval=FALSE---------------------------------------------------------------
#  SeuratData::InstallData("stxBrain")

## ----message=FALSE, eval=FALSE------------------------------------------------
#  library(Seurat)
#  library(SeuratData)
#  library(ggplot2)
#  library(patchwork)
#  brain <- LoadData("stxBrain", type = "anterior1")

## ----eval=FALSE---------------------------------------------------------------
#  brain <- SCTransform(brain, assay = "Spatial", verbose = FALSE, variable.features.n = 10000)

## ----eval=FALSE---------------------------------------------------------------
#  brain <- RunPCA(brain, assay = "SCT", verbose = FALSE,
#                  rev.pca = TRUE, reduction.name = "pca.rev", reduction.key="PCR_")
#  E <- brain@reductions$pca.rev@feature.loadings

## ----eval=FALSE---------------------------------------------------------------
#  library(msigdbr)
#  pathwaysDF <- msigdbr("mouse", category="C2", subcategory = "CP:KEGG")
#  pathways <- split(pathwaysDF$gene_symbol, pathwaysDF$gs_name)

## ----message=FALSE, eval=FALSE------------------------------------------------
#  set.seed(1)
#  
#  gesecaRes <- geseca(pathways, E, minSize = 15, maxSize = 500, center = FALSE)

## ----fig.width=8, fig.height=8, out.width="100%", eval=FALSE------------------
#  
#  topPathways <- gesecaRes[, pathway] |> head(4)
#  
#  for (ppn in topPathways) {
#      pp <- pathways[[ppn]]
#      pp <- intersect(pp, rownames(E))
#  
#      score <- colSums(brain@assays$SCT@scale.data[pp, ])/sqrt(length(pp))
#      brain@meta.data[[ppn]] <- score
#  }
#  SpatialFeaturePlot(brain, features = topPathways, )

