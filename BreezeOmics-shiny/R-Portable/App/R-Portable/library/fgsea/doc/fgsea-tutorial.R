## ----message=FALSE------------------------------------------------------------
library(fgsea)
library(data.table)
library(ggplot2)

## ----echo=FALSE---------------------------------------------------------------
library(BiocParallel)
register(SerialParam())

## -----------------------------------------------------------------------------
data(examplePathways)
data(exampleRanks)
set.seed(42)

## -----------------------------------------------------------------------------
fgseaRes <- fgsea(pathways = examplePathways, 
                  stats    = exampleRanks,
                  minSize  = 15,
                  maxSize  = 500)

## -----------------------------------------------------------------------------
head(fgseaRes[order(pval), ])

## -----------------------------------------------------------------------------
fgseaRes <- fgsea(pathways = examplePathways, 
                  stats    = exampleRanks,
                  eps      = 0.0,
                  minSize  = 15,
                  maxSize  = 500)

head(fgseaRes[order(pval), ])

## ---- fig.width=7, fig.height=4-----------------------------------------------
plotEnrichment(examplePathways[["5991130_Programmed_Cell_Death"]],
               exampleRanks) + labs(title="Programmed Cell Death")


## ---- fig.width=7, fig.height=8, fig.retina=2---------------------------------
topPathwaysUp <- fgseaRes[ES > 0][head(order(pval), n=10), pathway]
topPathwaysDown <- fgseaRes[ES < 0][head(order(pval), n=10), pathway]
topPathways <- c(topPathwaysUp, rev(topPathwaysDown))
plotGseaTable(examplePathways[topPathways], exampleRanks, fgseaRes, 
              gseaParam=0.5)

## ---- fig.width=7, fig.height=8, fig.retina=2, warning=FALSE------------------
collapsedPathways <- collapsePathways(fgseaRes[order(pval)][padj < 0.01], 
                                      examplePathways, exampleRanks)
mainPathways <- fgseaRes[pathway %in% collapsedPathways$mainPathways][
                         order(-NES), pathway]
plotGseaTable(examplePathways[mainPathways], exampleRanks, fgseaRes, 
              gseaParam = 0.5)

## ----message=FALSE------------------------------------------------------------
fwrite(fgseaRes, file="fgseaRes.txt", sep="\t", sep2=c("", " ", ""))

## ----message=FALSE------------------------------------------------------------
library(org.Mm.eg.db)
fgseaResMain <- fgseaRes[match(mainPathways, pathway)]
fgseaResMain[, leadingEdge := mapIdsList(
                                     x=org.Mm.eg.db, 
                                     keys=leadingEdge,
                                     keytype="ENTREZID", 
                                     column="SYMBOL")]
fwrite(fgseaResMain, file="fgseaResMain.txt", sep="\t", sep2=c("", " ", ""))

## ----message=FALSE, warning=FALSE---------------------------------------------
pathways <- reactomePathways(names(exampleRanks))
fgseaRes <- fgsea(pathways, exampleRanks, maxSize=500)
head(fgseaRes)

## -----------------------------------------------------------------------------
rnk.file <- system.file("extdata", "naive.vs.th1.rnk", package="fgsea")
gmt.file <- system.file("extdata", "mouse.reactome.gmt", package="fgsea")

## -----------------------------------------------------------------------------
ranks <- read.table(rnk.file,
                    header=TRUE, colClasses = c("character", "numeric"))
ranks <- setNames(ranks$t, ranks$ID)
str(ranks)

## -----------------------------------------------------------------------------
pathways <- gmtPathways(gmt.file)
str(head(pathways))

## ----warning=FALSE------------------------------------------------------------
fgseaRes <- fgsea(pathways, ranks, minSize=15, maxSize=500)
head(fgseaRes)

