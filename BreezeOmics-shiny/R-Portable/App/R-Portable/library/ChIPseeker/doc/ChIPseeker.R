## ----style, echo=FALSE, results='asis', message=FALSE-------------------------
knitr::opts_chunk$set(tidy         = FALSE,
                      warning      = FALSE,
                      message      = FALSE)

CRANpkg <- function (pkg) {
    cran <- "https://CRAN.R-project.org/package"
    fmt <- "[%s](%s=%s)"
    sprintf(fmt, pkg, cran, pkg)
}

Biocpkg <- function (pkg) {
    sprintf("[%s](http://bioconductor.org/packages/%s)", pkg, pkg)
}

Biocannopkg <- Biocpkg

## ----echo=FALSE, results='hide', message=FALSE--------------------------------
library(GenomicFeatures)
library(GenomicRanges)
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
library(org.Hs.eg.db)
library(ggplot2)
library(clusterProfiler)
library(ReactomePA)
library(ChIPseeker)

## -----------------------------------------------------------------------------
## loading packages
library(ChIPseeker)
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene
library(clusterProfiler)

## -----------------------------------------------------------------------------
files <- getSampleFiles()
print(files)
peak <- readPeakFile(files[[4]])
peak

## ----fig.height=8, fig.width=10-----------------------------------------------
covplot(peak, weightCol="V5")

## ----fig.height=4, fig.width=10-----------------------------------------------
covplot(peak, weightCol="V5", chrs=c("chr17", "chr18"), xlim=c(4.5e7, 5e7))

## -----------------------------------------------------------------------------
## promoter <- getPromoters(TxDb=txdb, upstream=3000, downstream=3000)
## tagMatrix <- getTagMatrix(peak, windows=promoter)
##
## to speed up the compilation of this vignettes, we use a precalculated tagMatrix
data("tagMatrixList")
tagMatrix <- tagMatrixList[[4]]

## ----fig.cap="Heatmap of ChIP peaks binding to TSS regions", fig.align="center", fig.height=12, fig.width=4----
tagHeatmap(tagMatrix, xlim=c(-3000, 3000), color="red")

## ----eval=FALSE---------------------------------------------------------------
#  peakHeatmap(files[[4]], TxDb=txdb, upstream=3000, downstream=3000, color="red")

## ----eval=TRUE, fig.cap="Average Profile of ChIP peaks binding to TSS region", fig.align="center", fig.height=4, fig.width=7----
plotAvgProf(tagMatrix, xlim=c(-3000, 3000),
            xlab="Genomic Region (5'->3')", ylab = "Read Count Frequency")

## ----eval=FALSE---------------------------------------------------------------
#  plotAvgProf2(files[[4]], TxDb=txdb, upstream=3000, downstream=3000,
#               xlab="Genomic Region (5'->3')", ylab = "Read Count Frequency")

## ----fig.cap="Average Profile of ChIP peaks binding to TSS region", fig.align="center", fig.height=4, fig.width=7, eval=F----
#  plotAvgProf(tagMatrix, xlim=c(-3000, 3000), conf = 0.95, resample = 1000)

## ----eval=F-------------------------------------------------------------------
#  ## The results of binning method and normal method are nearly the same.
#  tagMatrix_binning <- getTagMatrix(peak = peak, TxDb = txdb,
#                                    upstream = 3000, downstream = 3000,
#                                    type = "start_site", by = "gene",
#                                    weightCol = "V5", nbin = 800)

## ----eval=F-------------------------------------------------------------------
#  ## Here uses `plotPeakProf2` to do all things in one step.
#  ## Gene body regions having lengths smaller than nbin will be filtered
#  ## A message will be given to warning users about that.
#  ## >> 9 peaks(0.872093%), having lengths smaller than 800bp, are filtered...
#  
#  ## the ignore_strand is FALSE in default. We put here to emphasize that.
#  ## We will not show it again in the below example
#  plotPeakProf2(peak = peak, upstream = rel(0.2), downstream = rel(0.2),
#                conf = 0.95, by = "gene", type = "body", nbin = 800,
#                TxDb = txdb, weightCol = "V5",ignore_strand = F)

## ----eval=F-------------------------------------------------------------------
#  ## The first method using getBioRegion(), getTagMatrix() and plotPeakProf() to plot in three steps.
#  genebody <- getBioRegion(TxDb = txdb,
#                           by = "gene",
#                           type = "body")
#  
#  matrix_no_flankextension <- getTagMatrix(peak,windows = genebody, nbin = 800)
#  
#  plotPeakProf(matrix_no_flankextension,conf = 0.95)
#  
#  ## The second method of using getTagMatrix() and plotPeakProf() to plot in two steps
#  matrix_actual_extension <- getTagMatrix(peak,windows = genebody, nbin = 800,
#                                          upstream = 1000,downstream = 1000)
#  plotPeakProf(matrix_actual_extension,conf = 0.95)
#  

## ----eval=F-------------------------------------------------------------------
#  five_UTR_body <- getTagMatrix(peak = peak,
#                                TxDb = txdb,
#                                upstream = rel(0.2),
#                                downstream = rel(0.2),
#                                type = "body",
#                                by = "5UTR",
#                                weightCol = "V5",
#                                nbin = 50)
#  
#  plotPeakProf(tagMatrix = five_UTR_body, conf = 0.95)

## ----eval=F-------------------------------------------------------------------
#  TTS_matrix <- getTagMatrix(peak = peak,
#                             TxDb = txdb,
#                             upstream = 3000,
#                             downstream = 3000,
#                             type = "end_site",
#                             by = "gene",
#                             weightCol = "V5")
#  
#  plotPeakProf(tagMatrix = TTS_matrix, conf = 0.95)

## -----------------------------------------------------------------------------
peakAnno <- annotatePeak(files[[4]], tssRegion=c(-3000, 3000),
                         TxDb=txdb, annoDb="org.Hs.eg.db")

## ---- eval = FALSE------------------------------------------------------------
#  library(EnsDb.Hsapiens.v75)
#  edb <- EnsDb.Hsapiens.v75
#  seqlevelsStyle(edb) <- "UCSC"
#  
#  peakAnno.edb <- annotatePeak(files[[4]], tssRegion=c(-3000, 3000),
#                               TxDb=edb, annoDb="org.Hs.eg.db")

## ----fig.cap="Genomic Annotation by pieplot", fig.align="center", fig.height=6, fig.width=8----
plotAnnoPie(peakAnno)

## ----fig.cap="Genomic Annotation by barplot", fig.align="center", fig.height=4, fig.width=10----
plotAnnoBar(peakAnno)

## ----fig.cap="Genomic Annotation by vennpie", fig.align="center", fig.height=8, fig.width=11----
vennpie(peakAnno)

## ----eval=F, fig.cap="Genomic Annotation by upsetplot", fig.align="center", fig.height=8, fig.width=12----
#  upsetplot(peakAnno)

## ----eval=F, fig.cap="Genomic Annotation by upsetplot", fig.align="center", fig.height=8, fig.width=12----
#  upsetplot(peakAnno, vennpie=TRUE)

## ----fig.cap="Distribution of Binding Sites", fig.align="center", fig.height=2, fig.width=6----
plotDistToTSS(peakAnno,
              title="Distribution of transcription factor-binding loci\nrelative to TSS")

## ----fig.width=8, fig.height=5------------------------------------------------
library(ReactomePA)

pathway1 <- enrichPathway(as.data.frame(peakAnno)$geneId)
head(pathway1, 2)

gene <- seq2gene(peak, tssRegion = c(-1000, 1000), flankDistance = 3000, TxDb=txdb)
pathway2 <- enrichPathway(gene)
head(pathway2, 2)
dotplot(pathway2)

## ----eval=TRUE, fig.cap="Average Profiles of ChIP peaks among different experiments", fig.align="center", fig.height=4, fig.width=6----
## promoter <- getPromoters(TxDb=txdb, upstream=3000, downstream=3000)
## tagMatrixList <- lapply(files, getTagMatrix, windows=promoter)
##
## to speed up the compilation of this vigenette, we load a precaculated tagMatrixList
data("tagMatrixList")
plotAvgProf(tagMatrixList, xlim=c(-3000, 3000))

## ----eval=FALSE, fig.cap="Average Profiles of ChIP peaks among different experiments", fig.align="center", fig.height=7, fig.width=6----
#  plotAvgProf(tagMatrixList, xlim=c(-3000, 3000), conf=0.95,resample=500, facet="row")

## ----eval=F-------------------------------------------------------------------
#  ## normal method
#  plotPeakProf2(files, upstream = 3000, downstream = 3000, conf = 0.95,
#                by = "gene", type = "start_site", TxDb = txdb,
#                facet = "row")
#  
#  ## binning method
#  plotPeakProf2(files, upstream = 3000, downstream = 3000, conf = 0.95,
#                by = "gene", type = "start_site", TxDb = txdb,
#                facet = "row", nbin = 800)
#  

## ----eval=TRUE, fig.cap="Heatmap of ChIP peaks among different experiments", fig.align="center", fig.height=8, fig.width=7----
tagHeatmap(tagMatrixList, xlim=c(-3000, 3000), color=NULL)

## ----eval=F-------------------------------------------------------------------
#  plotPeakProf2(files, upstream = rel(0.2), downstream = rel(0.2),
#                conf = 0.95, by = "gene", type = "body",
#                TxDb = txdb, facet = "row", nbin = 800)

## -----------------------------------------------------------------------------
peakAnnoList <- lapply(files, annotatePeak, TxDb=txdb,
                       tssRegion=c(-3000, 3000), verbose=FALSE)

## ----fig.cap="Genomic Annotation among different ChIPseq data", fig.align="center", fig.height=4, fig.width=6----
plotAnnoBar(peakAnnoList)

## ----fig.cap="Distribution of Binding Sites among different ChIPseq data", fig.align="center", fig.height=5, fig.width=8----
plotDistToTSS(peakAnnoList)

## ----fig.width=8.5, fig.height=8.5--------------------------------------------
genes = lapply(peakAnnoList, function(i) as.data.frame(i)$geneId)
names(genes) = sub("_", "\n", names(genes))
compKEGG <- compareCluster(geneCluster   = genes,
                         fun           = "enrichKEGG",
                         pvalueCutoff  = 0.05,
                         pAdjustMethod = "BH")
dotplot(compKEGG, showCategory = 15, title = "KEGG Pathway Enrichment Analysis")

## ----fig.cap="Overlap of annotated genes", fig.align="center", fig.height=7, fig.width=7----
genes= lapply(peakAnnoList, function(i) as.data.frame(i)$geneId)
vennplot(genes)

## -----------------------------------------------------------------------------
p <- GRanges(seqnames=c("chr1", "chr3"),
             ranges=IRanges(start=c(1, 100), end=c(50, 130)))
shuffle(p, TxDb=txdb)

## -----------------------------------------------------------------------------
enrichPeakOverlap(queryPeak     = files[[5]],
                  targetPeak    = unlist(files[1:4]),
                  TxDb          = txdb,
                  pAdjustMethod = "BH",
                  nShuffle      = 50,
                  chainFile     = NULL,
                  verbose       = FALSE)

## -----------------------------------------------------------------------------
getGEOspecies()

## -----------------------------------------------------------------------------
getGEOgenomeVersion()

## -----------------------------------------------------------------------------
hg19 <- getGEOInfo(genome="hg19", simplify=TRUE)
head(hg19)

## ----eval=FALSE---------------------------------------------------------------
#  downloadGEObedFiles(genome="hg19", destDir="hg19")

## ----eval=FALSE---------------------------------------------------------------
#  gsm <- hg19$gsm[sample(nrow(hg19), 10)]
#  downloadGSMbedFiles(gsm, destDir="hg19")

## ----echo=FALSE---------------------------------------------------------------
sessionInfo()

