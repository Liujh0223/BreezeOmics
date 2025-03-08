### R code from vignette source 'Rsamtools-Overview.Rnw'

###################################################
### code chunk number 1: style
###################################################
BiocStyle::latex()


###################################################
### code chunk number 2: options
###################################################
options(width=60)


###################################################
### code chunk number 3: preliminaries
###################################################
library(Rsamtools)


###################################################
### code chunk number 4: ScanBamParam
###################################################
which <- GRanges(c(
    "seq1:1000-2000",
    "seq2:100-1000",
    "seq2:1000-2000"
))
## equivalent:
## GRanges(
##     seqnames = c("seq1", "seq2", "seq2"),
##     ranges = IRanges(
##         start = c(1000, 100, 1000),
##         end = c(2000, 1000, 2000)
##     )
## )
what <- c("rname", "strand", "pos", "qwidth", "seq")
param <- ScanBamParam(which=which, what=what)


###################################################
### code chunk number 5: scanBam
###################################################
bamFile <- system.file("extdata", "ex1.bam", package="Rsamtools")
bam <- scanBam(bamFile, param=param)


###################################################
### code chunk number 6: scanBam-elts-1
###################################################
class(bam)
names(bam)


###################################################
### code chunk number 7: scanBam-elts-2
###################################################
class(bam[[1]])
names(bam[[1]])


###################################################
### code chunk number 8: scanBam-elts-class
###################################################
sapply(bam[[1]], class)


###################################################
### code chunk number 9: scanBam-to-IRanges
###################################################
.unlist <- function (x)
{
    ## do.call(c, ...) coerces factor to integer, which is undesired
    x1 <- x[[1L]]
    if (is.factor(x1)) {
        structure(unlist(x), class = "factor", levels = levels(x1))
    } else {
        do.call(c, x)
    }
}
bam <- unname(bam) # names not useful in unlisted result
elts <- setNames(bamWhat(param), bamWhat(param))
lst <- lapply(elts, function(elt) .unlist(lapply(bam, "[[", elt)))


###################################################
### code chunk number 10: lst-to-DataFrame
###################################################
head(do.call("DataFrame", lst))


###################################################
### code chunk number 11: indexed-file
###################################################
list.files(dirname(bamFile), pattern="ex1.bam(.bai)?")


###################################################
### code chunk number 12: bam-remote (eval = FALSE)
###################################################
## which <- GRanges("6:100000-110000")
## param <- ScanBamParam(which=which, what=scanBamWhat())
## na19240bam <- scanBam(na19240url, param=param)


###################################################
### code chunk number 13: summaryFunction
###################################################
summaryFunction <-
    function(seqname, bamFile, ...)
{
    param <- ScanBamParam(
        what=c('pos', 'qwidth'),
        which=GRanges(seqname, IRanges(1, 1e7)),
        flag=scanBamFlag(isUnmappedQuery=FALSE)
    )
    x <- scanBam(bamFile, ..., param=param)[[1]]
    coverage(IRanges(x[["pos"]], width=x[["qwidth"]]))
}


###################################################
### code chunk number 14: summaryByChromosome
###################################################
seqnames <- paste("seq", 1:2, sep="")
cvg <- lapply(seqnames, summaryFunction, bamFile)
names(cvg) <- seqnames
cvg


###################################################
### code chunk number 15: keggrest (eval = FALSE)
###################################################
## ## uses KEGGREST, dplyr, and tibble packages
## org <- "hsa"
## 
## caffeine_pathway <-
##     KEGGREST::keggList("pathway", org) %>%
##     tibble::enframe(name = "pathway_id", value = "pathway") %>%
##     dplyr::filter(startsWith(.data$pathway, "Caffeine metabolism"))
## 
## egid <-
##     KEGGREST::keggLink(org, "pathway") %>%
##     tibble::enframe(name = "pathway_id", value = "gene_id") %>%
##     dplyr::left_join(x = caffeine_pathway, by = "pathway_id") %>%
##     dplyr::mutate(gene_id = sub("hsa:", "", gene_id)) %>%
##     pull(gene_id)


###################################################
### code chunk number 16: caffeine-kegg
###################################################
egid <- c("10", "1544", "1548", "1549", "7498", "9")


###################################################
### code chunk number 17: txdb-transcripts
###################################################
library(TxDb.Hsapiens.UCSC.hg18.knownGene)
bamRanges <- transcripts(
    TxDb.Hsapiens.UCSC.hg18.knownGene,
    filter=list(gene_id=egid)
)
seqlevels(bamRanges) <-                 # translate seqlevels
    sub("chr", "", seqlevels(bamRanges))
lvls <- seqlevels(bamRanges)            # drop unused levels
seqlevels(bamRanges) <- lvls[lvls %in% as.character(seqnames(bamRanges))]

bamRanges


###################################################
### code chunk number 18: bamRanges-genome
###################################################
unique(genome(bamRanges))


###################################################
### code chunk number 19: BamViews-parts
###################################################
slxMaq09 <- local({
    fl <- system.file("extdata", "slxMaq09_urls.txt", package="Rsamtools")
    readLines(fl)
})


###################################################
### code chunk number 20: BamViews-construct
###################################################
bamExperiment <-
    list(description="Caffeine metabolism views on 1000 genomes samples",
         created=date())
bv <- BamViews(
    slxMaq09, bamRanges=bamRanges, bamExperiment=bamExperiment
)
metadata(bamSamples(bv)) <-
    list(description="Solexa/MAQ samples, August 2009",
         created="Thu Mar 25 14:08:42 2010")
bv


###################################################
### code chunk number 21: BamViews-query
###################################################
bamExperiment(bv)


###################################################
### code chunk number 22: bamIndicies (eval = FALSE)
###################################################
## bamIndexDir <- tempfile()
## indexFiles <- paste(bamPaths(bv), "bai", sep=".")
## dir.create(bamIndexDir)
## bv <- BamViews(
##     slxMaq09,
##     file.path(bamIndexDir, basename(indexFiles)), # index file location
##     bamRanges=bamRanges,
##     bamExperiment=bamExperiment
## )
## 
## idxFiles <- mapply(
##     download.file, indexFiles,
##     bamIndicies(bv),
##     MoreArgs=list(method="curl")
## )


###################################################
### code chunk number 23: readGAlignments (eval = FALSE)
###################################################
## library(GenomicAlignments)
## olaps <- readGAlignments(bv)


###################################################
### code chunk number 24: olaps
###################################################
library(GenomicAlignments)
load(system.file("extdata", "olaps.Rda", package="Rsamtools"))
olaps
head(olaps[[1]])


###################################################
### code chunk number 25: read-lengths
###################################################
head(t(sapply(olaps, function(elt) range(qwidth(elt)))))


###################################################
### code chunk number 26: focal
###################################################
rng <- bamRanges(bv)[1]
strand(rng) <- "*"
olap1 <- endoapply(olaps, subsetByOverlaps, rng)
olap1 <- lapply(olap1, "seqlevels<-", value=as.character(seqnames(rng)))
head(olap1[[24]])


###################################################
### code chunk number 27: olap-cvg
###################################################
minw <- min(sapply(olap1, function(elt) min(start(elt))))
maxw <- max(sapply(olap1, function(elt) max(end(elt))))
cvg <- endoapply(
    olap1, coverage,
    shift=-start(ranges(bamRanges[1])),
    width=width(ranges(bamRanges[1]))
)
cvg[[1]]


###################################################
### code chunk number 28: olap-cvg-as-m
###################################################
m <- matrix(unlist(lapply(cvg, lapply, as.vector)), ncol=length(cvg))
summary(rowSums(m))
summary(colSums(m))


###################################################
### code chunk number 29: sessionInfo
###################################################
packageDescription("Rsamtools")
sessionInfo()


###################################################
### code chunk number 30: bam-avail (eval = FALSE)
###################################################
## library(RCurl)
## ftpBase <-
##     "ftp://ftp-trace.ncbi.nih.gov/1000genomes/ftp/pilot_data/data/"
## indivDirs <-
##     strsplit(getURL(ftpBase, ftplistonly=TRUE), "\n")[[1]]
## alnDirs <-
##     paste(ftpBase, indivDirs, "/alignment/", sep="")
## urls0 <-
##     strsplit(getURL(alnDirs, dirlistonly=TRUE), "\n")


###################################################
### code chunk number 31: bam-index (eval = FALSE)
###################################################
## urls <- urls[sapply(urls0, length) != 0]
## fls0 <- unlist(unname(urls0))
## fls1 <- fls0[grepl("bai$", fls0)]
## fls <- fls1[sapply(strsplit(fls1, "\\."), length)==7]


###################################################
### code chunk number 32: slxMaq09 (eval = FALSE)
###################################################
## urls1 <- Filter(
##     function(x) length(x) != 0,
##     lapply(urls, function(x) x[grepl("SLX.maq.*2009_08.*bai$", x)])
## )
## slxMaq09.bai <-
##    mapply(paste, names(urls1), urls1, sep="", USE.NAMES=FALSE)
## slxMaq09 <- sub(".bai$", "", slxMaq09.bai) #$


###################################################
### code chunk number 33: bamIndicies (eval = FALSE)
###################################################
## bamIndexDir <- tempfile()
## dir.create(bamIndexDir)
## idxFiles <- mapply(
##     download.file, slxMaq09.bai,
##     file.path(bamIndexDir, basename(slxMaq09.bai)) ,
##     MoreArgs=list(method="curl")
## )


