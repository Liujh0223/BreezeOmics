### R code from vignette source 'rtracklayer.Rnw'

###################################################
### code chunk number 1: initialize
###################################################
options(width=70)


###################################################
### code chunk number 2: rtl-init
###################################################
library("humanStemCell")
data(fhesc)
library("genefilter")
filtFhesc <- nsFilter(fhesc)[[1]]
library("limma")
design <- model.matrix(~filtFhesc$Diff) 
hesclim <- lmFit(filtFhesc, design) 
hesceb <- eBayes(hesclim) 
tab <- topTable(hesceb, coef = 2, adjust.method = "BH", n = 7676) 
tab2 <- tab[(tab$logFC > 1) & (tab$adj.P.Val < 0.01),]
affyIDs <- rownames(tab2)
library("microRNA")
data(hsTargets)
library("hgu133plus2.db")
entrezIDs <- mappedRkeys(hgu133plus2ENTREZID[affyIDs])
library("org.Hs.eg.db")
mappedEntrezIDs <- entrezIDs[entrezIDs %in% mappedkeys(org.Hs.egENSEMBLTRANS)]
ensemblIDs <- mappedRkeys(org.Hs.egENSEMBLTRANS[mappedEntrezIDs])
targetMatches <- match(ensemblIDs, hsTargets$target, 0)
## same as data(targets)
targets <- hsTargets[targetMatches,]
targets$chrom <- paste("chr", targets$chrom, sep = "")


###################################################
### code chunk number 3: rtl-miRNA-track
###################################################
library(rtracklayer)
library(GenomicRanges)
## call data(targets) if skipping first block
head(targets)
targetRanges <- IRanges(targets$start, targets$end)
targetTrack <- with(targets, 
                    GRangesForUCSCGenome("hg18", chrom, targetRanges, strand, 
                                         name, target))


###################################################
### code chunk number 4: rtl-miRNA-track-seqinfo
###################################################
genome(targetTrack)
head(seqlengths(targetTrack))


###################################################
### code chunk number 5: feature-data-accessors
###################################################
head(seqnames(targetTrack))
head(start(targetTrack))


###################################################
### code chunk number 6: sol-1
###################################################
head(strand(targetTrack))
head(width(targetTrack))
data.frame(chrom = as.factor(seqnames(targetTrack)), 
           start = start(targetTrack), end = end(targetTrack),
           strand = as.factor(strand(targetTrack)))


###################################################
### code chunk number 7: subset-features
###################################################
## get the first 10 targets
first10 <- targetTrack[1:10]
## get pos strand targets
posTargets <- targetTrack[strand(targetTrack) == "+"]
## get the targets on chr1
chr1Targets <- targetTrack[seqnames(targetTrack) == "chr1"]


###################################################
### code chunk number 8: sol-2
###################################################
negChr2Targets <- targetTrack[strand(targetTrack) == "-" & 
                              seqnames(targetTrack) == "chr2"]


###################################################
### code chunk number 9: export (eval = FALSE)
###################################################
## export(targetTrack, "targets.bed")


###################################################
### code chunk number 10: import (eval = FALSE)
###################################################
## restoredTrack <- import("targets.bed")


###################################################
### code chunk number 11: sol-3
###################################################
export(targetTrack, "targets.gff")
targetGff <- import("targets.gff")
targetChar <- export(targetTrack, format = "gff1")


###################################################
### code chunk number 12: browserSession (eval = FALSE)
###################################################
## session <- browserSession("UCSC")


###################################################
### code chunk number 13: genomeBrowsers
###################################################
genomeBrowsers()


###################################################
### code chunk number 14: layTrack (eval = FALSE)
###################################################
## track(session, "targets") <- targetTrack


###################################################
### code chunk number 15: sol-4 (eval = FALSE)
###################################################
## session$target100 <- targetTrack[1:100]


###################################################
### code chunk number 16: take-subset
###################################################
subTargetTrack <- targetTrack[1] # get first feature


###################################################
### code chunk number 17: view-subset (eval = FALSE)
###################################################
## view <- browserView(session, subTargetTrack * -10, pack = "targets")


###################################################
### code chunk number 18: view-subset-multi (eval = FALSE)
###################################################
## view <- browserView(session, targetTrack[1:5] * -10, pack = "targets")


###################################################
### code chunk number 19: sol-6 (eval = FALSE)
###################################################
## viewOut <- browserView(session, range(view) * -2)
## viewFull <- browserView(session, full = "targets")


###################################################
### code chunk number 20: browseGenome (eval = FALSE)
###################################################
## browseGenome(targetTrack, range = subTargetTrack * -10)


###################################################
### code chunk number 21: browseGenome-simple (eval = FALSE)
###################################################
## browseGenome(subTargetTrack)


###################################################
### code chunk number 22: get-track-names (eval = FALSE)
###################################################
## loaded_tracks <- trackNames(session)


###################################################
### code chunk number 23: get-track-data (eval = FALSE)
###################################################
## subTargetTrack <- track(session, "targets")


###################################################
### code chunk number 24: get-track-segment (eval = FALSE)
###################################################
## chr1Targets <- track(session, "targets", chr1Targets)


###################################################
### code chunk number 25: sol-7 (eval = FALSE)
###################################################
## region <- range(subTargetTrack) + 500
## targetSNP <- track(session, "snp130", region)
## as.data.frame(targetSNP)
## targetGene <- track(session, "knownGene", region)
## as.data.frame(targetGene)


###################################################
### code chunk number 26: genomeSegment-view (eval = FALSE)
###################################################
## segment <- range(view)


###################################################
### code chunk number 27: tracks-view (eval = FALSE)
###################################################
## visible_tracks <- trackNames(view)
## trackNames(view) <- visible_tracks


###################################################
### code chunk number 28: track-modes-view (eval = FALSE)
###################################################
## modes <- ucscTrackModes(view)


###################################################
### code chunk number 29: set-track-modes (eval = FALSE)
###################################################
## modes["targets"]
## modes["targets"] <- "full"
## ucscTrackModes(view) <- modes


###################################################
### code chunk number 30: browserViews (eval = FALSE)
###################################################
## views <- browserViews(session)
## length(views)


###################################################
### code chunk number 31: sol-8 (eval = FALSE)
###################################################
## viewTarget <- track(session, "targets", range(view))
## trackNames(view) <- c("snp130", "knownGene", "targets")
## ucscTrackModes(view)["knownGene"] <- "hide"


###################################################
### code chunk number 32: load-snp
###################################################
library(rtracklayer)
data(cpneTrack)


###################################################
### code chunk number 33: datavals-accessor
###################################################
head(score(cpneTrack))


###################################################
### code chunk number 34: trackData
###################################################
plot(start(cpneTrack), score(cpneTrack))


###################################################
### code chunk number 35: layTrack-snp (eval = FALSE)
###################################################
## session <- browserSession()
## session$cpne <- cpneTrack


###################################################
### code chunk number 36: browserView-snp (eval = FALSE)
###################################################
## view <- browserView(session, range(cpneTrack[1:5,]), full = "cpne")


###################################################
### code chunk number 37: layTrack-snp2 (eval = FALSE)
###################################################
## track(session, "cpne2", autoScale = FALSE, yLineOnOff = TRUE, 
##       yLineMark = quantile(score(cpneTrack), .25)) <- cpneTrack
## view <- browserView(session, range(cpneTrack[1:5,]), full = "cpne2")


###################################################
### code chunk number 38: search-nrsf
###################################################
library(BSgenome.Hsapiens.UCSC.hg19)
nrsfHits <- matchPattern("TCAGCACCATGGACAG", Hsapiens[["chr1"]])
length(nrsfHits)  # number of hits


###################################################
### code chunk number 39: track-nrsf
###################################################
nrsfTrack <- GenomicData(ranges(nrsfHits), strand="+", chrom="chr1",
                         genome = "hg19")


###################################################
### code chunk number 40: browserView-nrsf (eval = FALSE)
###################################################
## session <- browseGenome(nrsfTrack, range = range(nrsfTrack[1]) * -10)


###################################################
### code chunk number 41: rmsk.e2f3 (eval = FALSE)
###################################################
## library (rtracklayer)
## mySession = browserSession("UCSC")
## genome(mySession) <- "hg19"
## e2f3.tss.grange <- GRanges("chr6", IRanges(20400587, 20403336))
## tbl.rmsk <- getTable(
##    ucscTableQuery(mySession, track="rmsk", 
##                    range=e2f3.tss.grange, table="rmsk"))


###################################################
### code chunk number 42: uwDgfEncodeExample (eval = FALSE)
###################################################
## track.name <- "wgEncodeUwDgf"
## table.name <- "wgEncodeUwDgfK562Hotspots"
## e2f3.grange <- GRanges("chr6", IRanges(20400587, 20403336))
## mySession <- browserSession ()
## tbl.k562.dgf.e2f3 <- getTable(ucscTableQuery (mySession, track=track.name, 
##                               range=e2f3.grange, table=table.name))
## tbl.k562.dgf.hg19 <- getTable(ucscTableQuery (mySession, track=track.name, 
##                                               table=table.name))


###################################################
### code chunk number 43: trackAndTableNameDiscovery (eval = FALSE)
###################################################
## mySession <- browserSession ()
## genome(mySession) <- "hg19"
##   # 177 tracks in October 2012
## track.names <- trackNames(ucscTableQuery(mySession))
##   # chose a few tracks at random from this set, and discover how 
##   # many tables they hold
## tracks <- track.names [c (99, 81, 150, 96, 90)]
## sapply(tracks, function(track) {
##      length(tableNames(ucscTableQuery(mySession, track=track)))
##      })


###################################################
### code chunk number 44: session-info
###################################################
  sessionInfo()


