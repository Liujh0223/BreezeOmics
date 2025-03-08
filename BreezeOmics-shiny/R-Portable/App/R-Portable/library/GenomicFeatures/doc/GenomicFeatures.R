## ----loadGenomicFeatures------------------------------------------------------
suppressPackageStartupMessages(library('GenomicFeatures'))

## ----loadDb-------------------------------------------------------------------
samplefile <- system.file("extdata", "hg19_knownGene_sample.sqlite",
                          package="GenomicFeatures")
txdb <- loadDb(samplefile)
txdb

## ----loadPackage--------------------------------------------------------------
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene #shorthand (for convenience)
txdb

## ----seqlevels----------------------------------------------------------------
head(seqlevels(txdb))

## ----seqlevels2---------------------------------------------------------------
seqlevels(txdb) <- "chr1"

## ----seqlevels3---------------------------------------------------------------
seqlevels(txdb) <- seqlevels0(txdb)

## ----seqlevels4---------------------------------------------------------------
seqlevels(txdb) <- "chr15"
seqlevels(txdb)

## ----selectExample------------------------------------------------------------
keys <- c("100033416", "100033417", "100033420")
columns(txdb)
keytypes(txdb)
select(txdb, keys = keys, columns="TXNAME", keytype="GENEID")

## ----selectExercise-----------------------------------------------------------
columns(txdb)
cols <- c("TXNAME", "TXSTRAND", "TXCHROM")
select(txdb, keys=keys, columns=cols, keytype="GENEID")

## ----transcripts1-------------------------------------------------------------
GR <- transcripts(txdb)
GR[1:3]

## ----transcripts2-------------------------------------------------------------
tx_strand <- strand(GR)
tx_strand
sum(runLength(tx_strand))
length(GR)

## ----transcripts3-------------------------------------------------------------
GR <- transcripts(txdb, filter=list(tx_chrom = "chr15", tx_strand = "+"))
length(GR)
unique(strand(GR))

## ----transcripts4-------------------------------------------------------------
PR <- promoters(txdb, upstream=2000, downstream=400)
PR

## ----exonsExer1---------------------------------------------------------------
EX <- exons(txdb)
EX[1:4]
length(EX)
length(GR)

## ----transcriptsBy------------------------------------------------------------
GRList <- transcriptsBy(txdb, by = "gene")
length(GRList)
names(GRList)[10:13]
GRList[11:12]

## ----exonsBy------------------------------------------------------------------
GRList <- exonsBy(txdb, by = "tx")
length(GRList)
names(GRList)[10:13]
GRList[[12]]

## ----internalID---------------------------------------------------------------
GRList <- exonsBy(txdb, by = "tx")
tx_ids <- names(GRList)
head(select(txdb, keys=tx_ids, columns="TXNAME", keytype="TXID"))

## ----introns-UTRs-------------------------------------------------------------
length(intronsByTranscript(txdb))
length(fiveUTRsByTranscript(txdb))
length(threeUTRsByTranscript(txdb))

## ----extract------------------------------------------------------------------
library(BSgenome.Hsapiens.UCSC.hg19)
tx_seqs1 <- extractTranscriptSeqs(Hsapiens, TxDb.Hsapiens.UCSC.hg19.knownGene,
                                  use.names=TRUE)

## ----translate1---------------------------------------------------------------
suppressWarnings(translate(tx_seqs1))

## ----betterTranslation--------------------------------------------------------
cds_seqs <- extractTranscriptSeqs(Hsapiens,
                                  cdsBy(txdb, by="tx", use.names=TRUE))
translate(cds_seqs)

## ----supportedUCSCtables------------------------------------------------------
supportedUCSCtables(genome="mm9")

## ----makeTxDbFromUCSC, eval=FALSE---------------------------------------------
#  mm9KG_txdb <- makeTxDbFromUCSC(genome="mm9", tablename="knownGene")

## ----makeTxDbFromBiomart, eval=FALSE------------------------------------------
#  mmusculusEnsembl <- makeTxDbFromBiomart(dataset="mmusculus_gene_ensembl")

## ----saveDb-1, eval=FALSE-----------------------------------------------------
#  saveDb(mm9KG_txdb, file="fileName.sqlite")

## ----loadDb-1, eval=FALSE-----------------------------------------------------
#  mm9KG_txdb <- loadDb("fileName.sqlite")

## ----SessionInfo, echo=FALSE--------------------------------------------------
sessionInfo()

