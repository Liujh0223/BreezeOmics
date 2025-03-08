## ----setup, echo = FALSE--------------------------------------------------------------------------
knitr::opts_chunk$set(error = TRUE, cache = FALSE, eval = TRUE, out.width = "100%")
httr::set_config(httr::config(ssl_verifypeer = FALSE))
options(width=100)

## ----useEnsembl-----------------------------------------------------------------------------------
library(biomaRt)
ensembl <- useEnsembl(biomart = "genes", dataset = "hsapiens_gene_ensembl")

## ---- listEnsembl---------------------------------------------------------------------------------
listEnsembl()

## ----ensembl1-------------------------------------------------------------------------------------
ensembl <- useEnsembl(biomart = "genes")

## ----ensembl2-------------------------------------------------------------------------------------
ensembl

## ----listDatasets---------------------------------------------------------------------------------
datasets <- listDatasets(ensembl)
head(datasets)

## ----searchDatasets, echo = TRUE, eval = TRUE-----------------------------------------------------
searchDatasets(mart = ensembl, pattern = "hsapiens")

## ----ensembl3, eval=TRUE--------------------------------------------------------------------------
ensembl <- useDataset(dataset = "hsapiens_gene_ensembl", mart = ensembl)

## ----ensembl4, eval = FALSE-----------------------------------------------------------------------
#  ensembl <- useEnsembl(biomart = "genes", dataset = "hsapiens_gene_ensembl")

## ---- mirrors, eval = FALSE-----------------------------------------------------------------------
#  ensembl <- useEnsembl(biomart = "ensembl",
#                     dataset = "hsapiens_gene_ensembl",
#                     mirror = "useast")

## ----archiveMarts, echo = TRUE, eval = TRUE-------------------------------------------------------
listEnsemblArchives()

## ----archiveMarts3, echo = TRUE, eval = TRUE------------------------------------------------------
listEnsembl(version = 95)
ensembl95 <- useEnsembl(biomart = 'genes', 
                       dataset = 'hsapiens_gene_ensembl',
                       version = 95)

## ----listEnsemblGenomes---------------------------------------------------------------------------
listEnsemblGenomes()

## ---- plants1-------------------------------------------------------------------------------------
ensembl_plants <- useEnsemblGenomes(biomart = "plants_mart")
searchDatasets(ensembl_plants, pattern = "Arabidopsis")

## -------------------------------------------------------------------------------------------------
ensembl_arabidopsis <- useEnsemblGenomes(biomart = "plants_mart", 
                                         dataset = "athaliana_eg_gene")

## ----filters--------------------------------------------------------------------------------------
filters = listFilters(ensembl)
filters[1:5,]

## ----attributes-----------------------------------------------------------------------------------
attributes = listAttributes(ensembl)
attributes[1:5,]

## ----getBM1, echo=TRUE, eval=TRUE-----------------------------------------------------------------
affyids <- c("202763_at","209310_s_at","207500_at")
getBM(attributes = c('affy_hg_u133_plus_2', 'entrezgene_id'),
      filters = 'affy_hg_u133_plus_2',
      values = affyids, 
      mart = ensembl)

## ----searchAttributes, echo = TRUE, eval = TRUE---------------------------------------------------
searchAttributes(mart = ensembl, pattern = "hgnc")

## ----searchFilters, echo = TRUE, eval = TRUE------------------------------------------------------
searchFilters(mart = ensembl, pattern = "ensembl.*id")

## ----filtervalues, fig.cap='The options available to the Chromosome/Scaffold field are limited to a pretermined list based on the values in this dataset.', echo = FALSE----
knitr::include_graphics('filtervalues.png')

## ----chromosomeNames, results = FALSE-------------------------------------------------------------
listFilterOptions(mart = ensembl, filter = "chromosome_name")

## ----listFilterOptions, results = TRUE------------------------------------------------------------
searchFilterOptions(mart = ensembl, filter = "chromosome_name", 
                    pattern = "^GL")
searchFilterOptions(mart = ensembl, filter = "phenotype_description", 
                    pattern = "Crohn")

## ----filterType-----------------------------------------------------------------------------------
filterType("with_affy_hg_u133_plus_2", ensembl)

## ----attributePages-------------------------------------------------------------------------------
pages = attributePages(ensembl)
pages

## ----listAttributes-------------------------------------------------------------------------------
head(listAttributes(ensembl, page="feature_page"))

## ----columnsAndKeyTypes---------------------------------------------------------------------------
mart <- useEnsembl(dataset = "hsapiens_gene_ensembl", biomart='ensembl')
head(keytypes(mart), n = 3)
head(columns(mart), n = 3)

## ----keys1----------------------------------------------------------------------------------------
k = keys(mart, keytype="chromosome_name")
head(k, n=3)

## ----keys2----------------------------------------------------------------------------------------
k = keys(mart, keytype="chromosome_name", pattern="LRG")
head(k, n=3)

## ----select---------------------------------------------------------------------------------------
affy=c("202763_at","209310_s_at","207500_at")
select(mart, keys=affy, columns=c('affy_hg_u133_plus_2','entrezgene_id'),
  keytype='affy_hg_u133_plus_2')

## ----cacheInfo------------------------------------------------------------------------------------
biomartCacheInfo()

## ----cache-location, echo=1:2---------------------------------------------------------------------
Sys.setenv(BIOMART_CACHE = tempdir())
biomartCacheInfo()
Sys.unsetenv("BIOMART_CACHE")

## ----task1, echo=TRUE,eval=TRUE-------------------------------------------------------------------
affyids=c("202763_at","209310_s_at","207500_at")
getBM(attributes = c('affy_hg_u133_plus_2', 'hgnc_symbol', 'chromosome_name',
                   'start_position', 'end_position', 'band'),
      filters = 'affy_hg_u133_plus_2', 
      values = affyids, 
      mart = ensembl)

## ----task2, echo=TRUE,eval=TRUE-------------------------------------------------------------------
entrez=c("673","837")
goids = getBM(attributes = c('entrezgene_id', 'go_id'), 
              filters = 'entrezgene_id', 
              values = entrez, 
              mart = ensembl)
head(goids)

## ----task3, echo=TRUE,eval=TRUE-------------------------------------------------------------------
 go=c("GO:0051330","GO:0000080","GO:0000114","GO:0000082")
 chrom=c(17,20,"Y")
 getBM(attributes= "hgnc_symbol",
        filters=c("go","chromosome_name"),
        values=list(go, chrom), mart=ensembl)

## ----task4, echo=TRUE,eval=TRUE-------------------------------------------------------------------
refseqids = c("NM_005359","NM_000546")
ipro = getBM(attributes=c("refseq_mrna","interpro","interpro_description"), 
             filters="refseq_mrna",
             values=refseqids, 
             mart=ensembl)
ipro

## ----task5, eval = TRUE---------------------------------------------------------------------------
getBM(attributes = c('affy_hg_u133_plus_2','ensembl_gene_id'), 
      filters = c('chromosome_name','start','end'),
      values = list(16,1100000,1250000), 
      mart = ensembl)

## ----task6, echo=TRUE, eval = TRUE----------------------------------------------------------------
getBM(attributes = c('entrezgene_id','hgnc_symbol'), 
      filters = 'go', 
      values = 'GO:0004707', 
      mart = ensembl)

## ----task7, eval=TRUE-----------------------------------------------------------------------------
entrez=c("673","7157","837")
getSequence(id = entrez, 
            type="entrezgene_id",
            seqType="coding_gene_flank",
            upstream=100, 
            mart=ensembl) 

## ----task8, echo=TRUE,eval=TRUE-------------------------------------------------------------------
utr5 = getSequence(chromosome=3, start=185514033, end=185535839,
                   type="entrezgene_id",
                   seqType="5utr", 
                   mart=ensembl)
utr5

## ----task9, echo=TRUE, eval=TRUE------------------------------------------------------------------
protein = getSequence(id=c(100, 5728),
                      type="entrezgene_id",
                      seqType="peptide", 
                      mart=ensembl)
protein

## ----task10, echo=TRUE, eval=TRUE-----------------------------------------------------------------
snpmart = useEnsembl(biomart = "snp", dataset="hsapiens_snp")

## ----task10b--------------------------------------------------------------------------------------
getBM(attributes = c('refsnp_id','allele','chrom_start','chrom_strand'), 
      filters = c('chr_name','start','end'), 
      values = list(8, 148350, 148420), 
      mart = snpmart)

## ----getLDS, cache = TRUE-------------------------------------------------------------------------
human <- useEnsembl("ensembl", dataset = "hsapiens_gene_ensembl")
mouse <- useEnsembl("ensembl", dataset = "mmusculus_gene_ensembl")
getLDS(attributes = c("hgnc_symbol","chromosome_name", "start_position"),
       filters = "hgnc_symbol", values = "TP53",
       mart = human,
       attributesL = c("refseq_mrna","chromosome_name","start_position"), 
       martL = mouse)

## ---- ssl-verifypeer, eval = FALSE----------------------------------------------------------------
#  httr::set_config(httr::config(ssl_verifypeer = FALSE))

## ---- ssl-cipher-list, eval = FALSE---------------------------------------------------------------
#  httr::set_config(httr::config(ssl_cipher_list = "DEFAULT@SECLEVEL=1"))

## ----sessionInfo----------------------------------------------------------------------------------
sessionInfo()

