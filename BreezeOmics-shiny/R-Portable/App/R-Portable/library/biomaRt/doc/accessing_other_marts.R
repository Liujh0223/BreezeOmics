## ----setup, echo = FALSE--------------------------------------------------------------------------
knitr::opts_chunk$set(error = TRUE, cache = FALSE, eval = TRUE)
options(width=100)

## -------------------------------------------------------------------------------------------------
library(biomaRt)

## ----test-wormbase-ssl, echo = FALSE--------------------------------------------------------------
if(grepl(try(httr::GET('https://parasite.wormbase.org'), silent = TRUE)[1], 
         pattern = "sslv3 alert handshake")) {
    httr::set_config(httr::config(ssl_cipher_list = "DEFAULT@SECLEVEL=1"))  
}

## ----wormbase, echo=TRUE, eval=TRUE---------------------------------------------------------------
listMarts(host = "parasite.wormbase.org")
wormbase <- useMart(biomart = "parasite_mart", 
                    host = "https://parasite.wormbase.org", 
                    port = 443)

## ----wormbase-2, echo=TRUE, eval=TRUE-------------------------------------------------------------
listDatasets(wormbase)
wormbase <- useDataset(mart = wormbase, dataset = "wbps_gene")
head(listFilters(wormbase))
head(listAttributes(wormbase))
getBM(attributes = c("external_gene_id", "wbps_transcript_id", "transcript_biotype"), 
      filters = "gene_name", 
      values = c("unc-26","his-33"), 
      mart = wormbase)
     

## ---- phytozome-13, echo = TRUE, eval = TRUE------------------------------------------------------
phytozome_v13 <- useMart(biomart = "phytozome_mart", 
                dataset = "phytozome", 
                host = "https://phytozome-next.jgi.doe.gov")

## ---- pytozome-2----------------------------------------------------------------------------------
getBM(attributes = c("organism_name", "gene_name1"), 
      filters = "gene_name_filter", 
      values = "82092", 
      mart = phytozome_v13)

## ----sessionInfo----------------------------------------------------------------------------------
sessionInfo()
warnings()

