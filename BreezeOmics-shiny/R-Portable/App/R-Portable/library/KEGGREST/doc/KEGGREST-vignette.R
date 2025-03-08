## ----setup, echo=FALSE--------------------------------------------------------
library(knitr)
options(width=80)

## ----wrap-hook, echo=FALSE----------------------------------------------------
hook_output = knit_hooks$get('output')
knit_hooks$set(output = function(x, options) {
  # this hook is used only when the linewidth option is not NULL
  if (!is.null(n <- options$linewidth)) {
    x = knitr:::split_lines(x)
    # any lines wider than n should be wrapped
    if (any(nchar(x) > n)) x = strwrap(x, width = n)
    x = paste(x, collapse = '\n')
  }
  hook_output(x, options)
})

## ----listDatabases------------------------------------------------------------
library(KEGGREST)
listDatabases()

## ----get_organisms------------------------------------------------------------
org <- keggList("organism")
head(org)

## ----list_queryables----------------------------------------------------------
queryables <- c(listDatabases(), org[,1], org[,2])

## ----query_hsa, eval=FALSE----------------------------------------------------
#  keggList("hsa")

## ----keggGet------------------------------------------------------------------
query <- keggGet(c("hsa:10458", "ece:Z5100"))

## ----querylength--------------------------------------------------------------
length(query)

## ----explore------------------------------------------------------------------
names(query[[1]])
query[[1]]$ENTRY
query[[1]]$DBLINKS

## ----aaseq--------------------------------------------------------------------
keggGet(c("hsa:10458", "ece:Z5100"), "aaseq") ## retrieves amino acid sequences

## ----ntseq--------------------------------------------------------------------
keggGet(c("hsa:10458", "ece:Z5100"), "ntseq") ## retrieves nucleotide sequences

## ----png----------------------------------------------------------------------
png <- keggGet("hsa05130", "image") 
t <- tempfile()
library(png)
writePNG(png, t)
if (interactive()) browseURL(t)

## ----separate_keywords, linewidth=80------------------------------------------
head(keggFind("genes", c("shiga", "toxin")))

## ----keyphrase, linewidth=80--------------------------------------------------
head(keggFind("genes", "shiga toxin"))

## ----formula------------------------------------------------------------------
head(keggFind("compound", "C7H10O5", "formula"))

## ----formula2-----------------------------------------------------------------
head(keggFind("compound", "O5C7", "formula"))

## ----exact_mass---------------------------------------------------------------
keggFind("compound", 174.05, "exact_mass")

## ----mol_weight---------------------------------------------------------------
head(keggFind("compound", 300:310, "mol_weight"))

## ----conv_with_ids------------------------------------------------------------
keggConv("ncbi-proteinid", c("hsa:10458", "ece:Z5100"))

## ----conv_species_kegg_to_geneid----------------------------------------------
head(keggConv("eco", "ncbi-geneid"))

## ----conv_species_geneid_to_kegg----------------------------------------------
head(keggConv("ncbi-geneid", "eco"))


## ----keggLink-----------------------------------------------------------------
head(keggLink("pathway", "hsa"))

## ----keggLink2----------------------------------------------------------------
keggLink("pathway", c("hsa:10458", "ece:Z5100"))

