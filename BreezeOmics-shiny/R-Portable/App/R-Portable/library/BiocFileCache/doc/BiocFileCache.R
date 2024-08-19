## ----setup, echo=FALSE--------------------------------------------------------
knitr::opts_chunk$set(collapse=TRUE)

## ---- eval = FALSE------------------------------------------------------------
#  if (!"BiocManager" %in% rownames(installed.packages()))
#       install.packages("BiocManager")
#  BiocManager::install("BiocFileCache", dependencies=TRUE)

## ---- results='hide', warning=FALSE, message=FALSE----------------------------
library(BiocFileCache)

## -----------------------------------------------------------------------------
path <- tempfile()
bfc <- BiocFileCache(path, ask = FALSE)

## -----------------------------------------------------------------------------
bfccache(bfc)
length(bfc)

## -----------------------------------------------------------------------------
bfc

## -----------------------------------------------------------------------------
bfcinfo(bfc)

## -----------------------------------------------------------------------------
savepath <- bfcnew(bfc, "NewResource", ext=".RData")
savepath

## now we can use that path in any save function
m = matrix(1:12, nrow=3)
save(m, file=savepath)

## and that file will be tracked in the cache
bfcinfo(bfc)

## -----------------------------------------------------------------------------
fl1 <- tempfile(); file.create(fl1)
add2 <- bfcadd(bfc, "Test_addCopy", fl1)                 # copy
# returns filepath being tracked in cache
add2
# the name is the unique rid in the cache
rid2 <- names(add2)

fl2 <- tempfile(); file.create(fl2)
add3 <- bfcadd(bfc, "Test2_addMove", fl2, action="move") # move
rid3 <- names(add3)

fl3 <- tempfile(); file.create(fl3)
add4 <- bfcadd(bfc, "Test3_addAsis", fl3, rtype="local",
	       action="asis") # reference
rid4 <- names(add4)

file.exists(fl1)    # TRUE - copied from original location
file.exists(fl2)    # FALSE - moved from original location
file.exists(fl3)    # TRUE - left asis, original location tracked

## -----------------------------------------------------------------------------
url <- "http://httpbin.org/get"
add5 <- bfcadd(bfc, "TestWeb", fpath=url)
rid5 <- names(add5)

url2<- "https://en.wikipedia.org/wiki/Bioconductor"
add6 <- bfcadd(bfc, "TestWeb", fpath=url2)
rid6 <- names(add6)

# add a remote resource but don't initially download
add7 <- bfcadd(bfc, "TestNoDweb", fpath=url2, download=FALSE)
rid7 <- names(add7)
# let's look at our BiocFileCache object now
bfc
bfcinfo(bfc)

## -----------------------------------------------------------------------------
bfcquery(bfc, "Web")

bfcquery(bfc, "copy")

q1 <- bfcquery(bfc, "wiki")
q1
class(q1)

## -----------------------------------------------------------------------------
bfccount(q1)

## -----------------------------------------------------------------------------
bfcsubWeb = bfc[paste0("BFC", 5:6)]
bfcsubWeb
bfcinfo(bfcsubWeb)

## -----------------------------------------------------------------------------
bfc[["BFC2"]]
bfcpath(bfc, "BFC2")
bfcpath(bfc, "BFC5")
bfcrpath(bfc, rids="BFC5")
bfcrpath(bfc)
bfcrpath(bfc, c("http://httpbin.org/get","Test3_addAsis"))

## -----------------------------------------------------------------------------
bfcneedsupdate(bfc, "BFC5")
bfcneedsupdate(bfc, "BFC6")
bfcneedsupdate(bfc)

## -----------------------------------------------------------------------------
fileBeingReplaced <- bfc[[rid3]]
fileBeingReplaced

# fl3 was created when we were adding resources
fl3

bfc[[rid3]]<-fl3
bfc[[rid3]]

## -----------------------------------------------------------------------------
bfcinfo(bfc, "BFC1")
bfcupdate(bfc, "BFC1", rname="FirstEntry")
bfcinfo(bfc, "BFC1")

## -----------------------------------------------------------------------------
suppressPackageStartupMessages({
    library(dplyr)
})
bfcinfo(bfc, "BFC6") %>% select(rid, rpath, fpath)
bfcupdate(bfc, "BFC6", fpath=url, rname="Duplicate", ask=FALSE)
bfcinfo(bfc, "BFC6") %>% select(rid, rpath, fpath)

## -----------------------------------------------------------------------------
rid <- "BFC5"
test <- !identical(bfcneedsupdate(bfc, rid), FALSE) # 'TRUE' or 'NA'
if (test)
    bfcdownload(bfc, rid, ask=FALSE)

## -----------------------------------------------------------------------------
names(bfcinfo(bfc))
meta <- as.data.frame(list(rid=bfcrid(bfc)[1:3], idx=1:3))
bfcmeta(bfc, name="resourceData") <- meta
names(bfcinfo(bfc))

## -----------------------------------------------------------------------------
bfcmetalist(bfc)
bfcmeta(bfc, name="resourceData")

## -----------------------------------------------------------------------------
bfcmetaremove(bfc, name="resourceData")

## ----eval=FALSE---------------------------------------------------------------
#  bfcmeta(name="resourceData") <- meta
#  Error in bfcmeta(name = "resourceData") <- meta :
#    target of assignment expands to non-language object

## ----eval=FALSE---------------------------------------------------------------
#  bfc <- BiocFileCache()
#  bfcmeta(bfc, name="resourceData") <- meta

## -----------------------------------------------------------------------------
# let's remind ourselves of our object
bfc

bfcremove(bfc, "BFC6")
bfcremove(bfc, "BFC1")

# let's look at our BiocFileCache object now
bfc

## -----------------------------------------------------------------------------
# create a new entry that hasn't been used
path <- bfcnew(bfc, "UseMe")
rmMe <- names(path)
# We also have a file not being tracked because we updated rpath

bfcsync(bfc)

# you can suppress the messages and just have a TRUE/FALSE
bfcsync(bfc, FALSE)

#
# Let's do some cleaning to have a synced object
#
bfcremove(bfc, rmMe)
unlink(fileBeingReplaced)

bfcsync(bfc)

## ----eval=FALSE---------------------------------------------------------------
#  # export entire biocfilecache
#  exportbfc(bfc)
#  
#  # export the first 4 entries of biocfilecache
#  # as a compressed tar
#  exportbfc(bfc, rids=paste0("BFC", 1:4),
#  	  outputFile="BiocFileCacheExport.tar.gz", compression="gzip")
#  
#  # export the subsetted object of web resources as zip
#  sub1 <- bfc[bfcrid(bfcquery(bfc, "web", field='rtype'))]
#  exportbfc(sub1, outputFile = "BiocFileCacheExportWeb.zip",
#  	  outMethod="zip")

## ----eval=FALSE---------------------------------------------------------------
#  
#  bfc <- importbfc("BiocFileCacheExport.tar")
#  
#  bfc2 <- importbfc("BiocFileCacheExport.tar.gz", compression="gzip")
#  
#  bfc3 <- importbfc("BiocFileCacheExportWeb.zip", archiveMethod="unzip")

## -----------------------------------------------------------------------------
tbl <- data.frame(rtype=c("web","web"),
		      rpath=c(NA_character_,NA_character_),
		  fpath=c("http://httpbin.org/get",
			  "https://en.wikipedia.org/wiki/Bioconductor"),
		      keywords = c("httpbin", "wiki"), stringsAsFactors=FALSE)
tbl

## ----eval=FALSE---------------------------------------------------------------
#  
#  newbfc <- makeBiocFileCacheFromDataFrame(tbl,
#  					 cache=file.path(tempdir(),"BFC"),
#  					 actionWeb="copy",
#  					 actionLocal="copy",
#  					 metadataName="resourceMetadata")
#  

## ----eval=FALSE---------------------------------------------------------------
#  cleanbfc(bfc)

## ----eval=FALSE---------------------------------------------------------------
#  removebfc(bfc)

## -----------------------------------------------------------------------------
## paste to avoid long line in vignette
url <- paste(
    "ftp://ftp.ensembl.org/pub/release-71/gtf",
    "homo_sapiens/Homo_sapiens.GRCh37.71.gtf.gz",
    sep="/")

## ---- eval=FALSE--------------------------------------------------------------
#  library(BiocFileCache)
#  bfc <- BiocFileCache()
#  path <- bfcrpath(bfc, url)

## ---- eval=FALSE--------------------------------------------------------------
#  gtf <- rtracklayer::import.gff(path)

## ---- eval=FALSE--------------------------------------------------------------
#  gtf <- rtracklayer::import.gff(bfcrpath(BiocFileCache(), url))

## ---- eval=FALSE--------------------------------------------------------------
#  library(BiocFileCache)
#  bfc <- BiocFileCache("~/my-experiment/results")

## ---- eval=FALSE--------------------------------------------------------------
#  suppressPackageStartupMessages({
#      library(DESeq2)
#      library(airway)
#  })
#  data(airway)
#  dds <- DESeqDataData(airway, design = ~ cell + dex)
#  result <- DESeq(dds)

## ---- eval=FALSE--------------------------------------------------------------
#  saveRDS(result, bfcnew(bfc, "airway / DESeq standard analysis"))

## ---- eval=FALSE--------------------------------------------------------------
#  result <- readRDS(bfcrpath(bfc, "airway / DESeq standard analysis"))

## ----eval=FALSE---------------------------------------------------------------
#  suppressPackageStartupMessages({
#      library(BiocFileCache)
#      library(rtracklayer)
#  })
#  
#  # load the cache
#  path <- file.path(tempdir(), "tempCacheDir")
#  bfc <- BiocFileCache(path)
#  
#  # the web resource of interest
#  url <- "ftp://ftp.ensembl.org/pub/release-71/gtf/homo_sapiens/Homo_sapiens.GRCh37.71.gtf.gz"
#  
#  # check if url is being tracked
#  res <- bfcquery(bfc, url)
#  
#  if (bfccount(res) == 0L) {
#  
#      # if it is not in cache, add
#      ans <- bfcadd(bfc, rname="ensembl, homo sapien", fpath=url)
#  
#  } else {
#  
#    # if it is in cache, get path to load
#    rid = res %>% filter(fpath == url) %>% collect(Inf) %>% `[[`("rid")
#    ans <- bfcrpath(bfc, rid)
#  
#    # check to see if the resource needs to be updated
#    check <- bfcneedsupdate(bfc, rid)
#    # check can be NA if it cannot be determined, choose how to handle
#    if (is.na(check)) check <- TRUE
#    if (check){
#      ans < - bfcdownload(bfc, rid)
#    }
#  }
#  
#  # ans is the path of the file to load
#  ans
#  
#  # we know because we search for the url that the file is a .gtf.gz,
#  # if we searched on other terms we can use 'bfcpath' to see the
#  # original fpath to know the appropriate load/read/import method
#  bfcpath(bfc, names(ans))
#  
#  temp = GTFFile(ans)
#  info = import(temp)

## ----eval=TRUE----------------------------------------------------------------

#
# A simplier test to see if something is in the cache
# and if not start tracking it is using `bfcrpath`
#

suppressPackageStartupMessages({
    library(BiocFileCache)
    library(rtracklayer)
})

# load the cache
path <- file.path(tempdir(), "tempCacheDir")
bfc <- BiocFileCache(path, ask=FALSE)

# the web resources of interest
url <- "ftp://ftp.ensembl.org/pub/release-71/gtf/homo_sapiens/Homo_sapiens.GRCh37.71.gtf.gz"

url2 <- "ftp://ftp.ensembl.org/pub/release-71/gtf/rattus_norvegicus/Rattus_norvegicus.Rnor_5.0.71.gtf.gz"

# if not in cache will download and create new entry
pathsToLoad <- bfcrpath(bfc, c(url, url2))

pathsToLoad

# now load files as see fit
info = import(GTFFile(pathsToLoad[1]))
class(info)
summary(info)

## ----eval=FALSE---------------------------------------------------------------
#  #
#  # One could also imagine the following:
#  #
#  
#  library(BiocFileCache)
#  
#  # load the cache
#  bfc <- BiocFileCache()
#  
#  #
#  # Do some work!
#  #
#  
#  # add a location in the cache
#  filepath <- bfcnew(bfc, "R workspace")
#  
#  save(list = ls(), file=filepath)
#  
#  # now the R workspace is being tracked in the cache

## ---- eval=FALSE--------------------------------------------------------------
#  .get_cache <-
#      function()
#  {
#      cache <- tools::R_user_dir("MyNewPackage", which="cache")
#      BiocFileCache::BiocFileCache(cache)
#  }

## ---- eval=FALSE--------------------------------------------------------------
#  download_data_file <-
#      function( verbose = FALSE )
#  {
#      fileURL <- "http://a_path_to/someremotefile.tsv.gz"
#  
#      bfc <- .get_cache()
#      rid <- bfcquery(bfc, "geneFileV2", "rname")$rid
#      if (!length(rid)) {
#  	 if( verbose )
#  	     message( "Downloading GENE file" )
#  	 rid <- names(bfcadd(bfc, "geneFileV2", fileURL ))
#      }
#      if (!isFALSE(bfcneedsupdate(bfc, rid)))
#  	bfcdownload(bfc, rid)
#  
#      bfcrpath(bfc, rids = rid)
#  }

## -----------------------------------------------------------------------------
url <- "http://bioconductor.org/packages/stats/bioc/BiocFileCache/BiocFileCache_stats.tab"

headFile <-                         # how to process file before caching
    function(from, to)
{
    dat <- readLines(from)
    writeLines(head(dat), to)
    TRUE
}

rid <- bfcquery(bfc, url, "fpath")$rid
if (!length(rid))                   # not in cache, add but do not download
    rid <- names(bfcadd(bfc, url, download = FALSE))

update <- bfcneedsupdate(bfc, rid)  # TRUE if newly added or stale
if (!isFALSE(update))               # download & process
    bfcdownload(bfc, rid, ask = FALSE, FUN = headFile)

rpath <- bfcrpath(bfc, rids=rid)    # path to processed result
readLines(rpath)                    # read processed result

## ---- eval=FALSE--------------------------------------------------------------
#         # make sure you have permissions on the cache/files
#         # use at own risk
#  
#  	moveFiles<-function(package){
#  	    olddir <- path.expand(rappdirs::user_cache_dir(appname=package))
#  	    newdir <- tools::R_user_dir(package, which="cache")
#  	    dir.create(path=newdir, recursive=TRUE)
#  	    files <- list.files(olddir, full.names =TRUE)
#  	    moveres <- vapply(files,
#  		FUN=function(fl){
#  		  filename = basename(fl)
#  		  newname = file.path(newdir, filename)
#  		  file.rename(fl, newname)
#  		},
#  		FUN.VALUE = logical(1))
#  	    if(all(moveres)) unlink(olddir, recursive=TRUE)
#  	}
#  
#  
#  	package="BiocFileCache"
#  	moveFiles(package)
#  

## ---- eval=FALSE--------------------------------------------------------------
#  library(BiocFileCache)
#  
#  
#  package = "BiocFileCache"
#  
#  BFC_CACHE = rappdirs::user_cache_dir(appname=package)
#  Sys.setenv(BFC_CACHE = BFC_CACHE)
#  bfc = BiocFileCache(BFC_CACHE)
#  ## CAUTION: This removes the cache and all downloaded resources
#  removebfc(bfc, ask=FALSE)
#  
#  ## create new empty cache in new default location
#  bfc = BiocFileCache(ask=FALSE)
#  

## -----------------------------------------------------------------------------
sessionInfo()


