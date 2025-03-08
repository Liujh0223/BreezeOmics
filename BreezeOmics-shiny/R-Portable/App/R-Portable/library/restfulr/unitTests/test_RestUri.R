### STRATEGY: access local example Solr instance
###           perform same operations as basic rsolr tests, but directly

test_RestUri_construction <- function() {
  test.uri <- "http://example.com"
  rc <- RestUri(test.uri)
  checkIdentical(as.character(rc), sub("/$", "", test.uri))
  checkIdentical(as.character(rc$sqlrest), paste0(test.uri, "/sqlrest"))
  checkIdentical(as.character(rc$"a space"),
                 paste0(test.uri, URLencode("/a space")))
  checkIdentical(as.character(rc[["a space"]]),
                 paste0(test.uri, URLencode("/a space")))
  checkException(RestUri(rep(test.uri, 2)), silent=TRUE)
  checkException(RestUri(NA), silent=TRUE)
  checkException(RestUri(test.uri, protocol=NULL), silent=TRUE)
}

test_RestUri_CRUD <- function() {
  solr <- rsolr::TestSolr()
  uri <- RestUri(solr$uri)

  id <- "1112211111"
  input <- list(id=id, name="my name!")
  create(uri$update$json, list(input))
  response <- read(uri$update, commit="true", wt="json")
  checkIdentical(response$responseHeader$status, 0)
  response2 <- read(uri$update, commit="true", wt="json")
  checkIdentical(response, response2)
  
  doc <- read(uri$select, q=paste0("id:", id), wt="json")
  checkIdentical(doc$response$docs[[1]][1:2], input)

  checkException(read(uri$sqlwork)) # 404
}
