test_RestContainer_CRUD <- function() {
  solr <- rsolr::TestSolr()
  uri <- RestUri(solr$uri)

  id <- "1112211111"
  input <- list(id=id, name="my name!")
  container(uri$update$json)[] <- list(input)

  cont <- container(uri)
  response <- cont[["update", commit="true", wt="json"]]
  checkIdentical(response$responseHeader$status, 0)
  response2 <- cont[["update", commit="true", wt="json"]]
  checkIdentical(response, response2)
  
  doc <- cont[["select", q=paste0("id:", id), wt="json"]]
  checkIdentical(doc$response$docs[[1]][1:2], input)

  checkException(cont$sqlwork)
}
