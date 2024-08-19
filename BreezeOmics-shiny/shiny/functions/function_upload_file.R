prjpath <- function(classa){
  tmp_root_path <- "tmp/"
  if ( ! dir.exists(tmp_root_path)) {dir.create(tmp_root_path)}
  tmp_classa_path <- paste0(tmp_root_path, classa, "/")
  if ( ! dir.exists(tmp_classa_path)) {dir.create(tmp_classa_path)}
  dir <- strftime(Sys.time(), "%Y%m%d_%H_%M_%S")
  tmp_classb_path <- paste0(tmp_classa_path, dir, "/")
  if (dir.exists(paste0(tmp_classb_path, dir))) {unlink(paste0(tmp_classb_path, dir), recursive = T)}
      dir.create(tmp_classb_path)
  return(tmp_classb_path)
}