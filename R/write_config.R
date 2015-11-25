#' Write a config file
#' 
#' @param config a config data.frame to write
#' @param file the file name/path to write to
#' @export
write_config <- function(config, file) {
  write.table(config, file=file, sep="\t", row.names=FALSE)
}