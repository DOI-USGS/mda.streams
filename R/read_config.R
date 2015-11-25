#' Read a config file
#' 
#' @param file a config filename or path/filename
#' @return config data.frame
#' @export
read_config <- function(file) {
  read.table(file, sep="\t", header=TRUE, stringsAsFactors=FALSE)
}