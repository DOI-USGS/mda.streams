#' Download and read a config file from a metab run on SB
#' 
#' @param title the metab run title. See options using \code{list_metab_runs()}
#' @param config_file the name of the config file. It's usually but not always 
#'   'config.tsv'; use \code{list_metab_run_files(title)} to check
#' @param on_local_exists character indicating what to do if the folder already 
#'   contains a file with the intended download name
#' @export
get_config <- function(title, config_file="config.tsv", on_local_exists='skip') {
  file <- download_metab_run(title, files=config_file, on_local_exists=on_local_exists)
  read.table(file, sep="\t", header=TRUE, colClasses='character')
}