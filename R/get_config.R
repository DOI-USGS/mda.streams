#' Download and read a config file from a metab run on SB
#' 
#' @param title the metab run title. See options using \code{list_metab_runs()}
#' @param config_file the name of the config file. It's usually but not always 
#'   'config.tsv'; use \code{list_metab_run_files(title)} to check
#' @param on_local_exists character indicating what to do if the folder already 
#'   contains a file with the intended download name
#' @export
#' @importFrom utils read.table
get_config <- function(title, config_file=grep('config', list_metab_run_files(title), value=TRUE)[1], on_local_exists='skip') {
  file <- download_metab_run(title, files=list(config_file), on_local_exists=on_local_exists)
  file <- file[substring(file, nchar(file)-3) == '.tsv']
  if(length(file) > 1) stop("couldn't narrow file list down to one config file; specify config_file explicitly")
  read_config(file)
}