#' Open a file
#' 
#' Opens the specified file
#' 
#' @param path a file path
#' @export
view_file <- function(path) {
  switch(
    .Platform$OS.type,
    'windows'=suppressWarnings(shell(paste0("explorer ", path), intern=TRUE)), #http://stackoverflow.com/questions/11031317/open-windows-explorer-with-specific-path-using-system-command
    'unix'=system(paste0("open ", path))) #http://stackoverflow.com/questions/11780810/launch-mac-finder-window-with-specified-path
  invisible()
}

#' Open a folder or a file's folder
#' 
#' Opens the specified folder or the containing folder of the specified file
#' 
#' @param path a file path
#' @export
view_folder <- function(path) {
  is_dir <- dir.exists(path)
  if(is_dir) {
    view_file(path)
  } else {
    view_file(dirname(path))
  }
}
